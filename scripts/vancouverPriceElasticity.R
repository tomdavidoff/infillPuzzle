# vancouverDuplexElasticity.R
# Legacy (pre-2018-reform) duplex vs SFD price per sqft by area,
# against the area's SFD size elasticity. Augments vancouverPriceElasticity.R
# Tom Davidoff
# 11/7/26

library(sf)
library(DBI)
library(RSQLite)
library(data.table)
library(fixest)

yrs     <- 2014:2018   # sale window; every duplex sold here is pre-reform stock
MAXAGE  <- 30          # effective-age cap, applied to both products
GEO     <- "CTNAME"    # or "neighbourhoodDescription" (BCA neighbourhood)
NDUPZ   <- 5           # duplexes: keep the top-NDUPZ zoning districts by duplex sales; SFD: R1 only
MIN_SFD <- 20          # min SFD sales (33/50 ft) per area
MIN_DUP <- 10          # min duplex sales per area
split   <- FALSE       # TRUE: SFD level and elasticity from disjoint folios

# ---- sales: SFD + anything "uplex" ------------------------------------------
con <- dbConnect(SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")
q <- "SELECT s.conveyancePrice, s.conveyanceDate, f.rollNumber, i.MB_effective_year,
  i.MB_total_finished_area, i.land_width, i.land_depth, d.actualUseDescription, d.neighbourhoodDescription
FROM folio f
JOIN sales s ON s.folioID = f.folioID
JOIN folioDescription d ON d.folioID = f.folioID
JOIN residentialInventory i ON i.roll_number = f.rollNumber
WHERE f.jurisdictionCode = '200'
  AND s.conveyanceTypeDescription = 'Improved Single Property Transaction'
  AND (d.actualUseDescription IN ('Residential Dwelling with Suite', 'Single Family Dwelling')
       OR d.actualUseDescription LIKE '%uplex%')"
df <- as.data.table(dbGetQuery(con, q))
dbDisconnect(con)
print(table(df$actualUseDescription))

num <- c("conveyancePrice", "MB_total_finished_area", "land_width", "land_depth", "MB_effective_year")
df[, (num) := lapply(.SD, as.numeric), .SDcols = num]
df[, convDate := as.IDate(conveyanceDate)]
df <- df[year(convDate) %in% yrs]
df[, rollStart := floor(as.numeric(rollNumber) / 1000)]
df <- df[!grepl("^2 Acres", actualUseDescription)]   # large estates, not duplexes
df[, dup  := grepl("uplex", actualUseDescription, ignore.case = TRUE)]
df[, prod := fifelse(dup, "DUP", "SFD")]

# area/price by duplex description (diagnostic only)
print(df[dup == TRUE, .(medArea = median(MB_total_finished_area, na.rm = TRUE),
                        medPrice = median(conveyancePrice, na.rm = TRUE), .N),
         keyby = actualUseDescription])

# ---- geography: parcel points -> zoning (all districts) and tract ------------
ct <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp", quiet = TRUE)
ct <- st_transform(st_make_valid(ct[ct$PRUID == "59", "CTNAME"]), 3005)

gpkg <- list.files("~/bigFiles/latestSpatialBCA", pattern = "\\.gpkg$", full.names = TRUE)[1]
g <- st_read(gpkg, quiet = TRUE, query =
  "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
g <- g[!st_is_empty(g), ]
g$rollStart <- floor(as.numeric(g$ROLL_NUMBER) / 1000)
g <- g[g$rollStart %in% df$rollStart & !duplicated(g$rollStart), "rollStart"]
gPt <- st_point_on_surface(st_make_valid(st_transform(g, 3005)))

zn <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson", quiet = TRUE)
zn <- st_make_valid(st_transform(zn[, c("zoning_district", "zoning_category")], 3005))
ctV <- ct[lengths(st_intersects(ct, st_as_sfc(st_bbox(gPt)))) > 0, ]

gPt <- st_join(gPt, zn,  join = st_intersects)
gPt <- st_join(gPt, ctV, join = st_intersects)
gPt <- gPt[!duplicated(gPt$rollStart), ]
df <- merge(df, as.data.table(st_drop_geometry(gPt)), by = "rollStart", all.x = TRUE)

# where do legacy duplex sales sit, by current zoning? (before R1 restriction)
zt <- dcast(df[, .N, by = .(zoning_district, prod)], zoning_district ~ prod,
            value.var = "N", fill = 0)
print(zt[order(-DUP)])

dupZones <- head(zt[order(-DUP), zoning_district], NDUPZ)
print(dupZones)
df <- df[(prod == "SFD" & zoning_category %in% "R1") |
         (prod == "DUP" & zoning_district %in% dupZones)]
df[, gid := get(GEO)]
df <- df[!is.na(gid)]

# duplex prevalence: share of all SFD+duplex sales in the area
shr <- df[, .(nSFDall = sum(!dup), nDUPall = sum(dup)), by = gid]
shr[, dupShare := nDUPall / (nSFDall + nDUPall)]

# ---- common cleaning ---------------------------------------------------------
df <- df[conveyancePrice > 0 & MB_total_finished_area > 0]
df[, `:=`(lsqft     = log(MB_total_finished_area),
          lage      = log(pmax(year(convDate) - MB_effective_year, 0) + 1),
          convMonth = format(convDate, "%Y-%m"))]
df <- df[!is.na(lage) & lage <= log(MAXAGE + 1)]

# SFD: lot-dimension filters as in vancouverPriceElasticity.R
sfd <- df[prod == "SFD"]
sfd[land_width == 0, land_width := NA]
sfd[land_depth == 0, land_depth := NA]
sfd <- sfd[!is.na(land_depth) & !is.na(land_width)]
qc  <- quantile(sfd$land_depth, c(0.2, 0.8))
sfd <- sfd[land_depth %between% qc]
sfd[, w := round(land_width)]
sfd[w > 47 & w < 53, w := 50]
sfd <- sfd[w %in% c(33, 50)]
sfd[, N := .N, by = gid]
sfd <- sfd[N >= MIN_SFD]

# duplex: anything with "uplex" in actualUseDescription; no lot filter
du <- df[prod == "DUP"]

sA <- if (split) sfd[rollStart %% 2 == 0] else sfd
sB <- if (split) sfd[rollStart %% 2 == 1] else sfd

# ---- SFD elasticity: area-specific slope on log sqft, area x width FE -------
m2 <- feols(log(conveyancePrice) ~ i(gid, lsqft) + lage | gid^w + convMonth, sB)
k  <- grep("^gid::", names(coef(m2)))
el <- data.table(gid  = sub("^gid::(.*):lsqft$", "\\1", names(coef(m2))[k]),
                 elas = coef(m2)[k], seEl = se(m2)[k])

# ---- levels: area x product FE in log ppsf, one pooled hedonic ---------------
# unconditional on lot size
lv <- rbind(sA, du, fill = TRUE)
lv[, gp := paste(gid, prod, sep = "|")]
m1 <- feols(log(conveyancePrice / MB_total_finished_area) ~ lage | gp + convMonth, lv)
fe <- fixef(m1)$gp
lev <- data.table(gp = names(fe), lppsf = as.numeric(fe))
lev[, c("gid", "prod") := tstrsplit(gp, "|", fixed = TRUE)]
lev <- dcast(lev, gid ~ prod, value.var = "lppsf")
setnames(lev, c("DUP", "SFD"), c("lppsfDUP", "lppsfSFD"))
lev[, gap := lppsfDUP - lppsfSFD]

cnt <- lv[, .(nSFD = sum(prod == "SFD"), nDUP = sum(prod == "DUP")), by = gid]

out <- Reduce(function(a, b) merge(a, b, by = "gid"), list(lev, el, cnt, shr))
print(table(cut(out$nDUP, c(-1, 0, 4, 9, 19, 49, Inf))))   # support: duplex sales per area
out <- out[nDUP >= MIN_DUP & !is.na(gap)]
if (nrow(out) < 5) stop("Fewer than 5 areas with >= MIN_DUP duplex sales; raise NDUPZ or loosen MIN_DUP / MAXAGE")
print(out[order(-dupShare)])

# ---- does SFD elasticity predict the duplex - SFD gap? -----------------------
print(out[, .(corGapElas = cor(gap, elas), corGapLevel = cor(gap, lppsfSFD),
              corElasLevel = cor(elas, lppsfSFD), nAreas = .N)])
hi <- out[dupShare >= median(dupShare)]
print(etable(
  feols(gap ~ elas, out),
  feols(gap ~ elas, out, weights = ~nDUP),
  feols(gap ~ elas + lppsfSFD, out, weights = ~nDUP),   # elasticity vs level horse race
  feols(gap ~ elas, hi, weights = ~nDUP),               # high-duplex-share half
  vcov = "hetero",
  headers = c("unwtd", "wt nDUP", "+ SFD level", "dupShare >= med")))

fit <- feols(gap ~ elas, out, weights = ~nDUP)
drawGap <- function() {
  plot(out$elas, out$gap,
       cex = sqrt(out$nDUP / median(out$nDUP)), pch = 21,
       bg  = gray(1 - out$dupShare / max(out$dupShare)),
       xlab = "SFD size elasticity (log price on log sqft)",
       ylab = "log ppsf: legacy duplex minus SFD",
       main = sprintf("%s, %d-%d sales (size = duplex N, shade = duplex share)",
                      GEO, min(yrs), max(yrs)))
  abline(h = 0, lty = 3)
  abline(coef(fit), col = "red")
  text(out$elas, out$gap, out$gid, cex = 0.55, pos = 3)
}
drawGap()
png("duplexGapVsElas.png", width = 900, height = 700, res = 120)
drawGap()
dev.off()

# ---- 33-ft SFD tract log ppsf vs size elasticity, coloured by longitude -------
# built here from the R1 SFD sample (sfd); same spec as vancouverPriceElasticity.R
stopifnot(GEO == "CTNAME")
ONTARIO <- -123.1036

s33 <- sfd[w == 33]
s33[, N33 := .N, by = gid]
s33 <- s33[N33 >= MIN_SFD]
m1_33 <- feols(log(conveyancePrice / MB_total_finished_area) ~ lage | gid + convMonth, s33)
m2_33 <- feols(log(conveyancePrice) ~ i(gid, lsqft) + lage | gid + convMonth, s33)
fe33 <- fixef(m1_33)$gid
k33  <- grep("^gid::", names(coef(m2_33)))

ctLon <- ctV[!duplicated(ctV$CTNAME), ]
ctLon <- data.table(gid = ctLon$CTNAME,
                    lon = st_coordinates(st_transform(st_point_on_surface(ctLon), 4326))[, 1])

p33 <- Reduce(function(a, b) merge(a, b, by = "gid"), list(
  data.table(gid = names(fe33), lppsf = as.numeric(fe33)),
  data.table(gid = sub("^gid::(.*):lsqft$", "\\1", names(coef(m2_33))[k33]), elas = coef(m2_33)[k33]),
  unique(s33[, .(gid, N = N33)]),
  ctLon))

pal <- hcl.colors(101, "Blue-Red 3")        # blue = west, red = east
lr  <- range(p33$lon)
pos <- ifelse(p33$lon < ONTARIO,
              0.5 * (p33$lon - lr[1]) / (ONTARIO - lr[1]),
              0.5 + 0.5 * (p33$lon - ONTARIO) / (lr[2] - ONTARIO))
p33[, col := pal[round(pmin(pmax(pos, 0), 1) * 100) + 1]]

print(p33[, .(cor = cor(lppsf, elas), corWest = cor(lppsf[lon < ONTARIO], elas[lon < ONTARIO]),
              corEast = cor(lppsf[lon >= ONTARIO], elas[lon >= ONTARIO]), nTracts = .N)])

draw33 <- function() {
  plot(p33$elas, p33$lppsf, pch = 21, bg = p33$col, col = "grey30",
       cex = sqrt(p33$N / median(p33$N)),
       xlab = "SFD size elasticity, 33-ft lots",
       ylab = "log price per sqft (tract FE), 33-ft lots",
       main = sprintf("Vancouver R1 tracts, %d-%d sales (size = N)", min(yrs), max(yrs)))
  abline(lm(lppsf ~ elas, p33), lty = 2)
  legend("bottomright", bty = "n", pt.cex = 1.5, pch = 21, col = "grey30",
         pt.bg = pal[c(1, 51, 101)], title = "Longitude",
         legend = c(sprintf("%.3f (west)", lr[1]), "Ontario St", sprintf("%.3f (east)", lr[2])))
}
draw33()
png("lppsf33VsElasLon.png", width = 900, height = 700, res = 120)
draw33()
dev.off()
