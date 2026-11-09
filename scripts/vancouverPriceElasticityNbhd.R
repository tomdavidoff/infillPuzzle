# vancouverPriceElasticityNbhd.R
# Neighbourhood-level version of vancouverPriceElasticity.R (BCA neighbourhoods, no tracts)
# SFD price level and size elasticity by neighbourhood x lot width, legacy duplex vs SFD
# price gap against SFD elasticity, and 33-ft level vs elasticity by longitude
# Tom Davidoff
# 11/7/26

library(sf)
library(DBI)
library(RSQLite)
library(data.table)
library(fixest)

yrs     <- 2012:2018   # sale window; every duplex sold here is pre-reform stock
MAXAGE  <- 30          # effective-age cap, both products
SFD_R1  <- TRUE        # SFD sales: keep parcels currently zoned R1 only
NDUPZ   <- 5           # duplex sales: keep the top-NDUPZ zoning districts by duplex sales
MIN_SFD <- 20          # min SFD sales per area x width cell
MIN_DUP <- 10          # min duplex sales per area
ONTARIO <- -123.1036   # East/West divide for the longitude plot
split   <- FALSE       # TRUE: level and elasticity from disjoint folios

# ============================ data ===========================================
# 2019 data with sales through 2018: SFD + anything "uplex"
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

print(df[dup == TRUE, .(medArea = median(MB_total_finished_area, na.rm = TRUE),
                        medPrice = median(conveyancePrice, na.rm = TRUE), .N),
         keyby = actualUseDescription])

# ---- geography: parcel points -> zoning and longitude -------------------------
gpkg <- list.files("~/bigFiles/latestSpatialBCA", pattern = "\\.gpkg$", full.names = TRUE)[1]
g <- st_read(gpkg, quiet = TRUE, query =
  "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
g <- g[!st_is_empty(g), ]
g$rollStart <- floor(as.numeric(g$ROLL_NUMBER) / 1000)
g <- g[g$rollStart %in% df$rollStart & !duplicated(g$rollStart), "rollStart"]
gPt <- st_point_on_surface(st_make_valid(st_transform(g, 3005)))
gPt$lon <- st_coordinates(st_transform(gPt, 4326))[, 1]

zn <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson", quiet = TRUE)
zn <- st_make_valid(st_transform(zn[, c("zoning_district", "zoning_category")], 3005))

gPt <- st_join(gPt, zn,  join = st_intersects)
gPt <- gPt[!duplicated(gPt$rollStart), ]
df <- merge(df, as.data.table(st_drop_geometry(gPt)), by = "rollStart", all.x = TRUE)

# where do legacy duplex sales sit, by current zoning?
zt <- dcast(df[, .N, by = .(zoning_district, prod)], zoning_district ~ prod,
            value.var = "N", fill = 0)
print(zt[order(-DUP)])
dupZones <- head(zt[order(-DUP), zoning_district], NDUPZ)
print(dupZones)

df <- df[(prod == "SFD" & (!SFD_R1 | zoning_category %in% "R1")) |
         (prod == "DUP" & zoning_district %in% dupZones)]
df[, gid := neighbourhoodDescription]
df <- df[!is.na(gid)]

# duplex prevalence: share of all SFD+duplex sales in the area
shr <- df[, .(nSFDall = sum(!dup), nDUPall = sum(dup)), by = gid]
shr[, dupShare := nDUPall / (nSFDall + nDUPall)]

# ---- cleaning ----------------------------------------------------------------
df <- df[conveyancePrice > 0 & MB_total_finished_area > 0]
df[, `:=`(lsqft     = log(MB_total_finished_area),
          lage      = log(pmax(year(convDate) - MB_effective_year, 0) + 1),
          convMonth = format(convDate, "%Y-%m"))]
df <- df[!is.na(lage) & lage <= log(MAXAGE + 1)]

# SFD: lot dims (0 is the missing sentinel), depth trim, 33/50 widths
sfd <- df[prod == "SFD"]
sfd[land_width == 0, land_width := NA]
sfd[land_depth == 0, land_depth := NA]
sfd <- sfd[!is.na(land_depth) & !is.na(land_width)]
qc  <- quantile(sfd$land_depth, c(0.2, 0.8))
sfd <- sfd[land_depth %between% qc]
sfd[, w := round(land_width)]
sfd[w > 47 & w < 53, w := 50]
sfd <- sfd[w %in% c(33, 50)]
sfd[, tw := paste(gid, w, sep = "_")]
sfd[, N := .N, by = tw]
sfd <- sfd[N >= MIN_SFD]

# duplex: anything "uplex"; no lot filter
du <- df[prod == "DUP"]

sA <- if (split) sfd[rollStart %% 2 == 0] else sfd
sB <- if (split) sfd[rollStart %% 2 == 1] else sfd

# ============ A. SFD level and elasticity by neighbourhood x width ======================
# level: area x width FE in log ppsf, net of log age and month
mA1 <- feols(log(conveyancePrice / MB_total_finished_area) ~ lage | tw + convMonth, sA)
feA <- fixef(mA1)$tw

# elasticity: one regression, area x width slopes on log sqft
mA2 <- feols(log(conveyancePrice) ~ i(tw, lsqft) + lage | tw + convMonth, sB)
k   <- grep("^tw::", names(coef(mA2)))
elA <- data.table(tw = sub("^tw::(.*):lsqft$", "\\1", names(coef(mA2))[k]),
                  elas = coef(mA2)[k], se = se(mA2)[k])

dPEl <- merge(merge(unique(sfd[, .(tw, gid, w, N)]),
                    data.table(tw = names(feA), lppsf = as.numeric(feA)), by = "tw"),
              elA, by = "tw")
print(summary(dPEl))
print(dPEl[, .(medLppsf = median(lppsf), medElas = median(elas), nAreas = .N), keyby = w])
for (p in c(0, .25, .5, .75))
  print(dPEl[N >= quantile(N, p), .(pN = p, cor = cor(lppsf, elas), nAreas = .N), keyby = w])

elW <- dcast(dPEl, gid ~ w, value.var = c("lppsf", "elas", "N"))
print(elW[, cor(elas_33, elas_50, use = "complete.obs")])
print(elW[, cor(lppsf_33, lppsf_50, use = "complete.obs")])
print(summary(elW))
print(cor(elW[N_33 > 30 & N_50 > 30, .(elas_33, elas_50, lppsf_33, lppsf_50)], use = "complete.obs"))

# ============ B. 33-ft level vs elasticity, coloured by longitude ==============
# neighbourhood longitude: median over its SFD sales' parcel points
nbLon <- sfd[!is.na(lon), .(lon = median(lon)), by = gid]
p33 <- merge(dPEl[w == 33], nbLon, by = "gid")

pal <- hcl.colors(101, "Blue-Red 3")        # blue = west, red = east
lr  <- range(p33$lon)
pos <- ifelse(p33$lon < ONTARIO,
              0.5 * (p33$lon - lr[1]) / (ONTARIO - lr[1]),
              0.5 + 0.5 * (p33$lon - ONTARIO) / (lr[2] - ONTARIO))
p33[, col := pal[round(pmin(pmax(pos, 0), 1) * 100) + 1]]

print(p33[, .(cor = cor(lppsf, elas),
              corWest = cor(lppsf[lon < ONTARIO], elas[lon < ONTARIO]),
              corEast = cor(lppsf[lon >= ONTARIO], elas[lon >= ONTARIO]), nNbhds = .N)])

draw33 <- function() {
  plot(p33$elas, p33$lppsf, pch = 21, bg = p33$col, col = "grey30",
       cex = sqrt(p33$N / median(p33$N)),
       xlab = "SFD size elasticity, 33-ft lots",
       ylab = "log price per sqft (neighbourhood FE), 33-ft lots",
       main = sprintf("Vancouver BCA neighbourhoods, %d-%d sales (size = N)", min(yrs), max(yrs)))
  abline(lm(lppsf ~ elas, p33), lty = 2)
  legend("bottomright", bty = "n", pt.cex = 1.5, pch = 21, col = "grey30",
         pt.bg = pal[c(1, 51, 101)], title = "Longitude",
         legend = c(sprintf("%.3f (west)", lr[1]), "Ontario St", sprintf("%.3f (east)", lr[2])))
}
draw33()
png("lppsf33VsElasLonNbhd.png", width = 900, height = 700, res = 120)
draw33()
dev.off()

# ============ C. legacy duplex - SFD gap vs SFD elasticity =====================
# SFD elasticity pooled across widths: area-specific slope, area x width FE
mC2 <- feols(log(conveyancePrice) ~ i(gid, lsqft) + lage | tw + convMonth, sB)
k   <- grep("^gid::", names(coef(mC2)))
elC <- data.table(gid  = sub("^gid::(.*):lsqft$", "\\1", names(coef(mC2))[k]),
                  elas = coef(mC2)[k], seEl = se(mC2)[k])

# levels: area x product FE in log ppsf, one pooled hedonic, unconditional on lot size
lv <- rbind(sA, du, fill = TRUE)
lv[, gp := paste(gid, prod, sep = "|")]
mC1 <- feols(log(conveyancePrice / MB_total_finished_area) ~ lage | gp + convMonth, lv)
feC <- fixef(mC1)$gp
lev <- data.table(gp = names(feC), lppsf = as.numeric(feC))
lev[, c("gid", "prod") := tstrsplit(gp, "|", fixed = TRUE)]
lev <- dcast(lev, gid ~ prod, value.var = "lppsf")
setnames(lev, c("DUP", "SFD"), c("lppsfDUP", "lppsfSFD"))
lev[, gap := lppsfDUP - lppsfSFD]

cnt <- lv[, .(nSFD = sum(prod == "SFD"), nDUP = sum(prod == "DUP")), by = gid]

out <- Reduce(function(a, b) merge(a, b, by = "gid"), list(lev, elC, cnt, shr))
print(table(cut(out$nDUP, c(-1, 0, 4, 9, 19, 49, Inf))))   # support: duplex sales per area
out <- out[nDUP >= MIN_DUP & !is.na(gap)]
print(out[order(-dupShare)])

if (nrow(out) < 5) {
  message("Fewer than 5 areas with >= MIN_DUP duplex sales; skipping duplex regressions")
} else {
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
         main = sprintf("Neighbourhoods, %d-%d sales (size = duplex N, shade = duplex share)",
                        min(yrs), max(yrs)))
    abline(h = 0, lty = 3)
    abline(coef(fit), col = "red")
    text(out$elas, out$gap, out$gid, cex = 0.55, pos = 3)
  }
  drawGap()
  png("duplexGapVsElasNbhd.png", width = 900, height = 700, res = 120)
  drawGap()
  dev.off()
}

lv[, lppsfRaw := log(conveyancePrice / MB_total_finished_area)]
mn <- dcast(lv[, .(m = mean(lppsfRaw)), by = .(gid, prod)], gid ~ prod, value.var = "m")
setnames(mn, c("DUP", "SFD"), c("mDUP", "mSFD"))
mn[, dlppsf := mDUP - mSFD]
out <- merge(out, mn, by = "gid")
print(out[order(elas), .(gid, elas, mSFD, mDUP, dlppsf, nDUP)])
print(etable(
  feols(dlppsf ~ elas,        out, weights = ~nDUP),
  feols(dlppsf ~ mSFD,        out, weights = ~nDUP),
  feols(dlppsf ~ elas + mSFD, out, weights = ~nDUP),
  vcov = "hetero", headers = c("elas", "lppsf", "both")))
