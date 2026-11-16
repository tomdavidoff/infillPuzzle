# vancouverElasticityPremium.R
# Local SFD size elasticity, price per sqft, and legacy duplex ppsf premium, pre-reform (2012 to 2018-09-17).
# Follows vancouverPriceElasticity.R (area-specific slope of log price on log sqft; duplex - SFD gap from area x product
# FE in a pooled log ppsf hedonic), with the controls of eq. ppsfHedonic (sale-year FE, log effective year).
# SFD: 33-ft lots now R1-1 (sample from vancouverPPSF33.R). Duplex: strata duplex units in any zone (non-strata
# duplex records carry one unit's area but sell as a building). Areas: census tract and BCA neighbourhood.
# Writes text/elasPpsfCor.tex (table), text/elasPpsfDupCor.tex (table), text/elasPremium.tex (macros)
# Tom Davidoff 11/15/26

library(data.table)
library(RSQLite)
library(sf)
library(fixest)

D_REFORM <- as.IDate("2018-09-18")
YEARS    <- 2012:2018
MIN_OBS  <- 20     # SFD sales per area (as vancouverPPSF33.R)
MIN_DUP  <- 10     # duplex sales per area (as vancouverPriceElasticity.R)
ONT      <- -123.101
fBCA19   <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
fGeo26   <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
fZoning  <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
fTract   <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
fSFD     <- "~/DropboxExternal/dataProcessed/ppsf33Sales.rds"
OUT      <- "../text/"
lost <- function(n0, n1, what) cat(sprintf("%-45s %7d -> %7d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

# ---- validation: area-specific slopes recovered on simulated data ----
set.seed(42)
J <- 40; sim <- data.table(area = rep(sprintf("a%02d", 1:J), each = 150))
trueB <- data.table(area = sprintf("a%02d", 1:J), b = runif(J, 0.2, 0.9), lev = rnorm(J, 0, 0.3))
sim <- merge(sim, trueB, by = "area")
sim[, `:=`(lsqft = rnorm(.N, 7.7, 0.3), yr = sample(YEARS, .N, TRUE), lEff = log(sample(1920:2018, .N, TRUE)))]
sim[, lp := lev + b * lsqft + 2 * lEff + 0.08 * (yr - 2012) + rnorm(.N, 0, 0.15)]
ms <- feols(lp ~ i(area, lsqft) + lEff | yr + area, sim)
estB <- data.table(area = sub("^area::(.*):lsqft$", "\\1", names(coef(ms))[grep("^area::", names(coef(ms)))]),
                   bHat = coef(ms)[grep("^area::", names(coef(ms)))])
v <- merge(trueB, estB, by = "area")
cat(sprintf("simulation: cor(true, est slope) = %.3f, mean abs error = %.3f, lEff = %.2f (true 2)\n",
            v[, cor(b, bHat)], v[, mean(abs(b - bHat))], coef(ms)["lEff"]))
stopifnot(v[, cor(b, bHat)] > 0.9, abs(coef(ms)["lEff"] - 2) < 0.5)

# ---- SFD sample (33-ft, now R1-1) ----
sfd <- readRDS(fSFD)
stopifnot(!anyDuplicated(sfd$saleID), all(sfd$date < D_REFORM))
sfd[, prod := "SFD"]
cat("SFD sales:", nrow(sfd), "\n")

# ---- duplex sales, same window and filters, any zone ----
con <- dbConnect(SQLite(), path.expand(fBCA19))
du <- as.data.table(dbGetQuery(con, "
  SELECT s.saleID, s.folioID, s.conveyanceDate, CAST(s.conveyancePrice AS REAL) AS price, f.rollNumber,
         d.actualUseDescription AS use, d.neighbourhoodDescription AS nbhd,
         CAST(i.MB_total_finished_area AS REAL) AS sqft,
         CAST(i.MB_effective_year AS REAL) AS effYr, CAST(i.MB_year_built AS REAL) AS yrBuilt
  FROM sales s
  JOIN folio f ON f.folioID = s.folioID
  JOIN folioDescription d ON d.folioID = s.folioID
  JOIN residentialInventory i ON i.roll_number = f.rollNumber AND trim(i.jurisdiction) = '200'
  WHERE trim(f.jurisdictionCode) = '200' AND d.actualUseDescription LIKE 'Duplex%'
    AND s.conveyanceTypeDescription = 'Improved Single Property Transaction'"))
multi <- as.data.table(dbGetQuery(con, "SELECT saleID FROM sales GROUP BY saleID HAVING COUNT(DISTINCT folioID) > 1"))$saleID
dbDisconnect(con)
n0 <- nrow(du); du <- du[!saleID %in% multi]; stopifnot(!anyDuplicated(du$saleID)); lost(n0, nrow(du), "duplex: drop multi-folio conveyances")
du[, `:=`(date = as.IDate(conveyanceDate), rollStart = floor(as.numeric(rollNumber) / 1000))]
du[, yr := year(date)]
n0 <- nrow(du); du <- du[yr %in% YEARS & date < D_REFORM];                       lost(n0, nrow(du), "duplex: 2012 to pre-duplex reform")
n0 <- nrow(du); du <- du[grepl("^Duplex, Strata", use)];                        lost(n0, nrow(du), "duplex: strata units only")
n0 <- nrow(du); du <- du[price > 0 & sqft > 0 & effYr > 1800 & yrBuilt > 1800]; lost(n0, nrow(du), "duplex: valid fields")
n0 <- nrow(du); du <- du[yr >= yrBuilt];                                        lost(n0, nrow(du), "duplex: sale not before built")
g26 <- readRDS(fGeo26)[, .(lon = mean(longitude), lat = mean(latitude)), by = rollStart]
n0 <- nrow(du); du <- merge(du, g26, by = "rollStart"); stopifnot(!anyDuplicated(du$saleID)); lost(n0, nrow(du), "duplex: located")
sfD <- st_transform(st_as_sf(du, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 26910)
zn  <- st_transform(st_make_valid(st_read(fZoning, quiet = TRUE)[, c("zoning_district", "zoning_category")]), 26910)
zi  <- st_intersects(sfD, zn)
cat("duplex points in >1 zoning polygon:", sum(lengths(zi) > 1), "\n")
du[, `:=`(zone = zn$zoning_district[sapply(zi, function(i) if (length(i)) i[1] else NA)],
          zcat = zn$zoning_category[sapply(zi, function(i) if (length(i)) i[1] else NA)])]
cat("duplex sales by current zoning district:\n"); print(du[, .N, by = .(zone, zcat)][order(-N)][1:12])
# RT vs R1-1: no polygon overlap, and no duplex in an RT district also falls in R1-1
rt <- zn[grepl("^RT", zn$zoning_district), ]; r1 <- zn[zn$zoning_district == "R1-1", ]
ov <- sum(as.numeric(st_area(st_intersection(st_union(rt), st_union(r1)))))
cat(sprintf("RT polygons: %d (%s); area of RT n R1-1 overlap: %.1f m2\n", nrow(rt), paste(sort(unique(rt$zoning_district)), collapse = ","), ov))
stopifnot(ov < 1)
ct <- st_transform(st_make_valid(st_read(fTract, quiet = TRUE)[, c("CTUID", "CTNAME")]), 26910)
j  <- st_intersects(sfD, ct); stopifnot(all(lengths(j) <= 1))
du[, CTUID := ct$CTUID[sapply(j, function(i) if (length(i)) i else NA_integer_)]]
n0 <- nrow(du); du <- du[!is.na(CTUID) & nbhd != ""];                           lost(n0, nrow(du), "duplex: tract and neighbourhood")
du[, `:=`(lppsf = log(price / sqft), lEff = log(effYr), prod = "DUP")]
q <- quantile(du$lppsf, c(.005, .995))
n0 <- nrow(du); du <- du[lppsf %between% q];                                    lost(n0, nrow(du), "duplex: log ppsf within 0.5-99.5 pctile")
cat(sprintf("duplex sales: %d | median ppsf $%.0f | median sqft %.0f\n", nrow(du), median(du$price / du$sqft), median(du$sqft)))

# ---- estimates by area ----
byArea <- function(area) {
  s  <- sfd[sfd[, .N, by = area][N >= MIN_OBS], on = area]
  s[, `:=`(gid = get(area), lp = log(price), lsqft = log(sqft))]
  m1 <- feols(lppsf ~ lEff | yr + gid, s)                                  # = vancouverPPSF33.R level
  m2 <- feols(lp ~ i(gid, lsqft) + lEff | yr + gid, s, vcov = "hetero")   # area size elasticity
  k  <- grep("^gid::", names(coef(m2)))
  est <- data.table(gid = sub("^gid::(.*):lsqft$", "\\1", names(coef(m2))[k]), elas = coef(m2)[k], seElas = se(m2)[k])
  lv0 <- fixef(m1)$gid
  est <- merge(est, data.table(gid = names(lv0), lppsf = lv0 - mean(lv0)), by = "gid")
  est <- merge(est, s[, .(nSFD = .N, lon = mean(lon)), by = gid], by = "gid")
  d  <- du[, .(gid = get(area), lppsf, lEff, yr, prod)]
  pl <- rbind(s[, .(gid, lppsf, lEff, yr, prod)], d[gid %in% est$gid])
  m3 <- feols(lppsf ~ lEff | yr + gid^prod, pl)
  f3 <- fixef(m3)[["gid^prod"]]
  f3 <- data.table(k = names(f3), fe = f3)[, c("gid", "prod") := tstrsplit(k, "_", fixed = TRUE)]
  f3 <- dcast(f3, gid ~ prod, value.var = "fe")[, .(gid, dupPremium = DUP - SFD)]
  est <- merge(est, f3, by = "gid", all.x = TRUE)
  est <- merge(est, d[, .(nDUP = .N), by = gid], by = "gid", all.x = TRUE)
  est[is.na(nDUP), nDUP := 0]
  est[, west := lon < ONT]
  list(est = est, nSales = nrow(s), lEffDup = coef(m3)["lEff"])
}
rT <- byArea("CTUID")
rN <- byArea("nbhd")
stopifnot(!anyDuplicated(rT$est$gid), !anyDuplicated(rN$est$gid))
# consistency with vancouverPPSF33.R levels
chk <- merge(rT$est[, .(gid, lppsf)], fread("~/DropboxExternal/dataProcessed/ppsf33Tract.csv")[, .(gid = as.character(id), withEff)], by = "gid")
cat("cor with vancouverPPSF33.R tract FE:", round(chk[, cor(lppsf, withEff)], 4), "\n")
for (r in list(rT, rN)) print(r$est[, .(nAreas = .N, meanElas = mean(elas), sdElas = sd(elas), medSE = median(seElas),
                                        withDup = sum(nDUP >= MIN_DUP), meanPremium = mean(dupPremium[nDUP >= MIN_DUP]))])

# ---- table 1: cor(ppsf, elasticity), all areas, and within West / East ----
c1 <- function(e, lev) e[, .(level = lev, n = .N, all = cor(lppsf, elas), nW = sum(west), west = cor(lppsf[west], elas[west]),
                             nE = sum(!west), east = cor(lppsf[!west], elas[!west]), corWestElas = cor(as.numeric(west), elas))]
t1 <- rbind(c1(rT$est, "Census tract"), c1(rN$est, "Neighbourhood"))
print(t1)
f2 <- function(x) sprintf("%.2f", x)
tab1 <- c("\\begin{tabular}{lcccccc}", "\\hline",
  " & \\multicolumn{2}{c}{All} & \\multicolumn{2}{c}{West Side} & \\multicolumn{2}{c}{East Side} \\\\",
  "Level & Areas & Corr. & Areas & Corr. & Areas & Corr. \\\\", "\\hline",
  t1[, sprintf("%s & %d & %s & %d & %s & %d & %s \\\\", level, n, f2(all), nW, f2(west), nE, f2(east))],
  "\\hline", "\\end{tabular}")
writeLines(c("% generated by scripts/vancouverElasticityPremium.R; do not edit", tab1), paste0(OUT, "elasPpsfCor.tex"))

# ---- table 2: areas with >= MIN_DUP duplex sales: correlations among ppsf, elasticity, duplex premium ----
panel <- function(e, lev) {
  x <- e[nDUP >= MIN_DUP & !is.na(dupPremium)]
  C <- cor(x[, .(lppsf, elas, dupPremium)])
  c(sprintf("\\multicolumn{4}{l}{\\textit{%s (%d areas, %s duplex sales)}} \\\\", lev, nrow(x), format(sum(x$nDUP), big.mark = ",")),
    sprintf("Log price/sqft & 1 & & \\\\"),
    sprintf("Size elasticity & %s & 1 & \\\\", f2(C[2, 1])),
    sprintf("Duplex premium & %s & %s & 1 \\\\", f2(C[3, 1]), f2(C[3, 2])))
}
dT <- rT$est[nDUP >= MIN_DUP]; dN <- rN$est[nDUP >= MIN_DUP]
print(cor(dT[, .(lppsf, elas, dupPremium)])); print(cor(dN[, .(lppsf, elas, dupPremium)]))
tab2 <- c("\\begin{tabular}{lccc}", "\\hline", " & Log price/sqft & Size elasticity & Duplex premium \\\\", "\\hline",
  panel(rT$est, "Census tracts"), "\\hline", panel(rN$est, "Neighbourhoods"), "\\hline", "\\end{tabular}")
writeLines(c("% generated by scripts/vancouverElasticityPremium.R; do not edit", tab2), paste0(OUT, "elasPpsfDupCor.tex"))

fwrite(rT$est, "~/DropboxExternal/dataProcessed/elasPremiumTract.csv")
fwrite(rN$est, "~/DropboxExternal/dataProcessed/elasPremiumNeighbourhood.csv")

fmtInt <- function(x) formatC(x, format = "d", big.mark = ",")
mac <- c(
  sprintf("\\newcommand{\\elasMeanTract}{%.2f}", mean(rT$est$elas)),
  sprintf("\\newcommand{\\elasMeanNbhd}{%.2f}", mean(rN$est$elas)),
  sprintf("\\newcommand{\\elasPpsfCorTract}{%.2f}", t1[1, all]),
  sprintf("\\newcommand{\\elasPpsfCorNbhd}{%.2f}", t1[2, all]),
  sprintf("\\newcommand{\\dupSales}{%s}", fmtInt(nrow(du))),
  sprintf("\\newcommand{\\dupMinSales}{%d}", MIN_DUP),
  sprintf("\\newcommand{\\dupWestTracts}{%d}", dT[west == TRUE, .N]),
  sprintf("\\newcommand{\\dupRTShare}{%.0f}", 100 * du[grepl("^RT", zone), .N] / nrow(du)),
  sprintf("\\newcommand{\\dupAreasTract}{%d}", nrow(dT)),
  sprintf("\\newcommand{\\dupAreasNbhd}{%d}", nrow(dN)),
  sprintf("\\newcommand{\\dupPremiumMeanTract}{%.0f}", 100 * (exp(mean(dT$dupPremium)) - 1)),
  sprintf("\\newcommand{\\dupPremiumMeanNbhd}{%.0f}", 100 * (exp(mean(dN$dupPremium)) - 1)),
  sprintf("\\newcommand{\\dupPremPpsfCorTract}{%.2f}", dT[, cor(dupPremium, lppsf)]),
  sprintf("\\newcommand{\\dupPremPpsfCorNbhd}{%.2f}", dN[, cor(dupPremium, lppsf)]),
  sprintf("\\newcommand{\\dupPremElasCorTract}{%.2f}", dT[, cor(dupPremium, elas)]),
  sprintf("\\newcommand{\\dupPremElasCorNbhd}{%.2f}", dN[, cor(dupPremium, elas)]))
writeLines(c("% generated by scripts/vancouverElasticityPremium.R; do not edit", mac), paste0(OUT, "elasPremium.tex"))
cat(mac, sep = "\n")
