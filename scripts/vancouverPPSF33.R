# vancouverPPSF33.R
# Local price per sqft (eq. ppsfHedonic): log(price / finished area) on sale-year FE, (optionally) log effective
# year, and area FE, for single-family sales from 2012 to the Sept 2018 duplex approval on 33-ft lots now zoned R1-1. Areas: census tract (CTUID)
# and BCA neighbourhood. Plots tract/neighbourhood FE with vs without the effective-year control.
# Writes text/ppsf33.tex, text/ppsf33Tract.png, text/ppsf33Neighbourhood.png
# Tom Davidoff 11/15/26

library(data.table)
library(RSQLite)
library(sf)
library(fixest)
library(ggplot2)
library(ggrepel)

YEARS   <- 2012:2018
D_REFORM <- as.IDate("2018-09-18")  # council approves duplexes in RS (as vancouverPermitLogit.R); sales end before it
MIN_OBS <- 20                 # min sales per area
ONT     <- -123.101           # Ontario St: East/West divide
fBCA19  <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
fGeo26  <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
fZoning <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
fTract  <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
OUT     <- "../text/"

lost <- function(n0, n1, what) cat(sprintf("%-45s %7d -> %7d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

con <- dbConnect(SQLite(), path.expand(fBCA19))
s <- as.data.table(dbGetQuery(con, "
  SELECT s.saleID, s.folioID, s.conveyanceDate, CAST(s.conveyancePrice AS REAL) AS price, f.rollNumber,
         d.neighbourhoodDescription AS nbhd,
         CAST(COALESCE(NULLIF(d.landWidth, ''), i.land_width) AS REAL) AS w,
         CAST(i.MB_total_finished_area AS REAL) AS sqft,
         CAST(i.MB_effective_year AS REAL) AS effYr, CAST(i.MB_year_built AS REAL) AS yrBuilt
  FROM sales s
  JOIN folio f ON f.folioID = s.folioID
  JOIN folioDescription d ON d.folioID = s.folioID
  JOIN residentialInventory i ON i.roll_number = f.rollNumber AND trim(i.jurisdiction) = '200'
  WHERE trim(f.jurisdictionCode) = '200'
    AND d.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
    AND s.conveyanceTypeDescription = 'Improved Single Property Transaction'"))
nAll <- as.numeric(dbGetQuery(con, "
  SELECT COUNT(*) FROM sales s JOIN folio f USING(folioID) JOIN folioDescription d USING(folioID)
  WHERE trim(f.jurisdictionCode) = '200'
    AND d.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
    AND s.conveyanceTypeDescription = 'Improved Single Property Transaction'"))
dbDisconnect(con)
n0 <- nrow(s); s <- s[s[, .N, by = saleID][N == 1], on = "saleID"][, N := NULL]
lost(n0, nrow(s), "drop multi-folio conveyances")
stopifnot(!anyDuplicated(s$saleID))
lost(nAll, nrow(s), "SFD arm's-length sales -> inventory join")

s[, `:=`(date = as.IDate(conveyanceDate), rollStart = floor(as.numeric(rollNumber) / 1000))]
s[, yr := year(date)]
n0 <- nrow(s); s <- s[yr %in% YEARS & date < D_REFORM];                   lost(n0, nrow(s), "sale 2012 to pre-duplex reform")
n0 <- nrow(s); s <- s[price > 0 & sqft > 0 & effYr > 1800 & yrBuilt > 1800]; lost(n0, nrow(s), "price, sqft, effective/built year valid")
n0 <- nrow(s); s <- s[yr >= yrBuilt];                                       lost(n0, nrow(s), "sale not before house built")
n0 <- nrow(s); s <- s[round(w) == 33];                                      lost(n0, nrow(s), "lot width rounds to 33 ft")

g26 <- readRDS(fGeo26)[, .(lon = mean(longitude), lat = mean(latitude)), by = rollStart]
n0 <- nrow(s); s <- merge(s, g26, by = "rollStart"); stopifnot(!anyDuplicated(s$saleID))
lost(n0, nrow(s), "located via 2026 rollStart centroid")
sfS <- st_transform(st_as_sf(s, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 26910)
z   <- st_transform(st_make_valid(st_read(fZoning, quiet = TRUE)), 26910)
inR1 <- lengths(st_intersects(sfS, z[z$zoning_district == "R1-1", ])) > 0
n0 <- nrow(s); s <- s[inR1]; sfS <- sfS[inR1, ];                            lost(n0, nrow(s), "lot now zoned R1-1")
ct <- st_transform(st_make_valid(st_read(fTract, quiet = TRUE)[, c("CTUID", "CTNAME")]), 26910)
j  <- st_intersects(sfS, ct)
stopifnot(all(lengths(j) <= 1))
s[, `:=`(CTUID = ct$CTUID[sapply(j, function(i) if (length(i)) i else NA_integer_)],
         CTNAME = ct$CTNAME[sapply(j, function(i) if (length(i)) i else NA_integer_)])]
n0 <- nrow(s); s <- s[!is.na(CTUID) & nbhd != ""];                          lost(n0, nrow(s), "tract and neighbourhood assigned")

s[, `:=`(lppsf = log(price / sqft), lEff = log(effYr))]
q <- quantile(s$lppsf, c(.005, .995))
n0 <- nrow(s); s <- s[lppsf %between% q];                                   lost(n0, nrow(s), "log ppsf within 0.5-99.5 pctile")
cat(sprintf("sales: %d | median ppsf $%.0f | median sqft %.0f | median eff yr %.0f\n",
            nrow(s), median(s$price / s$sqft), median(s$sqft), median(s$effYr)))
print(s[, .(N = .N, medPPSF = round(median(price / sqft))), keyby = yr])

areaFE <- function(dd, area) {
  na <- dd[, .N, by = area]
  cat(sprintf("%s: %d areas with 33-ft R1-1 sales, %d with >= %d (sales kept %.1f%%)\n", area, nrow(na),
              na[N >= MIN_OBS, .N], MIN_OBS, 100 * na[N >= MIN_OBS, sum(N)] / nrow(dd)))
  dd <- dd[na[N >= MIN_OBS], on = area]
  m1 <- feols(as.formula(paste("lppsf ~ lEff | yr +", area)), dd, vcov = "hetero")
  m0 <- feols(as.formula(paste("lppsf ~ 1 | yr +", area)), dd, vcov = "hetero")
  f1 <- fixef(m1)[[area]]; f0 <- fixef(m0)[[area]]
  out <- data.table(id = names(f1), withEff = f1 - mean(f1), noEff = f0[names(f1)] - mean(f0[names(f1)]))
  geo <- dd[, .(N = .N, lon = mean(lon), lat = mean(lat), CTNAME = CTNAME[1]), by = c(area)]
  setnames(geo, area, "id")
  list(m1 = m1, m0 = m0, est = merge(out, geo, by = "id"), nSales = nrow(dd))
}
rT <- areaFE(s, "CTUID")
rN <- areaFE(s, "nbhd")
stopifnot(nrow(rT$est) == uniqueN(s[, .N, by = CTUID][N >= MIN_OBS]$CTUID))
etable(rT$m1, rT$m0, rN$m1, rN$m0, headers = c("tract", "tract", "nbhd", "nbhd"))

for (r in list(rT, rN)) r$est[, west := lon < ONT]
rT$est[, label := sub("^0+", "", CTNAME)]
rN$est[, label := tools::toTitleCase(tolower(id))]
summ <- function(e) e[, .(nAreas = .N, corWithNo = cor(withEff, noEff), sdWith = sd(withEff), sdNo = sd(noEff),
                          westMinusEast = mean(withEff[west]) - mean(withEff[!west]), corLon = cor(withEff, -lon))]
print(rbind(cbind(level = "tract", summ(rT$est)), cbind(level = "nbhd", summ(rN$est))))

# tract FE averaged to neighbourhood (sales-weighted) vs neighbourhood FE
tn <- s[CTUID %in% rT$est$id, .N, by = .(CTUID, nbhd)]
tn <- merge(tn, rT$est[, .(CTUID = id, withEff)], by = "CTUID")[, .(tractAvg = weighted.mean(withEff, N)), by = nbhd]
tn <- merge(tn, rN$est[, .(nbhd = id, withEff)], by = "nbhd")
cat("cor(neighbourhood FE, sales-weighted mean tract FE):", round(tn[, cor(tractAvg, withEff)], 3), "N =", nrow(tn), "\n")

plotFE <- function(e, file, what, labels = TRUE) {
  p <- ggplot(e, aes(withEff, noEff, colour = lon)) +
    geom_abline(linetype = 2, colour = "grey60") +
    geom_point(aes(size = N)) +
    (if (labels) geom_text_repel(aes(label = label), colour = "black", size = 2.3, max.overlaps = Inf, segment.size = 0.2, show.legend = FALSE)) +
    scale_colour_gradient(low = "grey85", high = "grey10", name = "Longitude\n(darker = east)") +
    scale_size_area(max_size = 3, name = "Sales") +
    labs(x = "Log ppsf, with effective-year control", y = "Log ppsf, without effective-year control",
         title = paste0(what, " price per square foot, 33-ft lots now R1-1, ", min(YEARS), "-", format(D_REFORM - 1, "%b %Y"), " sales")) +
    coord_equal() + theme_minimal(base_size = 9)
  ggsave(file, p, width = 7.5, height = 7, dpi = 200)
}
plotFE(rT$est, paste0(OUT, "ppsf33Tract.png"), "Census tract", labels = FALSE)
plotFE(rN$est, paste0(OUT, "ppsf33Neighbourhood.png"), "Neighbourhood")

fwrite(rT$est, "~/DropboxExternal/dataProcessed/ppsf33Tract.csv")
fwrite(rN$est, "~/DropboxExternal/dataProcessed/ppsf33Neighbourhood.csv")

sT <- summ(rT$est); sN <- summ(rN$est)
fmtInt <- function(x) formatC(x, format = "d", big.mark = ",")
mac <- c(
  sprintf("\\newcommand{\\ppsfSalesTract}{%s}", fmtInt(rT$nSales)),
  sprintf("\\newcommand{\\ppsfSalesNbhd}{%s}", fmtInt(rN$nSales)),
  sprintf("\\newcommand{\\ppsfNTract}{%d}", sT$nAreas),
  sprintf("\\newcommand{\\ppsfNNbhd}{%d}", sN$nAreas),
  sprintf("\\newcommand{\\ppsfMinObs}{%d}", MIN_OBS),
  sprintf("\\newcommand{\\ppsfFirstYear}{%d}", min(YEARS)),
  sprintf("\\newcommand{\\ppsfLastYear}{%d}", max(YEARS)),
  sprintf("\\newcommand{\\ppsfEffCoefTract}{%.2f}", coef(rT$m1)["lEff"]),
  sprintf("\\newcommand{\\ppsfEffCoefNbhd}{%.2f}", coef(rN$m1)["lEff"]),
  sprintf("\\newcommand{\\ppsfCorEffTract}{%.2f}", sT$corWithNo),
  sprintf("\\newcommand{\\ppsfCorEffNbhd}{%.2f}", sN$corWithNo),
  sprintf("\\newcommand{\\ppsfWestPremiumTract}{%.0f}", 100 * (exp(sT$westMinusEast) - 1)),
  sprintf("\\newcommand{\\ppsfWestPremiumNbhd}{%.0f}", 100 * (exp(sN$westMinusEast) - 1)),
  sprintf("\\newcommand{\\ppsfCorTractNbhd}{%.2f}", tn[, cor(tractAvg, withEff)])
)
writeLines(c("% generated by scripts/vancouverPPSF33.R; do not edit", mac), paste0(OUT, "ppsf33.tex"))
cat(mac, sep = "\n")
