# lotElasticityNew.R
# Local lot-size elasticity for new builds: area-specific slope of log price on log lot area (width x depth), with
# sale-year, age and area FE, for SFD sales 2012 to 2018-09-17 on lots (any width) now zoned R1-1, age = sale year - min(year built,
# effective year) <= MAX_AGE (default 14). Compares with the paper's log ppsf and size
# elasticity (vancouverElasticityPremium.R) by census tract and BCA neighbourhood.
# Writes text/elasLotCor.tex (Table 1: tract correlations of size elasticity, lot elasticity, duplex premium, log ppsf,
# West Side; complete cases except the duplex premium, available for tracts with >= MIN_DUP duplex sales), text/lotElas.tex
# Tom Davidoff 11/21/26

library(data.table)
library(RSQLite)
library(sf)
library(fixest)

D_REFORM <- as.IDate("2018-09-18")
YEARS    <- 2012:2018
MAX_AGE  <- if (length(commandArgs(TRUE))) as.integer(commandArgs(TRUE)[1]) else 14  # Rscript lotElasticityNew.R 4; text/ outputs only at the default
MIN_OBS  <- 20
MIN_DUP  <- 10     # as vancouverElasticityPremium.R
OUT      <- "../text/"
ONT      <- -123.101
fBCA19   <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
fGeo26   <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
fZoning  <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
fTract   <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
lost <- function(n0, n1, what) cat(sprintf("%-45s %7d -> %7d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

con <- dbConnect(SQLite(), path.expand(fBCA19))
s <- as.data.table(dbGetQuery(con, "
  SELECT s.saleID, s.conveyanceDate, CAST(s.conveyancePrice AS REAL) AS price, f.rollNumber,
         d.neighbourhoodDescription AS nbhd,
         CAST(COALESCE(NULLIF(d.landWidth, ''), i.land_width) AS REAL) AS w,
         CAST(COALESCE(NULLIF(d.landDepth, ''), i.land_depth) AS REAL) AS dep,
         CAST(i.MB_total_finished_area AS REAL) AS sqft,
         CAST(i.MB_effective_year AS REAL) AS effYr, CAST(i.MB_year_built AS REAL) AS yrBuilt
  FROM sales s
  JOIN folio f ON f.folioID = s.folioID
  JOIN folioDescription d ON d.folioID = s.folioID
  JOIN residentialInventory i ON i.roll_number = f.rollNumber AND trim(i.jurisdiction) = '200'
  WHERE trim(f.jurisdictionCode) = '200'
    AND d.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
    AND s.conveyanceTypeDescription = 'Improved Single Property Transaction'"))
multi <- as.data.table(dbGetQuery(con, "SELECT saleID FROM sales GROUP BY saleID HAVING COUNT(DISTINCT folioID) > 1"))$saleID
dbDisconnect(con)
n0 <- nrow(s); s <- s[!saleID %in% multi & !saleID %in% s[duplicated(saleID), saleID]]; lost(n0, nrow(s), "drop multi-folio conveyances")
stopifnot(!anyDuplicated(s$saleID))
s[, `:=`(date = as.IDate(conveyanceDate), rollStart = floor(as.numeric(rollNumber) / 1000))]
s[, yr := year(date)]
n0 <- nrow(s); s <- s[yr %in% YEARS & date < D_REFORM];                          lost(n0, nrow(s), "sale 2012 to pre-duplex reform")
n0 <- nrow(s); s <- s[price > 0 & w > 0 & dep > 0 & effYr > 1800 & yrBuilt > 1800]; lost(n0, nrow(s), "price, lot dims, years valid")
s[, age := yr - pmin(yrBuilt, effYr)]
n0 <- nrow(s); s <- s[age >= 0 & yr >= effYr];                                   lost(n0, nrow(s), "built and effective year not after sale")
n0 <- nrow(s); s <- s[age <= MAX_AGE];                                           lost(n0, nrow(s), sprintf("age <= %d by both years", MAX_AGE))

g26 <- readRDS(fGeo26)[, .(lon = mean(longitude), lat = mean(latitude)), by = rollStart]
n0 <- nrow(s); s <- merge(s, g26, by = "rollStart"); stopifnot(!anyDuplicated(s$saleID)); lost(n0, nrow(s), "located via 2026 rollStart centroid")
sfS <- st_transform(st_as_sf(s, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 26910)
z   <- st_transform(st_make_valid(st_read(fZoning, quiet = TRUE)), 26910)
inR1 <- lengths(st_intersects(sfS, z[z$zoning_district == "R1-1", ])) > 0
n0 <- nrow(s); s <- s[inR1]; sfS <- sfS[inR1, ];                                 lost(n0, nrow(s), "lot now zoned R1-1")
ct <- st_transform(st_make_valid(st_read(fTract, quiet = TRUE)[, c("CTUID", "CTNAME")]), 26910)
j  <- st_intersects(sfS, ct); stopifnot(all(lengths(j) <= 1))
s[, CTUID := ct$CTUID[sapply(j, function(i) if (length(i)) i else NA_integer_)]]
n0 <- nrow(s); s <- s[!is.na(CTUID) & nbhd != ""];                               lost(n0, nrow(s), "tract and neighbourhood assigned")
s[, `:=`(lp = log(price), llot = log(w * dep))]
q <- quantile(log(s$price / (s$w * s$dep)), c(.005, .995))
n0 <- nrow(s); s <- s[log(price / (w * dep)) %between% q];                       lost(n0, nrow(s), "log price/lot sqft within 0.5-99.5 pctile")
cat(sprintf("new-build sales: %d | median price $%.0f | median lot %.0f sqft | width 33: %.0f%%, 50: %.0f%% | median age %d\n",
            nrow(s), median(s$price), median(exp(s$llot)), 100 * mean(round(s$w) == 33), 100 * mean(round(s$w) == 50), as.integer(median(s$age))))
print(s[, .N, keyby = yr]); print(s[, .N, keyby = age])
if (MAX_AGE == 14) saveRDS(s, "~/DropboxExternal/dataProcessed/lotElasNewSales.rds")   # reused by sumStats.R
print(feols(lp ~ llot | yr + age, s, vcov = "hetero"))

byArea <- function(area, ext) {
  na <- s[, .(N = .N, sdLot = sd(llot)), by = area]
  cat(sprintf("%s: %d areas, %d with >= %d sales (sales kept %.1f%%)\n", area, nrow(na), na[N >= MIN_OBS, .N], MIN_OBS,
              100 * na[N >= MIN_OBS, sum(N)] / nrow(s)))
  d <- s[na[N >= MIN_OBS], on = area][, gid := as.character(get(area))]
  m <- feols(lp ~ i(gid, llot) | yr + age + gid, d, vcov = "hetero")
  k <- grep("^gid::", names(coef(m)))
  est <- data.table(gid = sub("^gid::(.*):llot$", "\\1", names(coef(m))[k]), lotElas = coef(m)[k], seLot = se(m)[k])
  est <- merge(est, d[, .(nNew = .N, sdLot = sd(llot)), by = gid], by = "gid")
  n0 <- nrow(est); est <- merge(est, fread(ext, colClasses = list(character = "gid"))[, .(gid, lppsf, elas, west)], by = "gid")
  cat(sprintf("%s: %d of %d lot-elasticity areas have paper ppsf/elasticity\n", area, nrow(est), n0))
  print(est[, .(n = .N, meanLot = mean(lotElas), sdLotElas = sd(lotElas), medSE = median(seLot), medSdLogLot = median(sdLot))])
  w <- function(x) x[, .(lppsf, elas, lotElas, westD = as.numeric(west))]
  cat("correlations, all areas:\n"); print(round(cor(w(est)), 2))
  cat("weighted by 1/SE^2 of lot elasticity:\n"); print(round(cov.wt(w(est), 1 / est$seLot^2, cor = TRUE)$cor, 2))
  for (side in c(TRUE, FALSE)) { x <- est[west == side]
    cat(sprintf("%s (%d areas): cor(lppsf, lot) %.2f, cor(elas, lot) %.2f\n", if (side) "West" else "East", nrow(x),
                x[, cor(lppsf, lotElas)], x[, cor(elas, lotElas)])) }
  est
}
eT <- byArea("CTUID", "~/DropboxExternal/dataProcessed/elasPremiumTract.csv")
eN <- byArea("nbhd",  "~/DropboxExternal/dataProcessed/elasPremiumNeighbourhood.csv")
print(eN[order(lppsf), .(gid, nNew, lppsf = round(lppsf, 2), elas = round(elas, 2), lotElas = round(lotElas, 2), seLot = round(seLot, 2))])
fwrite(eT, sprintf("~/DropboxExternal/dataProcessed/lotElasNewTract_age%d.csv", MAX_AGE))
fwrite(eN, sprintf("~/DropboxExternal/dataProcessed/lotElasNewNeighbourhood_age%d.csv", MAX_AGE))

# ---- Table 1: tract correlations (default age limit only) ----
if (MAX_AGE == 14) {
  pr <- fread("~/DropboxExternal/dataProcessed/elasPremiumTract.csv", colClasses = list(character = "gid"))[, .(gid, dupPremium, nDUP)]
  n0 <- nrow(eT); t <- merge(eT, pr, by = "gid"); stopifnot(nrow(t) == n0, !anyDuplicated(t$gid))
  t[nDUP < MIN_DUP, dupPremium := NA]
  v <- c(elas = "Built elasticity", lotElas = "Lot size elasticity", dupPremium = "Duplex premium", lppsf = "Log price/sqft", west = "West Side")
  X <- t[, .(elas, lotElas, dupPremium, lppsf, west = as.numeric(west))]
  C <- cor(X, use = "pairwise.complete.obs"); print(round(C, 3))
  nDupT <- t[!is.na(dupPremium), .N]; nDupW <- t[!is.na(dupPremium) & west == TRUE, .N]
  stopifnot(nDupT == 10, all(complete.cases(X[, -"dupPremium"])))
  rows <- sapply(seq_along(v), function(i) paste(v[i], "&", paste(c(sprintf("%.2f", C[i, seq_len(i - 1)]), "1", rep("", length(v) - i)), collapse = " & "), "\\\\"))
  tab <- c("% generated by scripts/lotElasticityNew.R; do not edit", "\\begin{tabular}{lccccc}", "\\hline",
           paste(" &", paste(v, collapse = " & "), "\\\\"), "\\hline", rows, "\\hline", "\\end{tabular}")
  writeLines(tab, paste0(OUT, "elasLotCor.tex")); cat(tab, sep = "\n")
  fI <- function(x) formatC(x, format = "d", big.mark = ",")
  mac <- c("% generated by scripts/lotElasticityNew.R; do not edit",
    sprintf("\\newcommand{\\lotElasMaxAgePlusOne}{%d}", MAX_AGE + 1),
    sprintf("\\newcommand{\\lotElasSales}{%s}", fI(nrow(s))),
    sprintf("\\newcommand{\\lotElasMinSales}{%d}", MIN_OBS),
    sprintf("\\newcommand{\\lotElasNTract}{%d}", nrow(t)),
    sprintf("\\newcommand{\\lotElasNWestTract}{%d}", t[west == TRUE, .N]),
    sprintf("\\newcommand{\\lotElasNDupTract}{%d}", nDupT),
    sprintf("\\newcommand{\\lotElasNDupWestTract}{%d}", nDupW),
    sprintf("\\newcommand{\\lotElasMeanTract}{%.2f}", mean(t$lotElas)))
  writeLines(mac, paste0(OUT, "lotElas.tex")); cat(mac, sep = "\n")
}
