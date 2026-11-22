# fsrEffYear2018.R
# Box plot of FSR (finished area / lot area) by effective year, 2019 BCA roll (2018 stock): single family folios on
# 33-ft lots now zoned R1-1. Effective years 2014+ each get a box; earlier years are pooled into quartiles of the
# pre-2014 effective-year distribution.
# Writes text/fsrEffYear2018.png
# Tom Davidoff 11/21/26

library(data.table)
library(RSQLite)
library(sf)
library(ggplot2)

fBCA19  <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
fGeo26  <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
fZoning <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
lost <- function(n0, n1, what) cat(sprintf("%-45s %7d -> %7d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

con <- dbConnect(SQLite(), path.expand(fBCA19))
d <- as.data.table(dbGetQuery(con, "
  SELECT f.rollNumber,
         CAST(COALESCE(NULLIF(d.landWidth, ''), i.land_width) AS REAL) AS w,
         CAST(COALESCE(NULLIF(d.landDepth, ''), i.land_depth) AS REAL) AS dep,
         CAST(i.MB_total_finished_area AS REAL) AS sqft, CAST(i.MB_effective_year AS REAL) AS effYr
  FROM folio f
  JOIN folioDescription d ON d.folioID = f.folioID
  JOIN residentialInventory i ON i.roll_number = f.rollNumber AND trim(i.jurisdiction) = '200'
  WHERE trim(f.jurisdictionCode) = '200'
    AND d.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')"))
dbDisconnect(con)
n0 <- nrow(d); d <- unique(d); lost(n0, nrow(d), "exact duplicate rows")
stopifnot(!anyDuplicated(d$rollNumber))
d[, rollStart := floor(as.numeric(rollNumber) / 1000)]
n0 <- nrow(d); d <- d[, if (.N == 1) .SD, by = rollStart];                lost(n0, nrow(d), "one SFD folio per rollStart")
n0 <- nrow(d); d <- d[round(w) == 33];                                    lost(n0, nrow(d), "lot width rounds to 33 ft")
n0 <- nrow(d); d <- d[dep > 0 & sqft > 0 & effYr > 1800];                 lost(n0, nrow(d), "depth, sqft, effective year valid")

g26 <- readRDS(fGeo26)[, .(lon = mean(longitude), lat = mean(latitude)), by = rollStart]
n0 <- nrow(d); d <- merge(d, g26, by = "rollStart"); stopifnot(!anyDuplicated(d$rollStart))
lost(n0, nrow(d), "located via 2026 rollStart centroid")
sfD <- st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 26910)
z   <- st_transform(st_make_valid(st_read(fZoning, quiet = TRUE)), 26910)
inR1 <- lengths(st_intersects(sfD, z[z$zoning_district == "R1-1", ])) > 0
n0 <- nrow(d); d <- d[inR1];                                              lost(n0, nrow(d), "lot now zoned R1-1")

d[, fsr := sqft / (w * dep)]
print(quantile(d$fsr, c(0, .001, .01, .5, .99, .999, 1)))
n0 <- nrow(d); d <- d[fsr > 0.05 & fsr < 2];                              lost(n0, nrow(d), "0.05 < FSR < 2")

qb <- quantile(d[effYr < 2014, effYr], 0:4 / 4); print(qb)
d[effYr < 2014, bin := { k <- cut(effYr, qb, include.lowest = TRUE, labels = FALSE); lo <- c(qb[1], qb[2:4] + 1); paste0(lo[k], "-", qb[k + 1]) }]
d[effYr >= 2014, bin := as.character(effYr)]
d[, bin := factor(bin, d[, .(m = min(effYr)), by = bin][order(m), bin])]
print(d[, .(N = .N, p25 = quantile(fsr, .25), p50 = median(fsr), p75 = quantile(fsr, .75)), keyby = bin], digits = 3)

p <- ggplot(d, aes(bin, fsr)) + geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3) +
  labs(x = "Effective year", y = "FSR (finished area / lot area)") + theme_bw()
ggsave("../text/fsrEffYear2018.png", p, width = 7, height = 4.5, dpi = 300)
