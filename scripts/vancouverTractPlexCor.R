# vancouverTractPlexCor.R
# 2018 tract price levels and size elasticities (RS single-family), and monthly
# cross-tract correlations with the product shares (single / duplex / multiplex) of
# new RS/R1-1 permits. Run on all lots and on round(lot width) == 33 only.
#   mlppsf : tract median log(assessed total / finished sqft), 2019 roll (July 2018 values)
#   hres   : tract mean residual, pooled 2015-18 sales hedonic on log sqft + age, unconditional
#            on lot size (no tract terms)
#   elastA : tract cov/var slope of log assessed value on log sqft (assessments)
#   elastS : tract cov/var slope of log price (demeaned by sale month) on log sqft (sales)
# Elasticities never come from the hedonic regression that produces hres.

library(data.table)
library(sf)
library(DBI)
library(RSQLite)
library(fixest)
library(ggplot2)

bca19     <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
gpkg      <- "~/bigFiles/latestSpatialBCA/2026-04-08_bca_folios.gpkg"
fTract    <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
fZone     <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
permitURL <- "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/issued-building-permits/exports/csv?delimiter=%2C"
outDir    <- "text"; dir.create(outDir, showWarnings = FALSE)

MIN_OBS   <- 30
SALE_YRS  <- c(2015, 2018)
DATE_COL  <- "issuedate"
MONTH_MIN <- as.IDate("2018-01-01")
EVENTS    <- as.IDate(c("2018-09-01", "2023-10-01"))
MEAS      <- c("mlppsf", "hres", "elastA", "elastS")
SHARES    <- c(single = "isSingle", duplex = "isDuplex", multi = "isMulti")

rollKey <- function(x) suppressWarnings(as.numeric(gsub("[^0-9]", "", x)))
cc <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 5 || sd(x[ok]) == 0 || sd(y[ok]) == 0) NA_real_ else cor(x[ok], y[ok])
}

# ---- tracts; 2026 parcels -> tract (centroid) ----
ct <- st_read(fTract, quiet = TRUE)
ct <- st_transform(st_make_valid(ct[ct$PRUID == "59", "CTNAME"]), 3005)

g <- st_read(gpkg, quiet = TRUE, query =
  "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
g <- st_transform(st_make_valid(g), 3005)
g$rollStart <- floor(rollKey(g$ROLL_NUMBER) / 1000)
geo <- as.data.table(st_drop_geometry(st_join(st_centroid(g), ct, join = st_within)))
geo <- unique(geo[!is.na(CTNAME), .(rollStart, CTNAME)], by = "rollStart")

# ---- 2019-roll RS single-family folios and pre-reform sales ----
con <- dbConnect(SQLite(), bca19)
b <- data.table(dbGetQuery(con, "
  SELECT f.folioID, f.rollNumber,
         CAST(i.MB_total_finished_area AS REAL) AS sqft,
         CAST(i.MB_effective_year AS REAL)      AS effYr,
         CAST(COALESCE(NULLIF(d.landWidth, ''), i.land_width) AS REAL) AS w,
         CAST(COALESCE(NULLIF(d.landDepth, ''), i.land_depth) AS REAL) AS dep,
         v.totalValue
  FROM folio f
  JOIN folioDescription d     ON d.folioID = f.folioID
  JOIN residentialInventory i ON i.roll_number = f.rollNumber
  JOIN (SELECT folioID, SUM(CAST(landValue AS REAL) + CAST(improvementValue AS REAL)) AS totalValue
        FROM valuation GROUP BY folioID) v ON v.folioID = f.folioID
  WHERE f.jurisdictionCode = '200'
    AND d.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
    AND i.zoning LIKE 'RS%'"))
s <- data.table(dbGetQuery(con, "
  SELECT folioID, conveyanceDate, CAST(conveyancePrice AS REAL) AS price
  FROM sales WHERE conveyanceTypeDescription = 'Improved Single Property Transaction'"))
dbDisconnect(con)

b <- b[, if (.N == 1) .SD, by = folioID]
b[, rollStart := floor(rollKey(rollNumber) / 1000)]
b <- merge(b, geo, by = "rollStart")
b <- b[sqft > 0 & totalValue > 0]

s[, `:=`(year = as.integer(substr(conveyanceDate, 1, 4)), ym = substr(conveyanceDate, 1, 7))]
h <- merge(s[year %between% SALE_YRS & price > 1e5],
           b[, .(folioID, CTNAME, sqft, effYr, w, dep)], by = "folioID")
h <- h[!is.na(effYr)]
h[, age := pmax(year - effYr, 0)]

# ---- tract measures from a folio/sales sample ----
tractMeasures <- function(bb, hh, minObs = MIN_OBS) {
  tA <- bb[, .(nA = .N,
               mlppsf = median(log(totalValue / sqft)),
               elastA = cov(log(totalValue), log(sqft)) / var(log(sqft))),
           by = CTNAME][nA >= minObs]
  hh <- copy(hh)
  r <- feols(log(price) ~ log(sqft) + poly(age, 3) | ym, data = hh)
  hh[, res := NA_real_]
  hh[obs(r), res := resid(r)]
  hh[, lpDm := log(price) - mean(log(price)), by = ym]
  tS <- hh[, .(nS = .N,
               hres = mean(res, na.rm = TRUE),
               elastS = cov(lpDm, log(sqft)) / var(log(sqft))),
           by = CTNAME][nS >= minObs]
  merge(tA, tS, by = "CTNAME", all = TRUE)
}

# levels from one random half of folios, elasticities from the other (both directions)
splitCheck <- function(bb, hh) {
  set.seed(20181)
  ids <- unique(bb$folioID)
  h1  <- ids[sample(c(TRUE, FALSE), length(ids), replace = TRUE)]
  t1  <- tractMeasures(bb[folioID %in% h1],  hh[folioID %in% h1],  MIN_OBS / 2)
  t2  <- tractMeasures(bb[!folioID %in% h1], hh[!folioID %in% h1], MIN_OBS / 2)
  x <- rbind(merge(t1[, .(CTNAME, mlppsf, hres)], t2[, .(CTNAME, elastA, elastS)], by = "CTNAME"),
             merge(t2[, .(CTNAME, mlppsf, hres)], t1[, .(CTNAME, elastA, elastS)], by = "CTNAME"))
  round(cor(x[, .(mlppsf, hres)], x[, .(elastA, elastS)], use = "pairwise.complete.obs"), 3)
}

# ---- permits (live): RS/R1-1 new single / duplex / multiplex ----
p <- fread(permitURL)
setnames(p, tolower(names(p)))
p <- p[typeofwork == "New Building" &
       grepl("Single Detached|Duplex|Multiple Dwelling|Multiplex", specificusecategory)]
p[, isMulti  := grepl("Multiple Dwelling|Multiplex", specificusecategory)]
p[, isDuplex := !isMulti & grepl("Duplex", specificusecategory)]
p[, isSingle := !isMulti & !isDuplex]
p[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]
p[, month := as.IDate(paste0(substr(as.character(get(DATE_COL)), 1, 7), "-01"))]
p <- p[!is.na(lat) & month >= MONTH_MIN]

z  <- st_read(fZone, quiet = TRUE)
z  <- st_transform(st_make_valid(z[grepl("^(R1-1|RS)", z$zoning_district), ]), 3005)
sp <- st_transform(st_as_sf(p[, .(permitnumber, specificusecategory, isSingle, isDuplex, isMulti,
                                  month, lon, lat)],
                            coords = c("lon", "lat"), crs = 4326), 3005)
sp <- sp[lengths(st_intersects(sp, z)) > 0, ]
sp <- st_join(sp, ct, join = st_within)
sp <- st_join(sp, g[, "rollStart"], join = st_intersects)
pm <- unique(as.data.table(st_drop_geometry(sp))[!is.na(CTNAME)], by = "permitnumber")
pm <- merge(pm, unique(b[, .(rollStart, w19 = round(w))], by = "rollStart"),
            by = "rollStart", all.x = TRUE)

print(pm[, .N, by = .(specificusecategory, isSingle, isDuplex, isMulti)][order(-N)])
print(pm[, .(N = .N, single = mean(isSingle), duplex = mean(isDuplex), multi = mean(isMulti),
             matched2019SF = mean(!is.na(w19)), w33 = mean(w19 == 33, na.rm = TRUE)),
         by = year(month)][order(year)])

# ---- one specification: measures, monthly correlations, plot ----
runSpec <- function(tag, bb, hh, pp) {
  tr <- tractMeasures(bb, hh)
  cat("\n==== spec:", tag, "| tracts:", nrow(tr), "| with all four:",
      nrow(na.omit(tr[, ..MEAS])), "| permits:", nrow(pp), "====\n")
  print(round(cor(tr[, ..MEAS], use = "pairwise.complete.obs"), 3))
  cat("split-sample (levels x elasticities, disjoint folios):\n")
  print(splitCheck(bb, hh))

  m <- merge(pp[, c(list(nP = .N), lapply(.SD, mean)), by = .(CTNAME, month), .SDcols = SHARES],
             tr, by = "CTNAME")
  cm <- rbindlist(lapply(names(SHARES), function(k)
    m[, c(list(share = k, nTract = .N), lapply(.SD, function(v) cc(get(SHARES[k]), v))),
      by = month, .SDcols = MEAS]))
  setorder(cm, share, month)

  cl <- melt(cm, id.vars = c("share", "month", "nTract"), variable.name = "measure", value.name = "cor")
  cl[, share := factor(share, levels = names(SHARES))]
  ggplot(cl[!is.na(cor)], aes(month, cor)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = EVENTS, linetype = 2, colour = "grey50") +
    geom_point(aes(size = nTract), alpha = .4) +
    geom_smooth(method = "loess", span = .3, se = FALSE) +
    facet_grid(share ~ measure) +
    labs(x = NULL, y = "cross-tract cor(product share of permits, 2018 measure)",
         size = "tracts", title = tag) +
    theme_bw()
  ggsave(file.path(outDir, paste0("tractPlexCorMonthly_", tag, ".png")), width = 12, height = 9)
  invisible(cm)
}

cmAll <- runSpec("allLots", b, h, pm)
cm33  <- runSpec("lot33", b[round(w) == 33], h[round(w) == 33], pm[w19 == 33])
