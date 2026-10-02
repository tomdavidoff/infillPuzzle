# vancouverR1FSRDate.R
# Realized FSR on 2018-vintage R1-1 single-family lots, by permitted product
# (single / duplex / multiplex) and permit application / issue date.
#   denominator: 2019-roll (mid-2018) SFD lot area, one folio per rollStart
#   numerator:   sum of 2026 MB_total_finished_area over all folios on that rollStart
#   dates/type:  New Building permits, read live from City of Vancouver open data,
#                matched to the 2018 SFD centroid within MATCHDIST
# Unit-scaling rule: a single-folio duplex (non-strata side-by-side/front-back,
# or strata with only one folio on the stem) records ONE unit's area in the MB
# fields; x2 reproduces the strata-duplex FSR distribution year by year (validated
# below). Single-folio multiplexes and other single-folio duplex uses are dropped
# from FSR tables (unit count unknown).
# Tom Davidoff
# 09/30/26

MATCHDIST <- 5       # m, permit point to 2018 SFD parcel centroid
BUILT_MIN <- 2019    # realized new build: max MB_year_built on rollStart >= this

library(data.table)
library(sf)
library(RSQLite)

sf_use_s2(TRUE)
rollKey <- function(x) as.numeric(gsub("[^0-9]", "", x))

dirGpkg   <- "~/bigFiles/latestSpatialBCA/"
gpkgFiles <- list.files(dirGpkg, pattern = "\\.gpkg$", full.names = TRUE)
stopifnot(length(gpkgFiles) == 1)
fileGpkg  <- gpkgFiles

fInvTxt      <- "~/DropboxExternal/dataRaw/Residential_inventory_202601/20260101_A09_Residential_Inventory_Extract.txt"
fGeo         <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
fnameSingles <- "~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds"
fZoning      <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
urlP         <- "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/issued-building-permits/exports/csv?delimiter=%2C"

# ============================================================================
# 1. 2019-roll (mid-2018) SFD inventory
# Source: vancouverMatch.R (08/13/26), same build, with one fix: that script
# deduped 2026 centroids via frank(rollStart) by rollStart, which averages
# ties, so any rollStart with >1 distinct 2026 centroid was dropped -- i.e.
# exactly the SFD -> strata duplex/multiplex conversions. If the rds predates
# this fix, delete it once so this block rebuilds it (see nFolio26 > 1 check).
# ============================================================================
if (!file.exists(fnameSingles)) {
  if (!file.exists(fGeo)) {
    dfG <- st_read(fileGpkg, query =
      "SELECT ROLL_NUMBER, geom
       FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
       WHERE JURISDICTION_CODE = '200'")
    dfG <- st_transform(st_make_valid(dfG), 4326)
    ctr <- st_coordinates(st_centroid(st_geometry(dfG)))
    dtGeo <- as.data.table(st_drop_geometry(dfG))
    dtGeo[, `:=`(longitude = ctr[, 1], latitude = ctr[, 2])]
    dtGeo[, rollStart := floor(as.numeric(ROLL_NUMBER) / 1000)]
    dtGeo <- unique(dtGeo[, .(rollStart, longitude, latitude)])
    saveRDS(dtGeo, fGeo)
  }
  dtGeo <- readRDS(fGeo)
  dtGeo <- dtGeo[, .(longitude = mean(longitude), latitude = mean(latitude)), by = rollStart]

  con <- dbConnect(SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")
  dtBCA19 <- data.table(dbGetQuery(con, "
    SELECT d.folioID, f.rollNumber, i.MB_effective_year, i.MB_total_finished_area,
       CAST(i.land_width AS NUMERIC) AS land_width, CAST(i.land_depth AS NUMERIC) AS land_depth,
       d.actualUseDescription,
       CAST(d.landWidth AS NUMERIC) AS landWidth, CAST(d.landDepth AS NUMERIC) AS landDepth,
       CAST(v.landValue AS NUMERIC) AS landValue
    FROM folio f
    JOIN residentialInventory i ON i.roll_number = f.rollNumber
    JOIN folioDescription d      ON d.folioID     = f.folioID
    JOIN valuation v             ON v.folioID     = f.folioID
    WHERE f.jurisdictionCode = '200'
  "))
  dbDisconnect(con)
  dtBCA19[, rollStart := floor(as.numeric(rollNumber) / 1000)]
  dtBCA19[is.na(landWidth), landWidth := land_width]
  dtBCA19[is.na(landDepth), landDepth := land_depth]
  dtBCA19 <- merge(dtBCA19, dtGeo, by = "rollStart", all.x = TRUE)
  dtSingles <- dtBCA19[actualUseDescription %in%
                         c("Single Family Dwelling", "Residential Dwelling with Suite"),
                       .(landWidth, landDepth, MB_total_finished_area, MB_effective_year,
                         rollNumber, folioID, longitude, latitude, landValue)]
  dtSingles <- dtSingles[!is.na(longitude) & !is.na(latitude)]
  saveRDS(dtSingles, fnameSingles)
}

dtS <- readRDS(fnameSingles)
dtS[, rollStart := floor(rollKey(rollNumber) / 1000)]
dtS[, c("landWidth", "landDepth", "MB_total_finished_area") :=
        lapply(.SD, as.numeric), .SDcols = c("landWidth", "landDepth", "MB_total_finished_area")]
dtS <- dtS[, if (.N == 1) .SD, by = rollStart]            # drop multi-folio 2019 rollStarts
dtS <- dtS[landWidth > 0 & landDepth > 0]
dtS[, lotArea := landWidth * landDepth]                    # sqft

# ---- R1-1 gate on the 2018 lot centroid ------------------------------------
dgZ <- st_read(fZoning, quiet = TRUE)
dgZ <- st_transform(st_make_valid(dgZ[dgZ$zoning_district == "R1-1", ]), 3005)
sfS <- st_transform(st_as_sf(dtS, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE), 3005)
inR1 <- lengths(st_intersects(sfS, dgZ)) > 0
dtS <- dtS[inR1]
sfS <- sfS[inR1, ]

# ============================================================================
# 2. 2026 folios on each rollStart (gpkg attributes via sqlite, no geometry)
#    + 2026 residential inventory for finished area and year built
# ============================================================================
con <- dbConnect(SQLite(), fileGpkg)
dt26 <- as.data.table(dbGetQuery(con,
  "SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION
   FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'"))
dbDisconnect(con)
dt26[, roll := rollKey(ROLL_NUMBER)]
dt26 <- unique(dt26, by = "roll")
dt26[, rollStart := floor(roll / 1000)]
dt26 <- dt26[rollStart %in% dtS$rollStart]

dtI <- fread(fInvTxt)
setnames(dtI, tolower(names(dtI)))
invCols <- c("roll_number", "jurisdiction", "mb_year_built", "mb_effective_year", "mb_total_finished_area")
if (!all(invCols %in% names(dtI))) { print(names(dtI)); stop("inventory column names differ") }
dtI <- dtI[as.numeric(jurisdiction) == 200]
dtI[, roll := rollKey(roll_number)]                        # one row per roll (verified)
dtI[, (invCols[-(1:2)]) := lapply(.SD, as.numeric), .SDcols = invCols[-(1:2)]]

dt26 <- merge(dt26, dtI[, .(roll, mb_year_built, mb_effective_year, mb_total_finished_area)],
              by = "roll", all.x = TRUE)

dtR <- dt26[, .(nFolio26 = .N,
                nArea26  = sum(!is.na(mb_total_finished_area)),
                area26   = sum(mb_total_finished_area, na.rm = TRUE),
                yb26     = if (all(is.na(mb_year_built))) NA_real_ else max(mb_year_built, na.rm = TRUE),
                use26    = paste(sort(unique(ACTUAL_USE_DESCRIPTION)), collapse = "; ")),
            by = rollStart]

# ============================================================================
# 3. Permits, live. New Building only; principal product + laneway flag.
# ============================================================================
dtP <- fread(urlP)
setnames(dtP, tolower(names(dtP)))
dtP <- dtP[typeofwork == "New Building" & !is.na(geo_point_2d) & geo_point_2d != ""]
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]
dtP[, product := fcase(grepl("Multiple Dwelling", specificusecategory), "multiplex",
                       grepl("Duplex",            specificusecategory), "duplex",
                       grepl("Single",            specificusecategory), "single",
                       default = NA_character_)]
dtP[, laneway := grepl("Laneway", specificusecategory)]
dtP <- dtP[!is.na(product) | laneway]
dtP[, applied := as.IDate(permitnumbercreateddate)]
dtP[, issued  := as.IDate(issuedate)]

sfP <- st_transform(st_as_sf(dtP, coords = c("lon", "lat"), crs = 4326), 3005)
nn  <- st_nearest_feature(sfP, sfS)
dtP[, dist := as.numeric(st_distance(sfP, sfS[nn, ], by_element = TRUE))]
dtP[, rollStart := dtS$rollStart[nn]]
dtP <- dtP[dist <= MATCHDIST]

dtPP   <- dtP[!is.na(product)][order(rollStart, -issued)][, .SD[1], by = rollStart]  # latest issued
dtLane <- unique(dtP[laneway == TRUE, .(rollStart, laneway)])

# ============================================================================
# 4. Assemble lot-level file; unit scaling
# ============================================================================
dt <- merge(dtS[, .(rollStart, landWidth, landDepth, lotArea, longitude, latitude,
                    area18 = MB_total_finished_area)],
            dtR, by = "rollStart", all.x = TRUE)
dt <- merge(dt, dtPP[, .(rollStart, permitnumber, product, specificusecategory,
                         applied, issued, projectvalue, dist)],
            by = "rollStart", all.x = TRUE)
dt <- merge(dt, dtLane, by = "rollStart", all.x = TRUE)
dt[is.na(laneway), laneway := FALSE]

dt[, fsr18 := area18 / lotArea]
dt[, fsr26 := area26 / lotArea]
dt[, built := !is.na(yb26) & yb26 >= BUILT_MIN & nArea26 == nFolio26]
dt[, completed := built & !is.na(issued) & yb26 >= year(issued)]   # 2025 issues: design area, provisional
dt[, `:=`(applyYear = year(applied), issueYear = year(issued))]
dt[, widthBin := fcase(landWidth > 30 & landWidth <= 36, "33",
                       landWidth > 36 & landWidth <= 40, "40",
                       landWidth > 41 & landWidth <= 50, "50",
                       default = "other")]

halfUse <- c("Duplex, Non-Strata Side by Side or Front / Back",
             "Duplex, Strata Front / Back", "Duplex, Strata Side by Side")
dt[, oneFolioMulti := completed & product %in% c("duplex", "multiplex") & nFolio26 == 1]
dt[, unitScale := fifelse(oneFolioMulti & product == "duplex" & use26 %in% halfUse, 2, 1)]
dt[, fsrAdj := fsr26 * unitScale]
dt[, fsrOK := completed & !(oneFolioMulti & unitScale == 1)]

# ============================================================================
# 5. Checks
# ============================================================================
cat("\n=== sample flow ===\n")
cat("2018 R1-1 SFD lots:", nrow(dt),
    "| with principal permit:", dt[!is.na(product), .N],
    "| permit + completed:", dt[completed == TRUE, .N],
    "| in FSR tables:", dt[fsrOK == TRUE, .N], "\n")
cat("rollStarts with >1 folio in 2026:", dt[nFolio26 > 1, .N],
    " <- near zero means the singles rds predates the dedup fix; delete and rerun\n")

cat("\n=== single-folio duplex/multiplex by 2026 use (scaled x2 / dropped) ===\n")
print(dt[oneFolioMulti == TRUE, .(N = .N, p50raw = round(median(fsr26), 3), scale = unitScale[1]),
         keyby = .(product, use26)])

cat("\n=== x2 validation: scaled single-folio vs multi-folio duplex, median FSR ===\n")
print(dcast(dt[fsrOK == TRUE & product == "duplex",
               .(N = .N, p50 = round(median(fsrAdj), 3)),
               by = .(issueYear, grp = fifelse(nFolio26 >= 2, "multiFolio", "scaledX2"))],
            issueYear ~ grp, value.var = c("p50", "N")))

cat("\n=== completion coverage by issue year (truncation) ===\n")
print(dcast(dt[!is.na(product)], issueYear ~ product,
            value.var = "completed", fun.aggregate = function(x) round(mean(x), 2)))

# ============================================================================
# 6. Results
# ============================================================================
summFSR <- function(x) list(N = length(x), mean = round(mean(x), 3),
                            p25 = round(quantile(x, .25), 3), p50 = round(median(x), 3),
                            p75 = round(quantile(x, .75), 3))

cat("\n=== realized FSR by product x issue year ===\n")
print(dt[fsrOK == TRUE, summFSR(fsrAdj), keyby = .(product, issueYear)])

cat("\n=== realized FSR by product x application year ===\n")
print(dt[fsrOK == TRUE, summFSR(fsrAdj), keyby = .(product, applyYear)])

cat("\n=== median FSR, duplex minus single, by issue year ===\n")
gap <- dcast(dt[fsrOK == TRUE & product %in% c("single", "duplex"),
                .(p50 = median(fsrAdj)), by = .(issueYear, product)],
             issueYear ~ product, value.var = "p50")
gap[, gap := round(duplex - single, 3)]
print(gap)

cat("\n=== realized FSR by product x width bin x laneway ===\n")
print(dt[fsrOK == TRUE, summFSR(fsrAdj), keyby = .(product, widthBin, laneway)])

cat("\n=== 2018 FSR of the replaced SFD, by later product ===\n")
print(dt[fsrOK == TRUE & !is.na(fsr18), summFSR(fsr18), keyby = product])

cat("\n=== single FSR by application month, 2023-2024 ===\n")
sm <- dt[product == "single" & applied %between% as.IDate(c("2023-01-01", "2024-12-31"))]
sm[, applyMonth := format(applied, "%Y-%m")]
cov <- sm[, .(nPermit = .N, cover = round(mean(fsrOK), 2)), keyby = applyMonth]
res <- sm[fsrOK == TRUE, .(N = .N,
                           p25 = round(quantile(fsrAdj, .25), 3),
                           p50 = round(median(fsrAdj), 3),
                           p75 = round(quantile(fsrAdj, .75), 3),
                           shareNew = round(mean(fsrAdj <= 0.63), 2)),
          keyby = applyMonth]
print(merge(cov, res, by = "applyMonth", all.x = TRUE))

cat("\n=== duplex & multiplex FSR by application month, 2023-2024 ===\n")
pm <- dt[product %in% c("duplex", "multiplex") &
         applied %between% as.IDate(c("2022-01-01", "2024-12-31"))]
pm[, applyMonth := format(applied, "%Y-%m")]
cov <- pm[, .(nPermit = .N, cover = round(mean(fsrOK), 2)), keyby = .(product, applyMonth)]
res <- pm[fsrOK == TRUE, .(N = .N,
                           p25 = round(quantile(fsrAdj, .25), 3),
                           p50 = round(median(fsrAdj), 3),
                           p75 = round(quantile(fsrAdj, .75), 3),
                           shareHi = round(mean(fsrAdj > 0.725), 2)),
          keyby = .(product, applyMonth)]
print(merge(cov, res, by = c("product", "applyMonth"), all.x = TRUE))

cat("\n=== permit mix by application month (all permits on 2018 R1-1 SFD lots) ===\n")
mix <- dcast(dt[!is.na(product) & applied %between% as.IDate(c("2022-01-01", "2024-12-31"))
               ][, applyMonth := format(applied, "%Y-%m")],
             applyMonth ~ product, fun.aggregate = length, value.var = "permitnumber")
mix[, dupShare := round(duplex / (single + duplex), 2)]
print(mix)

library(fixest)

ONTARIO_LON <- -123.1036
CUT <- 2023L * 12L + 10L                      # month index of Nov 2023 (first full R1-1 month)

pp <- dt[!is.na(product) & !is.na(applied)]
pp[, side := fifelse(longitude < ONTARIO_LON, "West", "East")]
pp[, ym := year(applied) * 12L + month(applied) - 1L]

mc <- pp[, .(dup = sum(product == "duplex"), mx = sum(product == "multiplex"),
             sgl = sum(product == "single")), keyby = .(side, ym)]
mc <- mc[CJ(side = c("East", "West"), ym = (CUT - 14L):(CUT + 11L)), on = .(side, ym)]
for (v in c("dup", "mx", "sgl")) set(mc, which(is.na(mc[[v]])), v, 0L)

prePost <- function(sd, k, donut, incMx) {
  m <- mc[side == sd]
  pre  <- if (donut) setdiff((CUT - k - 2L):(CUT - 1L), CUT - 1:2) else (CUT - k):(CUT - 1L)
  post <- CUT:(CUT + k - 1L)
  m[, num := dup + incMx * mx]
  m[, den := dup + sgl + incMx * mx]
  a <- m[ym %in% pre]; b <- m[ym %in% post]
  p1 <- sum(a$num) / sum(a$den); p2 <- sum(b$num) / sum(b$den)
  p  <- (sum(a$num) + sum(b$num)) / (sum(a$den) + sum(b$den))
  z  <- (p2 - p1) / sqrt(p * (1 - p) * (1 / sum(a$den) + 1 / sum(b$den)))
  s1 <- a[den > 0, num / den]; s2 <- b[den > 0, num / den]   # empty months dropped
  tm <- (mean(s2) - mean(s1)) / sqrt(var(s1) / length(s1) + var(s2) / length(s2))
  data.table(side = sd, k, donut, incMx,
             pre = round(p1, 3), nPre = sum(a$den), post = round(p2, 3), nPost = sum(b$den),
             diff = round(p2 - p1, 3), z = round(z, 2), tMonthly = round(tm, 2),
             sglPre = round(mean(a$sgl), 1), sglPost = round(mean(b$sgl), 1),
             dupPre = round(mean(a$dup), 1), dupPost = round(mean(b$dup), 1))
}

cat("\n=== pre/post duplex share around Oct 17 2023, by side ===\n")
specs <- CJ(side = c("West", "East"), k = c(6L, 12L), donut = c(FALSE, TRUE), incMx = c(FALSE, TRUE))
print(rbindlist(Map(prePost, specs$side, specs$k, specs$donut, specs$incMx)))

cat("\n=== diff-in-diff LPM: duplex vs single, post x West, month-clustered ===\n")
w <- pp[product %in% c("single", "duplex") & ym %between% c(CUT - 12L, CUT + 11L)]
w[, `:=`(y = as.integer(product == "duplex"), post = as.integer(ym >= CUT),
         west = as.integer(side == "West"))]
print(etable(feols(y ~ post * west, w, cluster = ~ym),
             feols(y ~ post * west, w[!ym %in% (CUT - 1:2)], cluster = ~ym),
             feols(y ~ post * west, w[ym %between% c(CUT - 6L, CUT + 5L)], cluster = ~ym),
             headers = c("12mo", "12mo donut", "6mo")))

cat("\n=== diff-in-diff LPM, 33-ft lots only ===\n")
w33 <- w[widthBin == "33"]
print(etable(feols(y ~ post * west, w33, cluster = ~ym),
             feols(y ~ post * west, w33[!ym %in% (CUT - 1:2)], cluster = ~ym),
             feols(y ~ post * west, w33[ym %between% c(CUT - 6L, CUT + 5L)], cluster = ~ym),
             headers = c("12mo", "12mo donut", "6mo")))
cat("N by side x post (33 ft):\n"); print(w33[, .N, keyby = .(west, post)])
