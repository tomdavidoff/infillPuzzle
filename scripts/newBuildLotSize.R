# newBuildLotSize.R
# Post-reform (2019-2024 built) duplex vs SFD lot-size analysis, East/West (Ontario St).
# FOUR lot-width estimates per parcel, cross-checked:
#   wGpkgCol  - LAND_WIDTH column, 2026 gpkg folio descriptions
#   wGeom     - width implied by parcel polygon area (st_area or FEATURE_AREA_SQM) / depth
#   wRoll19   - inherited 2019 SFD landWidth via rollStart back-match (pre-redevelopment)
#   wSpatial  - 2019 SFD landWidth via nearest-folio spatial join (fallback for unmatched roll)
# Parcel-level stats dedup to one row per rollStart; sales kept at unit level.
# Reads only; no writes beyond the guarded bca19 singles rds (already exists).
# Tom Davidoff
# 09/08/26

BUILT_MINYEAR <- 2019
BUILT_MAXYEAR <- 2024
SALE_MINYEAR  <- 2022      # "recent" price overlay
SALE_MAXYEAR  <- 2026
ONTARIO_LON   <- -123.1036
MATCHDIST     <- 30        # m, spatial fallback gate (looser than permit-match: parcel centroids)
WIDTH_TOL     <- 3         # ft, agreement tolerance across methods

library(data.table)
library(sf)
library(RSQLite)
library(fixest)

sf_use_s2(TRUE)

dirGpkg   <- "~/bigFiles/latestSpatialBCA/"
fInvTxt   <- "~/DropboxExternal/dataRaw/Residential_inventory_202601/20260101_A09_Residential_Inventory_Extract.txt"
fSingles  <- "~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds"   # has rollNumber, landWidth, lon/lat
fPermits  <- "~/DropboxExternal/dataRaw/issued-building-permitsDwellingUses.csv"

gpkgFiles <- list.files(dirGpkg, pattern = "\\.gpkg$", full.names = TRUE)
stopifnot(length(gpkgFiles) == 1)
fileGpkg <- gpkgFiles

# ============================================================================
# 1. 2026 gpkg: folio descriptions (use, width, depth, area, geom) + values
# ============================================================================
dfD <- st_read(fileGpkg, query =
  "SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD,
          LAND_WIDTH, LAND_DEPTH, LAND_SIZE, FEATURE_AREA_SQM, geom
   FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
dfD <- st_make_valid(dfD)

# geom-based area (m^2) and centroid lon/lat. FEATURE_AREA_SQM is on the table;
# compute st_area too and compare -- if they agree, prefer the column (cheaper).
dfD$areaGeom <- as.numeric(st_area(dfD))                    # m^2 from polygon
dfD4326 <- st_transform(dfD, 4326)
ctr <- st_coordinates(st_centroid(st_geometry(dfD4326)))

dtD <- as.data.table(st_drop_geometry(dfD))
dtD[, `:=`(longitude = ctr[,1], latitude = ctr[,2])]
dtD[, rollNum   := as.numeric(ROLL_NUMBER)]                 # double: 15 digits safe, int would NA
dtD[, rollStart := floor(rollNum / 1000)]                  # parcel stem (strips unit suffix)

cat("=== FEATURE_AREA_SQM vs st_area(geom) agreement (should be ~1:1) ===\n")
print(dtD[!is.na(FEATURE_AREA_SQM) & FEATURE_AREA_SQM > 0 & areaGeom > 0,
          .(n = .N,
            ratio_p10 = quantile(areaGeom/FEATURE_AREA_SQM, .10),
            ratio_p50 = quantile(areaGeom/FEATURE_AREA_SQM, .50),
            ratio_p90 = quantile(areaGeom/FEATURE_AREA_SQM, .90))])
# use the column where present & sane, else the computed area
dtD[, areaM2 := fifelse(!is.na(FEATURE_AREA_SQM) & FEATURE_AREA_SQM > 0,
                        FEATURE_AREA_SQM, areaGeom)]
dtD[, areaFt2 := areaM2 * 10.7639]

# ============================================================================
# 2. inventory .txt: structure (year built, finished area) keyed on Roll_Number
# ============================================================================
dtI <- fread(fInvTxt)
setnames(dtI, make.names(names(dtI)))                      # safe names
dtI[, rollNum := as.numeric(Roll_Number)]
dtI <- dtI[, .(rollNum,
               MB_Year_Built      = as.numeric(MB_Year_Built),
               MB_Effective_Year  = as.numeric(MB_Effective_Year),
               MB_Total_Finished_Area = as.numeric(MB_Total_Finished_Area),
               invZoning = Zoning)]
dtD <- merge(dtD, dtI, by = "rollNum", all.x = TRUE)       # full-roll join (unit level)

# ============================================================================
# 3. width method 1 (column) and method 2 (geom area / depth)
# ============================================================================
dtD[, wGpkgCol := LAND_WIDTH]
# geom width: area / depth, when depth is present & plausible; feet throughout
dtD[, wGeom := fifelse(!is.na(LAND_DEPTH) & LAND_DEPTH > 10, areaFt2 / LAND_DEPTH, NA_real_)]

# ============================================================================
# 4. width methods 3 & 4: inherit 2019 SFD width by rollStart, then spatial
# ============================================================================
dtS19 <- as.data.table(readRDS(fSingles))                  # rollNumber, landWidth, longitude, latitude
dtS19[, rollNum19   := as.numeric(rollNumber)]
dtS19[, rollStart19 := floor(rollNum19 / 1000)]

# 3: rollStart back-match (one 2019 width per stem; dedup 2019 side first)
w19 <- dtS19[!is.na(landWidth) & landWidth > 0,
             .(wRoll19 = median(landWidth)), by = .(rollStart = rollStart19)]
dtD <- merge(dtD, w19, by = "rollStart", all.x = TRUE)
cat("\n=== rollStart back-match rate to 2019 SFD width ===\n")
cat("gpkg parcels:", uniqueN(dtD$rollStart),
    "| with a 2019 rollStart width:", dtD[!is.na(wRoll19), uniqueN(rollStart)], "\n")

# 4: spatial fallback -- nearest 2019 SFD folio, gate at MATCHDIST
#    only computed for parcels lacking a rollStart match, to save the NN cost
needSpatial <- dtD[is.na(wRoll19) & !is.na(longitude) & !is.na(latitude)]
if (nrow(needSpatial) > 0) {
  sfNeed <- st_transform(st_as_sf(needSpatial[, .(rollNum, longitude, latitude)],
                                  coords = c("longitude","latitude"), crs = 4326), 3005)
  sf19   <- st_transform(st_as_sf(dtS19[!is.na(longitude) & !is.na(latitude) &
                                        !is.na(landWidth) & landWidth > 0,
                                        .(landWidth, longitude, latitude)],
                                  coords = c("longitude","latitude"), crs = 4326), 3005)
  idx  <- st_nearest_feature(sfNeed, sf19)
  dNN  <- as.numeric(st_distance(sfNeed, sf19[idx, ], by_element = TRUE))
  wsp  <- as.data.table(st_drop_geometry(sf19))[idx]$landWidth
  needSpatial[, `:=`(wSpatial = fifelse(dNN <= MATCHDIST, wsp, NA_real_), spatDist = dNN)]
  dtD <- merge(dtD, needSpatial[, .(rollNum, wSpatial, spatDist)], by = "rollNum", all.x = TRUE)
} else dtD[, `:=`(wSpatial = NA_real_, spatDist = NA_real_)]

# combined "best" 2019-based width: rollStart if present else spatial
dtD[, w2019 := fifelse(!is.na(wRoll19), wRoll19, wSpatial)]

# ============================================================================
# 5. classify: type, side, new-build vintage (dedup to parcel for lot stats)
# ============================================================================
sfdUse <- c("Single Family Dwelling", "Residential Dwelling with Suite")
dtD[, type := fifelse(grepl("Duplex", ACTUAL_USE_DESCRIPTION), "duplex",
              fifelse(ACTUAL_USE_DESCRIPTION %in% sfdUse,      "SFD", NA_character_))]
dtD[, side := fifelse(longitude < ONTARIO_LON, "West", "East")]
dtD[, newBuild := MB_Year_Built %between% c(BUILT_MINYEAR, BUILT_MAXYEAR)]

# one row per parcel: for strata duplexes both unit-folios carry identical parcel
# geom/width, so median within rollStart is the parcel value (dedup, no double count)
parcel <- dtD[!is.na(type),
              .(type      = type[1],
                side      = side[1],
                newBuild  = any(newBuild, na.rm = TRUE),
                MB_Year_Built = median(MB_Year_Built, na.rm = TRUE),
                finArea   = median(MB_Total_Finished_Area, na.rm = TRUE),
                wGpkgCol  = median(wGpkgCol,  na.rm = TRUE),
                wGeom     = median(wGeom,     na.rm = TRUE),
                w2019     = median(w2019,     na.rm = TRUE),
                wRoll19   = median(wRoll19,   na.rm = TRUE),
                wSpatial  = median(wSpatial,  na.rm = TRUE),
                nUnits    = .N),
              by = rollStart]

# ============================================================================
# 6. DO THE METHODS AGREE?  (the completeness check you asked for)
# ============================================================================
cat("\n=== coverage: non-missing width by method (new-build parcels) ===\n")
nb <- parcel[newBuild == TRUE & !is.na(type)]
for (m in c("wGpkgCol","wGeom","wRoll19","wSpatial","w2019"))
  cat(sprintf("  %-9s present: %4d / %4d\n", m, sum(!is.na(nb[[m]])), nrow(nb)))

cat("\n=== pairwise agreement on new-build parcels (correlation; median |diff| ft) ===\n")
pairs <- list(c("wGpkgCol","wGeom"), c("wGpkgCol","w2019"),
              c("wGeom","w2019"),    c("wRoll19","wSpatial"))
for (p in pairs) {
  d <- nb[!is.na(get(p[1])) & !is.na(get(p[2]))]
  if (nrow(d) < 5) { cat(sprintf("  %s vs %s: n<5\n", p[1], p[2])); next }
  cat(sprintf("  %-9s vs %-9s  n=%4d  cor=%.3f  med|diff|=%.1f  agree(<%dft)=%.0f%%\n",
              p[1], p[2], nrow(d),
              cor(d[[p[1]]], d[[p[2]]]),
              median(abs(d[[p[1]]] - d[[p[2]]])),
              WIDTH_TOL,
              100*mean(abs(d[[p[1]]] - d[[p[2]]]) < WIDTH_TOL)))
}

cat("\n=== where methods disagree: worst 10 new-build duplex parcels ===\n")
print(nb[type=="duplex"][order(-abs(wGpkgCol - w2019))][1:10,
        .(rollStart, side, wGpkgCol = round(wGpkgCol), wGeom = round(wGeom),
          wRoll19 = round(wRoll19), wSpatial = round(wSpatial), MB_Year_Built)])

# ============================================================================
# 7. THE PUZZLE FOR NEW PRODUCT: lot-width distribution, duplex vs SFD x side
#    Reported under each width method so we can see the answer isn't an artifact
# ============================================================================
widthDist <- function(wcol) {
  cat(sprintf("\n=== new-build lot width [%s]: round-width mix, duplex vs SFD x side ===\n", wcol))
  d <- nb[!is.na(get(wcol))]
  d[, wc := round(get(wcol))]
  # share landing on 33 vs 50 vs other, by type x side
  d[, wclass := fifelse(wc %in% 32:34, "33", fifelse(wc %in% 49:51, "50", "other"))]
  print(dcast(d, type + side ~ wclass, fun.aggregate = length, value.var = "rollStart"))
  cat(" median width by type x side:\n")
  print(d[, .(n = .N, p25 = quantile(get(wcol),.25),
              p50 = median(get(wcol)), p75 = quantile(get(wcol),.75)),
          by = .(type, side)][order(type, side)])
}
for (w in c("wGpkgCol","w2019")) widthDist(w)   # column-based and 2019-inherited

# ============================================================================
# 8. RECENT SALES price overlay (unit level -- both half-duplex sales count)
# ============================================================================
dfSale <- st_read(fileGpkg, query =
  "SELECT ROLL_NUMBER, CONVEYANCE_DATE, CONVEYANCE_PRICE, CONVEYANCE_TYPE_DESCRIPTION
   FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_SALES_SV
   WHERE JURISDICTION_CODE = '200'")
dtSale <- as.data.table(st_drop_geometry(dfSale))
cat("\n=== distinct conveyance types (pick the arm's-length one) ===\n")
print(dtSale[, .N, by = CONVEYANCE_TYPE_DESCRIPTION][order(-N)])

dtSale[, rollNum := as.numeric(ROLL_NUMBER)]
dtSale[, price   := as.numeric(CONVEYANCE_PRICE)]
dtSale[, saleYear := as.numeric(substr(CONVEYANCE_DATE, 1, 4))]
dtSale <- dtSale[CONVEYANCE_TYPE_DESCRIPTION == "Improved Single Property Transaction" &
                 price > 0 & saleYear %between% c(SALE_MINYEAR, SALE_MAXYEAR)]

# join unit-level sale to unit-level use/vintage/side (dtD, not parcel), + parcel width
dtU <- merge(dtSale, dtD[, .(rollNum, type, side, newBuild, MB_Year_Built,
                             MB_Total_Finished_Area, w2019, wGpkgCol)],
             by = "rollNum")
dtUn <- dtU[newBuild == TRUE & !is.na(type)]
cat("\n=== recent (", SALE_MINYEAR, "-", SALE_MAXYEAR,
    ") sales of new-build stock: n by type x side ===\n", sep="")
print(dtUn[, .(n = .N, p50price = median(price)), by = .(type, side)][order(type, side)])

cat("\n=== new-build price-sqft elasticity, duplex vs SFD (recent sales) ===\n")
dtUn[, wUse := fifelse(!is.na(w2019), w2019, wGpkgCol)]
for (ty in c("SFD","duplex")) {
  d <- dtUn[type == ty & !is.na(MB_Total_Finished_Area) & MB_Total_Finished_Area > 0]
  cat(sprintf("\n---- %s (n=%d) ----\n", ty, nrow(d)))
  if (nrow(d) < 10) { cat("  too few obs\n"); next }
  print(summary(feols(log(price) ~ log(MB_Total_Finished_Area) | MB_Year_Built + saleYear,
                      data = d, vcov = "hetero")))
}
