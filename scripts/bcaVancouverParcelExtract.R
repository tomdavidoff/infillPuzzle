# bcaVancouverParcelExtract.R
# One-time generalized spatial extract for City of Vancouver BCA parcels.
# Iterates over property types in propertyTypeMap (singleFamily, strata,
# duplex) and writes one folio-keyed RDS per type with a unified schema:
#
#   folioID, rollNumber, jurisdiction, actualUseDescription,
#   landWidth, landDepth, MB_effective_year, MB_total_finished_area,
#   lon, lat, zoning2025, geom
#
# IMPORTANT: actualUseDescription is the BCA 2019 vintage, pulled from
# folioDescription in REVD19_and_inventory_extracts.sqlite3. The
# geopackage carries a 2025-vintage ACTUAL_USE_DESCRIPTION which would
# misclassify parcels that have since been redeveloped (e.g. a 2019 SF
# parcel built as a duplex in 2022 shows up as duplex in the 2025
# layer). Using the 2019 type preserves the "former single family"
# sample correctly for downstream permit choice work.
#
# zoning2025 comes from a spatial join against vancouver_zoning.geojson
# (2025 vintage). For singleFamily and duplex outputs we additionally
# filter to zoning2025 == "R1-1" so the written file is exactly the
# parcels eligible for the R1-1 small-multiplex policy regime. Strata
# is written unfiltered (strata buildings are allowed anywhere).
#
# Output is an sf object (geom column is the parcel polygon) so that
# downstream point-in-polygon joins (e.g. attaching permits to parcels
# via st_within) work directly. lon/lat are the polygon centroids and
# remain available for non-spatial workflows.
#
# For strata, landWidth/landDepth will mostly be NA (unit-level folios
# carry no meaningful lot dimensions on the spatial layer). For SF and
# duplex, both are populated. The COV pipeline filters on lot dims
# only when relevant.
#
# Output files (one per type present in the map):
#   ~/DropboxExternal/dataProcessed/bca_vancouver_singleFamily.rds  (R1-1 only)
#   ~/DropboxExternal/dataProcessed/bca_vancouver_strata.rds        (all zones)
#   ~/DropboxExternal/dataProcessed/bca_vancouver_duplex.rds        (R1-1 only)
#
# Tom Davidoff
# 05/18/26

library(data.table)
library(sf)
library(DBI)
library(RSQLite)

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
BCA_GPKG_DIR    <- "~/bigFiles/latestSpatialBCA/"
SQLITE_FILE     <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
ZONING_GEOJSON  <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
TYPE_MAP_FILE   <- "~/DropboxExternal/dataProcessed/propertyTypeMap.rds"
OUT_DIR         <- "~/DropboxExternal/dataProcessed/"
VANCOUVER_JUR   <- "200"
TYPES_TO_RUN    <- c("singleFamily", "strata", "duplex")
# Types that should be filtered to R1-1 before writing (strata = unfiltered).
TYPES_R1_1_ONLY <- c("singleFamily") # not duplex bc almost all in RT, "duplex")
TARGET_ZONE     <- "R1-1"

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
if (!file.exists(path.expand(TYPE_MAP_FILE)))
  stop("propertyTypeMap.rds not found. Run bcaPropertyTypes.R first.")
typeMap <- readRDS(path.expand(TYPE_MAP_FILE))

cat("\nProperty types to extract:\n")
for (tp in TYPES_TO_RUN) {
  cats <- typeMap[[tp]]
  cat(sprintf(" -- %s (%d categories) --\n", tp, length(cats)))
  for (s in cats) cat("    ", s, "\n")
}

allCats <- unique(unlist(typeMap[TYPES_TO_RUN]))
if (length(allCats) == 0) stop("No categories to extract; check propertyTypeMap.")

gpkgFiles <- list.files(path.expand(BCA_GPKG_DIR),
                        pattern = "\\.gpkg$", full.names = TRUE)
if (length(gpkgFiles) == 0) stop("No .gpkg in ", BCA_GPKG_DIR)
gpkg <- gpkgFiles[1]

# ---------------------------------------------------------------------------
# 1. Spatial pull -- ALL Vancouver residential parcels. We will filter
#    on type using the 2019 vintage from SQLite, not the 2025 vintage
#    that lives in the geopackage.
# ---------------------------------------------------------------------------
cat(sprintf("\nReading geopackage: %s\n", gpkg))
qSpatial <- sprintf(paste(
  "SELECT ROLL_NUMBER, JURISDICTION_CODE, geom",
  "FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
  "WHERE JURISDICTION_CODE = '%s'"),
  VANCOUVER_JUR)

sfAll <- st_read(gpkg,
                 layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
                 query = qSpatial, quiet = TRUE)
cat(sprintf("Spatial rows pulled (all Vancouver): %d\n", nrow(sfAll)))
if (nrow(sfAll) == 0) stop("Spatial query returned zero rows.")

sfAll <- st_transform(sfAll, 4326)
# Some BCA parcel polygons have self-intersecting loops; fix before any
# geometry op (centroid / spatial join) that uses S2.
sfAll <- st_make_valid(sfAll)
cent  <- st_centroid(st_geometry(sfAll))
sfAll$lon <- st_coordinates(cent)[, 1]
sfAll$lat <- st_coordinates(cent)[, 2]

# ---- Zoning join (2025 vintage) -------------------------------------------
cat(sprintf("Reading zoning: %s\n", ZONING_GEOJSON))
sfZone <- st_read(path.expand(ZONING_GEOJSON), quiet = TRUE)
sfZone <- st_transform(sfZone, 4326)
sfZone <- sfZone[, "zoning_district"]
sfZone <- st_make_valid(sfZone)

# Assign each parcel the zoning polygon that contains its CENTROID.
# This is much faster than parcel-polygon to zoning-polygon overlay with
# largest=TRUE, and parcels are small enough relative to zoning blocks
# that the difference is negligible for COV. (If a parcel straddles a
# zoning boundary, the centroid lands in one zone and that wins.)
sfCent <- st_as_sf(data.frame(idx = seq_len(nrow(sfAll))),
                   geometry = cent, crs = 4326)
zHit   <- st_join(sfCent, sfZone, join = st_within)
# Centroid in a single polygon -> one row per parcel; if zoning has
# overlapping polygons we'd get duplicates, so collapse to first match.
zHit   <- zHit[!duplicated(zHit$idx), ]
sfAll$zoning2025 <- NA_character_
sfAll$zoning2025[zHit$idx] <- as.character(zHit$zoning_district)
cat(sprintf("Parcels with a 2025 zoning match: %d / %d\n",
            sum(!is.na(sfAll$zoning2025)), nrow(sfAll)))
cat("Top zoning2025 counts:\n")
print(as.data.table(st_drop_geometry(sfAll))[
        !is.na(zoning2025), .N, by = zoning2025][order(-N)][1:15])

# Keep geometry. Carry it through as a list-column on a data.table so
# the existing keyed joins below work unchanged; re-promote to sf at
# write time.
sfAll$rollNumber   <- trimws(as.character(sfAll$ROLL_NUMBER))
sfAll$jurisdiction <- trimws(as.character(sfAll$JURISDICTION_CODE))
sfAll <- sfAll[, c("rollNumber","jurisdiction","lon","lat","zoning2025")]
sfAll <- sfAll[!is.na(sfAll$rollNumber) & sfAll$rollNumber != "", ]
dtSpatial <- as.data.table(st_drop_geometry(sfAll))
dtSpatial[, geom := st_geometry(sfAll)]

# CRITICAL: 2025 roll numbers can differ from 2019 roll numbers for the
# same physical parcel. When a 2019 SF parcel was subdivided into a
# strata duplex by 2025, the 2025 layer has TWO rolls (one per unit)
# with new suffixes, while 2019 has ONE roll for the pre-subdivision
# parent. Joining on full rollNumber LOSES those parcels -- they end up
# with NA actualUseDescription and get dropped from the SF output,
# which is the very file downstream code uses to enforce "former SF"
# samples. We join on roll_base instead (the first ~7 digits, stripping
# the 3-digit unit suffix), and dedupe the 2025 spatial side to one
# representative polygon per roll_base.
dtSpatial[, roll_base := floor(as.numeric(rollNumber) / 1000)]
nBefore <- nrow(dtSpatial)
dtSpatial <- dtSpatial[!duplicated(roll_base)]
cat(sprintf("Deduped spatial rows by roll_base: %d -> %d\n",
            nBefore, nrow(dtSpatial)))

# ---------------------------------------------------------------------------
# 2. SQLite pulls -- folio (folioID), inventory (sqft, year, dims),
#    and folioDescription (2019 actualUseDescription + dim fallback).
# ---------------------------------------------------------------------------
con <- dbConnect(SQLite(), SQLITE_FILE)
folio <- as.data.table(dbGetQuery(con,
  sprintf("SELECT folioID, rollNumber, jurisdictionCode FROM folio
           WHERE jurisdictionCode = %s", VANCOUVER_JUR)))
inventory <- as.data.table(dbGetQuery(con,
  sprintf("SELECT roll_number, jurisdiction,
                  land_width, land_depth,
                  MB_effective_year, MB_total_finished_area
           FROM residentialInventory
           WHERE jurisdiction = %s", VANCOUVER_JUR)))
description <- as.data.table(dbGetQuery(con,
  "SELECT folioID, actualUseDescription, landWidth, landDepth
   FROM folioDescription"))
dbDisconnect(con)

folio[, rollNumber   := trimws(as.character(rollNumber))]
folio[, jurisdiction := trimws(as.character(jurisdictionCode))]
folio[, roll_base    := floor(as.numeric(rollNumber) / 1000)]
folio <- folio[!is.na(roll_base)]
folio <- folio[, .(folioID, roll_base, jurisdiction)]
# One folio per roll_base (most SF rolls are already unique; if any
# 2019 parcel had multiple folios sharing a roll_base, take the first).
folio <- unique(folio, by = c("roll_base","jurisdiction"))

inventory[, rollNumber   := trimws(as.character(roll_number))]
inventory[, jurisdiction := trimws(as.character(jurisdiction))]
inventory[, roll_base    := floor(as.numeric(rollNumber) / 1000)]
inventory <- inventory[!is.na(roll_base)]
inventory <- unique(inventory, by = c("roll_base","jurisdiction"))
inventory <- inventory[, .(roll_base, jurisdiction,
                           land_width, land_depth,
                           MB_effective_year, MB_total_finished_area)]

# folioDescription is the 2019-vintage source of actualUseDescription.
# Rename width/depth here so the inventory join doesn't collide and so
# the fallback reference is unambiguous downstream.
description[, folioID := as.character(folioID)]
setnames(description, c("landWidth","landDepth"),
                      c("fdLandWidth","fdLandDepth"))
description <- unique(description, by = "folioID")

# ---------------------------------------------------------------------------
# 3. Merge: spatial -> inventory (by roll/jurisdiction), then -> folio,
#    then attach folioDescription (which brings the 2019 actualUseDescription
#    and dim fallback). Result is folio-keyed.
# ---------------------------------------------------------------------------
setkey(dtSpatial, roll_base, jurisdiction)
setkey(inventory, roll_base, jurisdiction)
dt <- inventory[dtSpatial, nomatch = NA]

setkey(folio, roll_base, jurisdiction)
setkey(dt,    roll_base, jurisdiction)
dt <- folio[dt, nomatch = NA, allow.cartesian = TRUE]

dt[, folioID := as.character(folioID)]
setkey(description, folioID)
setkey(dt,          folioID)
dt <- description[dt, nomatch = NA]

# Unify lot-dimension columns: prefer inventory.land_width/land_depth,
# fall back to folioDescription.fdLandWidth/fdLandDepth.
dt[, landWidth := as.numeric(land_width)]
dt[, landDepth := as.numeric(land_depth)]
dt[is.na(landWidth) & !is.na(fdLandWidth), landWidth := as.numeric(fdLandWidth)]
dt[is.na(landDepth) & !is.na(fdLandDepth), landDepth := as.numeric(fdLandDepth)]

dtOut <- dt[, .(folioID, rollNumber, jurisdiction,
                actualUseDescription,
                landWidth, landDepth,
                MB_effective_year, MB_total_finished_area,
                lon, lat, zoning2025, geom)]

cat(sprintf("\nMerged folio-keyed rows: %d\n", nrow(dtOut)))
cat(sprintf("  with folioID:                   %d\n",
            sum(!is.na(dtOut$folioID))))
cat(sprintf("  with 2019 actualUseDescription: %d\n",
            sum(!is.na(dtOut$actualUseDescription))))
cat(sprintf("  with 2025 zoning2025:           %d\n",
            sum(!is.na(dtOut$zoning2025))))
cat(sprintf("  in R1-1:                        %d\n",
            sum(dtOut$zoning2025 == TARGET_ZONE, na.rm = TRUE)))
cat(sprintf("  with lon/lat:                   %d\n",
            sum(!is.na(dtOut$lon) & !is.na(dtOut$lat))))
cat(sprintf("  with sqft:                      %d\n",
            sum(!is.na(dtOut$MB_total_finished_area) &
                as.numeric(dtOut$MB_total_finished_area) > 0)))
cat(sprintf("  with lot dims:                  %d\n",
            sum(!is.na(dtOut$landWidth) & !is.na(dtOut$landDepth) &
                dtOut$landWidth > 0 & dtOut$landDepth > 0)))

cat("\nTop 2019 actualUseDescription values:\n")
print(dtOut[!is.na(actualUseDescription),
            .N, by = actualUseDescription][order(-N)][1:15])

# ---------------------------------------------------------------------------
# 4. Split by property type (2019 vintage), apply R1-1 filter for SF and
#    duplex, write one RDS each as sf.
# ---------------------------------------------------------------------------
for (tp in TYPES_TO_RUN) {
  cats <- typeMap[[tp]]
  if (length(cats) == 0) {
    cat(sprintf("\n[%s] no categories; skipping.\n", tp)); next
  }
  d <- dtOut[actualUseDescription %in% cats]
  preFilter <- nrow(d)
  if (tp %in% TYPES_R1_1_ONLY) {
    d <- d[!is.na(zoning2025) & zoning2025 == TARGET_ZONE]
    cat(sprintf("\n[%s] R1-1 filter: %d -> %d rows\n", tp, preFilter, nrow(d)))
  } else {
    cat(sprintf("\n[%s] no zoning filter (all zones kept): %d rows\n",
                tp, nrow(d)))
  }
  if (nrow(d) == 0) {
    cat(sprintf("  [%s] zero rows after filter; skipping.\n", tp)); next
  }
  # Promote to sf: the geom list-column becomes the active geometry.
  sfTp <- st_as_sf(d, sf_column_name = "geom", crs = 4326)
  outPath <- file.path(path.expand(OUT_DIR),
                       sprintf("bca_vancouver_%s.rds", tp))
  saveRDS(sfTp, outPath)
  cat(sprintf("  wrote %d rows -> %s\n", nrow(sfTp), outPath))
  cat(sprintf("  diagnostics: folioID = %d  lon/lat = %d  sqft = %d  lotdim = %d\n",
              sum(!is.na(sfTp$folioID)),
              sum(!is.na(sfTp$lon) & !is.na(sfTp$lat)),
              sum(!is.na(sfTp$MB_total_finished_area) &
                  as.numeric(sfTp$MB_total_finished_area) > 0),
              sum(!is.na(sfTp$landWidth) & !is.na(sfTp$landDepth) &
                  sfTp$landWidth > 0 & sfTp$landDepth > 0)))
  cat("  category counts:\n")
  print(as.data.table(st_drop_geometry(sfTp))[
            , .N, by = actualUseDescription][order(-N)])
}

cat("\nDone.\n")
