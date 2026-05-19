# bcaVancouverParcelExtract.R
# One-time generalized spatial extract for City of Vancouver BCA parcels.
# Iterates over property types in propertyTypeMap (singleFamily, strata,
# duplex) and writes one folio-keyed RDS per type with a unified schema:
#
#   folioID, rollNumber, jurisdiction, actualUseDescription,
#   landWidth, landDepth, MB_effective_year, MB_total_finished_area,
#   lon, lat
#
# For strata, landWidth/landDepth will mostly be NA (unit-level folios
# carry no meaningful lot dimensions on the spatial layer). For SF and
# duplex, both are populated. The COV pipeline filters on lot dims
# only when relevant.
#
# Centroids are computed from each parcel polygon. Strata buildings
# typically have one polygon serving all unit folios; the folio-level
# rows here share that building centroid, which is the correct location
# input for kernel-weighted spatial regressions.
#
# Output files (one per type present in the map):
#   ~/DropboxExternal/dataProcessed/bca_vancouver_singleFamily.rds
#   ~/DropboxExternal/dataProcessed/bca_vancouver_strata.rds
#   ~/DropboxExternal/dataProcessed/bca_vancouver_duplex.rds
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
BCA_GPKG_DIR  <- "~/bigFiles/latestSpatialBCA/"
SQLITE_FILE   <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
TYPE_MAP_FILE <- "~/DropboxExternal/dataProcessed/propertyTypeMap.rds"
OUT_DIR       <- "~/DropboxExternal/dataProcessed/"
VANCOUVER_JUR <- "200"
TYPES_TO_RUN  <- c("singleFamily", "strata", "duplex")

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

# Build SQL-safe IN clause.
sqlInList <- paste0("(",
  paste0("'", gsub("'", "''", allCats), "'", collapse = ", "), ")")

gpkgFiles <- list.files(path.expand(BCA_GPKG_DIR),
                        pattern = "\\.gpkg$", full.names = TRUE)
if (length(gpkgFiles) == 0) stop("No .gpkg in ", BCA_GPKG_DIR)
gpkg <- gpkgFiles[1]

# ---------------------------------------------------------------------------
# 1. Spatial pull -- ONE query covers all types.
# ---------------------------------------------------------------------------
cat(sprintf("\nReading geopackage: %s\n", gpkg))
qSpatial <- sprintf(paste(
  "SELECT ROLL_NUMBER, JURISDICTION_CODE, ACTUAL_USE_DESCRIPTION, geom",
  "FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
  "WHERE JURISDICTION_CODE = '%s'",
  "AND ACTUAL_USE_DESCRIPTION IN %s"),
  VANCOUVER_JUR, sqlInList)

sfAll <- st_read(gpkg,
                 layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
                 query = qSpatial, quiet = TRUE)
cat(sprintf("Spatial rows pulled: %d\n", nrow(sfAll)))
if (nrow(sfAll) == 0) stop("Spatial query returned zero rows.")

sfAll <- st_transform(sfAll, 4326)
cent  <- st_centroid(st_geometry(sfAll))
sfAll$lon <- st_coordinates(cent)[, 1]
sfAll$lat <- st_coordinates(cent)[, 2]
dtSpatial <- as.data.table(st_drop_geometry(sfAll))
dtSpatial[, rollNumber   := trimws(as.character(ROLL_NUMBER))]
dtSpatial[, jurisdiction := trimws(as.character(JURISDICTION_CODE))]
dtSpatial[, actualUseDescription := ACTUAL_USE_DESCRIPTION]
dtSpatial <- dtSpatial[, .(rollNumber, jurisdiction,
                           actualUseDescription, lon, lat)]
dtSpatial <- dtSpatial[!is.na(rollNumber) & rollNumber != ""]

# ---------------------------------------------------------------------------
# 2. SQLite pulls -- folio (folioID) and inventory (sqft, year, dims).
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
  sprintf("SELECT folioID, landWidth, landDepth FROM folioDescription")))
dbDisconnect(con)

folio[, rollNumber   := trimws(as.character(rollNumber))]
folio[, jurisdiction := trimws(as.character(jurisdictionCode))]
folio <- folio[, .(folioID, rollNumber, jurisdiction)]

inventory[, rollNumber   := trimws(as.character(roll_number))]
inventory[, jurisdiction := trimws(as.character(jurisdiction))]
inventory <- unique(inventory, by = c("rollNumber","jurisdiction"))
inventory <- inventory[, .(rollNumber, jurisdiction,
                           land_width, land_depth,
                           MB_effective_year, MB_total_finished_area)]

# folioDescription has its own width/depth columns; we'll prefer
# residentialInventory's land_width/land_depth and fall back to these.
# Rename here so the join doesn't collide with inventory's columns and
# so the fallback reference is unambiguous downstream.
description[, folioID := as.character(folioID)]
setnames(description, c("landWidth","landDepth"),
                      c("fdLandWidth","fdLandDepth"))
description <- unique(description, by = "folioID")

# ---------------------------------------------------------------------------
# 3. Merge: spatial -> inventory (by roll/jurisdiction), then -> folio,
#    then attach folioDescription dims as fallback. Result is folio-keyed.
# ---------------------------------------------------------------------------
setkey(dtSpatial, rollNumber, jurisdiction)
setkey(inventory, rollNumber, jurisdiction)
dt <- inventory[dtSpatial, nomatch = NA]

setkey(folio, rollNumber, jurisdiction)
setkey(dt,    rollNumber, jurisdiction)
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
                lon, lat)]

cat(sprintf("\nMerged folio-keyed rows: %d\n", nrow(dtOut)))
cat(sprintf("  with folioID:    %d\n", sum(!is.na(dtOut$folioID))))
cat(sprintf("  with lon/lat:    %d\n",
            sum(!is.na(dtOut$lon) & !is.na(dtOut$lat))))
cat(sprintf("  with sqft:       %d\n",
            sum(!is.na(dtOut$MB_total_finished_area) &
                as.numeric(dtOut$MB_total_finished_area) > 0)))
cat(sprintf("  with lot dims:   %d\n",
            sum(!is.na(dtOut$landWidth) & !is.na(dtOut$landDepth) &
                dtOut$landWidth > 0 & dtOut$landDepth > 0)))

# ---------------------------------------------------------------------------
# 4. Split by property type and write one RDS each.
# ---------------------------------------------------------------------------
for (tp in TYPES_TO_RUN) {
  cats <- typeMap[[tp]]
  if (length(cats) == 0) {
    cat(sprintf("\n[%s] no categories; skipping.\n", tp)); next
  }
  d <- dtOut[actualUseDescription %in% cats]
  outPath <- file.path(path.expand(OUT_DIR),
                       sprintf("bca_vancouver_%s.rds", tp))
  saveRDS(d, outPath)
  cat(sprintf("\n[%s] wrote %d rows -> %s\n", tp, nrow(d), outPath))
  cat(sprintf("  diagnostics: folioID = %d  lon/lat = %d  sqft = %d  lotdim = %d\n",
              sum(!is.na(d$folioID)),
              sum(!is.na(d$lon) & !is.na(d$lat)),
              sum(!is.na(d$MB_total_finished_area) &
                  as.numeric(d$MB_total_finished_area) > 0),
              sum(!is.na(d$landWidth) & !is.na(d$landDepth) &
                  d$landWidth > 0 & d$landDepth > 0)))
  cat("  category counts:\n")
  print(d[, .N, by = actualUseDescription][order(-N)])
}

cat("\nDone.\n")
