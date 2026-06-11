# bcaVancouverParcelExtract.R
# Extract COV BCA parcels by type (from propertyTypeMap), write one sf RDS each.
# Types classified by 2019 actualUseDescription (SQLite), NOT the 2025 gpkg field,
# so redeveloped parcels keep their former-SF status. roll_base routing also uses
# the 2019 strata flag on all sides: strata = full rollNumber, else floor(/1000).
# Tom Davidoff 05/18/26

library(data.table)
library(sf)
library(RSQLite)

GPKG_DIR    <- "~/bigFiles/latestSpatialBCA/"
SQLITE_FILE <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
ZONING_FILE <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
OUT_DIR     <- "~/DropboxExternal/dataProcessed/"
JUR         <- "200"
R1_1_ONLY   <- "singleFamily" # duplex/strata written unfiltered

typeMap <- readRDS(path.expand(file.path(OUT_DIR, "propertyTypeMap.rds")))

# spatial: all COV parcels, centroids, 2025 zoning by centroid-in-polygon
gpkg  <- list.files(path.expand(GPKG_DIR), pattern = "\\.gpkg$", full.names = TRUE)[1]
lyr   <- "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV"
sfAll <- st_read(gpkg, layer = lyr, quiet = TRUE,
                 query = sprintf("SELECT ROLL_NUMBER, JURISDICTION_CODE, geom FROM %s WHERE JURISDICTION_CODE = '%s'", lyr, JUR))
sfAll <- st_make_valid(st_transform(sfAll, 4326))
cent  <- st_centroid(st_geometry(sfAll))
sfAll$lon <- st_coordinates(cent)[, 1]
sfAll$lat <- st_coordinates(cent)[, 2]

sfZone <- st_make_valid(st_transform(st_read(path.expand(ZONING_FILE), quiet = TRUE), 4326))
zHit   <- st_join(st_as_sf(data.frame(idx = seq_len(nrow(sfAll))), geometry = cent, crs = 4326),
                  sfZone[, "zoning_district"], join = st_within)
zHit   <- zHit[!duplicated(zHit$idx), ]
sfAll$zoning2025 <- NA_character_
sfAll$zoning2025[zHit$idx] <- as.character(zHit$zoning_district)

sfAll$rollNumber   <- trimws(as.character(sfAll$ROLL_NUMBER))
sfAll$jurisdiction <- trimws(as.character(sfAll$JURISDICTION_CODE))
dtSpatial <- as.data.table(st_drop_geometry(sfAll))[, .(rollNumber, jurisdiction, lon, lat, zoning2025)]
dtSpatial[, geom := st_geometry(sfAll)]
dtSpatial <- dtSpatial[!is.na(rollNumber) & rollNumber != ""]

# SQLite: folio, inventory, description (2019 vintage)
con <- dbConnect(SQLite(), SQLITE_FILE)
folio <- as.data.table(dbGetQuery(con, sprintf(
  "SELECT folioID, rollNumber, jurisdictionCode AS jurisdiction FROM folio WHERE jurisdictionCode = %s", JUR)))
inventory <- as.data.table(dbGetQuery(con, sprintf(
  "SELECT roll_number AS rollNumber, jurisdiction, land_width, land_depth,
   MB_effective_year, MB_total_finished_area FROM residentialInventory WHERE jurisdiction = %s", JUR)))
description <- as.data.table(dbGetQuery(con,
  "SELECT folioID, actualUseDescription, landWidth AS fdLandWidth, landDepth AS fdLandDepth FROM folioDescription"))
dbDisconnect(con)

for (d in list(folio, inventory)) d[, `:=`(rollNumber = trimws(as.character(rollNumber)),
                                           jurisdiction = trimws(as.character(jurisdiction)))]
folio[, folioID := as.character(folioID)]
description[, folioID := as.character(folioID)]
description <- unique(description, by = "folioID")

# 2019 strata flag -> conditional roll_base, applied identically to all three tables
rollIsStrata <- unique(merge(folio, description[, .(folioID, actualUseDescription)], by = "folioID")[
  , .(rollNumber, jurisdiction, isStrata2019 = grepl("Strata", actualUseDescription, ignore.case = TRUE))],
  by = c("rollNumber", "jurisdiction"))

routeRollBase <- function(d) {
  d <- merge(d, rollIsStrata, by = c("rollNumber", "jurisdiction"), all.x = TRUE)
  d[is.na(isStrata2019), isStrata2019 := FALSE]
  d[, roll_base := fifelse(isStrata2019, as.numeric(rollNumber), floor(as.numeric(rollNumber) / 1000))]
  d[!is.na(roll_base)]
}
folio     <- unique(routeRollBase(folio),     by = c("roll_base", "jurisdiction"))[, .(folioID, roll_base, jurisdiction)]
inventory <- unique(routeRollBase(inventory), by = c("roll_base", "jurisdiction"))[
  , .(roll_base, jurisdiction, land_width, land_depth, MB_effective_year, MB_total_finished_area)]
dtSpatial <- routeRollBase(dtSpatial)[!duplicated(roll_base)]

# merge spatial -> inventory -> folio -> description; dim fallback to folioDescription
setkey(dtSpatial, roll_base, jurisdiction); setkey(inventory, roll_base, jurisdiction)
dt <- inventory[dtSpatial]
setkey(folio, roll_base, jurisdiction); setkey(dt, roll_base, jurisdiction)
dt <- folio[dt, allow.cartesian = TRUE]
setkey(description, folioID); setkey(dt, folioID)
dt <- description[dt]
dt[, landWidth := fcoalesce(as.numeric(land_width), as.numeric(fdLandWidth))]
dt[, landDepth := fcoalesce(as.numeric(land_depth), as.numeric(fdLandDepth))]
dtOut <- dt[, .(folioID, rollNumber, jurisdiction, actualUseDescription, landWidth, landDepth,
                MB_effective_year, MB_total_finished_area, lon, lat, zoning2025, geom)]
cat(sprintf("Merged rows: %d (R1-1: %d)\n", nrow(dtOut), sum(dtOut$zoning2025 == "R1-1", na.rm = TRUE)))

for (tp in c("singleFamily", "strata", "duplex")) {
  d <- dtOut[actualUseDescription %in% typeMap[[tp]]]
  if (tp %in% R1_1_ONLY) d <- d[zoning2025 == "R1-1"]
  if (nrow(d) == 0) { cat(sprintf("[%s] zero rows; skipped\n", tp)); next }
  outPath <- file.path(path.expand(OUT_DIR), sprintf("bca_vancouver_%s.rds", tp))
  saveRDS(st_as_sf(d, sf_column_name = "geom", crs = 4326), outPath)
  cat(sprintf("[%s] %d rows -> %s\n", tp, nrow(d), outPath))
}
