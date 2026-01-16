# mergePermitsZoningBCA.R
# Merge Vancouver permits with: Zoning, Census Tracts, and BC Assessment data
# Combines permitsZoning.R and mergePermitsBCA.R into a single workflow
# Now with option to use BCA 2026 parcel geometry for spatial matching
# Tom Davidoff / Claude
# 01/15/26

library(sf)
library(data.table)
library(RSQLite)

# ============================================================================
# Configuration
# ============================================================================

directory   <- "~/OneDrive - UBC/dataRaw"
outputDir   <- "~/OneDrive - UBC/dataProcessed"

permits_path <- file.path(directory, "vancouver_permits_full.csv")
zoning_path  <- file.path(directory, "vancouver_zoning.geojson")
ct_file      <- file.path(directory, "lct_000b21a_e/lct_000b21a_e.shp")
bcaFile19    <- file.path(directory, "REVD19_and_inventory_extracts.sqlite3")
bcaFile26    <- "/Volumes/T7Office/bigFiles/bca_folios.gpkg"

out_path     <- file.path(outputDir, "permits_zoning_ct_bca.csv")

# Threshold for using geometry-based matching vs address matching
# If >= this fraction of BCA 2019 records match to BCA 2026 geometry, use spatial join
LEVELUSEGEOM <- 0.95

# ============================================================================
# Address Standardization Function
# ============================================================================

streetRename <- data.table(
  short = c(" ST", " AVE", " AV", " RD", " DR", " BLVD", " CRES", " HWY", " PL", " PLZ"),
  long = c(" STREET", " AVENUE", " AVENUE", " ROAD", " DRIVE", " BOULEVARD", " CRESCENT", " HIGHWAY", " PLACE", " PLAZA")
)

standardizeAddress <- function(dt, addressCol = "address") {
  dt[, (addressCol) := toupper(get(addressCol))]
  
  # Remove city/province suffix and postal codes
  dt[, (addressCol) := gsub(", Vancouver, BC", "", get(addressCol), ignore.case = TRUE)]
  dt[, (addressCol) := gsub(" V[0-9][A-Z] [0-9][A-Z][0-9]$", "", get(addressCol))]
  
  # Standardize street types
  for (i in seq_len(nrow(streetRename))) {
    pattern_end <- paste0(streetRename$short[i], "$")
    pattern_mid <- paste0(streetRename$short[i], " ")
    replacement_mid <- paste0(streetRename$long[i], " ")
    
    dt[, (addressCol) := gsub(pattern_end, streetRename$long[i], get(addressCol))]
    dt[, (addressCol) := gsub(pattern_mid, replacement_mid, get(addressCol))]
  }
  
  invisible(dt)
}

# ============================================================================
# STEP 1: Load Permits
# ============================================================================

cat("=== STEP 1: Loading permits ===\n")
dtPermits <- fread(permits_path, drop = "projectdescription")

# Parse coordinates
ll <- tstrsplit(gsub("\\s+", "", dtPermits$geo_point_2d), ",", fixed = TRUE)
dtPermits[, `:=`(lat = as.numeric(ll[[1]]), lon = as.numeric(ll[[2]]))]
dtPermits <- dtPermits[!is.na(lat) & !is.na(lon)]

cat("Permits loaded:", nrow(dtPermits), "\n")

# ============================================================================
# STEP 2: Project-level grouping (same applicant/month within 25m)
# ============================================================================

cat("\n=== STEP 2: Creating project groups ===\n")

# Group by applicant + yearmonth
dtPermits[, grouper := paste0(yearmonth, applicant)]

# Compute within-group geographic spread
dtPermits[, groupDistance := {
  lon_adj <- cos(mean(lat, na.rm = TRUE) * pi / 180)
  sqrt(var(lat, na.rm = TRUE) + var(lon * lon_adj, na.rm = TRUE)) * 111000
}, by = grouper]

# Split dispersed groups (>25m spread)
dtPermits[, nGroup := .N, by = grouper]
dtPermits[, projectID := grouper]
dtPermits[groupDistance > 25, projectID := paste0(grouper, "_", .I)]

cat("Unique projects:", uniqueN(dtPermits$projectID), "\n")

# ============================================================================
# STEP 3: Spatial join with Zoning
# ============================================================================

cat("\n=== STEP 3: Joining with zoning ===\n")

sZoning <- st_read(zoning_path, quiet = TRUE)

sPermits <- st_as_sf(dtPermits, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
sPermits <- st_transform(sPermits, st_crs(sZoning))
sPermits <- st_join(sPermits, sZoning[, c("zoning_district", "zoning_category", "zoning_classification")], 
                    join = st_within)

cat("Permits with zoning attached\n")

# ============================================================================
# STEP 4: Spatial join with Census Tracts
# ============================================================================

cat("\n=== STEP 4: Joining with census tracts ===\n")

# City of Vancouver boundary (to clip census tracts)
cov_url <- "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/city-boundary/exports/geojson"
sCoV <- st_read(cov_url, quiet = TRUE)
sCoV <- st_make_valid(sCoV)
sCoV <- st_union(sCoV)

# Census tracts
sCT <- st_read(ct_file, quiet = TRUE)
sCT <- st_make_valid(sCT)
sCT <- st_transform(sCT, st_crs(sCoV))
sCT_cov <- st_intersection(sCT, st_make_valid(sCoV))

# Join permits with census tracts
sPermits <- st_transform(sPermits, st_crs(sCT_cov))
sPermits <- st_join(sPermits, sCT_cov[, c("CTUID", "DGUID", "CTNAME")], join = st_within)

cat("Permits with census tract attached\n")

# ============================================================================
# STEP 5: Check BCA 2019 -> BCA 2026 geometry match quality
# ============================================================================

cat("\n=== STEP 5: Checking BCA 2019 to BCA 2026 geometry match quality ===\n")

# Load BCA 2019 folios (Vancouver SFD/Suite only)
con <- dbConnect(SQLite(), bcaFile19)

dtFolio19 <- as.data.table(dbGetQuery(con, "
  SELECT f.folioID, f.rollNumber
  FROM folio f
  INNER JOIN folioDescription fd ON f.folioID = fd.folioID
  INNER JOIN address a ON f.folioID = a.folioID
  WHERE a.city = 'VANCOUVER'
    AND fd.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
"))

dbDisconnect(con)

# Get unique roll_base values from BCA 2019
dtFolio19[, roll_base := floor(as.numeric(rollNumber) / 1000)]
uniqueRollBase19 <- unique(dtFolio19$roll_base)
cat("BCA 2019 unique roll_base values:", length(uniqueRollBase19), "\n")
cat("BCA 2019 total Vancouver SFD/Suite folios:", nrow(dtFolio19), "\n")

# Load BCA 2026 roll numbers with geometry (Vancouver SFD/Suite/Duplex only)
cat("Loading BCA 2026 geometry data...\n")

if (file.exists(bcaFile26)) {
  sBCA26 <- st_read(
    bcaFile26,
    layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
    query = "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV 
             WHERE JURISDICTION_CODE='200' 
             AND (ACTUAL_USE_DESCRIPTION IN ('Residential Dwelling with Suite','Single Family Dwelling') 
                  OR INSTR(ACTUAL_USE_DESCRIPTION, 'uplex') > 0)",
    quiet = TRUE
  )
  
  # Get roll_base from BCA 2026
  dtBCA26 <- as.data.table(sBCA26)
  dtBCA26[, roll_base := floor(as.numeric(ROLL_NUMBER) / 1000)]
  uniqueRollBase26 <- unique(dtBCA26$roll_base)
  
  cat("BCA 2026 unique roll_base values:", length(uniqueRollBase26), "\n")
  
  # Calculate match rate
  matchedRollBase <- intersect(uniqueRollBase19, uniqueRollBase26)
  matchRate <- length(matchedRollBase) / length(uniqueRollBase19)
  
  # Also check at folio level
  dtFolio19[, hasGeomMatch := roll_base %in% uniqueRollBase26]
  folioMatchRate <- mean(dtFolio19$hasGeomMatch)
  
  cat("\n--- BCA 2019 -> BCA 2026 Match Quality ---\n")
  cat("Roll_base level match rate:", round(matchRate * 100, 2), "%\n")
  cat("Folio level match rate:", round(folioMatchRate * 100, 2), "%\n")
  cat("Threshold for using geometry:", LEVELUSEGEOM * 100, "%\n")
  
  USE_GEOMETRY <- folioMatchRate >= LEVELUSEGEOM
  cat("\n>>> Decision: ", ifelse(USE_GEOMETRY, "USE GEOMETRY-BASED MATCHING", "USE ADDRESS-BASED MATCHING"), " <<<\n")
  
} else {
  cat("BCA 2026 file not found at:", bcaFile26, "\n")
  cat("Falling back to address-based matching.\n")
  USE_GEOMETRY <- FALSE
  folioMatchRate <- NA
}

# ============================================================================
# STEP 6: BCA Join (Geometry or Address based)
# ============================================================================

if (USE_GEOMETRY) {
  # -------------------------------------------------------------------------
  # GEOMETRY-BASED MATCHING
  # -------------------------------------------------------------------------
  cat("\n=== STEP 6: Geometry-based BCA matching ===\n")
  
  # Get centroids from BCA 2026 polygons
  cat("Computing parcel centroids...\n")
  sBCA26_pt <- st_point_on_surface(sBCA26)
  sBCA26_pt <- sBCA26_pt[, c("ROLL_NUMBER")]
  dtBCA26_pt <- as.data.table(sBCA26_pt)
  dtBCA26_pt[, roll_base := floor(as.numeric(ROLL_NUMBER) / 1000)]
  print("DONE ROLL BASE")
  
  # Keep one centroid per roll_base (they should be the same or very close for subdivisions)
  # Use first() to get representative geometry
  sBCA26_unique <- sBCA26_pt[!duplicated(dtBCA26_pt$roll_base), ]
  dtBCA26_unique <- as.data.table(sBCA26_unique)
  dtBCA26_unique[, roll_base := floor(as.numeric(ROLL_NUMBER) / 1000)]
  print("DONE UNIQUE")
  
  # Load full BCA 2019 property data
  con <- dbConnect(SQLite(), bcaFile19)
  
  dtAddress <- as.data.table(dbGetQuery(con, "
    SELECT a.folioID, a.streetNumber, a.unitNumber,
           a.streetDirectionPrefix, a.streetName, a.streetDirectionSuffix, a.streetType
    FROM address a
    INNER JOIN folioDescription fd ON a.folioID = fd.folioID
    WHERE a.city = 'VANCOUVER'
      AND fd.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
  "))
  
  dtDescription <- as.data.table(dbGetQuery(con, "
    SELECT folioID, actualUseCode, actualUseDescription,
           landDimension, landWidth, landDepth,
           neighbourhoodCode, neighbourhoodDescription
    FROM folioDescription
    WHERE actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
  "))
  
  dtValuation <- as.data.table(dbGetQuery(con, "
    SELECT folioID, propertyClassCode, propertyClassDescription,
           landValue, improvementValue
    FROM valuation
  "))
  
  dtLegal <- as.data.table(dbGetQuery(con, "
    SELECT folioID, PID
    FROM legalDescription
  "))
  
  dtFolio <- as.data.table(dbGetQuery(con, "
    SELECT folioID, rollNumber
    FROM folio
  "))
  
  dtResidential <- as.data.table(dbGetQuery(con, "
    SELECT roll_number, MB_year_built, MB_effective_year, MB_total_finished_area,
           MB_num_storeys, num_bedrooms, num_full_baths
    FROM residentialInventory
  "))
  
  dbDisconnect(con)
  
  # Merge BCA 2019 tables
  dtBCA <- merge(dtAddress, dtDescription, by = "folioID", all.x = TRUE)
  dtBCA <- merge(dtBCA, dtValuation, by = "folioID", all.x = TRUE)
  dtBCA <- merge(dtBCA, dtLegal, by = "folioID", all.x = TRUE)
  dtBCA <- merge(dtBCA, dtFolio, by = "folioID", all.x = TRUE)
  setnames(dtResidential, "roll_number", "rollNumber")
  dtBCA <- merge(dtBCA, dtResidential, by = "rollNumber", all.x = TRUE)
  print("DONE MERGE BCA RESIDENTIAL")
  
  # Add roll_base for geometry lookup
  dtBCA[, roll_base := floor(as.numeric(rollNumber) / 1000)]
  
  # Merge BCA 2019 with BCA 2026 centroids via roll_base
  dtBCA <- merge(dtBCA, dtBCA26_unique[, .(roll_base, geom)], by = "roll_base", all.x = TRUE)
  
  # Convert to sf for spatial join with permits
  dtBCA_withGeom <- dtBCA[!is.na(geom)]
  sBCA <- st_as_sf(dtBCA_withGeom, sf_column_name = "geom")
  st_crs(sBCA) <- st_crs(sBCA26)
  
  cat("BCA records with geometry:", nrow(sBCA), "\n")
  cat("BCA records without geometry:", nrow(dtBCA) - nrow(dtBCA_withGeom), "\n")
  
  # Transform permits to BCA CRS and do spatial join (nearest neighbor within tolerance)
  sPermits_bca <- st_transform(sPermits, st_crs(sBCA))
  
  # REPLACEMENT CODE FOR GEOMETRY-BASED MATCHING SECTION
# Replace the section starting at "# Use st_nearest_feature for point-to-point matching"
# with this code:

  # Transform permits to BCA CRS
  sPermits_bca <- st_transform(sPermits, st_crs(sBCA))

  cat("Permits CRS:", st_crs(sPermits_bca)$epsg, "\n")
  cat("BCA CRS:", st_crs(sBCA)$epsg, "\n")
  cat("Number of permits:", nrow(sPermits_bca), "\n")
  cat("Number of BCA parcels:", nrow(sBCA), "\n")

  # Use st_nearest_feature for point-to-point matching with distance threshold
  cat("Finding nearest BCA parcels to permits...\n")
  nearest_idx <- st_nearest_feature(sPermits_bca, sBCA)

  # Check for NAs in nearest_idx
  cat("Permits with no nearest match:", sum(is.na(nearest_idx)), "\n")

  # Compute distances only for valid matches
  # Initialize distance vector with NA
  distances <- rep(NA_real_, nrow(sPermits_bca))

  valid_idx <- !is.na(nearest_idx)
  if (sum(valid_idx) > 0) {
    # Compute distances row by row for valid matches
    distances[valid_idx] <- as.numeric(
      st_distance(
        sPermits_bca[valid_idx, ],
        sBCA[nearest_idx[valid_idx], ],
        by_element = TRUE
      )
    )
  }

  # Only match if within 50m (to handle slight geocoding differences)
  MATCH_TOLERANCE_M <- 50
  valid_match <- !is.na(distances) & distances <= MATCH_TOLERANCE_M

  cat("Permits within", MATCH_TOLERANCE_M, "m of BCA parcel:", sum(valid_match), "\n")

  # Extract BCA data for valid matches
  bcaCols <- c("folioID", "rollNumber", "PID",
               "actualUseCode", "actualUseDescription",
               "landDimension", "landWidth", "landDepth",
               "neighbourhoodCode", "neighbourhoodDescription",
               "propertyClassCode", "propertyClassDescription",
               "landValue", "improvementValue",
               "MB_year_built", "MB_effective_year", "MB_total_finished_area",
               "MB_num_storeys", "num_bedrooms", "num_full_baths")

  dtPermits <- as.data.table(st_drop_geometry(sPermits_bca))
  dtBCA_dt <- as.data.table(st_drop_geometry(sBCA))

  # Add BCA columns - initialize all as NA first
  for (col in bcaCols) {
    if (col %in% names(dtBCA_dt)) {
      dtPermits[, (col) := dtBCA_dt[[col]][NA_integer_]]  # proper NA type
      dtPermits[valid_match, (col) := dtBCA_dt[[col]][nearest_idx[valid_match]]]
    }
  }

  dtPermits[, bcaMatchDistance := distances]
  dtPermits[!valid_match, bcaMatchDistance := NA_real_]

  cat("Permits matched to BCA (within", MATCH_TOLERANCE_M, "m):", sum(valid_match), "\n")
  cat("Permits not matched:", sum(!valid_match), "\n")

  dtMerge <- dtPermits
  
} else {
  # -------------------------------------------------------------------------
  # ADDRESS-BASED MATCHING (original method)
  # -------------------------------------------------------------------------
  cat("\n=== STEP 6: Address-based BCA matching ===\n")
  
  # Convert back to data.table for address matching
  dtPermits <- as.data.table(st_drop_geometry(sPermits))
  
  # Standardize permit addresses for matching
  dtPermits[, addressStd := address]
  standardizeAddress(dtPermits, "addressStd")
  
  # Load BCA data
  con <- dbConnect(SQLite(), bcaFile19)
  
  dtAddress <- as.data.table(dbGetQuery(con, "
    SELECT a.folioID, a.streetNumber, a.unitNumber,
           a.streetDirectionPrefix, a.streetName, a.streetDirectionSuffix, a.streetType,
           a.city
    FROM address a
    INNER JOIN folioDescription fd ON a.folioID = fd.folioID
    WHERE a.city = 'VANCOUVER'
      AND fd.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
  "))
  
  dtDescription <- as.data.table(dbGetQuery(con, "
    SELECT folioID, actualUseCode, actualUseDescription,
           landDimension, landWidth, landDepth,
           neighbourhoodCode, neighbourhoodDescription
    FROM folioDescription
    WHERE actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
  "))
  
  dtValuation <- as.data.table(dbGetQuery(con, "
    SELECT folioID, propertyClassCode, propertyClassDescription,
           landValue, improvementValue
    FROM valuation
  "))
  
  dtLegal <- as.data.table(dbGetQuery(con, "
    SELECT folioID, PID
    FROM legalDescription
  "))
  
  dtFolio <- as.data.table(dbGetQuery(con, "
    SELECT folioID, rollNumber
    FROM folio
  "))
  
  dtResidential <- as.data.table(dbGetQuery(con, "
    SELECT roll_number, MB_year_built, MB_effective_year, MB_total_finished_area,
           MB_num_storeys, num_bedrooms, num_full_baths
    FROM residentialInventory
  "))
  
  dbDisconnect(con)
  
  # Merge BCA tables
  dtBCA <- merge(dtAddress, dtDescription, by = "folioID", all.x = TRUE)
  dtBCA <- merge(dtBCA, dtValuation, by = "folioID", all.x = TRUE)
  dtBCA <- merge(dtBCA, dtLegal, by = "folioID", all.x = TRUE)
  dtBCA <- merge(dtBCA, dtFolio, by = "folioID", all.x = TRUE)
  setnames(dtResidential, "roll_number", "rollNumber")
  dtBCA <- merge(dtBCA, dtResidential, by = "rollNumber", all.x = TRUE)
  
  cat("BCA records loaded:", nrow(dtBCA), "\n")
  
  # Build standardized BCA address
  dtBCA[, addressStd := paste(streetNumber, streetName)]
  dtBCA[nchar(streetDirectionPrefix) > 0, addressStd := paste(streetNumber, streetDirectionPrefix, streetName)]
  dtBCA[nchar(streetType) > 0, addressStd := paste(addressStd, streetType)]
  standardizeAddress(dtBCA, "addressStd")
  
  # Select BCA columns to merge
  bcaCols <- c("addressStd", "folioID", "rollNumber", "PID", 
               "actualUseCode", "actualUseDescription",
               "landDimension", "landWidth", "landDepth",
               "neighbourhoodCode", "neighbourhoodDescription",
               "propertyClassCode", "propertyClassDescription",
               "landValue", "improvementValue",
               "MB_year_built", "MB_effective_year", "MB_total_finished_area",
               "MB_num_storeys", "num_bedrooms", "num_full_baths")
  dtBCA_slim <- dtBCA[, ..bcaCols]
  
  # Merge permits with BCA
  dtMerge <- merge(dtPermits, dtBCA_slim, by = "addressStd", all.x = TRUE)
  
  cat("Address merge complete\n")
}

# ============================================================================
# STEP 7: Flag matches and compute project-level stats
# ============================================================================

cat("\n=== STEP 7: Computing match statistics ===\n")

dtMerge[, bcaMatched := !is.na(folioID)]
dtMerge[, projectHasBCAMatch := any(bcaMatched), by = projectID]

# ============================================================================
# STEP 8: Summary Statistics
# ============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

cat("\nMatching method:", ifelse(USE_GEOMETRY, "GEOMETRY-BASED", "ADDRESS-BASED"), "\n")
if (!is.na(folioMatchRate)) {
  cat("BCA 2019->2026 geometry coverage:", round(folioMatchRate * 100, 2), "%\n")
}

# Overall counts
cat("\nTotal permits:", nrow(dtMerge), "\n")
cat("Unique projects:", uniqueN(dtMerge$projectID), "\n")

# Zoning coverage
cat("\nZoning coverage:\n")
cat("  With zoning:", sum(!is.na(dtMerge$zoning_district)), 
    "(", round(mean(!is.na(dtMerge$zoning_district)) * 100, 1), "%)\n")

# Census tract coverage
cat("\nCensus tract coverage:\n")
cat("  With CTUID:", sum(!is.na(dtMerge$CTUID)),
    "(", round(mean(!is.na(dtMerge$CTUID)) * 100, 1), "%)\n")

# BCA match rates
cat("\nBCA match rates:\n")
cat("  Permit-level:", round(mean(dtMerge$bcaMatched) * 100, 1), "%\n")

projectStats <- dtMerge[, .(projectHasBCAMatch = first(projectHasBCAMatch)), by = projectID]
cat("  Project-level:", round(mean(projectStats$projectHasBCAMatch) * 100, 1), "%\n")

# By specificusecategory
cat("\nBCA match by specificusecategory (project-level):\n")
categoryStats <- dtMerge[, .(
  projectHasBCAMatch = first(projectHasBCAMatch),
  specificusecategory = first(specificusecategory)
), by = projectID][, .(
  n_projects = .N,
  n_matched = sum(projectHasBCAMatch),
  pct_matched = round(mean(projectHasBCAMatch) * 100, 1)
), by = specificusecategory][order(-n_projects)]
print(head(categoryStats, 20))

# ============================================================================
# STEP 9: Output
# ============================================================================

cat("\n=== STEP 9: Writing output ===\n")

# Select output columns
outputCols <- c(
  # Permit identifiers
  "permitnumber", "address", "yearmonth", "permitnumbercreateddate",
  # Permit details
  "typeofwork", "projectvalue", "specificusecategory", "applicant", 
  "propertyuse", "geolocalarea",
  # Location
  "lat", "lon",
  # Project grouping
  "projectID", "groupDistance", "nGroup",
  # Zoning
  "zoning_district", "zoning_category", "zoning_classification",
  # Census tract
  "CTUID", "DGUID", "CTNAME",
  # BCA identifiers
  "folioID", "rollNumber", "PID",
  # BCA property info
  "actualUseCode", "actualUseDescription",
  "landDimension", "landWidth", "landDepth",
  "neighbourhoodCode", "neighbourhoodDescription",
  "propertyClassCode", "propertyClassDescription",
  "landValue", "improvementValue",
  # BCA residential inventory
  "MB_year_built", "MB_effective_year", "MB_total_finished_area",
  "MB_num_storeys", "num_bedrooms", "num_full_baths",
  # Match flags
  "bcaMatched", "projectHasBCAMatch"
)

# Add geometry match distance if available
if ("bcaMatchDistance" %in% names(dtMerge)) {
  outputCols <- c(outputCols, "bcaMatchDistance")
}

# Keep only columns that exist
outputCols <- intersect(outputCols, names(dtMerge))
dtOutput <- dtMerge[, ..outputCols]

fwrite(dtOutput, out_path)
cat("Output written to:", out_path, "\n")
cat("Output rows:", nrow(dtOutput), "\n")

# ============================================================================
# STEP 10: Project-level aggregation (optional summary table)
# ============================================================================

cat("\n=== STEP 10: Creating project-level summary ===\n")

dtProject <- dtMerge[, .(
  # First permit info (representative)
  address_first = first(address),
  yearmonth = first(yearmonth),
  typeofwork = first(typeofwork),
  specificusecategory = first(specificusecategory),
  applicant = first(applicant),
  geolocalarea = first(geolocalarea),
  
  # Aggregates
  n_permits = .N,
  total_projectvalue = sum(projectvalue, na.rm = TRUE),
  
  # Location (centroid)
  lat = mean(lat, na.rm = TRUE),
  lon = mean(lon, na.rm = TRUE),
  spread_m = first(groupDistance),
  
  # Spatial joins (should be same for all permits in project)
  zoning_district = first(zoning_district),
  zoning_category = first(zoning_category),
  CTUID = first(CTUID),
  CTNAME = first(CTNAME),
  
  # BCA (take first match if any)
  folioID = first(na.omit(folioID)),
  rollNumber = first(na.omit(rollNumber)),
  PID = first(na.omit(PID)),
  landValue = first(na.omit(landValue)),
  improvementValue = first(na.omit(improvementValue)),
  MB_year_built = first(na.omit(MB_year_built)),
  
  # Match status
  bcaMatched = any(bcaMatched)
), by = projectID]

project_out_path <- file.path(outputDir, "permits_project_level.csv")
fwrite(dtProject, project_out_path)
cat("Project-level summary written to:", project_out_path, "\n")
cat("Projects:", nrow(dtProject), "\n")

cat("\n=== DONE ===\n")
