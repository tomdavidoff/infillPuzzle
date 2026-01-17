# mergePermitsZoningBCA.R
# Merge Vancouver permits with: Zoning, Census Tracts, and BC Assessment data
# Uses BCA parcel polygons for spatial matching, BCA 2019 for property details
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
bcaFileGeo <- "~/OneDrive - UBC/dataProcessed/bca_vancouver_residential.rds"

out_path     <- file.path(outputDir, "permits_zoning_ct_bca.csv")

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

# Drop anything that isn't among work type and project type of interest


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
# STEP 5: Load BCA parcel polygons
# ============================================================================

cat("\n=== STEP 5: Loading BCA parcel polygons ===\n")

# Load Vancouver residential parcels with geometry
sBCA <- readRDS(bcaFileGeo)

cat("BCA geo parcels loaded:", nrow(sBCA), "\n")

# Add roll_base for linking to BCA 2019 data
sBCA$roll_base <- floor(as.numeric(sBCA$ROLL_NUMBER) / 1000)

# ============================================================================
# STEP 6: Load BCA 2019 property details
# ============================================================================

cat("\n=== STEP 6: Loading BCA 2019 property details ===\n")

con <- dbConnect(SQLite(), bcaFile19)

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
dtBCA19 <- merge(dtDescription, dtValuation, by = "folioID", all.x = TRUE)
dtBCA19 <- merge(dtBCA19, dtLegal, by = "folioID", all.x = TRUE)
dtBCA19 <- merge(dtBCA19, dtFolio, by = "folioID", all.x = TRUE)
setnames(dtResidential, "roll_number", "rollNumber")
dtBCA19 <- merge(dtBCA19, dtResidential, by = "rollNumber", all.x = TRUE)

# Add roll_base for joining
dtBCA19[, roll_base := floor(as.numeric(rollNumber) / 1000)]

cat("BCA 2019 records loaded:", nrow(dtBCA19), "\n")

# ============================================================================
# STEP 7: Spatial join - permits to BCA parcels (point in polygon)
# ============================================================================

cat("\n=== STEP 7: Spatial join - permits within BCA parcels ===\n")

# Transform permits to BCA CRS
sPermits_bca <- st_transform(sPermits, st_crs(sBCA))

# Make geometries valid
sBCA<- st_make_valid(sBCA)

# Spatial join: permit points within BCA parcel polygons
sPermits_bca <- st_join(sPermits_bca, sBCA[, c("ROLL_NUMBER", "roll_base")], 
                        join = st_within, left = TRUE)

cat("Spatial join complete\n")
cat("Permits matched to BCA parcel:", sum(!is.na(sPermits_bca$ROLL_NUMBER)), "\n")
cat("Permits not matched:", sum(is.na(sPermits_bca$ROLL_NUMBER)), "\n")

# ============================================================================
# STEP 8: Merge BCA 2019 property details via roll_base
# ============================================================================

cat("\n=== STEP 8: Merging BCA 2019 property details ===\n")

# Convert to data.table
dtMerge <- as.data.table(st_drop_geometry(sPermits_bca))

# Select columns to merge from BCA 2019 (exclude roll_base to avoid .x/.y)
bcaCols <- c("roll_base", "folioID", "rollNumber", "PID", 
             "actualUseCode", "actualUseDescription",
             "landDimension", "landWidth", "landDepth",
             "neighbourhoodCode", "neighbourhoodDescription",
             "propertyClassCode", "propertyClassDescription",
             "landValue", "improvementValue",
             "MB_year_built", "MB_effective_year", "MB_total_finished_area",
             "MB_num_storeys", "num_bedrooms", "num_full_baths")

dtBCA19_slim <- dtBCA19[, ..bcaCols]

# Merge on roll_base
dtMerge <- merge(dtMerge, dtBCA19_slim, by = "roll_base", all.x = TRUE)

cat("BCA 2019 details merged\n")

# ============================================================================
# STEP 9: Flag matches and compute project-level stats
# ============================================================================

cat("\n=== STEP 9: Computing match statistics ===\n")

dtMerge[, bcaMatched := !is.na(ROLL_NUMBER)]
dtMerge[, projectHasBCAMatch := any(bcaMatched), by = projectID]

# ============================================================================
# STEP 10: Summary Statistics
# ============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

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
cat("\nBCA match rates (geometry-based):\n")
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
# STEP 11: Output
# ============================================================================

cat("\n=== STEP 11: Writing output ===\n")

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
  "ROLL_NUMBER", "folioID", "rollNumber", "PID",
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

# Keep only columns that exist
outputCols <- intersect(outputCols, names(dtMerge))
dtOutput <- dtMerge[, ..outputCols]

fwrite(dtOutput, out_path)
cat("Output written to:", out_path, "\n")
cat("Output rows:", nrow(dtOutput), "\n")

# ============================================================================
# STEP 12: Project-level aggregation
# ============================================================================

cat("\n=== STEP 12: Creating project-level summary ===\n")

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
  ROLL_NUMBER = first(na.omit(ROLL_NUMBER)),
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
