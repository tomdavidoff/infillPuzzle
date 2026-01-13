# mergePermitsBCA.R
# Merge Vancouver permit data directly with BC Assessment data
# Replaces the two-step parcel intermediate merge with a direct join

library(data.table)
library(RSQLite)

# ============================================================================
# Configuration
# ============================================================================

directory <- "~/OneDrive - UBC/dataRaw"
bcaFile <- file.path(directory, "REVD19_and_inventory_extracts.sqlite3")
outputDir <- "~/OneDrive - UBC/dataProcessed"

f_permit <- "vancouver_permits_full.csv"

# ============================================================================
# Address Standardization
# ============================================================================

# Street type abbreviation mappings
streetRename <- data.table(
  short = c(" ST", " AVE", " AV", " RD", " DR", " BLVD", " CRES", " HWY", " PL", " PLZ"),
  long = c(" STREET", " AVENUE", " AVENUE", " ROAD", " DRIVE", " BOULEVARD", " CRESCENT", " HIGHWAY", " PLACE", " PLAZA")
)

standardizeAddress <- function(dt, addressCol = "address") {
  # Work on a copy of the column to avoid side effects
  dt[, (addressCol) := toupper(get(addressCol))]
  
  # Remove city/province suffix and postal codes
  dt[, (addressCol) := gsub(", Vancouver, BC", "", get(addressCol), ignore.case = TRUE)]
  dt[, (addressCol) := gsub(" V[0-9][A-Z] [0-9][A-Z][0-9]$", "", get(addressCol))]
  
  # Standardize street types (at end of string and before space)
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
# Load Permit Data
# ============================================================================

cat("Loading permit data...\n")
dtPermits <- fread(file.path(directory, f_permit))

# Create grouping variable for permits from same applicant/month
dtPermits[, grouper := paste0(yearmonth, applicant)]

# Parse coordinates and compute within-group distance
dtPermits[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]
dtPermits[, groupDistance := {
  lon_adj <- cos(mean(lat, na.rm = TRUE) * pi / 180)
  sqrt(var(lat, na.rm = TRUE) + var(lon * lon_adj, na.rm = TRUE)) * 111000
}, by = grouper]

# Split groups that are geographically dispersed (>25m)
dtPermits[, nGroup := .N, by = grouper]
dtPermits[, group2 := grouper]
dtPermits[groupDistance > 25, group2 := paste0(grouper, "_", .I)]

cat("Permits loaded:", nrow(dtPermits), "rows,", uniqueN(dtPermits$grouper), "groups\n")

# Standardize permit addresses
standardizeAddress(dtPermits, "address")

# ============================================================================
# Load BCA Data (2019 SQLite)
# ============================================================================

cat("Loading BCA data from SQLite...\n")

con <- dbConnect(SQLite(), bcaFile)

# Load address table filtered to Vancouver, joined with folioDescription
# to restrict to Single Family Dwelling and Residential Dwelling with Suite
dtAddress <- as.data.table(dbGetQuery(con, "
  SELECT a.folioID, a.addressID, a.primaryFlag, a.streetNumber, a.unitNumber,
         a.streetDirectionPrefix, a.streetName, a.streetDirectionSuffix, a.streetType,
         a.city, a.postalCode
  FROM address a
  INNER JOIN folioDescription fd ON a.folioID = fd.folioID
  WHERE a.city = 'VANCOUVER'
    AND fd.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
"))

# Load folio description - filtered to target actual uses
dtDescription <- as.data.table(dbGetQuery(con, "
  SELECT folioID, actualUseCode, actualUseDescription,
         landDimensionType, landDimensionTypeDescription,
         landDimension, landWidth, landDepth,
         neighbourhoodCode, neighbourhoodDescription
  FROM folioDescription
  WHERE actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
"))

# Load valuation data
dtValuation <- as.data.table(dbGetQuery(con, "
  SELECT folioID, propertyClassCode, propertyClassDescription,
         landValue, improvementValue
  FROM valuation
"))

# Load legal description for PID
dtLegal <- as.data.table(dbGetQuery(con, "
  SELECT folioID, PID, formattedLegalDescription
  FROM legalDescription
"))

# Load folio for roll number (needed to join residential inventory)
dtFolio <- as.data.table(dbGetQuery(con, "
  SELECT folioID, jurisdictionCode, rollNumber
  FROM folio
"))

# Load residential inventory (join via roll_number)
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

# Join residential inventory via roll_number
setnames(dtResidential, "roll_number", "rollNumber")
dtBCA <- merge(dtBCA, dtResidential, by = "rollNumber", all.x = TRUE)

cat("BCA records loaded:", nrow(dtBCA), "\n")

# Create standardized address from BCA components
dtBCA[, address := paste(streetNumber, streetName)]
dtBCA[nchar(streetDirectionPrefix) > 0, address := paste(streetNumber, streetDirectionPrefix, streetName)]
dtBCA[nchar(streetType) > 0, address := paste(address, streetType)]

standardizeAddress(dtBCA, "address")

# ============================================================================
# Merge Permits with BCA
# ============================================================================

cat("Merging permits with BCA data...\n")

dtMerge <- merge(dtPermits, dtBCA, by = "address", all.x = TRUE)

# Flag whether each permit matched
dtMerge[, matched := !is.na(folioID)]

# For each group, check if ANY permit matched
dtMerge[, groupHasMatch := any(matched), by = group2]

# ============================================================================
# Group-level matching: keep only matched permits when group has a match
# ============================================================================

# If a group has at least one match, drop the unmatched permits in that group
# (e.g., 1244, 1246, 1248 W 14th - if only 1246 matches, keep only 1246)
dtMergeFiltered <- dtMerge[matched == TRUE | groupHasMatch == FALSE]

cat("\nAfter group filtering:\n")
cat("  Original permits:", nrow(dtMerge), "\n")
cat("  Filtered permits:", nrow(dtMergeFiltered), "\n")
cat("  Dropped (unmatched in groups with matches):", nrow(dtMerge) - nrow(dtMergeFiltered), "\n")

# ============================================================================
# Match Statistics by Project Type
# ============================================================================

cat("\n=== Match Statistics by Project Type ===\n")

# Compute at PERMIT level first
permitStats <- dtMerge[, .(
  n_permits = .N,
  n_matched = sum(matched),
  pct_matched = round(mean(matched) * 100, 1)
), by = typeofwork][order(-n_permits)]

cat("\nBy typeofwork (permit-level):\n")
print(permitStats)

# Compute at GROUP level (did the group get at least one match?)
# This is the right denominator for multiplex projects
groupStats <- dtMerge[, .(
  groupHasMatch = first(groupHasMatch),
  typeofwork = first(typeofwork),
  n_permits_in_group = .N
), by = group2]

groupStatsSummary <- groupStats[, .(
  n_groups = .N,
  n_matched = sum(groupHasMatch),
  pct_matched = round(mean(groupHasMatch) * 100, 1)
), by = typeofwork][order(-n_groups)]

cat("\nBy typeofwork (group-level, at least one match):\n")
print(groupStatsSummary)

# If specificusecategory exists, show that too
if ("specificusecategory" %in% names(dtMerge)) {
  groupStatsCategory <- dtMerge[, .(
    groupHasMatch = first(groupHasMatch),
    specificusecategory = first(specificusecategory),
    n_permits_in_group = .N
  ), by = group2]
  
  categoryStats <- groupStatsCategory[, .(
    n_groups = .N,
    n_matched = sum(groupHasMatch),
    pct_matched = round(mean(groupHasMatch) * 100, 1)
  ), by = specificusecategory][order(-n_groups)]
  
  cat("\nBy specificusecategory (group-level):\n")
  print(head(categoryStats, 30))
}

# Overall stats
cat("\n=== Overall ===\n")
cat("Permit-level match rate:", round(mean(dtMerge$matched) * 100, 1), "%\n")
cat("Group-level match rate:", round(mean(groupStats$groupHasMatch) * 100, 1), "%\n")

# ============================================================================
# Output
# ============================================================================

# Select relevant columns for output (using filtered data)
outputCols <- intersect(
  c("address", "permitnumber", "yearmonth", "applicant", "propertyuse", 
    "specificusecategory", "typeofwork", "projectvalue", "lat", "lon",
    "grouper", "group2", "groupDistance",
    "folioID", "rollNumber", "PID", "actualUseCode", "actualUseDescription",
    "landDimension", "landWidth", "landDepth", 
    "neighbourhoodCode", "neighbourhoodDescription",
    "propertyClassCode", "propertyClassDescription",
    "landValue", "improvementValue",
    "MB_year_built", "MB_effective_year", "MB_total_finished_area",
    "MB_num_storeys", "num_bedrooms", "num_full_baths"),
  names(dtMergeFiltered)
)

dtOutput <- dtMergeFiltered[, ..outputCols]

outFile <- file.path(outputDir, "mergePermitsBCA.csv")
fwrite(dtOutput, outFile)
cat("\nOutput written to:", outFile, "\n")
cat("Output rows:", nrow(dtOutput), "\n")

# ============================================================================
# Summary Statistics
# ============================================================================

cat("\n=== Actual Use Distribution (matched permits) ===\n")
print(table(dtMergeFiltered[matched == TRUE, actualUseDescription]))

cat("\n=== Unmatched and matched address patterns (last word) ===\n")
dtMerge[,lastWord := sub(".* ([^ ]+)$", "\\1", address)]
dtUnmatched <- dtMerge[matched == FALSE & groupHasMatch == FALSE]
print(head(sort(table(dtUnmatched$lastWord), decreasing = TRUE), 20))
print(head(sort(table(dtMerge[matched==TRUE | groupHasMatch==TRUE,lastWord]), decreasing = TRUE), 20))

