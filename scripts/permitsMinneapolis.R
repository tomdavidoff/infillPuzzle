# permitsMinneapolis.R 
# R code to grab Minneapolis permits for single family / infill areas
# Tom Davidoff
# Adapted from Portland code by Claude
# Purpose: Analyze permit activity around Minneapolis 2040 zoning reform

library(data.table)
library(sf)
library(ggplot2)
library(tigris)

# ==========================================
# TOGGLE: "zip" or "tract"
# ==========================================
GEOGRAPHY <- "zip"
# ==========================================

options(timeout = 600)
options(tigris_use_cache = TRUE)

# --- MINNEAPOLIS PERMITS ---
# Based on header provided: data has HOUSING_TY, UNITS, YEAR, RES_PERMIT, ZIP_CODE, geometry
# CO_CODE 053 = Hennepin County
mpls_file <- "~/OneDrive - UBC/dataRaw/shp_econ_residential_building_permts/ResidentialPermits.shp"  # UPDATE THIS

message("Reading Minneapolis permit data...")
sfPermit <- st_read(mpls_file)
names(sfPermit) <- tolower(names(sfPermit))

# Check the CRS - your data showed NAD83 / UTM zone 15N (EPSG:26915)
# Transform to a common CRS if needed
if (st_crs(sfPermit)$epsg != 26915) {

  sfPermit <- st_transform(sfPermit, 26915)
}

# Convert to data.table for filtering, then back to sf
dtPermit <- as.data.table(sfPermit)

# --- FILTER FOR ANALYSIS PERIOD ---
# Minneapolis 2040: Adopted Dec 2018, effective Jan 1, 2020
# Option A: Pre-reform (2016-2019)
# Option B: Post-reform (2020-2023)
# Adjust as needed for your research question

#dtPermit <- dtPermit[year >= 2016 & year <= 2019]  # PRE-REFORM
 dtPermit <- dtPermit[year >= 2020 & year <= 2025]  # POST-REFORM

# --- FILTER FOR NEW CONSTRUCTION ---
# RES_PERMIT: NU = New, RM = Remodel (based on your header)
dtPermit <- dtPermit[res_permit == "NU"]
# reduce to Minneapolis city only
dtPermit <- dtPermit[ctu_name == "Minneapolis"]

# --- MINNEAPOLIS INFILL LOGIC ---
# Identify "plex" permits (2+ units, or non-SFD housing types)
# HOUSING_TY: SFD = Single-Family Detached
# HOUSING__1 appears to be the full description

dtPermit[, isPlex := units >= 2 | 
           !housing_ty %in% c("SFD") |
           grepl("duplex|triplex|fourplex|twinhome|townhouse", 
                 housing__1, ignore.case = TRUE)]

# Filter to Hennepin County (Minneapolis area) if needed
# CO_CODE 053 = Hennepin
dtPermit <- dtPermit[co_code == "053" | co_code == 53]

message("Permits after filtering: ", nrow(dtPermit))

# Convert back to sf
sfPermit <- st_as_sf(dtPermit)
sfPermit <- st_transform(sfPermit, 26915)

# --- MINNEAPOLIS ZONING DISTRICTS ---
zoning_file <- "~/OneDrive - UBC/dataRaw/Planning_Primary_Zoning/Planning_Primary_Zoning.shp"  # UPDATE if different

message("Reading Minneapolis zoning data...")
sfZoning <- st_read(zoning_file)
names(sfZoning) <- tolower(names(sfZoning))
sfZoning <- st_transform(sfZoning, 26915)

# Print zoning field names to identify the zone column
message("Zoning fields: ", paste(names(sfZoning), collapse = ", "))

# --- SPATIAL JOIN: PERMITS + ZONING ---
message("Joining permits to zoning...")
sfPermitZoned <- st_join(sfPermit, sfZoning)

# ==========================================
# GEOGRAPHY-SPECIFIC JOIN
# ==========================================

if (GEOGRAPHY == "tract") {
  message("Fetching census tracts...")
  sfTracts <- tracts(state = "MN", county = "Hennepin", year = 2021, cb = TRUE)
  sfTracts <- st_transform(sfTracts, 26915)
  sfFinal <- st_join(sfPermitZoned, sfTracts[, c("GEOID", "NAMELSAD")])
  dtFinal <- as.data.table(sfFinal)
  setnames(dtFinal, c("GEOID", "NAMELSAD"), c("geo_id", "geo_name"), skip_absent = TRUE)
} else {
  message("Fetching ZCTAs...")
  sfZips <- zctas(year = 2020, cb = TRUE)
  # Minneapolis area ZIPs start with 554 or 553
  sfZips <- sfZips[grepl("^55[34]", sfZips$ZCTA5CE20), ]
  sfZips <- st_transform(sfZips, 26915)
  sfFinal <- st_join(sfPermitZoned, sfZips[, c("ZCTA5CE20")])
  dtFinal <- as.data.table(sfFinal)
  setnames(dtFinal, "ZCTA5CE20", "geo_id", skip_absent = TRUE)
  dtFinal[, geo_name := paste("ZIP", geo_id)]
}

# ==========================================
# IDENTIFY R-ZONES (Single-Family Equivalent)
# ==========================================
# Minneapolis zoning: R1, R1A = single-family; R2, R2B, R3 = small multi-family
# The zoning field name may vary - check output above and adjust

# Print unique zone values to help identify the right field
message("Checking zoning categories...")
# Uncomment and run to see what zones exist:
# print(unique(dtFinal$zone))  # or whatever the zone column is called

# Filter for residential zones (adjust field name as needed)
# Pre-2020 Minneapolis had R1, R1A, R2, R2B, R3, R4, R5, R6
# These are the "interior" residential zones analogous to Portland's R-zones
print(head(dtFinal))
print(table(dtFinal[,land_use]))
print(table(dtFinal[,land_use_c]))
dtAnalysis <- dtFinal[grepl("UN1|UN2", land_use_c, ignore.case = TRUE)] # not UN3 that's more density

# Alternative: if the zone field has different naming, try:
# dtAnalysis <- dtFinal[grepl("residential", zone, ignore.case = TRUE)]

message("Permits in R-zones: ", nrow(dtAnalysis))

# ==========================================
# LOT-LEVEL COLLAPSE
# ==========================================
# Minneapolis data uses PIN for parcel ID

# Check what ID field exists
if ("pin" %in% names(dtAnalysis)) {
  parcel_id <- "address"
} else if ("property_id" %in% names(dtAnalysis)) {
  parcel_id <- "property_id"
} else {
  # Use address as fallback
  parcel_id <- "address"
  message("Warning: Using address as parcel ID - may not be unique")
}

# Collapse multiple permits per parcel into a single lot record
dtLots <- dtAnalysis[!is.na(geo_id) & !is.na(get(parcel_id)), .(
  isPlexLot = any(isPlex, na.rm = TRUE),
  total_units = sum(units, na.rm = TRUE),
  permit_count = .N
), by = c(parcel_id, "geo_id", "land_use_c", "geo_name")]

message("Total lots with permits: ", nrow(dtLots))
message("Lots with plex permits: ", sum(dtLots$isPlexLot))

# ==========================================
# GEOGRAPHY-LEVEL PROPENSITY (Based on Lots)
# ==========================================

dtGeo <- dtLots[, .(
  infill_lot_count = sum(isPlexLot, na.rm = TRUE),
  total_lots_active = .N,
  total_units = sum(total_units, na.rm = TRUE),
  propensity = sum(isPlexLot, na.rm = TRUE) / .N
), by = .(geo_id, geo_name)]

setorder(dtGeo, -total_lots_active)

# ==========================================
# SAVE & SUMMARY
# ==========================================
output_file <- sprintf("~/OneDrive - UBC/dataProcessed/minneapolis_%s_propensity.rds", GEOGRAPHY)
saveRDS(dtGeo, output_file)

cat("\n=== LOT-LEVEL SUMMARY BY", toupper(GEOGRAPHY), "===\n")
cat("Total Active Lots (R-Zones):", nrow(dtLots), "\n")
cat("Total Infill Lots (Plexes):", sum(dtLots$isPlexLot), "\n")
cat("Overall Infill Propensity (Lot-based):",
    round(sum(dtGeo$infill_lot_count) / sum(dtGeo$total_lots_active), 4), "\n")

print(head(dtGeo, 15))

# ==========================================
# DIAGNOSTIC: Check zoning field names
# ==========================================
cat("\n=== DIAGNOSTIC INFO ===\n")
cat("Zoning column names available:\n")
print(names(sfZoning)[1:min(10, length(names(sfZoning)))])
cat("\nSample zone values (first 20 unique):\n
")
# print(head(unique(dtFinal$land_use_c), 20))
