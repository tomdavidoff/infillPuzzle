# mergePermitsSpatial_v2.R
# Permit bundling for Vancouver residential permits
# Tom Davidoff / Claude
#
# BUNDLING: Distance ≤ 26m AND days apart ≤ 7
# 
# CLASSIFICATION (requires New Building or Addition/Alteration, NOT salvage/demo):
#   - multiplex: has Multiple Dwelling (new/alteration)
#   - duplex: has Duplex (new/alteration)
#   - laneway_single: has BOTH new/altered laneway AND new/altered single
#   - laneway_standalone: has new/altered laneway but NO new/altered single
#   - single_standalone: has new/altered single but no laneway

library(data.table)
library(sf)

# =============================================================================
# CONFIGURATION
# =============================================================================

MINYEAR <- 2017
MINVAL <- 250000
SPATIAL_THRESHOLD_M <- 26
TEMPORAL_THRESHOLD_DAYS <- 7

CATEGORIES_KEEP <- c(
  "Single Detached House", 
  "Laneway House", 
  "Single Detached House w/ Sec Suite",
  "Single Detached House w/Sec Suite",
  "Multiple Dwelling", 
  "Duplex", 
  "Duplex w/Secondary Suite",
  "Infill Single Detached House"
)

# Only these work types count for classification (exclude salvage/demo)
WORK_TYPES_SUBSTANTIVE <- c("New Building", "Addition / Alteration")

# =============================================================================
# LOAD AND FILTER
# =============================================================================

cat("Loading data...\n")
dtBCA <- readRDS("~/OneDrive - UBC/dataProcessed/bca_vancouver_residential.rds")
dtP <- fread("~/OneDrive - UBC/dataRaw/vancouver_permits_full.csv",
             select = c("geom", "geo_point_2d", "permitnumber", "permitnumbercreateddate",
                        "applicant", "typeofwork", "projectvalue", "specificusecategory", "address"))

# Keep all work types initially (we need salvage/demo for bundling, just not classification)
dtP[, permitDate := as.Date(permitnumbercreateddate)]
dtP[, yearCreated := year(permitDate)]
dtP <- dtP[yearCreated >= MINYEAR]
dtP <- dtP[specificusecategory %in% CATEGORIES_KEEP]
dtP <- dtP[projectvalue >= MINVAL | typeofwork %in% c("Salvage and Abatement", "Demolition / Deconstruction")]
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]
dtP <- dtP[!is.na(lat) & !is.na(lon)]

cat("  Permits after filtering:", nrow(dtP), "\n")

# =============================================================================
# SPATIAL SETUP
# =============================================================================

sfP <- st_as_sf(dtP, coords = c("lon", "lat"), crs = 4326)
sfP <- st_transform(sfP, 32610)  # UTM Zone 10N

# =============================================================================
# BUNDLE PERMITS
# =============================================================================

cat("Bundling (dist ≤", SPATIAL_THRESHOLD_M, "m AND days ≤", TEMPORAL_THRESHOLD_DAYS, ")...\n")

coords <- st_coordinates(sfP)
dates <- sfP$permitDate
n <- nrow(sfP)
bundle_id <- 1:n

for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    dist_m <- sqrt((coords[i,1] - coords[j,1])^2 + (coords[i,2] - coords[j,2])^2)
    days_apart <- abs(as.numeric(dates[i] - dates[j]))
    
    if (dist_m <= SPATIAL_THRESHOLD_M && days_apart <= TEMPORAL_THRESHOLD_DAYS) {
      old_bundle <- bundle_id[j]
      new_bundle <- bundle_id[i]
      bundle_id[bundle_id == old_bundle] <- new_bundle
    }
  }
}

bundle_id <- as.integer(factor(bundle_id))
sfP$bundle_id <- bundle_id
cat("  Bundles:", max(bundle_id), "\n")

# =============================================================================
# JOIN BCA + CENSUS TRACTS
# =============================================================================

sfBCA <- st_as_sf(dtBCA)
sfBCA <- st_transform(sfBCA, st_crs(sfP))
merged <- st_join(sfP, sfBCA, join = st_within)

fCT <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
if (file.exists(fCT)) {
  dCT <- st_read(fCT, quiet = TRUE)
  dCT <- st_transform(dCT, st_crs(merged))
  merged <- st_join(merged, dCT, join = st_within)
}

# =============================================================================
# CLASSIFY BUNDLES
# =============================================================================

dtM <- as.data.table(merged)

# Flag permit types - ONLY count substantive work (not salvage/demo)
dtM[, is_substantive := typeofwork %in% WORK_TYPES_SUBSTANTIVE]
dtM[, is_single := grepl("Single Detached", specificusecategory) & is_substantive]
dtM[, is_laneway := grepl("Laneway", specificusecategory) & is_substantive]
dtM[, is_duplex := grepl("Duplex", specificusecategory) & is_substantive]
dtM[, is_multi := grepl("Multiple", specificusecategory) & is_substantive]

# Aggregate to bundle level
dtOut <- dtM[, .(
  # Classification inputs (based on substantive work only)
  has_single = any(is_single),
  has_laneway = any(is_laneway),
  has_duplex = any(is_duplex),
  has_multi = any(is_multi),
  
  # Bundle info
  n_permits = .N,
  n_substantive = sum(is_substantive),
  total_value = sum(projectvalue, na.rm = TRUE),
  min_date = min(permitDate),
  max_date = max(permitDate),
  
  # Location
  CTUID = first(na.omit(CTUID)),
  ROLL_NUMBER = first(na.omit(ROLL_NUMBER)),
  address = first(address),
  geo_point_2d = first(geo_point_2d),
  
  # BCA fields
  MB_effective_year = first(na.omit(MB_effective_year)),
  MB_total_finished_area = first(na.omit(MB_total_finished_area)),
  neighbourhoodDescription = first(na.omit(neighbourhoodDescription)),
  LANDAREA = first(na.omit(LANDAREA)),
  
  # Permit details
  permit_numbers = paste(permitnumber, collapse = "; "),
  categories = paste(unique(specificusecategory), collapse = "; "),
  work_types = paste(unique(typeofwork), collapse = "; ")
), by = bundle_id]

# Classify bundle type
dtOut[, bundle_type := fcase(
  has_multi, "multiplex",
  has_duplex, "duplex",
  has_laneway & has_single, "laneway_single",
  has_laneway & !has_single, "laneway_standalone",
  has_single, "single_standalone",
  default = "other"
)]

# Clean up helper columns
dtOut[, c("has_single", "has_laneway", "has_duplex", "has_multi") := NULL]

# Reorder columns
setcolorder(dtOut, c("bundle_id", "bundle_type", "n_permits", "n_substantive", "total_value", 
                      "min_date", "max_date", "CTUID", "ROLL_NUMBER", 
                      "neighbourhoodDescription", "address", "geo_point_2d",
                      "MB_effective_year", "MB_total_finished_area", "LANDAREA",
                      "permit_numbers", "categories", "work_types"))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("BUNDLE SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n\n")

print(dtOut[, .N, by = bundle_type][order(-N)])

cat("\nlaneway_single bundles (new laneway + new single):\n")
print(dtOut[bundle_type == "laneway_single", .(bundle_id, n_permits, total_value, address, categories, work_types)])

cat("\nlaneway_standalone bundles (new laneway, no new single):\n")
print(dtOut[bundle_type == "laneway_standalone", .(bundle_id, n_permits, total_value, address, categories, work_types)])

# =============================================================================
# SAVE
# =============================================================================

outfile <- "~/OneDrive - UBC/dataProcessed/vancouverPermitBundles.rds"
saveRDS(dtOut, outfile)
cat("\nSaved:", outfile, "\n")
cat("  Rows:", nrow(dtOut), "\n")
