# Permit Grouping Analysis - Merge by Geolocation
# Goal: Link laneway and main house permits, identify new build pairs

# File paths
PERMIT_F <- "data/raw/vancouver_permits_full.csv"
PERMIT_LANEWAY_F <- "data/raw/issued-building-permitsLanewayHouse.csv"

# ---- 1. Load Data ----
permits_full <- read.delim(PERMIT_F, sep = ";", fileEncoding = "UTF-8",
                           stringsAsFactors = FALSE)

permits_laneway <- read.delim(PERMIT_LANEWAY_F, sep = ";", fileEncoding = "UTF-8",
                              stringsAsFactors = FALSE)

# Standardize column names (laneway file has capital letters)
names(permits_laneway) <- tolower(names(permits_laneway))

# ---- 2. Check geo_point_2d as lot identifier ----
cat("\n=== Checking geo_point_2d as lot identifier ===\n")

# Count permits per geo_point
full_geo_table <- table(permits_full$geo_point_2d)
lane_geo_table <- table(permits_laneway$geo_point_2d)

cat("Full permits - max permits per geo_point:", max(full_geo_table), "\n")
cat("Full permits - locations with multiple permits:", 
    sum(full_geo_table > 1), "/", length(full_geo_table), "\n")

cat("Laneway permits - max permits per geo_point:", max(lane_geo_table), "\n")
cat("Laneway permits - locations with multiple permits:", 
    sum(lane_geo_table > 1), "/", length(lane_geo_table), "\n")

# ---- 3. Merge by geo_point_2d ----
cat("\n=== Merging permits by geo_point_2d ===\n")

# This WILL create many-to-many (multiple permits over time at same location)
merged_permits <- merge(
  permits_laneway,
  permits_full,
  by = "geo_point_2d",
  suffixes = c("_lane", "_main"),
  all.x = FALSE  # inner join
)

cat("Laneway permits:", nrow(permits_laneway), "\n")
cat("Full permits:", nrow(permits_full), "\n")
cat("Merged records (many-to-many):", nrow(merged_permits), "\n")

# ---- 4. FILTER: Exclude laneway-to-laneway matches ----
cat("\n=== Filtering out laneway-to-laneway matches ===\n")

# Remove rows where "main" permit is actually a laneway
merged_permits <- merged_permits[
  merged_permits$specificusecategory_main != "Laneway House" | 
    is.na(merged_permits$specificusecategory_main),
]

cat("After removing laneway-to-laneway:", nrow(merged_permits), "\n")

# ---- 5. Check same-year, same-applicant pattern ----
cat("\n=== Validating merge: same year + same applicant ===\n")

same_year <- merged_permits[
  merged_permits$issueyear_lane == merged_permits$issueyear_main,
]

same_year_same_applicant <- same_year[
  same_year$applicantaddress_lane == same_year$applicantaddress_main,
]

cat("Permits issued same year:", nrow(same_year), "\n")
cat("Of those, same applicant address:", nrow(same_year_same_applicant), "\n")
if(nrow(same_year) > 0) {
  cat("Match rate:", round(nrow(same_year_same_applicant)/nrow(same_year)*100, 1), "%\n")
}

# ---- 6. Find new construction pairs (both main house AND laneway new builds) ----
cat("\n=== Finding new construction pairs (2018-2019) ===\n")

new_build_pairs <- merged_permits[
  merged_permits$issueyear_lane %in% c(2018, 2019) &
  merged_permits$issueyear_main %in% c(2018, 2019) &
  merged_permits$typeofwork_lane == "New Building" &
  merged_permits$typeofwork_main == "New Building",
]

# Select key columns
new_build_pairs_out <- new_build_pairs[, c(
  "geo_point_2d",
  "permitnumber_lane", "permitnumber_main",
  "issuedate_lane", "issuedate_main",
  "address_lane", "address_main",
  "projectvalue_lane", "projectvalue_main",
  "specificusecategory_main",
  "applicant_lane", "applicant_main"
)]

cat("New build pairs (2018-2019):", nrow(new_build_pairs), "\n")

# Look at specific use categories for main buildings in these pairs
cat("\nMain building types in new build pairs:\n")
print(table(new_build_pairs$specificusecategory_main))

# ---- 7. Strategy refinement: What IS the main house? ----
cat("\n=== Analysis: What permits should we consider 'main house'? ===\n")

# For each laneway, what other permits exist at same geo_point?
# Let's look at a sample

# Get unique geo_points from laneways
sample_geos <- unique(permits_laneway$geo_point_2d)[1:5]

cat("\nSample: permits at same geo_point as first 5 laneways:\n")
for(i in 1:length(sample_geos)) {
  geo <- sample_geos[i]
  
  lane_perms <- permits_laneway[permits_laneway$geo_point_2d == geo, ]
  full_perms <- permits_full[permits_full$geo_point_2d == geo, ]
  
  cat("\n--- Geo point", i, ":", geo, "---\n")
  cat("Laneway permits:", nrow(lane_perms), "\n")
  
  if(nrow(lane_perms) > 0) {
    cat("  ", lane_perms$permitnumber[1], "-", lane_perms$issuedate[1], 
        "-", lane_perms$typeofwork[1], "\n")
  }
  
  cat("Full permits at same location:", nrow(full_perms), "\n")
  if(nrow(full_perms) > 0) {
    for(j in 1:min(3, nrow(full_perms))) {
      cat("  ", full_perms$permitnumber[j], "-", full_perms$issuedate[j], 
          "-", full_perms$typeofwork[j], "-", 
          full_perms$specificusecategory[j], "\n")
    }
  }
}

# ---- 8. Export for BC Assessment matching ----
cat("\n=== Preparing export for BC Assessment matching ===\n")

# For 2018-2019, get all laneway permits with their location
permits_2018_2019 <- merged_permits[
  merged_permits$issueyear_lane %in% c(2018, 2019),
  c("geo_point_2d", "address_lane", "address_main", 
    "permitnumber_lane", "permitnumber_main",
    "issuedate_lane", "issuedate_main",
    "projectvalue_lane", "projectvalue_main",
    "typeofwork_main", "specificusecategory_main")
]

# Order by issue date
permits_2018_2019 <- permits_2018_2019[order(permits_2018_2019$issuedate_lane), ]

# Create output directory if needed
if(!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

write.csv(permits_2018_2019, "data/processed/permits_2018_2019_merged.csv", 
          row.names = FALSE)
write.csv(new_build_pairs_out, "data/processed/new_build_pairs_2018_2019.csv", 
          row.names = FALSE)

cat("\n=== Files exported ===\n")
cat("data/processed/permits_2018_2019_merged.csv (", nrow(permits_2018_2019), "rows )\n")
cat("data/processed/new_build_pairs_2018_2019.csv (", nrow(new_build_pairs_out), "rows )\n")

# ---- 9. Summary by year ----
cat("\n=== Summary Statistics ===\n")

years <- sort(unique(merged_permits$issueyear_lane))
summary_df <- data.frame(
  year = years,
  n_permits = NA,
  n_new_builds_main = NA,
  n_same_applicant = NA,
  pct_same_applicant = NA
)

for(i in 1:length(years)) {
  yr <- years[i]
  yr_data <- merged_permits[merged_permits$issueyear_lane == yr, ]
  
  summary_df$n_permits[i] <- nrow(yr_data)
  summary_df$n_new_builds_main[i] <- sum(yr_data$typeofwork_main == "New Building", na.rm = TRUE)
  summary_df$n_same_applicant[i] <- sum(yr_data$applicantaddress_lane == yr_data$applicantaddress_main, na.rm = TRUE)
  summary_df$pct_same_applicant[i] <- round(summary_df$n_same_applicant[i] / summary_df$n_permits[i] * 100, 1)
}

print(summary_df)

cat("\n=== Key Question ===\n")
cat("Should we match on:\n")
cat("  (a) Most recent permit at geo_point before laneway?\n")
cat("  (b) New build permits only?\n")
cat("  (c) Single Detached House permits only?\n")
cat("  (d) All non-laneway permits?\n")
