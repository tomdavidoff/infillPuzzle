# integrated_infill_analysis.R
# Combines three analyses:
# (1) Pricing propensity for duplexes/multiplexes by census tract
# (2) Built FSR by dwelling type (single/duplex)
# (3) Laneway home classification to break out single/single+laneway/duplex
# Tom Davidoff, 12/29/25

library(data.table)
library(fixest)
library(ggplot2)
library(scales)
library(stringr)
library(RSQLite)
library(sf)
library(readxl)

# ==========================================
# SETTINGS & PATHS
# ==========================================
GEO_LEVEL <- "census_tract" # Options: "bca_nbhd", "census_tract", "fsa_plus_1"
PCCF_RAW  <- "data/raw/pccfNat_fccpNat_062017.txt"
PCCF_RDS  <- "data/pccf_bc.rds"
BCA_DB    <- "~/OneDrive - UBC/Documents/data/bca/REVD16_and_inventory_extracts.sqlite3"
PERMIT_F  <- "data/raw/vancouver_permits_full.csv"
ZONING_F  <- "data/raw/vancouver_zoning.geojson"

# BCA 2025 files
BCA_DESC_F <- "~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv"
BCA_RESINV_F <- "~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt"
LANEWAY_F <- "data/raw/20231231_UBC_CustomLanewayReport.xlsx"

clean_pc  <- function(x) gsub(" ", "", toupper(trimws(x)))

# ==========================================
# PART 1: BCA 2016 PRICING DATA FOR PROPENSITY
# ==========================================
cat("\n=== PART 1: Loading BCA 2016 pricing data ===\n")

# Prepare PCCF crosswalk
if (!file.exists(PCCF_RDS)) {
    dtPccf <- fread(PCCF_RAW, sep = "\n", header = FALSE, encoding = "Latin-1")
    Encoding(dtPccf$V1) <- "latin1"
    dtPccf[, `:=`(
        postalCode  = substr(V1, 1, 6),
        cma         = substr(V1, 99, 101),
        censusTract = substr(V1, 103, 109),
        sli         = substr(V1, 162, 162) # 
    )]
    saveRDS(dtPccf[cma == "933" & sli == "1", .(postalCode, censusTract)], PCCF_RDS)
}

# Load BCA 2016 sales data (all residential)
db <- dbConnect(SQLite(), BCA_DB)
dtBCA2016 <- as.data.table(dbGetQuery(db, "
    SELECT s.folioID, s.conveyancePrice, s.conveyanceDate,
           i.MB_effective_year, i.MB_total_finished_area,
           d.actualUseDescription, d.neighbourhoodDescription, a.postalCode
    FROM sales s
    JOIN folio f ON s.folioID = f.folioID
    JOIN residentialInventory i ON f.rollNumber = i.roll_number
    JOIN folioDescription d ON f.folioID = d.folioID
    JOIN address a ON f.folioID = a.folioID
    WHERE s.conveyanceTypeDescription = 'Improved Single Property Transaction'
      AND i.jurisdiction = 200
"))
dbDisconnect(db)

# Keep all records
dtBCA2016_all <- dtBCA2016

# Numeric conversion and cleaning
cols <- c("conveyancePrice", "MB_effective_year", "MB_total_finished_area")
dtBCA2016_all[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
dtBCA2016_all <- dtBCA2016_all[!is.na(MB_total_finished_area) & conveyancePrice > 100000]

# filter types to only single and duplex 
print(table(dtBCA2016_all$actualUseDescription))
dtBCA2016_all <- dtBCA2016_all[grepl("Single Family Dwelling|Duplex|Residential Dwelling with Suite", actualUseDescription, ignore.case = TRUE)]
print(table(dtBCA2016_all$actualUseDescription))

# Metrics & Outliers
CUT <- 0.05
dtBCA2016_all[, `:=`(
    age = 2016 - MB_effective_year, 
    ppsf = conveyancePrice / MB_total_finished_area, 
    cleanPostal = clean_pc(postalCode),
    dwelling_type = ifelse(grepl("uplex", actualUseDescription, ignore.case = TRUE), "duplex", "single")
)]

# Filter to singles and duplexes only
dtBCA2016_all <- dtBCA2016_all[dwelling_type %in% c("single", "duplex")]

dtBCA2016_all <- dtBCA2016_all[
    age >= 0 &
    MB_total_finished_area %between% quantile(MB_total_finished_area, c(CUT, 1-CUT))
]

cat("BCA 2016 transactions loaded:", nrow(dtBCA2016_all), "\n")
cat("  Single:", dtBCA2016_all[dwelling_type == "single", .N], "\n")
cat("  Duplex:", dtBCA2016_all[dwelling_type == "duplex", .N], "\n")

# ==========================================
# PART 2: BCA 2025 FSR & LANEWAY ANALYSIS
# ==========================================
cat("\n=== PART 2: Loading BCA 2025 inventory for FSR analysis ===\n")

# Load descriptions
dtDesc <- fread(
  BCA_DESC_F,
  select = c("FOLIO_ID", "ROLL_NUMBER", "ACTUAL_USE_DESCRIPTION", "LAND_SIZE")
)
dtDesc[, Roll_Number := as.numeric(ROLL_NUMBER)]

# Load residential inventory (2019+)
dtRoll <- fread(
  BCA_RESINV_F,
  select = c("Roll_Number", "MB_Year_Built", "MB_Total_Finished_Area", 
             "Land_Area_Total", "Zoning")
)
dtRoll <- dtRoll[MB_Year_Built > 2018]

# Merge
dtMerge <- merge(dtDesc, dtRoll, by = "Roll_Number")

cat("BCA 2025 records (2019+):", nrow(dtMerge), "\n")

# Classify dwelling types
dtMerge[, dwelling_type := fcase(
  grepl("Duplex", ACTUAL_USE_DESCRIPTION, ignore.case = TRUE), "duplex",
  grepl("Single Family Dwelling", ACTUAL_USE_DESCRIPTION, ignore.case = TRUE), "single_family",
  ACTUAL_USE_DESCRIPTION == "Residential Dwelling with Suite", "suite",
  default = "other"
)]

# Unit-level prep
dt <- dtMerge[
  dwelling_type %in% c("single_family", "duplex") &
  !is.na(MB_Total_Finished_Area) &
  !is.na(Land_Area_Total) &
  Land_Area_Total > 0
]

dt[, `:=`(
  land_sqft = Land_Area_Total * 43560,
  roll_base = floor(Roll_Number / 1000)
)]

# Load laneway data
dtLaneway <- as.data.table(read_excel(LANEWAY_F, sheet = "DATA"))
setnames(dtLaneway, "ROLL_NUM", "Roll_Number_char")
dtLaneway[, `:=`(
  Roll_Number = as.numeric(Roll_Number_char),
  roll_base = floor(as.numeric(Roll_Number_char) / 1000)
)]

cat("Laneway records:", nrow(dtLaneway), "\n")

# ==========================================
# PART 3: LOT-LEVEL AGGREGATION WITH LANEWAY FLAG
# ==========================================
cat("\n=== PART 3: Aggregating to lot level with laneway classification ===\n")

# Aggregate to lot level
dt_lot <- dt[, .(
  dwelling_type = first(dwelling_type),
  land_sqft = first(land_sqft),
  total_floor_area = sum(MB_Total_Finished_Area),
  n_units = .N,
  year_built = max(MB_Year_Built)
), by = roll_base]

dt_lot[, fsr := total_floor_area / land_sqft]
dt_lot_clean <- dt_lot[land_sqft <= 10000 & fsr < 2]

# Add laneway flag
dt_lot_clean[, has_laneway := roll_base %in% dtLaneway$roll_base]

# Create detailed classification
dt_lot_clean[, dwelling_class := fcase(
  dwelling_type == "duplex", "duplex",
  dwelling_type == "single_family" & has_laneway, "single_with_laneway",
  dwelling_type == "single_family" & !has_laneway, "single_only",
  default = "other"
)]

cat("\n=== Dwelling classification (2019+) ===\n")
print(dt_lot_clean[, .N, by = dwelling_class])

cat("\n=== FSR by dwelling class ===\n")
print(dt_lot_clean[, .(
  n_lots = .N,
  mean_fsr = round(mean(fsr), 3),
  median_fsr = round(median(fsr), 3),
  mean_land = round(mean(land_sqft), 0),
  mean_floor = round(mean(total_floor_area), 0)
), by = dwelling_class])

# ==========================================
# PART 4: PERMIT DATA & INFILL PROPENSITY
# ==========================================
cat("\n=== PART 4: Loading permit data for infill propensity ===\n")

if (!file.exists(PERMIT_F)) {
    download.file(
        "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/issued-building-permits/exports/csv?delimiter=%3B", 
        PERMIT_F
    )
}
if (!file.exists(ZONING_F)) {
    download.file(
        "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/zoning-districts-and-labels/exports/geojson", 
        ZONING_F
    )
}

dtPermit <- fread(PERMIT_F)
setnames(dtPermit, make.names(names(dtPermit)))

# Filter Major Infill Projects
dtMajor <- dtPermit[
    as.IDate(issuedate) >= "2019-01-01" &
    typeofwork %in% c("New Building", "Addition / Alteration") &
    projectvalue > 200000
]

# Enhanced infill classification including laneways and multiplexes
dtMajor[, `:=`(
    infillType = fcase(
        grepl("multiplex", projectdescription, ignore.case = TRUE), "multiplex",
        grepl("uplex|Multiple Dwelling", specificusecategory, ignore.case = TRUE) |
            grepl("convert|two-family|2-family", projectdescription, ignore.case = TRUE), "duplex",
        grepl("laneway", specificusecategory, ignore.case = TRUE) |
            grepl("laneway", projectdescription, ignore.case = TRUE), "laneway",
        default = "major_sfd_reno"
    ),
    cleanPostal = clean_pc(str_sub(address, -7))
)]

cat("Permits by infill type:\n")
print(dtMajor[, .N, by = infillType])

# Extract Lat/Long and Spatial Join
dtMajor[, c("Lat", "Lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]
sfPermit <- st_join(
    st_as_sf(dtMajor[!is.na(Lat)], coords = c("Lon", "Lat"), crs = 4326),
    st_read(ZONING_F, quiet = TRUE)
)

# Filter for R1-1 Zone
dtRZone <- as.data.table(sfPermit)[grepl("R1-1", zoning_district)]

cat("Permits in R1-1 zone:", nrow(dtRZone), "\n")

# ==========================================
# PART 5: GEOGRAPHIC CROSSWALK & MERGE
# ==========================================
cat("\n=== PART 5: Creating geographic crosswalk ===\n")

if (GEO_LEVEL == "bca_nbhd") {
    crosswalk <- dtBCA2016_all[, .N, by = .(cleanPostal, geo_id = neighbourhoodDescription)]
    crosswalk <- crosswalk[order(cleanPostal, -N)][, .(geo_id = geo_id[1]), by = cleanPostal]
} else if (GEO_LEVEL == "census_tract") {
    crosswalk <- readRDS(PCCF_RDS)[, .(cleanPostal = postalCode, geo_id = censusTract)]
} else {
    crosswalk <- dtBCA2016_all[, .(geo_id = str_sub(cleanPostal, 1, 4)), by = cleanPostal]
}

# Merge data to geography
dtBCAGeo <- merge(dtBCA2016_all, crosswalk, by = "cleanPostal")
dtPermitGeo <- merge(dtRZone, crosswalk, by = "cleanPostal")

cat("BCA records with geography:", nrow(dtBCAGeo), "\n")
cat("Permit records with geography:", nrow(dtPermitGeo), "\n")

# ==========================================
# PART 6: PRICING ANALYSIS - SINGLE VS DUPLEX
# ==========================================
cat("\n=== PART 6: Comparing single vs duplex pricing (2016 data) ===\n")

# Overall comparison
price_comparison <- dtBCAGeo[, .(
    mean_ppsf = mean(ppsf, na.rm = TRUE),
    median_ppsf = median(ppsf, na.rm = TRUE),
    mean_sqft = mean(MB_total_finished_area, na.rm = TRUE),
    n_sales = .N
), by = dwelling_type]

print(price_comparison)

# Regression: duplex vs single price per sqft
m_dwelling <- feols(ppsf ~ dwelling_type + log(age + 1) + MB_total_finished_area | geo_id, 
                    data = dtBCAGeo)
print(summary(m_dwelling))

# ==========================================
# PART 7: PRICING SLOPE (CONCAVITY) BY TRACT
# ==========================================
cat("\n=== PART 7: Calculating pricing slope by census tract ===\n")

# BCA Pricing Slope (for singles only, to match original analysis)
m_slopes <- feols(
    ppsf ~ log(age + 1) + i(geo_id, MB_total_finished_area) | geo_id, 
    data = dtBCAGeo[dwelling_type == "single"]
)

dtSlopes <- as.data.table(broom::tidy(m_slopes))[grepl("MB_total_finished_area", term)]
dtSlopes[, geo_id := gsub("geo_id::(.*):MB_total_finished_area", "\\1", term)]

# Calculate Mean Price per SqFt by tract
dtBcaMeans <- dtBCAGeo[dwelling_type == "single", .(
    meanPPSF = mean(ppsf, na.rm = TRUE),
    meanPPSF_duplex = NA_real_  # placeholder for later
), by = geo_id]

# Add duplex mean prices where available
duplex_means <- dtBCAGeo[dwelling_type == "duplex", .(
    meanPPSF_duplex = mean(ppsf, na.rm = TRUE)
), by = geo_id]

dtBcaMeans <- merge(dtBcaMeans, duplex_means, by = "geo_id", all.x = TRUE, suffixes = c("", "_dup"))
dtBcaMeans[!is.na(meanPPSF_duplex_dup), meanPPSF_duplex := meanPPSF_duplex_dup]
dtBcaMeans[, meanPPSF_duplex_dup := NULL]

# ==========================================
# PART 8: INFILL PROPENSITY BY TYPE
# ==========================================
cat("\n=== PART 8: Calculating infill propensity by type ===\n")

# Calculate propensity for duplex, laneway, multiplex, and total infill
dtInfill <- dtPermitGeo[, .(
    propensity_duplex = sum(infillType == "duplex") / .N,
    propensity_laneway = sum(infillType == "laneway") / .N,
    propensity_multiplex = sum(infillType == "multiplex") / .N,
    propensity_total = sum(infillType %in% c("duplex", "laneway", "multiplex")) / .N,
    n_duplex = sum(infillType == "duplex"),
    n_laneway = sum(infillType == "laneway"),
    n_multiplex = sum(infillType == "multiplex"),
    totalProjects = .N
), by = geo_id]

cat("Census tracts with permit data:", nrow(dtInfill), "\n")

# ==========================================
# PART 9: FINAL MERGE & ANALYSIS
# ==========================================
cat("\n=== PART 9: Creating final merged dataset ===\n")

dtFinal <- merge(dtInfill, dtSlopes, by = "geo_id", all.x = TRUE)
dtFinal <- merge(dtFinal, dtBcaMeans, by = "geo_id", all.x = TRUE)

cat("Final dataset:", nrow(dtFinal), "census tracts\n")

# Summary statistics
cat("\n=== Summary by infill type ===\n")
print(dtFinal[totalProjects > 10, .(
    n_tracts = .N,
    mean_propensity_duplex = mean(propensity_duplex),
    mean_propensity_laneway = mean(propensity_laneway),
    mean_propensity_multiplex = mean(propensity_multiplex),
    mean_slope = mean(estimate, na.rm = TRUE),
    mean_ppsf = mean(meanPPSF, na.rm = TRUE)
)])

# ==========================================
# PART 10: VISUALIZATIONS
# ==========================================
cat("\n=== PART 10: Creating visualizations ===\n")

# Plot 1: Duplex propensity vs pricing slope
p1 <- ggplot(dtFinal[totalProjects > 10], 
             aes(x = estimate, y = propensity_duplex)) +
    geom_point(aes(size = totalProjects), alpha = 0.4) +
    geom_smooth(method = "lm", color = "firebrick") +
    scale_y_continuous(labels = percent) +
    labs(
        title = paste("Duplex Propensity vs Pricing Concavity:", GEO_LEVEL),
        x = "BCA Pricing Slope ($/sqft per sqft)",
        y = "Duplex Propensity"
    ) +
    theme_minimal()

ggsave(paste0("text/duplex_propensity_", GEO_LEVEL, ".png"), p1, width = 8, height = 6)

# Plot 2: Laneway propensity vs pricing slope
p2 <- ggplot(dtFinal[totalProjects > 10], 
             aes(x = estimate, y = propensity_laneway)) +
    geom_point(aes(size = totalProjects), alpha = 0.4) +
    geom_smooth(method = "lm", color = "darkgreen") +
    scale_y_continuous(labels = percent) +
    labs(
        title = paste("Laneway Propensity vs Pricing Concavity:", GEO_LEVEL),
        x = "BCA Pricing Slope ($/sqft per sqft)",
        y = "Laneway Propensity"
    ) +
    theme_minimal()

ggsave(paste0("text/laneway_propensity_", GEO_LEVEL, ".png"), p2, width = 8, height = 6)

# Plot 3: Multiplex propensity vs pricing slope
p3 <- ggplot(dtFinal[totalProjects > 10], 
             aes(x = estimate, y = propensity_multiplex)) +
    geom_point(aes(size = totalProjects), alpha = 0.4) +
    geom_smooth(method = "lm", color = "darkorange") +
    scale_y_continuous(labels = percent) +
    labs(
        title = paste("Multiplex Propensity vs Pricing Concavity:", GEO_LEVEL),
        x = "BCA Pricing Slope ($/sqft per sqft)",
        y = "Multiplex Propensity"
    ) +
    theme_minimal()

ggsave(paste0("text/multiplex_propensity_", GEO_LEVEL, ".png"), p3, width = 8, height = 6)

# Plot 4: FSR by dwelling class (from PART 3)
p4 <- ggplot(dt_lot_clean, aes(x = land_sqft, y = fsr, color = dwelling_class)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
        title = "FSR by Dwelling Class (2019+)",
        x = "Lot Size (sqft)",
        y = "FSR (Main Building only, from BC Assessment)",
        color = "Dwelling Class"
    ) +
    theme_minimal()

ggsave("text/fsr_by_dwelling_class.png", p4, width = 10, height = 6)

# ==========================================
# PART 11: REGRESSIONS
# ==========================================
cat("\n=== PART 11: Regression results ===\n")

# Duplex propensity
m_duplex_ppsf <- feols(propensity_duplex ~ meanPPSF, 
                       data = dtFinal[totalProjects > 10])
m_duplex_full <- feols(propensity_duplex ~ estimate + meanPPSF, 
                       data = dtFinal[totalProjects > 10])

cat("\n--- Duplex Propensity Regressions ---\n")
print(etable(m_duplex_ppsf, m_duplex_full))

# Laneway propensity
m_laneway_ppsf <- feols(propensity_laneway ~ meanPPSF, 
                        data = dtFinal[totalProjects > 10])
m_laneway_full <- feols(propensity_laneway ~ estimate + meanPPSF, 
                        data = dtFinal[totalProjects > 10])

cat("\n--- Laneway Propensity Regressions ---\n")
print(etable(m_laneway_ppsf, m_laneway_full))

# Multiplex propensity
m_multiplex_ppsf <- feols(propensity_multiplex ~ meanPPSF, 
                          data = dtFinal[totalProjects > 10])
m_multiplex_full <- feols(propensity_multiplex ~ estimate + meanPPSF, 
                          data = dtFinal[totalProjects > 10])

cat("\n--- Multiplex Propensity Regressions ---\n")
print(etable(m_multiplex_ppsf, m_multiplex_full))

# Combined infill
m_total_ppsf <- feols(propensity_total ~ meanPPSF, 
                      data = dtFinal[totalProjects > 10])
m_total_full <- feols(propensity_total ~ estimate + meanPPSF, 
                      data = dtFinal[totalProjects > 10])

cat("\n--- Total Infill Propensity Regressions ---\n")
print(etable(m_total_ppsf, m_total_full))

# ==========================================
# PART 12: EXPORT RESULTS
# ==========================================
cat("\n=== PART 12: Exporting results ===\n")

if (!dir.exists("data/processed")) {
    dir.create("data/processed", recursive = TRUE)
}

fwrite(dtFinal, "data/processed/infill_propensity_by_tract.csv")
fwrite(dt_lot_clean, "data/processed/lot_level_fsr_with_laneways.csv")
fwrite(price_comparison, "data/processed/single_vs_duplex_pricing.csv")

cat("\n=== Analysis complete! ===\n")
cat("Files exported:\n")
cat("  - data/processed/infill_propensity_by_tract.csv\n")
cat("  - data/processed/lot_level_fsr_with_laneways.csv\n")
cat("  - data/processed/single_vs_duplex_pricing.csv\n")
cat("\nPlots saved:\n")
cat("  - text/duplex_propensity_", GEO_LEVEL, ".png\n", sep = "")
cat("  - text/laneway_propensity_", GEO_LEVEL, ".png\n", sep = "")
cat("  - text/multiplex_propensity_", GEO_LEVEL, ".png\n", sep = "")
cat("  - text/fsr_by_dwelling_class.png\n")
