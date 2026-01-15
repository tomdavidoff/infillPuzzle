# bcaSales.R
# R to use 2019 sales to get census-tract level coefficients on duplex vs single family and also square footage coefficients on ppsf
# Tom Davidoff
# 01/14/26

# strategy:
# Get 2026 roll centroids, merge on roll/1000 (for subdivisions), confirm coverage for 2019 sales data (through 2018)
# merge with census tract centroids
# Regress coefficients, etc.

library(data.table)
library(sf)
library(RSQLite)
library(fixest)
library(ggplot2)
library(scales)
library(stringr)
library(readxl)

BCA19    <- "~/OneDrive - UBC/dataRaw/REVD19_and_inventory_extracts.sqlite3"
BCA26 <- "/Volumes/T7Office/bigFiles/bca_folios.gpkg"

# use 2026 data to get floor(roll number/1000) to get lot polygons -> centroid , maybe zoning 
fRollLatLon <- "~/OneDrive - UBC/dataProcessed/bca26RollCensusTract.rds"
fLaneway <-  "~/OneDrive - UBC/dataRaw/20231231_UBC_CustomLanewayReport.xlsx"


if (!file.exists(fRollLatLon)) {
  cat("Extracting lot centroids from BCA 2026 data...\n")
  desc26 <- st_read(BCA26,
      layer="WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
      query = "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV WHERE JURISDICTION_CODE=='200' AND (ACTUAL_USE_DESCRIPTION IN ('Residential Dwelling with Suite','Single Family Dwelling') OR INSTR(ACTUAL_USE_DESCRIPTION, 'uplex') > 0)", 
      quiet = TRUE
  )
  print(head(desc26))
  stDesc <- st_crs(desc26)
  roll_pt <- st_point_on_surface(desc26)
# convert geom to centroid
# --- 2021 Census Tracts (cartographic) via download
  ct_file <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
  sCT <- st_read(ct_file, quiet = TRUE)
  # sCT: your census tract sf (currently 3347)
  sCT <- st_make_valid(sCT)
  sCT<- st_transform(sCT, stDesc)

  roll_ct <- st_join( roll_pt, sCT[, c("CTUID", "DGUID", "CTNAME", "PRUID")], join = st_within, left = TRUE)
  print(head(roll_ct))
  saveRDS(roll_ct,fRollLatLon)

}

# Load BCA 2019 sales data (all residential)
fSales <- "~/OneDrive - UBC/dataProcessed/bca19Sales.rds"
if (!file.exists(fSales)) {
  cat("Extracting BCA 2019 sales data...\n")
  db <- dbConnect(SQLite(), BCA19)
  dtBCA2019 <- as.data.table(dbGetQuery(db, "
      SELECT s.folioID, s.conveyancePrice, s.conveyanceDate,
             i.MB_effective_year, i.MB_total_finished_area, i.land_width, i.land_depth, i.land_area, i.roll_number, i.zoning,
             d.actualUseDescription, d.neighbourhoodDescription, a.postalCode
      FROM sales s
      JOIN folio f ON s.folioID = f.folioID
      JOIN residentialInventory i ON f.rollNumber = i.roll_number
      JOIN folioDescription d ON f.folioID = d.folioID
      JOIN address a ON f.folioID = a.folioID
      WHERE s.conveyanceTypeDescription = 'Improved Single Property Transaction'
        AND i.jurisdiction = 200
        AND (d.actualUseDescription IN ('Residential Dwelling with Suite','Single Family Dwelling') OR INSTR(d.actualUseDescription, 'uplex') > 0)
  "))
  dbDisconnect(db)
  saveRDS(dtBCA2019,fSales)
  } 
dtBCA2019 <- readRDS(fSales)
print(head(dtBCA2019))

# merge back with census tract on roll number/1000
dtRollLatLon <- as.data.table(readRDS(fRollLatLon))
dtBCA2019[, roll_base := floor(as.numeric(roll_number) / 1000)]
dtRollLatLon[, roll_base := floor(as.numeric(ROLL_NUMBER) / 1000)]
dtBCA2019 <- merge(dtBCA2019, dtRollLatLon[, .(roll_base, CTUID, DGUID, CTNAME)], by = "roll_base", all.x = TRUE)
print(table(is.na(dtBCA2019[, CTUID])))
print(table(dtBCA2019[is.na(CTUID), actualUseDescription]))
print(table(dtBCA2019[!is.na(CTUID), actualUseDescription]))


# Numeric conversion and cleaning
cols <- c("conveyancePrice", "MB_effective_year", "MB_total_finished_area")
dtBCA2019[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
dtBCA2019 <- dtBCA2019[!is.na(MB_total_finished_area) & conveyancePrice > 100000]

# filter types to only single and duplex 
# Metrics & Outliers
CUT <- 0.05

print(names(dtBCA2019))
print(head(dtBCA2019))
dtBCA2019[, `:=`(
    age = year(conveyanceDate) - MB_effective_year, 
    ppsf = conveyancePrice / MB_total_finished_area, 
    dwelling_type = ifelse(grepl("uplex", actualUseDescription, ignore.case = TRUE), "duplex", "single")
)]

dtBCA2019 <- dtBCA2019[
    age >= 0 &
    MB_total_finished_area %between% quantile(MB_total_finished_area, c(CUT, 1-CUT))
]

cat("BCA 2019 transactions loaded:", nrow(dtBCA2019), "\n")
cat("  Single:", dtBCA2019[dwelling_type == "single", .N], "\n")
cat("  Duplex:", dtBCA2019[dwelling_type == "duplex", .N], "\n")

# ==========================================
# Exclude from the sales homes with laneways in some (all?) specifications
# ==========================================
cat("\n=== PART 2: Loading BCA 2025 inventory for FSR analysis ===\n")
# Load laneway data

dtLaneway <- as.data.table(read_excel(fLaneway, sheet = "DATA"))
setnames(dtLaneway, "ROLL_NUM", "Roll_Number_char")
dtLaneway[, `:=`(
  Roll_Number = as.numeric(Roll_Number_char),
  roll_base = floor(as.numeric(Roll_Number_char) / 1000)
)]
print(head(dtLaneway))

dtBCA2019 <- merge(dtBCA2019,dtLaneway[,.(roll_base,lanewayBuilt=YEAR_BUILT)],by="roll_base",all.x=TRUE)
dtBCA2019[, hasLaneway := !is.na(lanewayBuilt) & (year(conveyanceDate) >= lanewayBuilt) ]
print(names(dtBCA2019))
print(head(dtBCA2019))

MINYEAR <- 2014

dtBCA2019[,duplex:= ifelse(dwelling_type=="duplex",1,0)]
dtBCA2019[,nDuplex:=sum(duplex),by=.(CTUID)]
dtBCA2019 <- dtBCA2019[ grepl("RS",zoning)]
dtbca2019 <- dtBCA2019[ neighbourhoodDescription %chin% c("SHAUGNESSY","WEST END")==FALSE]
m_dwelling <- feols(ppsf ~ duplex + log(age + 1) | CTUID, data = dtBCA2019[hasLaneway==FALSE & year(conveyanceDate) >= MINYEAR])
m_sf <- feols(ppsf ~ log(age + 1) + MB_total_finished_area | CTUID, data = dtBCA2019[hasLaneway==FALSE & year(conveyanceDate) >= MINYEAR & nDuplex>20])
print(etable(m_dwelling,m_sf))

m_dwellingNbhd <- feols(ppsf ~ neighbourhoodDescription + duplex + neighbourhoodDescription:duplex + log(age + 1) | year(conveyanceDate) , data = dtBCA2019[hasLaneway==FALSE & year(conveyanceDate) >= MINYEAR])

m_sqftNbhd <- feols(ppsf ~ neighbourhoodDescription + log(MB_total_finished_area) + neighbourhoodDescription:log(MB_total_finished_area) + log(age + 1) | year(conveyanceDate) , data = dtBCA2019[hasLaneway==FALSE & year(conveyanceDate) >= MINYEAR])
print(etable(m_dwellingNbhd,m_sqftNbhd))

print(dtBCA2019[ , mean(ppsf), by = .(neighbourhoodDescription, duplex) ])

print(dtBCA2019[,median(MB_total_finished_area),by=.(duplex)])

print(table(dtBCA2019[,.(zoning)]))

q("no")
# Classify dwelling types
dtMerge[, dwelling_type := fcase(
  grepl("Duplex", ACTUAL_USE_DESCRIPTION, ignore.case = TRUE), "duplex",
  grepl("Single Family Dwelling", ACTUAL_USE_DESCRIPTION, ignore.case = TRUE), "single_family",
  ACTUAL_USE_DESCRIPTION == "Residential Dwelling with Suite", "single_family",
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
    crosswalk <- dtBCA2019[, .N, by = .(cleanPostal, geo_id = neighbourhoodDescription)]
    crosswalk <- crosswalk[order(cleanPostal, -N)][, .(geo_id = geo_id[1]), by = cleanPostal]
} else if (GEO_LEVEL == "census_tract") {
    crosswalk <- readRDS(PCCF_RDS)[, .(cleanPostal = postalCode, geo_id = censusTract)]
} else {
    crosswalk <- dtBCA2019[, .(geo_id = str_sub(cleanPostal, 1, 4)), by = cleanPostal]
}

# Merge data to geography
dtBCAGeo <- merge(dtBCA2019, crosswalk, by = "cleanPostal")
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

# Curiosity: duplex by neighbourhood
dtBCAGeo[,duplex:= ifelse(dwelling_type=="duplex",1,0)]
m_duplex <- feols(ppsf ~ log(age+1) + log(land_area) + i(geo_id,duplex) | geo_id + duplex,data=dtBCAGeo)

dtSlopes <- as.data.table(broom::tidy(m_slopes))[grepl("MB_total_finished_area", term)]
dtSlopes[, geo_id := gsub("geo_id::(.*):MB_total_finished_area", "\\1", term)]
dtDuplexCoef <- as.data.table(broom::tidy(m_duplex))[grepl("duplex", term)]
dtDuplexCoef[, geo_id := gsub("geo_id::(.*):duplex", "\\1", term)]
dtDuplexSlopes <- merge(dtSlopes, dtDuplexCoef[, .(geo_id, duplex_coef = estimate)], by = "geo_id", all.x = TRUE)
print(cor(dtDuplexSlopes$estimate, dtDuplexSlopes$duplex_coef, use = "complete.obs"))
ggplot(dtDuplexSlopes, aes(x=estimate,y=duplex_coef)) + geom_point() + geom_smooth(method="lm")
ggsave("text/duplexCoefSlope.png", width=8,height=6)
q("no")

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
