# getVancouverBuildings.R
# Compare FSR for single family with/without laneways
# Tom Davidoff, 12/28/25

library(data.table)
library(ggplot2)
library(readxl)

# ========================================
# 1. LOAD & MERGE DATA
# ========================================
dtDesc <- fread(
  "~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",
  select = c("FOLIO_ID", "ROLL_NUMBER", "ACTUAL_USE_DESCRIPTION", "LAND_SIZE")
)
dtDesc[, Roll_Number := as.numeric(ROLL_NUMBER)]

dtRoll <- fread(
  "~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt",
  select = c("Roll_Number", "MB_Year_Built", "MB_Total_Finished_Area", "Land_Area_Total", "Zoning")
)
dtRoll <- dtRoll[MB_Year_Built > 2018]

dtMerge <- merge(dtDesc, dtRoll, by = "Roll_Number")

# ========================================
# 2. CLASSIFY DWELLING TYPES
# ========================================
dtMerge[, dwelling_type := fcase(
  grepl("Duplex", ACTUAL_USE_DESCRIPTION, ignore.case = TRUE), "duplex",
  grepl("Single Family Dwelling", ACTUAL_USE_DESCRIPTION, ignore.case = TRUE), "single_family",
  ACTUAL_USE_DESCRIPTION == "Residential Dwelling with Suite", "suite",
  default = "other"
)]

# ========================================
# 3. UNIT-LEVEL DATA PREP
# ========================================
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

# ========================================
# 4. AGGREGATE TO LOT LEVEL
# ========================================
dt_lot <- dt[, .(
  dwelling_type = first(dwelling_type),
  land_sqft = first(land_sqft),
  total_floor_area = sum(MB_Total_Finished_Area),
  n_units = .N,
  year_built = max(MB_Year_Built)
), by = roll_base]

dt_lot[, fsr := total_floor_area / land_sqft]
dt_lot_clean <- dt_lot[land_sqft <= 10000 & fsr < 2]

# ========================================
# 5. LOAD LANEWAY DATA
# ========================================
dtNew <- as.data.table(read_excel("data/raw/20231231_UBC_CustomLanewayReport.xlsx", sheet = "DATA"))
setnames(dtNew, "ROLL_NUM", "Roll_Number_char")
dtNew[, `:=`(
  Roll_Number = as.numeric(Roll_Number_char),
  roll_base = floor(as.numeric(Roll_Number_char) / 1000)
)]

# ========================================
# 6. SINGLE FAMILY: WITH VS WITHOUT LANEWAY
# ========================================
# Filter to single family only
dt_sf <- dt_lot_clean[dwelling_type == "single_family"]

# Mark which lots have laneways
dt_sf[, has_laneway := roll_base %in% dtNew$roll_base]

# Summary comparison
print("=== Single Family: With vs Without Laneway ===")
dt_sf[, .(
  n_lots = .N,
  mean_fsr = mean(fsr),
  median_fsr = median(fsr),
  mean_land = mean(land_sqft)
), by = has_laneway]

# Regression: FSR controlling for lot size
m_laneway <- lm(fsr ~ has_laneway + land_sqft, data = dt_sf)
print(summary(m_laneway))

# Plot
ggplot(dt_sf, aes(x = land_sqft, y = fsr, color = has_laneway)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Single Family FSR: With vs Without Laneway (2019+)",
    x = "Lot Size (sqft)", y = "FSR (BC Assessment only, excludes laneway sqft)"
  ) +
  theme_minimal()

ggsave("text/sf_fsr_laneway_comparison.png", width = 8, height = 6)
