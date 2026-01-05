# reconcileLanewayFSR.R
# Triangulate laneway presence via permits AND roll number matching
# Tom Davidoff, 12/29/25

library(data.table)
library(ggplot2)
library(readxl)
library(stringr)
library(fixest)

clean_pc <- function(x) gsub(" ", "", toupper(trimws(x)))

# ==========================================
# PATHS
# ==========================================
PERMIT_F <- "data/raw/vancouver_permits_full.csv"
LANEWAY_F <- "data/raw/20231231_UBC_CustomLanewayReport.xlsx"

# ==========================================
# 1. LOAD PERMIT DATA
# ==========================================
dtPermit <- fread(PERMIT_F)
setnames(dtPermit, make.names(names(dtPermit)))

dtPermit <- dtPermit[as.IDate(issuedate) >= "2019-01-01"]
dtPermit[, cleanPostal := clean_pc(str_sub(address, -7))]

# ==========================================
# 2. FLAG LANEWAY FROM PERMIT DESCRIPTIONS
# ==========================================
dtPermit[, laneway_from_permit := grepl(
  "laneway|accessory dwelling|adu|coach house|carriage house|secondary suite",
  paste(projectdescription, specificusecategory, typeofwork),
  ignore.case = TRUE
)]

addr_has_laneway_permit <- dtPermit[laneway_from_permit == TRUE,
                                    .(has_laneway_permit = TRUE),
                                    by = cleanPostal]

# ==========================================
# 3. FLAG LANEWAY FROM ROLL NUMBER (2025 BCA DATA)
# ==========================================
dtLaneway <- as.data.table(read_excel(LANEWAY_F, sheet = "DATA"))
setnames(dtLaneway, "ROLL_NUM", "Roll_Number_char")
dtLaneway[, roll_base := floor(as.numeric(Roll_Number_char) / 1000)]

# Load 2025 BCA data (from your earlier code)
dtDesc <- fread(
  "~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",
  select = c("FOLIO_ID", "ROLL_NUMBER", "ACTUAL_USE_DESCRIPTION", "LAND_SIZE")
)
dtDesc[, Roll_Number := as.numeric(ROLL_NUMBER)]

dtRoll <- fread(
  "~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt",
  select = c("Roll_Number")
)

# Get address file for postal codes
dtAddr <- fread(
  "~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_addresses_20250331_REVD25.csv",
  select = c("FOLIO_ID", "POSTAL_CODE")
)

# Merge to get postal → roll_number
dtAddrMerge <- merge(dtAddr, dtDesc[, .(FOLIO_ID, Roll_Number)], by = "FOLIO_ID")
dtAddrMerge[, `:=`(
  cleanPostal = clean_pc(POSTAL_CODE),
  roll_base = floor(Roll_Number / 1000)
)]

# Flag addresses with laneways
addr_has_laneway_roll <- dtAddrMerge[roll_base %in% dtLaneway$roll_base,
                                     .(has_laneway_roll = TRUE),
                                     by = cleanPostal]

# ==========================================
# 4. MERGE & ANALYZE
# ==========================================
dtPermit <- merge(dtPermit, addr_has_laneway_permit, by = "cleanPostal", all.x = TRUE)
dtPermit <- merge(dtPermit, addr_has_laneway_roll, by = "cleanPostal", all.x = TRUE)

dtPermit[is.na(has_laneway_permit), has_laneway_permit := FALSE]
dtPermit[is.na(has_laneway_roll), has_laneway_roll := FALSE]
dtPermit[, has_laneway := has_laneway_permit | has_laneway_roll]

print("=== Laneway Detection Agreement ===")
print(dtPermit[, .N, by = .(has_laneway_permit, has_laneway_roll,year(issuedate))])

# Focus on main house permits
dtMain <- dtPermit[!laneway_from_permit &
                   grepl("Single|Dwelling", specificusecategory, ignore.case = TRUE)]

print("=== Main House Permits: With vs Without Laneway ===")
dtMain[, .(
  n_permits = .N,
  mean_value = mean(projectvalue, na.rm = TRUE),
  median_value = median(projectvalue, na.rm = TRUE)
), by = has_laneway]

# Regression
m1 <- feols(log(projectvalue) ~ has_laneway, data = dtMain[projectvalue > 0])
m2 <- feols(log(projectvalue) ~ has_laneway + factor(year(issuedate)),
            data = dtMain[projectvalue > 0])

etable(m1, m2)

# Plot
ggplot(dtMain[projectvalue < 2e6], aes(x = projectvalue, fill = has_laneway)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Main House Permit Values: With vs Without Laneway",
    x = "Project Value"
  ) +
  theme_minimal()

ggsave("text/main_house_value_distribution_laneway.png", width = 8, height = 6)
