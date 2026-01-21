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
  sCT <- st_make_valid(sCT)
  sCT <- st_transform(sCT, stDesc)

  roll_ct <- st_join(roll_pt, sCT[, c("CTUID", "DGUID", "CTNAME", "PRUID")], join = st_within, left = TRUE)
  print(head(roll_ct))
  saveRDS(roll_ct, fRollLatLon)
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
  saveRDS(dtBCA2019, fSales)
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
print(dtBCA2019[,.(land_area,land_width,land_depth)])
cols <- c("conveyancePrice", "MB_effective_year", "MB_total_finished_area","land_area","land_width","land_depth")
dtBCA2019[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
dtBCA2019 <- dtBCA2019[!is.na(MB_total_finished_area) & conveyancePrice > 100000]
dtBCA2019[,land_area_approximate:=land_width*land_depth]

# Metrics & Outliers
CUT <- 0.05

print(names(dtBCA2019))
print(head(dtBCA2019))
dtBCA2019[, `:=`(
    age = year(conveyanceDate) - MB_effective_year, 
    ppsf = conveyancePrice / MB_total_finished_area, 
    dwelling_type = fifelse(grepl("uplex", actualUseDescription, ignore.case = TRUE), "duplex", "single")
)]

dtBCA2019 <- dtBCA2019[
    age >= 0 &
    MB_total_finished_area %between% quantile(MB_total_finished_area, c(CUT, 1-CUT)) &
    ppsf %between% quantile(ppsf, c(CUT, 1-CUT))
]

cat("BCA 2019 transactions loaded:", nrow(dtBCA2019), "\n")
cat("  Single:", dtBCA2019[dwelling_type == "single", .N], "\n")
cat("  Duplex:", dtBCA2019[dwelling_type == "duplex", .N], "\n")

# ==========================================
# Exclude from the sales homes with laneways
# ==========================================

dtLaneway <- as.data.table(read_excel(fLaneway, sheet = "DATA"))
setnames(dtLaneway, "ROLL_NUM", "Roll_Number_char")
dtLaneway[, `:=`(
  Roll_Number = as.numeric(Roll_Number_char),
  roll_base = floor(as.numeric(Roll_Number_char) / 1000)
)]
print(head(dtLaneway))

dtBCA2019 <- merge(dtBCA2019, dtLaneway[, .(roll_base, lanewayBuilt = YEAR_BUILT)], by = "roll_base", all.x = TRUE)
dtBCA2019[, hasLaneway := !is.na(lanewayBuilt) & (year(conveyanceDate) >= lanewayBuilt)]
print(names(dtBCA2019))
print(head(dtBCA2019))

MINYEAR <- 2012

dtBCA2019[, duplex := fifelse(dwelling_type == "duplex", 1L, 0L)]
dtBCA2019[, nDuplex := sum(duplex), by = .(CTUID)]
dtBCA2019 <- dtBCA2019[grepl("RS", zoning)]
# Exclude Shaughnessy and West End
dtBCA2019 <- dtBCA2019[!neighbourhoodDescription %chin% c("SHAUGNESSY", "WEST END")]

print(dtBCA2019[year(conveyanceDate) > MINYEAR, quantile(ppsf), by = year(conveyanceDate)])

# Analysis sample: no laneways, post-MINYEAR
dtAnalysis <- dtBCA2019[hasLaneway == FALSE & year(conveyanceDate) >= MINYEAR]

m_dwelling <- feols(ppsf ~ duplex + log(age + 1) | CTUID, data = dtAnalysis)
m_sf <- feols(ppsf ~ log(age + 1) + MB_total_finished_area | CTUID, data = dtAnalysis[nDuplex > 20])
print(etable(m_dwelling, m_sf))

m_dwellingNbhd <- feols(ppsf ~ neighbourhoodDescription + duplex + neighbourhoodDescription:duplex + log(age + 1) | year(conveyanceDate), data = dtAnalysis)
m_sqftNbhd <- feols(ppsf ~ neighbourhoodDescription + log(MB_total_finished_area) + neighbourhoodDescription:log(MB_total_finished_area) + log(age + 1) | year(conveyanceDate), data = dtAnalysis)
print(etable(m_dwellingNbhd, m_sqftNbhd))

print(dtBCA2019[, mean(ppsf), by = .(neighbourhoodDescription, duplex)])
print(dtBCA2019[, median(MB_total_finished_area), by = .(duplex)])
print(table(dtBCA2019[, .(zoning)]))

# ==========================================
# Census tract slopes and means
# ==========================================

mainRegTract <- feols(ppsf ~ i(CTUID, MB_total_finished_area) + log(age + 1) + MB_total_finished_area | CTUID + duplex, data = dtAnalysis)
print(summary(mainRegTract))


dtSlopesTract <- as.data.table(coeftable(mainRegTract), keep.rownames = "term")[grepl("MB_total_finished_area", term)]
dtSlopesTract[, CTUID := gsub("CTUID::(.*):MB_total_finished_area", "\\1", term)]
print(head(dtSlopesTract))

MEANYEAR <- 2015
dtMeansTract <- dtBCA2019[hasLaneway == FALSE & year(conveyanceDate) >= MEANYEAR, 
                          .(meanPPSF = mean(ppsf, na.rm = TRUE), nobs = .N), 
                          by = CTUID]
dtMeansTract <- merge(dtMeansTract, dtSlopesTract[, .(CTUID, slope = Estimate)], by = "CTUID", all.x = TRUE)
print(head(dtMeansTract))
fwrite(dtMeansTract, "tables/bca19_mean_ppsf_slope_by_tract.csv")

ggplot(dtMeansTract[nobs > quantile(nobs, .025) & !is.na(slope)], aes(x = slope, y = meanPPSF)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Mean BCA Price per SqFt vs Pricing Slope by Census Tract", 
       x = "BCA Pricing Slope ($/sqft per sqft)", 
       y = "Mean Price per SqFt")
ggsave("text/bca19_mean_ppsf_vs_slope_tract.png")

cat("\nCorrelations (tract):\n")
print(cor(dtMeansTract[!is.na(slope), .(slope, meanPPSF)]))
print(cor(dtMeansTract[!is.na(slope) & nobs > quantile(nobs, .025), .(slope, meanPPSF)]))
print(cor(dtMeansTract[!is.na(slope) & nobs > quantile(nobs, .05), .(slope, meanPPSF)]))

# ==========================================
# Neighbourhood slopes and means
# Note much higher correlation slope,mean indicates better measure
# ==========================================

mainRegNbhd <- feols(ppsf ~ i(neighbourhoodDescription, MB_total_finished_area) + log(age + 1) + MB_total_finished_area | neighbourhoodDescription + duplex, data = dtAnalysis)
print(summary(mainRegNbhd))

dtSlopesNbhd <- as.data.table(coeftable(mainRegNbhd), keep.rownames = "term")[grepl("MB_total_finished_area", term)]
dtSlopesNbhd[, neighbourhoodDescription := gsub("neighbourhoodDescription::(.*):MB_total_finished_area", "\\1", term)]
print(head(dtSlopesNbhd))

dtMeansNbhd <- dtBCA2019[hasLaneway == FALSE & year(conveyanceDate) >= MEANYEAR, 
                         .(meanPPSF = mean(ppsf, na.rm = TRUE), nobs = .N), 
                         by = neighbourhoodDescription]
dtMeansNbhd <- merge(dtMeansNbhd, dtSlopesNbhd[, .(neighbourhoodDescription, slope = Estimate)], by = "neighbourhoodDescription", all.x = TRUE)
print(head(dtMeansNbhd))
fwrite(dtMeansNbhd, "tables/bca19_mean_ppsf_slope_by_neighbourhood.csv")

ggplot(dtMeansNbhd[nobs > quantile(nobs, .025) & !is.na(slope)], aes(x = slope, y = meanPPSF)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Mean BCA Price per SqFt vs Pricing Slope by Neighbourhood", 
       x = "BCA Pricing Slope ($/sqft per sqft)", 
       y = "Mean Price per SqFt")
ggsave("text/bca19_mean_ppsf_vs_slope_neighbourhood.png")

# ============
# Do neighbourhood regression but exclude duplexes, limit to newer homes, and include land area
# ============

print(summary(dtAnalysis))
print(table(dtAnalysis[,.(is.na(land_area_approximate),duplex)]))
dtLimit <- dtAnalysis[duplex==0  & !is.na(land_area_approximate) & land_area_approximate>0 & age<quantile(age,.75)]
print(summary(dtLimit))
limitRegNbhd <- feols(ppsf ~ i(neighbourhoodDescription, MB_total_finished_area) + log(age + 1) + log(land_area_approximate) + MB_total_finished_area | neighbourhoodDescription + duplex, data = dtLimit)
print(summary(limitRegNbhd))

dtSlopesNbhdLimit <- as.data.table(coeftable(limitRegNbhd), keep.rownames = "term")[grepl("MB_total_finished_area", term)]
dtSlopesNbhdLimit[, neighbourhoodDescription := gsub("neighbourhoodDescription::(.*):MB_total_finished_area", "\\1", term)]
print(head(dtSlopesNbhdLimit))

dtMeansNbhdLimit <- dtLimit[hasLaneway == FALSE & year(conveyanceDate) >= MEANYEAR, 
                         .(meanPPSF = mean(ppsf, na.rm = TRUE), nobs = .N), 
                         by = neighbourhoodDescription]
dtMeansNbhdLimit <- merge(dtMeansNbhdLimit, dtSlopesNbhdLimit[, .(neighbourhoodDescription, slope = Estimate)], by = "neighbourhoodDescription", all.x = TRUE)
print(head(dtMeansNbhdLimit))
fwrite(dtMeansNbhdLimit, "tables/bca19_mean_ppsf_slope_by_neighbourhood_limit.csv")


cat("\nLimited Correlations (neighbourhood):\n")
print(cor(dtMeansNbhd[!is.na(slope), .(slope, meanPPSF)]))
print(cor(dtMeansNbhdLimit[!is.na(slope), .(slope, meanPPSF)]))

q("no")
