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
library(geosphere)

# per google
latDowntown <- 49.2827
lonDowntown <- -123.1207

centroidsFile <- "~/OneDrive - UBC/dataProcessed/bca25Centroids.rds"
print(centroidsFile)
print(file.exists(centroidsFile))
if (!file.exists(centroidsFile)) {
  source("scripts/getBCAVancouverCentroids.R")
}
dtCentroids <- readRDS(centroidsFile)
coords <- st_coordinates(dtCentroids$centroid)
# add to data.table
dtCentroids[, `:=`(
  lon = coords[, 1],
  lat = coords[, 2]
)]

dtInventory <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt",select=c("Roll_Number","MB_Total_Finished_Area","Zoning","Land_Width_Width","Land_Depth_Depth","MB_Effective_Year"),colClasses=c(Roll_Number="character",MB_Total_Finished_Area="numeric",Zoning="character",Land_Width_Width="numeric",Land_Depth_Depth="numeric"))
print(head(dtInventory))
dtSales <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",select=c("ROLL_NUMBER","FOLIO_ID","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION"))
dtSales <- dtSales[CONVEYANCE_TYPE_DESCRIPTION=="Improved Single Property Transaction"]
dtSales[,saleYear:=as.numeric(substring(CONVEYANCE_DATE,1,4))]
print(head(dtSales))
MINYEAR <- 2014
MAXYEAR <- 2018
dtSales <- dtSales[saleYear>=MINYEAR & saleYear<=MAXYEAR]
dtDescription <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("FOLIO_ID","ROLL_NUMBER","ACTUAL_USE_DESCRIPTION","NEIGHBOURHOOD","JURISDICTION_CODE"))
dtDescription <- dtDescription[JURISDICTION_CODE==200]
print(head(dtDescription))
print(nrow(dtDescription))
print(head(dtCentroids))
dtMerge <- merge(dtDescription,dtCentroids,by="ROLL_NUMBER")
print(nrow(dtMerge))
dtMerge <- merge(dtMerge,dtInventory,by.x="ROLL_NUMBER",by.y="Roll_Number")
print("postInventory")
print(nrow(dtMerge))
dtMerge <- merge(dtMerge,dtSales,by="ROLL_NUMBER")
print(nrow(dtSales))
print(nrow(dtMerge))
# order by frequency
print(table(dtMerge[,ACTUAL_USE_DESCRIPTION])[order(table(dtMerge[,ACTUAL_USE_DESCRIPTION]),decreasing=TRUE)])
dtMerge <- dtMerge[ACTUAL_USE_DESCRIPTION %in% c("Strata-Lot Residence (Condominium)", "Residential Dwelling with Suite", "Single Family Dwelling", "Row Housing (Single Unit Ownership)") | grepl("Duplex",ACTUAL_USE_DESCRIPTION)]
# first question: among single family homes are neighbourhoods relevant?
dtMerge[,age:=saleYear-MB_Effective_Year]

dtMerge[,distDowntown:=geodist(cbind(lon,lat),c(lonDowntown,latDowntown))]
regValue <- feols(log(CONVEYANCE_PRICE) ~ log(MB_Total_Finished_Area) + log(age+1) +Latidtude,data=dtMerge )
print(head(dtMerge))
stop()
ddfd


BCA19 <- "~/OneDrive - UBC/dataRaw/REVD19_and_inventory_extracts.sqlite3"
#BCA26 <- "/Volumes/T7Office/bigFiles/bca_folios.gpkg"

# use 2026 data to get floor(roll number/1000) to get lot polygons -> centroid , maybe zoning
fRollLatLon <- "~/OneDrive - UBC/dataProcessed/bca26RollCensusTract.rds"
fLaneway <- "~/OneDrive - UBC/dataRaw/20231231_UBC_CustomLanewayReport.xlsx"

# drop duplex
if (!file.exists(fRollLatLon)) {
  cat("Extracting lot centroids from BCA 2026 data...\n")
  desc26 <- st_read(BCA26,
    layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
    query = "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV WHERE JURISDICTION_CODE=='200' AND ACTUAL_USE_DESCRIPTION IN ('Residential Dwelling with Suite','Single Family Dwelling')  > 0",
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
  "))
  dbDisconnect(db)
  saveRDS(dtBCA2019, fSales)
}
dtBCA2019 <- readRDS(fSales)
print(head(dtBCA2019))

# are duplexes everywhere?
dtBCA2019[,single:=actualUseDescription %chin% c('Residential Dwelling with Suite','Single Family Dwelling')]
dtBCA2019[, is_duplex := grepl("duplex", actualUseDescription, ignore.case = TRUE)]
print(dtBCA2019[,mean(single),by=neighbourhoodDescription])
dfdfdf
# merge back with census tract on roll number/1000
dtRollLatLon <- as.data.table(readRDS(fRollLatLon))
dtBCA2019[, roll_base := floor(as.numeric(roll_number) / 1000)]
dtRollLatLon[, roll_base := floor(as.numeric(ROLL_NUMBER) / 1000)]
dtBCA2019 <- merge(dtBCA2019, dtRollLatLon[, .(roll_base, CTUID, DGUID, CTNAME)], by = "roll_base", all.x = TRUE)
print(table(is.na(dtBCA2019[, CTUID])))
print(table(dtBCA2019[is.na(CTUID), actualUseDescription]))
print(table(dtBCA2019[!is.na(CTUID), actualUseDescription]))


# Numeric conversion and cleaning
cols <- c("conveyancePrice", "MB_effective_year", "MB_total_finished_area", "land_area", "land_width", "land_depth")
dtBCA2019[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
dtBCA2019[, land_area_approximate := land_width * land_depth]
dtBCA2019 <- dtBCA2019[!is.na(land_depth) & !is.na(land_width)]

# Metrics
dtBCA2019[, `:=`(
  age = year(conveyanceDate) - MB_effective_year,
  ppsf = conveyancePrice / MB_total_finished_area
)]

# Outlier removal (global)
CUT <- 0.025
dtBCA2019 <- dtBCA2019[!is.na(MB_total_finished_area) & !is.na(ppsf)]

dtBCA2019 <- dtBCA2019[age >= 0 & MB_total_finished_area %between% quantile(MB_total_finished_area, c(CUT, 1 - CUT))]
dtBCA2019 <- dtBCA2019[!is.na(MB_total_finished_area) & conveyancePrice > quantile(conveyancePrice, CUT) &
  ppsf %between% quantile(ppsf, c(CUT, 1 - CUT))]

cat("BCA 2019 transactions loaded:", nrow(dtBCA2019), "\n")

# ==========================================
# Exclude from the sales homes with laneways
# NO, don't because this is about MB slope
# ==========================================

dtLaneway <- as.data.table(read_excel(fLaneway, sheet = "DATA"))
setnames(dtLaneway, "ROLL_NUM", "Roll_Number_char")
dtLaneway[, `:=`(
  Roll_Number = as.numeric(Roll_Number_char),
  roll_base = floor(as.numeric(Roll_Number_char) / 1000)
)]

dtBCA2019 <- merge(dtBCA2019, dtLaneway[, .(roll_base, lanewayBuilt = YEAR_BUILT)], by = "roll_base", all.x = TRUE)
dtBCA2019[, hasLaneway := !is.na(lanewayBuilt) & (year(conveyanceDate) >= lanewayBuilt)]

MINYEAR <- 2012
MEANYEAR <- 2015
WIDTHCUT <- 0.15
DEPTHMIN <- 110
DEPTHMAX <- 150

dtBCA2019 <- dtBCA2019[grepl("RS", zoning)]
# Exclude Shaughnessy and West End
dtBCA2019 <- dtBCA2019[!neighbourhoodDescription %chin% c("SHAUGNESSY", "WEST END", "SOUTHLANDS")]

# ==========================================
# Function to run analysis at a given geographic level
# ==========================================

runAnalysis <- function(dt, groupVar, label) {
  cat("\n==========================================\n")
  cat("Running analysis at", label, "level\n")
  cat("==========================================\n")

  # Compute median land_width within each group
  dt[, landWidthMedian := median(land_width, na.rm = TRUE), by = groupVar]
  dt[, saleYear := year(conveyanceDate)]

  # Filter to observations near median width for that group
  dtFiltered <- dt[
    land_width >= landWidthMedian * (1 - WIDTHCUT) &
      land_width <= landWidthMedian * (1 + WIDTHCUT) &
      land_depth >= DEPTHMIN &
      land_depth < DEPTHMAX
  ]

  # Analysis sample: newer builds , post-MINYEAR
  MAXAGE <- 15
  dtAnalysis <- dtFiltered[age < 15 & year(conveyanceDate) >= MINYEAR]
  dtAnalysis[,landArea:=land_width*land_depth]
  print(head(dtAnalysis))
  cat("Analysis sample size:", nrow(dtAnalysis), "\n")

  # Build formulas dynamically

  fmlaLog <- as.formula(paste0(
    "log(ppsf) ~ 0 + i(", groupVar, ", log(MB_total_finished_area)) + log(age + 1) + log(landArea) | saleYear + ", groupVar
  )) # no main effect for Wald purposes

  # Run regressions
  print(fmlaLog)
  print(head(dtAnalysis))
  logReg <- feols(fmlaLog, data = dtAnalysis)
  print(r2(logReg, typ = c("r2", "ar2")))

  # Extract slopes
  # Extract elasticities
  dtElasticities <- as.data.table(coeftable(logReg), keep.rownames = "term")[grepl("log\\(MB_total_finished_area\\)", term)]
  patternLog <- paste0(groupVar, "::(.*):log\\(MB_total_finished_area\\)")
  dtElasticities[, (groupVar) := gsub(patternLog, "\\1", term)]

  # Compute means, adjust for CPI
  dtCPI <- fread("~/OneDrive - UBC/dataRaw/1810000501_databaseLoadingData.csv", select = c("REF_DATE", "VALUE", "Products and product groups"))
  dtCPI <- dtCPI[`Products and product groups` == "All-items"]
  setnames(dtCPI, c("REF_DATE", "VALUE"), c("year", "CPI"))
  dtCPI[, CPI := CPI / CPI[year == 2025]]
  dtFiltered <- merge(dtFiltered, dtCPI, by.x = "saleYear", by.y = "year", all.x = TRUE)
  print(head(dtFiltered))
  print(head(dtCPI))

  dtMeans <- dtFiltered[hasLaneway == FALSE & year(conveyanceDate) >= MEANYEAR,
    .(meanPPSF = mean(ppsf / CPI, na.rm = TRUE), nobs = .N),
    by = groupVar
  ]

  # Merge slopes and elasticities
  dtMeans <- merge(dtMeans, dtElasticities[, c(groupVar, "Estimate"), with = FALSE], by = groupVar, all.x = TRUE)
  setnames(dtMeans, "Estimate", "elasticity")

  # Output
  fwrite(dtMeans, paste0("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_", label, ".csv"))

  # Plot
  p <- ggplot(dtMeans[nobs > quantile(nobs, .025) & !is.na(elasticity)], aes(x = elasticity, y = meanPPSF)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(
      title = paste("Mean BCA Price per SqFt vs Pricing Slope by", label),
      x = "BCA Pricing elasticity ($/sqft per sqft)",
      y = "Mean Price per SqFt"
    )
  ggsave(paste0("text/bca19_mean_ppsf_vs_elasticity_", label, ".png"), plot = p)

  # Correlations
  cat("\nCorrelations (", label, "):\n", sep = "")
  print(cor(dtMeans[!is.na(elasticity), .(elasticity, meanPPSF)]))
  print(cor(dtMeans[!is.na(elasticity) & nobs > quantile(nobs, .05), .(elasticity, meanPPSF)]))
  print(cor(dtMeans[!is.na(elasticity) & nobs > quantile(nobs, .1), .(elasticity, meanPPSF)]))
  print(summary(dtMeans[!is.na(elasticity) & nobs > quantile(nobs, .025), .(elasticity, meanPPSF)]))
  print(summary(dtMeans[!is.na(elasticity) & nobs > quantile(nobs, .05), .(elasticity, meanPPSF)]))
  print(summary(dtMeans[!is.na(elasticity) & nobs > quantile(nobs, .1), .(elasticity, meanPPSF)]))

  return(dtMeans)
}

# ==========================================
# Run analyses
# ==========================================

dtResultsTract <- runAnalysis(copy(dtBCA2019), "CTUID", "tract")
dtResultsNbhd <- runAnalysis(copy(dtBCA2019), "neighbourhoodDescription", "neighbourhood")


# ++++++++++++++++++++++++++++++++++++++++++
# Now check quality with 2025 sales out of sample
# ==========================================
#
dtSales <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD24_20240331/bca_folio_sales_20240331_REVD24.csv", select = c("ROLL_NUMBER", "CONVEYANCE_DATE", "CONVEYANCE_PRICE"))
dtDescription <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD24_20240331/bca_folio_descriptions_20240331_REVD24.csv", select = c("ROLL_NUMBER", "ACTUAL_USE_DESCRIPTION", "NEIGHBOURHOOD", "JURISDICTION"))
dtDescription <- dtDescription[ACTUAL_USE_DESCRIPTION %chin% c("Single Family Dwelling", "Residential Dwelling with Suite") & JURISDICTION == "City of Vancouver"]
dtSales <- merge(dtSales, dtDescription, by = "ROLL_NUMBER")
print(head(dtSales))
dtInventory <- fread("~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt", select = c("Roll_Number", "MB_Effective_Year", "MB_Total_Finished_Area", "Land_Width_Width", "Land_Depth_Depth"), colClasses = list(character = c("Roll_Number"), numeric = c("MB_Effective_Year", "MB_Total_Finished_Area", "Land_Width_Width", "Land_Depth_Depth")))
print(head(dtInventory))
dtSales[, neighbourhoodDescription := str_to_upper(NEIGHBOURHOOD)]

# replace " - " and " & " with "/"
dtSales[, neighbourhoodDescription := gsub(" - ", "/", neighbourhoodDescription)]
dtSales[, neighbourhoodDescription := gsub(" & ", "/", neighbourhoodDescription)]
dtSales[, year := substring(CONVEYANCE_DATE, 1, 4)]
dtSales <- merge(dtSales, dtInventory, by.x = "ROLL_NUMBER", by.y = "Roll_Number")
dtSales <- merge(dtSales, dtRollLatLon[, .(ROLL_NUMBER, CTNAME)], by = "ROLL_NUMBER")
dtPriceND <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_neighbourhood.csv")
setnames(dtPriceND, old = c("neighbourhoodDescription", "meanPPSF", "elasticity"), new = c("neighbourhoodDescription", "meanPPSFND", "elasticityND"))
dtSales <- merge(dtSales, dtPriceND, by = "neighbourhoodDescription")
dtSales[, age := as.numeric(saleYear) - MB_Effective_Year]
dtSales[, ppsf := log(CONVEYANCE_PRICE / MB_Total_Finished_Area)]
dtPriceCT <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_tract.csv", colClasses = list(character = c("CTUID"), numeric = c("meanPPSF", "elasticity")))
setnames(dtPriceCT, old = c("meanPPSF", "elasticity"), new = c("meanPPSFCT", "elasticityCT"))
dtPriceCT[, CTNAME := substring(CTUID, 4, 10)]
dtSales <- merge(dtSales, dtPriceCT, by = "CTNAME")
dtSales[, medWidth := median(Land_Width_Width), by = CTNAME]
dtSales <- dtSales[Land_Depth_Depth %in% c(DEPTHMIN, DEPTHMAX)]
dtSales[, logLandArea := log(Land_Width_Width * Land_Depth_Depth)]
dtSales[, logAge := log(age + 1)]
rA <- feols(ppsf ~ 0 + log(MB_Total_Finished_Area) * elasticityND + logAge + logLandArea | CTNAME + year, cluster = "neighbourhoodDescription", data = dtSales)
rB <- feols(ppsf ~ 0 + log(MB_Total_Finished_Area) * elasticityCT + logAge + logLandArea | CTNAME + year, cluster = "neighbourhoodDescription", data = dtSales)
etable(rA, rB, file = "text/outSample.tex", digits = 3, tex = TRUE, replace = TRUE)
