# bcaSalesAll.R
# R to evaluate elasticity of price per square foot among *all* BCA sales by census tract
# Tom Davidoff

# 01/26/26
library(data.table)
library(ggplot2)
library(sf)
library(RSQLite)
library(fixest)
library(scales)
library(stringr)
library(readxl)

BCA19    <- "~/OneDrive - UBC/dataRaw/REVD19_and_inventory_extracts.sqlite3"
BCA26 <- "/Volumes/T7Office/bigFiles/bca_folios.gpkg"

# use 2026 data to get floor(roll number/1000) to get lot polygons -> centroid , maybe zoning 
fRollLatLon <- "~/OneDrive - UBC/dataProcessed/bca26RollCensusTractAll.rds"
fLaneway <-  "~/OneDrive - UBC/dataRaw/20231231_UBC_CustomLanewayReport.xlsx"

if (!file.exists(fRollLatLon)) {
  cat("Extracting lot centroids from BCA 2026 data...\n")
  desc26 <- st_read(BCA26,
      layer="WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
      query = "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV WHERE JURISDICTION_CODE=='200' ", 
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
fSales <- "~/OneDrive - UBC/dataProcessed/bca19SalesAll.rds"
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

# merge back with census tract on roll number/1000
dtRollLatLon <- as.data.table(readRDS(fRollLatLon))
dtBCA2019[, roll_base := floor(as.numeric(roll_number) / 1000)]
dtRollLatLon[, roll_base := floor(as.numeric(ROLL_NUMBER) / 1000)]
# print variance of as.numeric CTUID within roll_base to check
dTest <- dtRollLatLon[, .(nCTUID = uniqueN(CTUID),nGeom=uniqueN(geom)), by = roll_base]
print(summary(dTest[, nCTUID]))
print(dtRollLatLon[, .(nCTUID = uniqueN(CTUID)), by = roll_base][nCTUID > 1])
q("no")
dtBCA2019 <- merge(dtBCA2019, dtRollLatLon[, .(roll_base, CTUID, DGUID, CTNAME)], by = "roll_base", all.x = TRUE)
print(table(is.na(dtBCA2019[, CTUID])))
print(table(dtBCA2019[is.na(CTUID), actualUseDescription]))
print(table(dtBCA2019[!is.na(CTUID), actualUseDescription]))


# Numeric conversion and cleaning
cols <- c("conveyancePrice", "MB_effective_year", "MB_total_finished_area","land_area","land_width","land_depth")
dtBCA2019[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
dtBCA2019 <- dtBCA2019[!is.na(MB_total_finished_area) & conveyancePrice > 100000]
dtBCA2019[, land_area_approximate := land_width * land_depth]
dtBCA2019 <- dtBCA2019[!is.na(land_depth) & !is.na(land_width)]

# Metrics
dtBCA2019[, `:=`(
    age = year(conveyanceDate) - MB_effective_year, 
    ppsf = conveyancePrice / MB_total_finished_area 
)]

# Outlier removal (global)
CUT <- 0.05
dtBCA2019 <- dtBCA2019[
    age >= 0 &
    MB_total_finished_area %between% quantile(MB_total_finished_area, c(CUT, 1-CUT)) &
    ppsf %between% quantile(ppsf, c(CUT, 1-CUT))
]

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
dtBCA2019 <- dtBCA2019[!neighbourhoodDescription %chin% c("SHAUGNESSY", "WEST END")]

# ==========================================
# Function to run analysis at a given geographic level
# ==========================================

runAnalysis <- function(dt, groupVar, label) {
  cat("\n==========================================\n")
  cat("Running analysis at", label, "level\n")
  cat("==========================================\n")
  
  # Compute median land_width within each group
  dt[, landWidthMedian := median(land_width, na.rm = TRUE), by = groupVar]
  
  # Filter to observations near median width for that group
  dtFiltered <- dt[
    land_width >= landWidthMedian * (1 - WIDTHCUT) & 
    land_width <= landWidthMedian * (1 + WIDTHCUT) &
    land_depth >= DEPTHMIN & 
    land_depth < DEPTHMAX
  ]
  
  # Analysis sample: newer builds , post-MINYEAR
  MAXAGE <- 15
  dtAnalysis <- dtFiltered[age<15 & year(conveyanceDate) >= MINYEAR]
  cat("Analysis sample size:", nrow(dtAnalysis), "\n")
 # Note can't have land measurements with all types 
  # Build formulas dynamically
  fmlaMain <- as.formula(paste0(
    "ppsf ~ i(", groupVar, ", MB_total_finished_area) + log(age + 1) + MB_total_finished_area | ", groupVar
  ))

  fmlaLog <- as.formula(paste0(
    "log(ppsf) ~ i(", groupVar, ", log(MB_total_finished_area)) + log(age + 1) + log(MB_total_finished_area)  | ", groupVar 
  ))# no main effect for Wald purposes
  
  fmlaLogRestricted <- as.formula(paste0(
  "log(ppsf) ~ log(MB_total_finished_area) + log(age + 1)  | ", groupVar
))

  # Run regressions
  mainReg <- feols(fmlaMain, data = dtAnalysis)
  print(r2(mainReg, typ = c("r2", "ar2")))
  
  logReg <- feols(fmlaLog, data = dtAnalysis)
  print(r2(logReg, typ = c("r2", "ar2")))

  logRegRestricted <- feols(fmlaLogRestricted, data = dtAnalysis)
  print(r2(logRegRestricted,typ=c("r2","ar2")))

  # Extract slopes
  dtSlopes <- as.data.table(coeftable(mainReg), keep.rownames = "term")[grepl("MB_total_finished_area", term)]
  # Extract group identifier from term
  pattern <- paste0(groupVar, "::(.*):MB_total_finished_area")
  dtSlopes[, (groupVar) := gsub(pattern, "\\1", term)]
  
  # Extract elasticities
  dtElasticities <- as.data.table(coeftable(logReg), keep.rownames = "term")[grepl("log\\(MB_total_finished_area\\)", term)]
  patternLog <- paste0(groupVar, "::(.*):log\\(MB_total_finished_area\\)")
  dtElasticities[, (groupVar) := gsub(patternLog, "\\1", term)]
  
  # Compute means
  dtMeans <- dtFiltered[hasLaneway == FALSE & year(conveyanceDate) >= MEANYEAR, 
                        .(meanPPSF = mean(ppsf, na.rm = TRUE), nobs = .N), 
                        by = groupVar]
  
  # Merge slopes and elasticities
  dtMeans <- merge(dtMeans, dtSlopes[, c(groupVar, "Estimate"), with = FALSE], by = groupVar, all.x = TRUE)
  setnames(dtMeans, "Estimate", "slope")
  dtMeans <- merge(dtMeans, dtElasticities[, c(groupVar, "Estimate"), with = FALSE], by = groupVar, all.x = TRUE)
  setnames(dtMeans, "Estimate", "elasticity")
  
  # Output
  fwrite(dtMeans, paste0("tables/bca19_mean_ppsf_slope_All_by_", label, ".csv"))
  
  # Plot
  p <- ggplot(dtMeans[nobs > quantile(nobs, .025) & !is.na(slope)], aes(x = slope, y = meanPPSF)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    labs(title = paste("Mean BCA Price per SqFt vs Pricing Slope by", label), 
         x = "BCA Pricing Slope ($/sqft per sqft)", 
         y = "Mean Price per SqFt")
  ggsave(paste0("text/bca19_mean_ppsf_vs_slope_", label, ".png"), plot = p)
  
  # Correlations
  cat("\nCorrelations (", label, "):\n", sep = "")
  print(cor(dtMeans[!is.na(slope), .(slope, meanPPSF)]))
  print(cor(dtMeans[!is.na(elasticity), .(elasticity, meanPPSF)]))
  print(cor(dtMeans[!is.na(slope) & nobs > quantile(nobs, .025), .(slope, meanPPSF)]))
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

q("no")


