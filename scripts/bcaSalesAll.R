# bcaSalesAll.R
# R to evaluate elasticity of price per square foot among *all* BCA sales by neighbourhood description (not tract as need obs of all types...)
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

# use 2026 data to get floor(roll number/1000) to get lot polygons -> centroid , maybe zoning 

# Load BCA 2019 sales data (all residential)
fSales <- "~/OneDrive - UBC/dataProcessed/bca19SalesAll.rds"
if (!file.exists(fSales)) {
  cat("Extracting BCA 2019 sales data...\n")
  db <- dbConnect(SQLite(), BCA19)
  dtBCA2019 <- as.data.table(dbGetQuery(db, "
      SELECT s.folioID, s.conveyancePrice, s.conveyanceDate,
             i.MB_effective_year, i.MB_total_finished_area, i.roll_number, i.zoning,
             d.actualUseDescription, d.neighbourhoodDescription, 
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
dtBCA2019[, duplexTownhome:=(grepl("uplex", actualUseDescription, ignore.case=TRUE) | actualUseDescription=="Row Housing (Single Unit Ownership)")]
dtBCA2019[, single:=(actualUseDescription %in% c("Residential Dwelling with Suite","Single Family Dwelling"))]
print(summary(dtBCA2019))

print(table(dtBCA2019$actualUseDescription))

# Numeric conversion and cleaning
cols <- c("conveyancePrice", "MB_effective_year", "MB_total_finished_area")
dtBCA2019[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
dtBCA2019 <- dtBCA2019[!is.na(MB_total_finished_area) & conveyancePrice > 100000]
print(dtBCA2019[,mean(is.na(MB_total_finished_area)),by=single])
print(summary(dtBCA2019$MB_total_finished_area))
print(dtBCA2019[year(conveyanceDate)>=2015 & !is.na(MB_total_finished_area) ,.N,by=c("single","neighbourhoodDescription")])

# Metrics
dtBCA2019[, `:=`(
    age = year(conveyanceDate) - MB_effective_year, 
    ppsf = conveyancePrice / MB_total_finished_area 
)]

# Outlier removal (global)
CUT <- 0.01
dtBCA2019 <- dtBCA2019[
    age >= 0 &
    MB_total_finished_area %between% quantile(MB_total_finished_area, c(CUT, 1-CUT)) &
    ppsf %between% quantile(ppsf, c(CUT, 1-CUT))
]
print(dtBCA2019[year(conveyanceDate)>2014 & !is.na(MB_total_finished_area) ,.N,by=c("single","neighbourhoodDescription")])

cat("BCA 2019 transactions loaded:", nrow(dtBCA2019), "\n")

MINYEAR <- 2015
MEANYEAR <- 2012

dtBCA2019 <- dtBCA2019[!neighbourhoodDescription %chin% c("SHAUGNESSY", "WEST END","SOUTHLANDS")]

# ==========================================
# Function to run analysis at a given geographic level
# ==========================================

runAnalysis <- function(dt, groupVar, label) {
  cat("\n==========================================\n")
  cat("Running analysis at", label, "level\n")
  cat("==========================================\n")
  
dtAnalysis <- dt
  
  
  # Build formulas dynamically
  fmlaMain <- as.formula(paste0(
    "ppsf ~ i(", groupVar, ", MB_total_finished_area) + log(age + 1) + MB_total_finished_area | ", groupVar
  ))

  fmlaLog <- as.formula(paste0(
    "log(ppsf) ~ i(", groupVar, ", log(MB_total_finished_area)) + log(age + 1) + log(MB_total_finished_area)  | ", groupVar 
  ))# no main effect for Wald purposes
  

  # Run regressions
  mainReg <- feols(fmlaMain, data = dtAnalysis)
  print(r2(mainReg, typ = c("r2", "ar2")))
  
  logReg <- feols(fmlaLog, data = dtAnalysis)
  print(r2(logReg, typ = c("r2", "ar2")))

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
  dtMeans <- dtAnalysis[year(conveyanceDate) >= MEANYEAR, 
                        .(meanPPSF = mean(ppsf, na.rm = TRUE), nobs = .N), 
                        by = groupVar]
  dtMeanDuplex <- dtAnalysis[year(conveyanceDate) >= MEANYEAR & single==0, .(meanPPSF_duplex = mean(ppsf, na.rm = TRUE), nobs_duplex = .N), by = groupVar]
  dtMeanSingle <- dtAnalysis[year(conveyanceDate) >= MEANYEAR & single==1, .(meanPPSF_single = mean(ppsf, na.rm = TRUE), nobs_single = .N), by = groupVar]
  dtMeans <- merge(dtMeans, dtMeanDuplex, by = groupVar, all.x = TRUE)
  dtMeans <- merge(dtMeans, dtMeanSingle, by = groupVar, all.x = TRUE)
  print("incoming")
  
  # Merge slopes and elasticities
  dtMeans <- merge(dtMeans, dtSlopes[, c(groupVar, "Estimate"), with = FALSE], by = groupVar, all.x = TRUE)
  setnames(dtMeans, "Estimate", "slope")
  dtMeans <- merge(dtMeans, dtElasticities[, c(groupVar, "Estimate"), with = FALSE], by = groupVar, all.x = TRUE)
  setnames(dtMeans, "Estimate", "elasticity")
  print(dtAnalysis[year(conveyanceDate) >= MEANYEAR & duplexTownhome==1, .N, by = neighbourhoodDescription][order(N)])
  
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
  print(cor(dtMeans[!is.na(elasticity) & nobs > quantile(nobs, .1), .(elasticity, meanPPSF)]))
  
  return(dtMeans)
}

# ==========================================
# Run analyses
# ==========================================

dtResultsNbhd <- runAnalysis(copy(dtBCA2019), "neighbourhoodDescription", "neighbourhood")

q("no")


