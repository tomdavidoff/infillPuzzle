# ppsfElasticity.R
# For each neighbourhood in Vancouver regional district, each year, compute ppsf and elasticity wrt sqft for single, duplex, both
# Then look at correlations; predict value of single vs duplex redevelopment on standard 33x122 lot
# Tom Davidoff
# 03/20/26

library(data.table)
library(ggplot2)
library(fixest)

# Centroids: used for East/West classification by longitude
dG <- readRDS("~/OneDrive - UBC/dataProcessed/bca25Centroids.rds")
print(head(dG))

# Drop missing and duplicate roll numbers from centroids
dG <- dG[!is.na(ROLL_NUMBER)]
dG[, n := .N, by = ROLL_NUMBER]
dG <- dG[n == 1][, n := NULL]

# Ontario Street longitude threshold for East/West split
LON_ONTARIO <- -123.1027

# Sales data
dS <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",
            select = c("ROLL_NUMBER","FOLIO_ID","CONVEYANCE_TYPE_DESCRIPTION","CONVEYANCE_DATE","CONVEYANCE_PRICE"))
dS[, year := as.numeric(substr(CONVEYANCE_DATE, 1, 4))]
MINYEAR <- 2016
MAXYEAR <- 2024
dS <- dS[year >= MINYEAR & year <= MAXYEAR & CONVEYANCE_TYPE_DESCRIPTION == "Improved Single Property Transaction"]

# Description / use classification
dD <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",
            select = c("ROLL_NUMBER","FOLIO_ID","JURISDICTION","JURISDICTION_CODE","NEIGHBOURHOOD","ACTUAL_USE_DESCRIPTION"))
dD[, use := ifelse(ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling","Residential Dwelling with Suite"), "single",
             ifelse(grepl("Duplex", ACTUAL_USE_DESCRIPTION), "duplex",
               ifelse(grepl("Strata-Lot Residence", ACTUAL_USE_DESCRIPTION), "condo", "Other")))]
print(table(dD[, use]))

# Metro Vancouver assessment areas
dtMapper <- fread("~/OneDrive - UBC/dataProcessed/regional_district_assessment_area.csv")
gV <- as.numeric(dtMapper[`Assessment Area` == 9, .(`Regional District`)][1])
needInventory <- unique(dtMapper[`Regional District` == gV, .(AA = `Assessment Area`)]$AA)

# Read residential inventory across all assessment areas
for (i in 1:length(needInventory)) {
  filename <- paste0("~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A",
                     ifelse(needInventory[i] < 10, paste0("0", needInventory[i]), needInventory[i]),
                     "_Residential_Inventory_Extract.txt")
  dtTemp <- fread(filename, select = c("Roll_Number","MB_Total_Finished_Area","Zoning","Land_Width_Width",
                                       "Land_Depth_Depth","MB_Effective_Year","Jurisdiction"),
                  colClasses = c(Roll_Number = "character", MB_Total_Finished_Area = "numeric",
                                 Zoning = "character", Land_Width_Width = "numeric",
                                 Land_Depth_Depth = "numeric"))
  dtInventory <- if (i == 1) dtTemp else rbind(dtInventory, dtTemp)
}
print(table(dtInventory[, Jurisdiction]))

MAXWIDTH <- 90
dtInventory <- dtInventory[Land_Width_Width <= MAXWIDTH | is.na(Land_Width_Width)]

# Merge key: numeric roll number + jurisdiction
dD[, ROLL_NUMBER := as.numeric(ROLL_NUMBER)]
dG[, ROLL_NUMBER := as.numeric(ROLL_NUMBER)]
dD <- dD[!is.na(ROLL_NUMBER)]
dG <- dG[!is.na(ROLL_NUMBER)]
dD <- merge(dD, dG[, .(ROLL_NUMBER, lon)], by = "ROLL_NUMBER")
dtInventory[, ROLL_NUMBER := as.numeric(Roll_Number)]
dtInventory <- dtInventory[!is.na(ROLL_NUMBER)]
dD <- dD[!is.na(ROLL_NUMBER)]
dD[, n := .N, by = ROLL_NUMBER]
dD <- dD[n == 1][, n := NULL]
dD[, id := paste(ROLL_NUMBER, JURISDICTION_CODE)]
dtInventory[, id := paste(ROLL_NUMBER, Jurisdiction)]

# ── Build single dtMerge unconditional on use type ──
dtMerge <- merge(dtInventory, dD, by = "id")
dtMerge <- merge(dtMerge, dS, by = "FOLIO_ID", all.x = TRUE)
dtMerge[, ppsf    := CONVEYANCE_PRICE / MB_Total_Finished_Area]
dtMerge[, logppsf := log(ppsf)]
dtMerge[, logsqft := log(MB_Total_Finished_Area)]
dtMerge[, age     := year - MB_Effective_Year]
dtMerge <- dtMerge[!is.na(logppsf) & !is.na(logsqft)]

# ── Lot geometry subset for estimation and prediction: 31-35' wide, >100' deep ──
LOT_WIDTH_LO <- 31; LOT_WIDTH_HI <- 35; LOT_DEPTH_MIN <- 100
dtLot <- dtMerge[Land_Width_Width >= LOT_WIDTH_LO & Land_Width_Width <= LOT_WIDTH_HI &
                 Land_Depth_Depth > LOT_DEPTH_MIN]

# ── Neighbourhood x year stats: single family on lot subset ──
MINAGE <- 0; MAXAGE <- 30; MINOBS <- 20
dtGroup <- dtLot[use == "single" & age >= MINAGE & age <= MAXAGE,
                 .(cov      = cov(logppsf, logsqft),
                   var      = var(logsqft),
                   mppsf    = mean(logppsf),
                   mlogsqft = mean(logsqft),
                   nGroup   = .N,
                   EastWest = ifelse(mean(lon, na.rm = TRUE) <= LON_ONTARIO, "West", "East")),
                 by = c("year","NEIGHBOURHOOD","JURISDICTION")]
dtGroup[, elasticity := cov / var]
dtGroup <- dtGroup[nGroup >= MINOBS]
print(summary(dtGroup))

# ── Elasticity regressions (metro and City of Vancouver) ──
print(summary(feols(elasticity ~ mppsf | year, data = dtGroup)))
print(summary(feols(elasticity ~ mppsf | year, data = dtGroup[JURISDICTION == "City of Vancouver"])))

# ── feols by use type for etable (full dtMerge, not lot subset) ──
rs <- feols(logppsf ~ logsqft | year + NEIGHBOURHOOD, data = dtMerge[use == "single"])
rd <- feols(logppsf ~ logsqft | year + NEIGHBOURHOOD, data = dtMerge[use == "duplex"])
rc <- feols(logppsf ~ logsqft | year + NEIGHBOURHOOD, data = dtMerge[use == "condo"])
etable(rs, rd, rc, tex = TRUE,
       title = "Elasticity of ppsf wrt sqft by type",
       fitstat = c("n","r2","ar2"), digits = 3,
       file = "text/ppsfElasticityByType.tex")

# ── Scatter: elasticity vs mean log ppsf, faceted by year (metro single, lot subset) ──
ggplot(dtGroup, aes(x = mppsf, y = elasticity, size = nGroup)) +
  geom_point() + facet_wrap(~year) + theme_bw() +
  labs(y = "Elasticity of ppsf wrt sqft", x = "Mean log ppsf", size = "N obs") +
  ylim(-1.25, .25)
ggsave("text/ppsfElasticitySingleMetro.png", width = 10, height = 6)

# ── Distribution plot: mean log ppsf by neighbourhood x year, City of Vancouver (single, all lots) ──
dtVanGroup <- dtMerge[JURISDICTION == "City of Vancouver" & use == "single",
                      .(mean_logppsf = mean(logppsf, na.rm = TRUE),
                        EastWest = ifelse(mean(lon, na.rm = TRUE) <= LON_ONTARIO, "West", "East")),
                      by = .(NEIGHBOURHOOD, year)]
p <- ggplot(dtVanGroup, aes(x = factor(year), y = mean_logppsf, colour = EastWest)) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.75) +
  scale_colour_manual(values = c("East" = "#E69F00", "West" = "#0072B2"),
                      name = "East/West of Ontario St") +
  theme_bw() +
  labs(title = "City of Vancouver — Mean log price/sqft by neighbourhood (single)",
       x = "Year", y = "Mean log price per sq ft")
print(p)
ggsave("text/logppsf_distribution_Van_single.png", plot = p, width = 10, height = 6)

# ── Prediction: 33x122 lot, 0.7 FSR single vs 2 x 0.35 FSR duplexes ──
LOT_W <- 33; LOT_D <- 122
SQFT_SINGLE <- 0.70 * LOT_W * LOT_D   # 2,805 sqft
SQFT_DUPLEX <- 0.35 * LOT_W * LOT_D   # 1,402 sqft per unit

# Predict log ppsf at target sqft using neighbourhood-year regression line:
#   logppsf_hat = mppsf + elasticity * (log(target_sqft) - mlogsqft)
# Do this for period of 2017-2018 because of endogeneity of prices to supply by neighbourhood.
# Do a regression per neighbourhood and compare regression among single family homes to duplex actuals
dtPred <- data.table(NEIGHBOURHOOD=character(),intercept=numeric(),elasticity=numeric(),fittedSingle=numeric(),meanSingle=numeric(),fittedDuplex=numeric(),meanDuplex=numeric())
for (n in unique(dtMerge[JURISDICTION=="City of Vancouver", NEIGHBOURHOOD])) {
	dtTemp <- dtMerge[JURISDICTION=="City of Vancouver" & NEIGHBOURHOOD==n & year %in% 2017:2018 & ((Land_Width_Width >= LOT_WIDTH_LO & Land_Width_Width <= LOT_WIDTH_HI & Land_Depth_Depth >= LOT_DEPTH_MIN )| is.na(Land_Width_Width)) & age >= MINAGE & age <= MAXAGE]
	if (nrow(dtTemp[use=="single"]) > 20) {
		m <- feols(logppsf ~ logsqft + log(age), data = dtTemp[use == "single"])
		intercept <- coef(m)["(Intercept)"]
		elasticity <- coef(m)["logsqft"]
		meanSingle <- mean(dtTemp[use == "single", exp(logppsf)], na.rm = TRUE)
		meanDuplex <- mean(dtTemp[use == "duplex", exp(logppsf)], na.rm = TRUE)
		dtPred <- rbind(dtPred, data.table(NEIGHBOURHOOD=n, intercept=intercept, elasticity=elasticity, fittedSingle = exp(intercept+elasticity*log(SQFT_SINGLE)),meanSingle=meanSingle, fittedDuplex=exp(intercept+elasticity*log(SQFT_DUPLEX)), meanDuplex=meanDuplex))
	} else {print(paste("Skipping", n, "due to insufficient observations"))}
}

fwrite(dtPred, "~/OneDrive - UBC/dataProcessed/ppsfElasticityPredictionsByNeighbourhood.csv")

