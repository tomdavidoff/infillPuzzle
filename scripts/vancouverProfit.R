# vancouverProfit_Neighbourhood.R
# estimate profitability of redevelopment choices in Vancouver at the Neighbourhood level
# Tom Davidoff
# Updated: 02/03/26

library(data.table)
library(ggplot2)
library(fixest)
library(sf)
library(xtable)
library(stringr)

# =============================================================================
# 1. GEOGRAPHY & CENSUS TRACT MAPPING
# =============================================================================
fGeo <- "~/OneDrive - UBC/dataProcessed/bca26FolioGeometryVancouver.rds"

if (!file.exists(fGeo)) {
  # Note: Extracting NEIGHBOURHOOD (BCA code) and joining with Census Tracts for Income
  dfG <- st_read(
    "/Volumes/T7Office/bigFiles/bca_folios_spatial_file_20251103/bca_folios.gpkg.gpkg",
    query = "SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD,
           SHAPE_AREA, SHAPE_LEN, geom, JURISDICTION
           FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
           WHERE JURISDICTION = 'City of Vancouver'"
  )

  fCT <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
  dCT <- st_read(fCT)
  dCT <- dCT[dCT$PRUID == "59", ]
  dCT <- st_transform(dCT, st_crs(dfG))

  dfG <- st_join(dfG, dCT, join = st_within)
  saveRDS(dfG, fGeo)
}

dfG <- readRDS(fGeo)
dtGeo <- as.data.table(dfG)[, .(ROLL_NUMBER, CTNAME, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD)]
dtGeo <- dtGeo[ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling", "Residential Dwelling with Suite")]
setnames(dtGeo, "CTNAME", "tract")

# =============================================================================
# 2. SALES & INVENTORY
# =============================================================================
dtSales <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD24_20240331/bca_folio_sales_20240331_REVD24.csv",
                 select = c("ROLL_NUMBER", "CONVEYANCE_DATE", "CONVEYANCE_PRICE"))

dtInventory <- fread("~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt",
                     select = c("Roll_Number", "MB_Effective_Year", "MB_Total_Finished_Area", "Land_Width_Width", "Land_Depth_Depth"),
                     colClasses = list(character = c("Roll_Number")))
setnames(dtInventory, "Roll_Number", "ROLL_NUMBER")

# Merge Sales + Inventory + Geo
dtSales <- merge(dtSales, dtInventory, by = "ROLL_NUMBER")
dtSales <- merge(dtSales, dtGeo, by = "ROLL_NUMBER")

dtSales[, year := as.numeric(substr(CONVEYANCE_DATE, 1, 4))]
dtSales <- dtSales[year >= 2019 & CONVEYANCE_PRICE >= 500000 & MB_Total_Finished_Area > 0]

# =============================================================================
# 3. NEIGHBOURHOOD-LEVEL PRICE & ELASTICITY
# =============================================================================
# Use the Neighbourhood-level pre-processed file
dtPriceND <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_neighbourhood.csv")
# Standardize names for merge
setnames(dtPriceND, "meanPPSF", "neighPPSF")

# Load Permit Choices
dtChoice <- readRDS("~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")

# Merge Permits with Neighbourhood Price data
dtMerge <- merge(dtChoice, dtPriceND, by = "neighbourhoodDescription")

# =============================================================================
# 4. INCOME (Aggregate Tract-level Income to Neighbourhood)
# =============================================================================
dtIncome <- fread("~/OneDrive - UBC/dataRaw/9810005801_databaseLoadingData.csv",
                  select = c("GEO", "DGUID", "Household income statistics (6)", "VALUE"))
setnames(dtIncome, c("DGUID", "Household income statistics (6)", "VALUE"), c("DGUID", "incomeStat", "medianIncome"))
dtIncome <- dtIncome[incomeStat == "Median household total income (2020) (2020 constant dollars)"]
dtIncome[, tract := substr(DGUID, 13, 19)]
dtIncome <- dtIncome[grepl("Vancouver",GEO)]

# Join income to dtMerge (via tract), then average to neighbourhood level
# first crosswalk tract to neighbourhood
# Calculate Neighbourhood Income directly from the Folio-Tract mapping
dtNeighbourhoodIncome <- as.data.table(dfG)[!is.na(CTNAME), .(
    # First get tract for every folio, then join income
    tract = CTNAME
), by = .(ROLL_NUMBER, NEIGHBOURHOOD)]

dtNeighbourhoodIncome <- merge(dtNeighbourhoodIncome, dtIncome[, .(tract, medianIncome)], by = "tract")

# Now average income up to the Neighbourhood level
dtNeighbourhoodIncome <- dtNeighbourhoodIncome[, .(
    neighMedianIncome = mean(medianIncome, na.rm = TRUE)
), by = "NEIGHBOURHOOD"]

# Now you can merge this directly into dtMerge without needing the 'tract' column
dtMerge <- merge(dtMerge, dtNeighbourhoodIncome, by.x = "neighbourhoodDescription", by.y = "NEIGHBOURHOOD", all.x = TRUE)

# =============================================================================
# 5. DATA CLEANING & CLASSIFICATION
# =============================================================================
dtMerge <- dtMerge[nobs > 4]
dtMerge[elasticity > 0, elasticity := 0]
dtMerge[elasticity < -1, elasticity := -1]

# Re-classify single into only vs laneway based on proximity
CUTDISTANCE <- 20
dtMerge[use == "single", use := ifelse(distToLaneway > CUTDISTANCE, "singleOnly", "singleLaneway")]
dtMerge[nearestLanewayPermit > CUTDISTANCE & use == "single", use := "singleOnly"]
dtMerge[nearestLanewayPermit <= CUTDISTANCE & use == "single", use := "singleLaneway"]
dtMerge <- dtMerge[use != "laneway"]

dtMerge[, c("lat", "lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]

# =============================================================================
# 6. SPATIAL PLOTS (NEIGHBOURHOOD LEVEL)
# =============================================================================
# PPSF Spatial Plot
ggplot(dtMerge, aes(x = lon, y = lat, color = neighPPSF)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c(name = "Neighbourhood PPSF") +
  theme_minimal()
ggsave("text/vancouverNeighbourhoodPPSFSpatial.png", width = 6, height = 6)

# PPSF Bins
dtMerge[, ppsfBin := cut(neighPPSF,
                         breaks = quantile(neighPPSF, probs = seq(0, 1, by = .1), na.rm = TRUE),
                         include.lowest = TRUE)]

# =============================================================================
# 7. PROFIT CALCULATIONS
# =============================================================================
FSR_SINGLEDUPLEX <- 0.7
FSR_LANEWAY <- 0.16
COSTPSF <- 500

dtMerge[, lotSizeSqft := as.numeric(landWidth) * as.numeric(landDepth)]
dtMerge[, buildableLaneway := pmax(as.numeric(MB_total_finished_area), (FSR_SINGLEDUPLEX + FSR_LANEWAY) * lotSizeSqft)]
dtMerge[, buildableDuplex := pmax(as.numeric(MB_total_finished_area), FSR_SINGLEDUPLEX * lotSizeSqft)]

dtMerge[, profitLaneway := buildableLaneway * (neighPPSF - COSTPSF)]
dtMerge[, profitDuplex  := buildableDuplex  * (neighPPSF * exp(elasticity / 2) - COSTPSF)]
dtMerge[, deltaProfit   := profitLaneway - profitDuplex]

# =============================================================================
# 8. NEIGHBOURHOOD CORRELATIONS
# =============================================================================
dtNeighbourhoodMean <- dtMerge[use != "laneway", .(
  PPSF = mean(neighPPSF),
  elasticity = mean(elasticity),
  lanewayShare = mean(use == "singleLaneway"),
  duplexShare = mean(use == "duplex"),
  multiplexShare = mean(use[as.numeric(substr(permitnumbercreateddate, 1, 4)) > 2023] == "multi", na.rm = TRUE),
  medianIncome = mean(neighMedianIncome, na.rm = TRUE)
), by = "neighbourhoodDescription"]

dtNeighbourhoodMean <- na.omit(dtNeighbourhoodMean)
mCor <- cor(dtNeighbourhoodMean[, .(PPSF, elasticity, lanewayShare, duplexShare, multiplexShare, medianIncome)])
print(xtable(mCor, digits = 2), floating = FALSE, file = "text/neighbourhoodCorrelationVancouver.tex")

# =============================================================================
# 9. STABLE PROBIT ESTIMATION (NEIGHBOURHOOD LEVEL)
# =============================================================================
dtSingles <- dtMerge[use %chin% c("singleOnly", "singleLaneway")]
dtSingles[, hasLaneway := as.integer(use == "singleLaneway")]
dtSingles[, permitYear := as.numeric(substr(permitnumbercreateddate, 1, 4))]
dtSingles[, lanewayFSR := fifelse(permitYear < 2024, 0.16, 0.25)]

neigh_levels <- sort(unique(dtSingles$neighbourhoodDescription))
Nn <- length(neigh_levels)
dtSingles[, neigh_id := match(neighbourhoodDescription, neigh_levels)]

# Scale X for numerical stability
dtSingles[, x := (lanewayFSR * neighPPSF) / 100]

piLanewayNLL_Neigh <- function(par, dt, Nn, lambda_beta = 1e-2) {
  alpha <- par[1]
  beta <- par[1 + (1:Nn)]
  beta <- beta - mean(beta)

  mu <- alpha * dt$x - beta[dt$neigh_id]
  p <- pmin(pmax(pnorm(mu), 1e-9), 1 - 1e-9)

  -sum(dt$hasLaneway * log(p) + (1 - dt$hasLaneway) * log(1 - p)) + lambda_beta * sum(beta^2)
}

fitLaneway <- optim(
  par = c(0, rep(0, Nn)),
  fn = piLanewayNLL_Neigh,
  dt = dtSingles,
  Nn = Nn,
  method = "L-BFGS-B",
  control = list(maxit = 2000)
)

alpha_hat <- fitLaneway$par[1]
beta_neigh_hat <- data.table(
  neighbourhoodDescription = neigh_levels,
  betaNeigh = fitLaneway$par[2:(Nn+1)] - mean(fitLaneway$par[2:(Nn+1)])
)

cat("\n=== Neighbourhood Probit Results ===\n")
cat("Alpha (Effect of PPSF * FSR):", alpha_hat, "\n")

# =============================================================================
# 10. MULTIPLEX SHARE ANALYSIS (2024-2025)
# =============================================================================
dtPlexNeigh <- dtMerge[
  as.numeric(substr(permitnumbercreateddate, 1, 4)) %in% c(2024, 2025) & use %chin% c("duplex", "multi"),
  .(
    plexShare = mean(use == "multi"),
    N = .N,
    meanPPSF_n = mean(neighPPSF),
    income_n = mean(neighMedianIncome, na.rm = TRUE)
  ),
  by = neighbourhoodDescription
]

dtPlexNeigh <- merge(dtPlexNeigh, beta_neigh_hat, by = "neighbourhoodDescription")

# Regression: Do neighborhood 'unobservables' from Laneways predict Multiplex uptake?
m_plex <- feols(plexShare ~ income_n + meanPPSF_n + betaNeigh,
                data = dtPlexNeigh[N >= 3], weights = ~N)
print(summary(m_plex))
