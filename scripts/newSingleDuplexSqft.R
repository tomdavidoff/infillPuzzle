# bcaFSRDistribution.R
# Distribution of FSR (floor-area / lot-area) for City of Vancouver
# single-family vs duplex parcels on ~33' lots, with MB_Effective_Year
# in the 2019-2023 duplex-permit era.
#
# Data sources:
#   - BCA 2026-01-09 spatial geopackage (same as bcaVancouverParcelExtract.R)
#     for COV polygons tagged by ActualUseDescription.
#   - BCA 2025-01-01 residential inventory text file
#     (~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/...)
#     for landWidth, landDepth, MB_Effective_Year, MB_Total_Finished_Area.
#
# Sample construction:
#   - Spatial filter: jurisdiction 200, ActualUseDescription in
#     propertyTypeMap$singleFamily or propertyTypeMap$duplex.
#   - Match to inventory by (Roll_Number, Jurisdiction).
#   - Width band: Land_Width_Width in [32.5, 33.5] (tight, excludes 34' and 32').
#   - Depth band: 25th-80th quantile of depth among SINGLE-FAMILY in the
#     width band, then applied to BOTH SF and duplex so the FSR comparison
#     is on a common lot-geometry footprint.
#   - Effective year: MB_Effective_Year in [2019, 2023].
#
# Substantive question: do duplex builders use the extra capacity, or
# build at the same FSR as SF on equivalent lots?
#
# Tom Davidoff
# 05/18/26

library(data.table)
library(sf)

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
BCA_GPKG_DIR   <- "~/bigFiles/latestSpatialBCA/"
INVENTORY_FILE <- "~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt"
TYPE_MAP_FILE  <- "~/DropboxExternal/dataProcessed/propertyTypeMap.rds"
VANCOUVER_JUR  <- "200"
WIDTH_BAND     <- c(32.5, 33.5)
EFFY_BAND      <- c(2019, 2023)
DEPTH_QUANTILES <- c(0.25, 0.80)

# ---------------------------------------------------------------------------
# 1. Property type map (need SF + duplex categories)
# ---------------------------------------------------------------------------
if (!file.exists(path.expand(TYPE_MAP_FILE)))
  stop("propertyTypeMap.rds not found. Run bcaPropertyTypes.R first.")
typeMap <- readRDS(path.expand(TYPE_MAP_FILE))
catsSF  <- typeMap$singleFamily
catsDup <- typeMap$duplex
cat("SF categories:\n"); for (s in catsSF)  cat("  ", s, "\n")
cat("Duplex categories:\n"); for (s in catsDup) cat("  ", s, "\n")

# ---------------------------------------------------------------------------
# 2. Spatial pull: COV polygons for SF + duplex types. Just need rolls and
#    a use-type tag; geometry isn't needed for the FSR computation, but the
#    spatial layer is the cleanest source of (rollNumber, ActualUseDescription)
#    pairs for COV.
# ---------------------------------------------------------------------------
gpkgFiles <- list.files(path.expand(BCA_GPKG_DIR),
                        pattern = "\\.gpkg$", full.names = TRUE)
if (length(gpkgFiles) == 0) stop("No .gpkg in ", BCA_GPKG_DIR)
gpkg <- gpkgFiles[1]

allCats <- c(catsSF, catsDup)
sqlInList <- paste0("(",
  paste0("'", gsub("'", "''", allCats), "'", collapse = ", "), ")")
qSpatial <- sprintf(paste(
  "SELECT ROLL_NUMBER, JURISDICTION_CODE, ACTUAL_USE_DESCRIPTION",
  "FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
  "WHERE JURISDICTION_CODE = '%s'",
  "AND ACTUAL_USE_DESCRIPTION IN %s"),
  VANCOUVER_JUR, sqlInList)

sfAll <- st_read(gpkg, query = qSpatial, quiet = TRUE)
dtSpatial <- as.data.table(st_drop_geometry(sfAll))
dtSpatial[, rollNumber   := as.numeric(ROLL_NUMBER)]
dtSpatial[, jurisdiction := trimws(as.character(JURISDICTION_CODE))]
dtSpatial[, actualUseDescription := ACTUAL_USE_DESCRIPTION]
dtSpatial[, propType := fifelse(actualUseDescription %in% catsSF,  "SF",
                        fifelse(actualUseDescription %in% catsDup, "duplex",
                                NA_character_))]
dtSpatial <- dtSpatial[!is.na(propType), .(rollNumber, jurisdiction, propType,
                                            actualUseDescription)]
cat(sprintf("\nSpatial rows (SF + duplex in COV): %d\n", nrow(dtSpatial)))
print(dtSpatial[, .N, by = propType])

# ---------------------------------------------------------------------------
# 3. Inventory text file
# ---------------------------------------------------------------------------
inv <- fread(path.expand(INVENTORY_FILE))
cat(sprintf("\nInventory rows (BC-wide): %d\n", nrow(inv)))

# COV only.
inv <- inv[Jurisdiction == as.integer(VANCOUVER_JUR)]
cat(sprintf("COV inventory rows: %d\n", nrow(inv)))

# Roll number to character. The inventory Roll_Number is i64 -- format
# trimming should align with the spatial side.
inv[, rollNumber   := as.numeric(Roll_Number)]
inv[, jurisdiction := trimws(as.character(Jurisdiction))]

# Keep only relevant columns to make the join cheap.
inv <- inv[, .(rollNumber, jurisdiction,
               landWidth   = as.numeric(Land_Width_Width),
               landDepth   = as.numeric(Land_Depth_Depth),
               sqft        = as.numeric(MB_Total_Finished_Area),
               effectiveYear = as.integer(MB_Effective_Year),
               yearBuilt     = as.integer(MB_Year_Built),
               landSqMeasure = as.numeric(Land_Sq_Measure))]

# ---------------------------------------------------------------------------
# 4. Merge spatial -> inventory
# ---------------------------------------------------------------------------
setkey(dtSpatial, rollNumber, jurisdiction)
setkey(inv,       rollNumber, jurisdiction)
dt <- inv[dtSpatial, nomatch = 0]
cat(sprintf("\nAfter merge: %d rows  (matched %.1f%% of spatial)\n",
            nrow(dt), 100 * nrow(dt) / nrow(dtSpatial)))
print(dt[, .N, by = propType])

# ---------------------------------------------------------------------------
# 5. Filters: width band, effective year band
# ---------------------------------------------------------------------------
dt[, landArea := landWidth * landDepth]

# Sanity check: width*depth vs Land_Sq_Measure (should be close).
sanityRows <- dt[!is.na(landSqMeasure) & landSqMeasure > 0 &
                 !is.na(landArea) & landArea > 0]
if (nrow(sanityRows) > 0) {
  diffPct <- 100 * (sanityRows$landArea - sanityRows$landSqMeasure) /
             sanityRows$landSqMeasure
  cat(sprintf("\nWidth*depth vs Land_Sq_Measure sanity: median diff = %.2f%%, ",
              median(diffPct, na.rm = TRUE)))
  cat(sprintf("p25 = %.2f%%, p75 = %.2f%%\n",
              quantile(diffPct, 0.25, na.rm = TRUE),
              quantile(diffPct, 0.75, na.rm = TRUE)))
}

dt <- dt[is.finite(landWidth) & is.finite(landDepth) & is.finite(sqft) &
         landWidth > 0 & landDepth > 0 & sqft > 0 &
         is.finite(effectiveYear)]

# Width filter applied to BOTH samples.
dtW <- dt[landWidth %between% WIDTH_BAND]
cat(sprintf("\nAfter width band [%.1f, %.1f]: n_SF = %d, n_duplex = %d\n",
            WIDTH_BAND[1], WIDTH_BAND[2],
            sum(dtW$propType == "SF"),
            sum(dtW$propType == "duplex")))

# Depth band: SF-derived quantiles within the width band, applied to both.
depthBand <- quantile(dtW[propType == "SF", landDepth],
                      DEPTH_QUANTILES, na.rm = TRUE)
cat(sprintf("Depth band (SF q%.2f-q%.2f within width band): [%.1f, %.1f]\n",
            DEPTH_QUANTILES[1], DEPTH_QUANTILES[2],
            depthBand[1], depthBand[2]))

dtWD <- dtW[landDepth %between% depthBand]
cat(sprintf("After depth band:               n_SF = %d, n_duplex = %d\n",
            sum(dtWD$propType == "SF"),
            sum(dtWD$propType == "duplex")))

# Effective-year filter.
dtFinal <- dtWD[effectiveYear %between% EFFY_BAND]
cat(sprintf("After MB_Effective_Year %d-%d:   n_SF = %d, n_duplex = %d\n",
            EFFY_BAND[1], EFFY_BAND[2],
            sum(dtFinal$propType == "SF"),
            sum(dtFinal$propType == "duplex")))

# ---------------------------------------------------------------------------
# 6. FSR
# ---------------------------------------------------------------------------
dtFinal[, FSR := sqft / landArea]
dtFinal <- dtFinal[is.finite(FSR) & FSR > 0 & FSR < 5]   # drop wild outliers

cat("\n============== FSR distribution ==============\n")
fsrTab <- dtFinal[, .(n = .N,
                      mean   = mean(FSR),
                      sd     = sd(FSR),
                      p10    = quantile(FSR, 0.10),
                      p25    = quantile(FSR, 0.25),
                      median = median(FSR),
                      p75    = quantile(FSR, 0.75),
                      p90    = quantile(FSR, 0.90)),
                  by = propType]
setorder(fsrTab, propType)
print(fsrTab)

# Two-sample t and KS as quick sanity tests.
sfFSR  <- dtFinal[propType == "SF",     FSR]
dupFSR <- dtFinal[propType == "duplex", FSR]
if (length(sfFSR) >= 5 && length(dupFSR) >= 5) {
  tt <- t.test(dupFSR, sfFSR)
  ks <- ks.test(dupFSR, sfFSR)
  cat(sprintf("\nDuplex vs SF mean FSR:  diff = %.3f  t-pval = %.3g\n",
              tt$estimate[1] - tt$estimate[2], tt$p.value))
  cat(sprintf("Duplex vs SF KS test:   D = %.3f   pval = %.3g\n",
              ks$statistic, ks$p.value))
}

# ---------------------------------------------------------------------------
# 7. Optional: write a small RDS for any downstream use
# ---------------------------------------------------------------------------
saveRDS(dtFinal[, .(rollNumber, propType, landWidth, landDepth, landArea,
                    sqft, effectiveYear, yearBuilt, FSR)],
        "~/DropboxExternal/dataProcessed/fsrDistribution_33lot_2019_23.rds")
cat("\nWrote fsrDistribution_33lot_2019_23.rds\n")
cat("\nDone.\n")
