# vancouverElasticityPrice.R
# Three-way COV pipeline (renamed from vancouverLotPlexByType.R):
#   (a) Strata           -- floor-ppsf elasticity wrt log(finished area).
#                           Null is zero (price linear in unit size); the
#                           interpretable test is spatial FLATNESS of the
#                           slope surface, not the point estimate.
#   (b) Single-family    -- 2014-2018, land-ppsf elasticity wrt log(lot area).
#   (c) Duplex premium   -- 2014-2018, pooled SF + duplex on ~33' lots.
#                           y = log(conveyancePrice / MB_total_finished_area),
#                           x = isDuplex. Premium is in price-per-sqft space
#                           (Tsur 5/21/26), so the SF/duplex comparison is
#                           apples-to-apples on the per-sqft margin.
#                           Global benchmark: feols(logPPSF ~ isDuplex | year)
#                           gives the average log-ppsf premium. The GWR slope
#                           surface gives that premium locally at each permit.
#
# Cambie corridor (Oak to Main, south of W 16th Ave) is excluded from
# BOTH the sales-side training data AND the permit fit points.
#
# Fit points = COV permit locations, so the three surfaces overlay.
# Property-type categories come from propertyTypeMap.rds (run
# bcaPropertyTypes.R first).
#
# Tom Davidoff
# 05/18/26 (rev: replace duplex elasticity sample with duplex price-premium
#           sample; renamed file)
# 05/19/26 (rev: Cambie corridor exclusion on sales and permits)
# 05/19/26 (rev: three matched localPPSF surfaces, one per race)
# 05/20/26 (rev: Gaussian kernel with adaptive k-NN bandwidth, replaces
#           bisquare. Eliminates the "no duplex sales in window" NA
#           problem at small k, since all weights are positive. The
#           bandwidth at each fit point is d_k * cfg$gaussianBwScale,
#           where d_k is the distance to the k-th nearest sale.
#           cfg$gaussianBwScale = 0.5 gives a Gaussian whose effective
#           neighborhood (~2 sigma) roughly matches the old bisquare
#           support, while the tails fall off smoothly rather than
#           hitting a hard zero.)
# 05/21/26 (rev: duplex premium LHS changed from log(conveyancePrice) to
#           log(conveyancePrice / MB_total_finished_area). Tsur's
#           apples-to-apples per-sqft comparison. Affects
#           mkSampleDuplexPremium only; section 7 inherits via the sample.
#           Column name slope_duplexPremium33 unchanged in the export, but
#           interpretation is now a log-ppsf premium, not a log-price one.)

library(data.table)
library(ggplot2)
library(ggspatial)
library(RSQLite)
library(sf)
library(fixest)

# ===========================================================================
# 1. GENERIC TOOLS
# ===========================================================================
# Adaptive Gaussian-kernel local linear regression.
#
# At each fit point s:
#   1. Compute distances d to all data points.
#   2. d_k = distance to the k-th nearest data point.
#   3. h = d_k * bwScale (bandwidth; sigma of the Gaussian).
#   4. w_i = exp(-0.5 * (d_i / h)^2).
#   5. Truncate weights below `weightFloor` (small-w tail is dropped to
#      avoid carrying ~0 weights through lm.wfit; default cuts at
#      ~4*sigma).
#   6. Run weighted OLS on (x, y) with weights w.
#
# vs the old bisquare:
#   * All in-keep weights are strictly positive (no hard cutoff at d_k).
#   * The "no variation in x within window" NA gate (sd(x)==0) almost
#     never fires, because the window effectively spans the whole data.
gwrLocal <- function(y, x, dataCoords, fitCoords, k = 1000, mcCores = 6,
                     bwScale = 0.5, weightFloor = 1e-4) {
  gaussian <- function(d, h) exp(-0.5 * (d / h)^2)
  naRow <- c(intercept = NA_real_, slope = NA_real_,
             seInt = NA_real_, seSlope = NA_real_, nEff = NA_real_)
  fitOne <- function(s) {
    tryCatch({
      d  <- sqrt((dataCoords[,1] - s[1])^2 + (dataCoords[,2] - s[2])^2)
      if (length(d) < k) return(naRow)
      dk <- sort(d, partial = k)[k]
      if (!is.finite(dk) || dk <= 0) return(naRow)
      h  <- dk * bwScale
      w  <- gaussian(d, h)
      keep <- w > weightFloor
      if (sum(keep) < 3) return(naRow)
      Xm <- cbind(1, x[keep]); ym <- y[keep]; wm <- w[keep]
      ok <- complete.cases(Xm, ym, wm) & is.finite(wm) & wm > 0
      Xm <- Xm[ok, , drop = FALSE]; ym <- ym[ok]; wm <- wm[ok]
      if (length(ym) < 3 || sd(Xm[, 2]) == 0) return(naRow)
      fit <- lm.wfit(Xm, ym, wm)
      resid <- ym - Xm %*% fit$coefficients
      sigma2 <- sum(wm * resid^2) / (sum(wm) - 2)
      XtWX_inv <- solve(crossprod(Xm * sqrt(wm)))
      se <- sqrt(diag(sigma2 * XtWX_inv))
      c(intercept = fit$coefficients[1], slope = fit$coefficients[2],
        seInt = se[1], seSlope = se[2], nEff = sum(wm))
    }, error = function(e) naRow)
  }
  resList <- parallel::mclapply(seq_len(nrow(fitCoords)),
                                function(i) fitOne(fitCoords[i,]),
                                mc.cores = mcCores)
  res <- do.call(rbind, resList)
  dt  <- as.data.table(res)
  setnames(dt, c("intercept","slope","seInt","seSlope","nEff"))
  dt[, `:=`(fitX = fitCoords[,1], fitY = fitCoords[,2])]
  dt[]
}

# Intercept-only adaptive Gaussian local mean. Same kernel and scaling
# as gwrLocal.
gwrLocalMean <- function(y, dataCoords, fitCoords, k = 1000, mcCores = 6,
                         bwScale = 0.5, weightFloor = 1e-4) {
  gaussian <- function(d, h) exp(-0.5 * (d / h)^2)
  fitOne <- function(s) {
    tryCatch({
      d <- sqrt((dataCoords[,1] - s[1])^2 + (dataCoords[,2] - s[2])^2)
      if (length(d) < k) return(NA_real_)
      dk <- sort(d, partial = k)[k]
      if (!is.finite(dk) || dk <= 0) return(NA_real_)
      h  <- dk * bwScale
      w  <- gaussian(d, h)
      keep <- w > weightFloor & is.finite(y)
      if (sum(keep) < 1) return(NA_real_)
      sum(w[keep] * y[keep]) / sum(w[keep])
    }, error = function(e) NA_real_)
  }
  unlist(parallel::mclapply(seq_len(nrow(fitCoords)),
                            function(i) fitOne(fitCoords[i,]),
                            mc.cores = mcCores))
}

trimByQuantile <- function(dt, cols, q) {
  for (c in cols) {
    lo <- quantile(dt[[c]], q,     na.rm = TRUE)
    hi <- quantile(dt[[c]], 1 - q, na.rm = TRUE)
    dt <- dt[get(c) %between% c(lo, hi)]
  }
  dt
}

projectCoords <- function(dt, crsOut) {
  sfTmp <- st_as_sf(dt[, .(lon, lat)], coords = c("lon","lat"), crs = 4326)
  sfTmp <- st_transform(sfTmp, crsOut)
  st_coordinates(sfTmp)
}

runGWR <- function(dtData, yCol, xCol, fitPointsLL, cfg, k = 1000) {
  dataCoords <- projectCoords(dtData, cfg$crsProj)
  fitCoords  <- projectCoords(fitPointsLL, cfg$crsProj)
  dtLocal <- gwrLocal(y           = dtData[[yCol]],
                      x           = dtData[[xCol]],
                      dataCoords  = dataCoords,
                      fitCoords   = fitCoords,
                      k           = k,
                      bwScale     = cfg$gaussianBwScale,
                      weightFloor = cfg$gaussianWeightFloor)
  dtLocal[, `:=`(lon = fitPointsLL$lon, lat = fitPointsLL$lat)]
  dtLocal[]
}

runGWRMean <- function(dtData, yCol, fitPointsLL, cfg, k = 1000) {
  dataCoords <- projectCoords(dtData, cfg$crsProj)
  fitCoords  <- projectCoords(fitPointsLL, cfg$crsProj)
  gwrLocalMean(y           = dtData[[yCol]],
               dataCoords  = dataCoords,
               fitCoords   = fitCoords,
               k           = k,
               bwScale     = cfg$gaussianBwScale,
               weightFloor = cfg$gaussianWeightFloor)
}

plotHeatmap <- function(dt, valueCol, cfg, legendTitle, outPath,
                        size = 0.8, option = "plasma") {
  p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = cfg$tileZoom) +
    geom_point(data = dt, aes(x = lon, y = lat, color = .data[[valueCol]]),
               size = size) +
    scale_color_viridis_c(option = option) +
    coord_sf(crs = 4326, xlim = cfg$xlim, ylim = cfg$ylim) +
    theme_void() +
    labs(color = legendTitle)
  ggsave(outPath, plot = p)
  invisible(p)
}

# Drop rows inside cfg$excludeBox (Cambie corridor).
applyExcludeBox <- function(dt, cfg, label = "") {
  bx <- cfg$excludeBox
  if (is.null(bx)) return(dt)
  in_box <- dt$lon >= bx$lonMin & dt$lon <= bx$lonMax &
            dt$lat >= bx$latMin & dt$lat <= bx$latMax
  cat(sprintf("applyExcludeBox[%s]: dropping %d / %d rows in Cambie box\n",
              label, sum(in_box, na.rm = TRUE), nrow(dt)))
  dt[!in_box | is.na(in_box)]
}

# ===========================================================================
# 2. COV CONFIG
# ===========================================================================
cfg <- list(
  city                = "Vancouver",
  crsProj             = 26910,
  xlim                = c(-123.23, -123.02),
  ylim                = c( 49.20,   49.32),
  tileZoom            = 12,
  sqliteFile          = "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3",
  parcelRDSDir        = "~/DropboxExternal/dataProcessed/",
  permitFile          = "~/DropboxExternal/dataRaw/issued-building-permits.csv",
  zoningGeojson       = "~/DropboxExternal/dataRaw/vancouver_zoning.geojson",
  targetZone          = "R1-1",
  propertyTypeMap     = "~/DropboxExternal/dataProcessed/propertyTypeMap.rds",
  trimDimsQ           = 0.05,
  trimYQ              = 0.01,
  salesYearRange      = c(2014, 2018),
  permitYearRange     = c(2019, 2023),
  bandwidthK          = 250,
  # Gaussian kernel parameters. bwScale * d_k is the sigma at each fit
  # point. 0.5 gives a Gaussian whose ~95% mass region roughly matches
  # the old bisquare support at the same k. weightFloor drops the tail
  # below e^-4.6 (~0.01) to save time on near-zero weights.
  gaussianBwScale     = 0.5,
  gaussianWeightFloor = 1e-4,
  # 33' lot band for the duplex-premium sample. Width measured in feet
  # in BCA. The band absorbs measurement slop without bleeding into
  # 50'/66' lots.
  lotWidthBand        = c(32, 34),
  # Cambie corridor exclusion: Oak St (W) to Main St (E), south of W
  # 16th Ave, southern edge open.
  excludeBox          = list(
    lonMin = -123.130,
    lonMax = -123.101,
    latMax =   49.258,
    latMin = -Inf
  )
)

# ===========================================================================
# 3. PROPERTY TYPE MAP
# ===========================================================================
if (!file.exists(path.expand(cfg$propertyTypeMap)))
  stop("propertyTypeMap.rds not found. Run bcaPropertyTypes.R first.")
propertyTypeMap <- readRDS(path.expand(cfg$propertyTypeMap))
cat("\nLoaded propertyTypeMap. Resolved categories:\n")
for (nm in c("singleFamily","strata","duplex")) {
  cat(sprintf(" -- %s --\n", nm))
  if (length(propertyTypeMap[[nm]]) == 0) cat("   (none)\n")
  for (s in propertyTypeMap[[nm]]) cat("    ", s, "\n")
}

# ===========================================================================
# 4. RAW LOADS
# ===========================================================================
loadParcels <- function(cfg, type) {
  path <- file.path(path.expand(cfg$parcelRDSDir),
                    sprintf("bca_vancouver_%s.rds", type))
  if (!file.exists(path))
    stop(sprintf("%s not found. Run bcaVancouverParcelExtract.R first.", path))
  as.data.table(readRDS(path))
}

loadSales <- function(cfg, dtParcels) {
  con <- dbConnect(SQLite(), cfg$sqliteFile)
  dt  <- as.data.table(dbGetQuery(con,
    paste("SELECT folioID, conveyanceDate, conveyancePrice,",
          "conveyanceTypeDescription FROM sales")))
  dbDisconnect(con)
  dt <- dt[conveyanceTypeDescription == "Improved Single Property Transaction",
           .(folioID, conveyanceDate, conveyancePrice)]
  dt[, folioID := as.character(folioID)]
  dtParcels <- copy(dtParcels)
  dtParcels[, folioID := as.character(folioID)]
  dt <- merge(dt, dtParcels, by = "folioID")
  dt[, conveyancePrice := as.numeric(conveyancePrice)]
  dt[, landWidth       := as.numeric(landWidth)]
  dt[, landDepth       := as.numeric(landDepth)]
  dt[, year            := as.numeric(substring(conveyanceDate, 1, 4))]
  dt[, sqft            := as.numeric(MB_total_finished_area)]
  dt[, age             := year - as.numeric(MB_effective_year)]
  dt[, landArea        := landWidth * landDepth]
  dt[]
}

loadPermits <- function(cfg) {
  dp <- fread(cfg$permitFile,
    select = c("PermitNumber","ProjectValue","TypeOfWork",
               "PropertyUse","SpecificUseCategory","geo_point_2d","YearMonth"))
  dp <- dp[PropertyUse == "Dwelling Uses" &
           TypeOfWork %in% c("New Building","Addition / Alteration")]
  dp[, c("permitLat","permitLon") := tstrsplit(geo_point_2d, ", ")]
  dp[, `:=`(permitLat = as.numeric(permitLat),
            permitLon = as.numeric(permitLon))]
  dp <- dp[!is.na(permitLat) & !is.na(permitLon)]
  dp[, Year := as.numeric(substring(YearMonth, 1, 4))]
  dp <- dp[Year %between% cfg$permitYearRange]
  dp <- dp[grepl("Duplex|Single Detached House|Laneway House|Multiple Dwelling",
                 SpecificUseCategory)]
  sfP <- st_as_sf(dp[, .(permitLat, permitLon)],
                  coords = c("permitLon","permitLat"), crs = 4326)
  dZ <- st_read(cfg$zoningGeojson, quiet = TRUE)
  dZ <- dZ[dZ$zoning_district == cfg$targetZone, "zoning_district"]
  sfP <- st_transform(sfP, st_crs(dZ))
  sfP <- st_join(sfP, dZ, join = st_within)
  sfP <- sfP[!is.na(sfP$zoning_district), ]
  sfP <- st_transform(sfP, 4326)
  data.table(lon = st_coordinates(sfP)[, 1],
             lat = st_coordinates(sfP)[, 2])
}

# ===========================================================================
# 5. SAMPLE BUILDERS
# ===========================================================================
mkSampleSF <- function(salesAll, cfg) {
  use <- propertyTypeMap$singleFamily
  d <- salesAll[actualUseDescription %in% use &
                year %between% cfg$salesYearRange &
                conveyancePrice > 0 & landArea > 0 &
                landWidth > 0 & landDepth > 0 &
                !is.na(age) & age >= 1]
  d[, ppsf := conveyancePrice / landArea]
  d <- d[is.finite(ppsf) & ppsf > 0]
  d <- trimByQuantile(d, c("landWidth","landDepth"), cfg$trimDimsQ)
  d <- trimByQuantile(d, "ppsf", cfg$trimYQ)
  d[, `:=`(y = log(ppsf), x = log(landArea))]
  d[!is.na(lon) & !is.na(lat) & is.finite(y) & is.finite(x)]
}

mkSampleStrata <- function(salesStrata, cfg) {
  use <- propertyTypeMap$strata
  d <- salesStrata[actualUseDescription %in% use &
                   year %between% cfg$salesYearRange &
                   conveyancePrice > 0 & !is.na(sqft) & sqft > 0]
  d[, ppsf := conveyancePrice / sqft]
  d <- d[is.finite(ppsf) & ppsf > 0]
  d <- trimByQuantile(d, "sqft", cfg$trimDimsQ)
  d <- trimByQuantile(d, "ppsf", cfg$trimYQ)
  d[, `:=`(y = log(ppsf), x = log(sqft))]
  d[!is.na(lon) & !is.na(lat) & is.finite(y) & is.finite(x)]
}

# Duplex price-per-sqft premium sample (Tsur 5/21/26 rev).
# LHS: log(conveyancePrice / MB_total_finished_area). Apples-to-apples
# per-sqft comparison between SF and duplex on ~33' lots. The GWR slope
# on isDuplex (returned as slope_duplexPremium33 downstream) is then a
# local log-ppsf premium for duplex vs SF.
mkSampleDuplexPremium <- function(salesAll, cfg) {
  useSF  <- propertyTypeMap$singleFamily
  useDup <- propertyTypeMap$duplex
  d <- salesAll[actualUseDescription %in% c(useSF, useDup) &
                year %between% cfg$salesYearRange &
                conveyancePrice > 0 & landWidth > 0 & landDepth > 0 &
                !is.na(sqft) & sqft > 0]
  d[, isDuplex := as.integer(actualUseDescription %in% useDup)]
  d <- d[landWidth %between% cfg$lotWidthBand]
  d[, ppsf := conveyancePrice / sqft]
  d <- d[is.finite(ppsf) & ppsf > 0]
  d <- trimByQuantile(d, "ppsf", cfg$trimYQ)
  d[, `:=`(y = log(ppsf), x = as.numeric(isDuplex))]
  d <- d[!is.na(lon) & !is.na(lat) & is.finite(y)]
  cat(sprintf("  width band [%.1f, %.1f]: n_SF = %d, n_duplex = %d\n",
              cfg$lotWidthBand[1], cfg$lotWidthBand[2],
              sum(d$isDuplex == 0), sum(d$isDuplex == 1)))
  d[]
}

sampleConfigs <- list(
  singleFamilyPre = list(
    builder     = mkSampleSF,
    dataSource  = "salesSF",
    label       = "Single-family, 2014-2018, land-ppsf vs log(lot area)",
    refValue    = NA_real_,
    fileTag     = "sfPre",
    extraGlobal = NULL),
  strataPre = list(
    builder     = mkSampleStrata,
    dataSource  = "salesStrata",
    label       = "Strata, 2014-2018, floor-ppsf vs log(unit sqft)",
    refValue    = 0,
    fileTag     = "strataPre",
    extraGlobal = NULL),
  duplexPremium33 = list(
    builder     = mkSampleDuplexPremium,
    dataSource  = "salesSFDuplex",
    label       = "Duplex price-per-sqft premium, 2014-2018, 33' lots",
    refValue    = NA_real_,
    fileTag     = "duplexPrem33",
    extraGlobal = function(d) {
      reg <- feols(y ~ x | year, data = d)
      cm  <- coeftable(reg)
      cat("\n  Simple duplex log-ppsf delta (year FE):\n")
      cat(sprintf("    beta = %.4f  SE = %.4f  ->  ppsf premium ~ %.1f%%\n",
                  cm["x", 1], cm["x", 2], 100 * (exp(cm["x", 1]) - 1)))
      invisible(reg)
    })
)

# ===========================================================================
# 6. PIPELINE
# ===========================================================================
dtSF      <- loadParcels(cfg, "singleFamily")
dtStrata  <- loadParcels(cfg, "strata")
dtDuplex  <- loadParcels(cfg, "duplex")

salesSF       <- loadSales(cfg, dtSF)
salesStrata   <- loadSales(cfg, dtStrata)
salesDuplex   <- loadSales(cfg, dtDuplex)
salesSFDuplex <- rbindlist(list(salesSF, salesDuplex), use.names = TRUE,
                           fill = TRUE)

permits   <- loadPermits(cfg)

cat("\n---- Applying Cambie corridor exclusion ----\n")
salesSF       <- applyExcludeBox(salesSF,       cfg, "salesSF")
salesStrata   <- applyExcludeBox(salesStrata,   cfg, "salesStrata")
salesDuplex   <- applyExcludeBox(salesDuplex,   cfg, "salesDuplex")
salesSFDuplex <- applyExcludeBox(salesSFDuplex, cfg, "salesSFDuplex")
permits       <- applyExcludeBox(permits,       cfg, "permits")

dataSources <- list(salesSF       = salesSF,
                    salesStrata   = salesStrata,
                    salesDuplex   = salesDuplex,
                    salesSFDuplex = salesSFDuplex)

cat(sprintf("\nPost-exclusion: SF = %d  strata = %d  duplex = %d  SF+duplex = %d  permits = %d\n",
            nrow(salesSF), nrow(salesStrata), nrow(salesDuplex),
            nrow(salesSFDuplex), nrow(permits)))

globalElast <- function(d) {
  reg <- feols(y ~ x | year, data = d)
  cm  <- coeftable(reg)
  list(beta = cm["x", 1], se = cm["x", 2], n = nrow(d))
}

resultsBoard <- list()

for (nm in names(sampleConfigs)) {
  sc <- sampleConfigs[[nm]]
  cat(sprintf("\n============== %s ==============\n", sc$label))
  src <- dataSources[[sc$dataSource]]
  if (is.null(src)) {
    cat(sprintf("Unknown dataSource '%s'; skipping.\n", sc$dataSource))
    next
  }
  d <- sc$builder(src, cfg)
  if (nrow(d) < 100) {
    cat(sprintf("Sample n = %d -- too small; skipping GWR.\n", nrow(d)))
    next
  }
  cat(sprintf("Sample n = %d  (years %d-%d)\n",
              nrow(d), as.integer(min(d$year)), as.integer(max(d$year))))

  ge <- globalElast(d)
  cat(sprintf("Global slope (year FE): beta = %.4f  SE = %.4f  n = %d\n",
              ge$beta, ge$se, ge$n))
  if (!is.na(sc$refValue)) {
    z <- (ge$beta - sc$refValue) / ge$se
    cat(sprintf("Test vs reference (%.2f): z = %.3f\n", sc$refValue, z))
  }
  if (!is.null(sc$extraGlobal)) sc$extraGlobal(d)

  dtLocal <- runGWR(d, yCol = "y", xCol = "x",
                    fitPointsLL = permits, cfg = cfg,
                    k = cfg$bandwidthK)
  nFail <- sum(!is.finite(dtLocal$slope))
  if (nFail > 0)
    cat(sprintf("GWR failed at %d / %d permit fit points (NA slope).\n",
                nFail, nrow(dtLocal)))
  cat("Local slope summary at permit fit points:\n")
  print(summary(dtLocal$slope))

  plotHeatmap(dtLocal[is.finite(slope)], "slope", cfg,
              legendTitle = sprintf("Local slope\n%s", sc$label),
              outPath     = sprintf("text/elasticity_%s.png", sc$fileTag))

  resultsBoard[[nm]] <- list(sample = d, gwr = dtLocal, globalElast = ge)
}

# ---------------------------------------------------------------------------
# Cross-sample slope comparison.
# ---------------------------------------------------------------------------
if (length(resultsBoard) >= 2) {
  cmp <- data.table(lon = permits$lon, lat = permits$lat)
  for (nm in names(resultsBoard)) {
    cmp[[paste0("slope_",   nm)]] <- resultsBoard[[nm]]$gwr$slope
    cmp[[paste0("seSlope_", nm)]] <- resultsBoard[[nm]]$gwr$seSlope
  }
  cat("\n============== Cross-sample slope correlations at permits ==============\n")
  num <- cmp[, .SD, .SDcols = patterns("^slope_")]
  print(round(cor(num, use = "complete.obs"), 3))
}

# ===========================================================================
# 7. THREE MATCHED LOCAL-PRICE-LEVEL SURFACES (one per race)
# ===========================================================================
# For each of the three samples, residualize y on the global linear FE
# structure (sample-appropriate), then run intercept-only GWR on the
# residuals at the permit fit points.
#
# Residualization controls (sample-by-sample):
#   singleFamilyPre  -- y = log(land ppsf).      Control: log(age) + year FE.
#   strataPre        -- y = log(floor ppsf).     Control: log(sqft) + year FE.
#   duplexPremium33  -- y = log(price/sqft).     Control: isDuplex + year FE.
#                       (post 5/21/26: y is already in ppsf space.)

if (!is.null(resultsBoard$singleFamilyPre)) {
  cat("\n---- SF localPPSF (residualized log ppsf, log(age) + year FE) ----\n")
  dSF <- resultsBoard$singleFamilyPre$sample
  regResSF <- feols(y ~ log(age) | year, data = dSF)
  dSF[, yResid := y - predict(regResSF, newdata = dSF)]
  localPPSF_singleFamilyPre <- runGWRMean(dSF, yCol = "yResid",
                                          fitPointsLL = permits, cfg = cfg,
                                          k = cfg$bandwidthK)
  if (exists("cmp")) {
    cmp[, localPPSF_singleFamilyPre := localPPSF_singleFamilyPre]
    cmp[, localPPSF := localPPSF_singleFamilyPre]
  }
  cat(sprintf("  finite values: %d / %d\n",
              sum(is.finite(localPPSF_singleFamilyPre)),
              length(localPPSF_singleFamilyPre)))
}

if (!is.null(resultsBoard$strataPre)) {
  cat("\n---- Strata localPPSF (residualized log ppsf, log(sqft) + year FE) ----\n")
  dS <- resultsBoard$strataPre$sample
  regResS <- feols(y ~ x | year, data = dS)
  dS[, yResid := y - predict(regResS, newdata = dS)]
  localPPSF_strataPre <- runGWRMean(dS, yCol = "yResid",
                                    fitPointsLL = permits, cfg = cfg,
                                    k = cfg$bandwidthK)
  if (exists("cmp")) cmp[, localPPSF_strataPre := localPPSF_strataPre]
  cat(sprintf("  finite values: %d / %d\n",
              sum(is.finite(localPPSF_strataPre)),
              length(localPPSF_strataPre)))
}

if (!is.null(resultsBoard$duplexPremium33)) {
  cat("\n---- Duplex33 localPPSF (residualized log ppsf, isDuplex + year FE) ----\n")
  dD <- resultsBoard$duplexPremium33$sample
  regResD <- feols(y ~ x | year, data = dD)
  dD[, yResid := y - predict(regResD, newdata = dD)]
  localPPSF_duplexPremium33 <- runGWRMean(dD, yCol = "yResid",
                                          fitPointsLL = permits, cfg = cfg,
                                          k = cfg$bandwidthK)
  if (exists("cmp"))
    cmp[, localPPSF_duplexPremium33 := localPPSF_duplexPremium33]
  cat(sprintf("  finite values: %d / %d\n",
              sum(is.finite(localPPSF_duplexPremium33)),
              length(localPPSF_duplexPremium33)))
}

# ===========================================================================
# 7B. COV ANALOGUE OF GREATER-VAN: cor(localPPSF, localSlope) at permits
# ===========================================================================
if (!is.null(resultsBoard$singleFamilyPre) &&
    exists("localPPSF_singleFamilyPre")) {
  cat("\n============== COV: cor(localPPSF, localSlope) at permits ==============\n")
  dtSlope <- resultsBoard$singleFamilyPre$gwr

  covDiag <- data.table(lon          = permits$lon,
                        lat          = permits$lat,
                        localPPSF    = localPPSF_singleFamilyPre,
                        localSlope   = dtSlope$slope,
                        seLocalSlope = dtSlope$seSlope)
  ok <- covDiag[is.finite(localPPSF) & is.finite(localSlope) &
                is.finite(seLocalSlope) & seLocalSlope > 0]
  cat(sprintf("Permits with finite estimates: %d / %d\n",
              nrow(ok), nrow(covDiag)))

  if (nrow(ok) >= 20) {
    seCut <- quantile(ok$seLocalSlope, 0.75, na.rm = TRUE)
    okTrim <- ok[seLocalSlope <= seCut]
    wCor <- function(x, y, w) {
      w <- w / sum(w)
      mx <- sum(w * x); my <- sum(w * y)
      cxy <- sum(w * (x - mx) * (y - my))
      vx  <- sum(w * (x - mx)^2)
      vy  <- sum(w * (y - my)^2)
      cxy / sqrt(vx * vy)
    }
    diagCOV <- rbindlist(list(
      data.table(flavour = "unweighted",
                 cor = cor(ok$localPPSF, ok$localSlope),
                 n   = nrow(ok)),
      data.table(flavour = "precision_wtd_1_over_se2",
                 cor = wCor(ok$localPPSF, ok$localSlope,
                            1 / ok$seLocalSlope^2),
                 n   = nrow(ok)),
      data.table(flavour = "trim_se_below_q0.75",
                 cor = if (nrow(okTrim) >= 5)
                   cor(okTrim$localPPSF, okTrim$localSlope) else NA_real_,
                 n   = nrow(okTrim))
    ))
    diagCOV[, level := sprintf("COV_permits_k%d", cfg$bandwidthK)]
    setcolorder(diagCOV, c("level","flavour","cor","n"))
    print(diagCOV)
    saveRDS(diagCOV,
            "~/DropboxExternal/dataProcessed/covElasticityPrice_diagCOV.rds")

    okPlot <- ok[, .(localPPSF, localSlope,
                     w = 1 / seLocalSlope^2)]
    pScatter <- ggplot(okPlot,
                       aes(x = localPPSF, y = localSlope, size = w)) +
      geom_point(alpha = 0.45) +
      scale_size(trans = "sqrt", guide = "none") +
      geom_hline(yintercept = 0, linetype = "dotted") +
      labs(title = sprintf("COV permits, k=%d: localPPSF vs localSlope (SF)",
                           cfg$bandwidthK),
           subtitle = "point size proportional to 1/SE^2 of localSlope",
           x = "localPPSF (location effect on log ppsf, age/year-residualized)",
           y = "localSlope (elasticity of log ppsf wrt log lot area)") +
      theme_minimal()
    ggsave("text/cov_localPPSF_vs_localSlope_SF.png", plot = pScatter,
           width = 6.5, height = 5, dpi = 130)
  } else {
    cat("Too few permits with finite estimates; skipping cor.\n")
  }
}

# ===========================================================================
# 8. UNIFIED EXPORT for downstream scripts (e.g. vancouverDuplexChoice.R)
# ===========================================================================
if (exists("cmp")) {
  cat(sprintf("\nUnified export: %d permits x %d columns\n",
              nrow(cmp), ncol(cmp)))
  print(names(cmp))
  saveRDS(cmp,
          "~/DropboxExternal/dataProcessed/covElasticityPrice_permitSurfaces.rds")
}

cat("\nDone.\n")
