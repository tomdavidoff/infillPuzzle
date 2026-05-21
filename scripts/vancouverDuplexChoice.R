# vancouverDuplexChoice.R
# Downstream of vancouverElasticityPrice.R.
# Permit-level discrete choice: does a permit build a duplex or a single
# detached house? Regressors are:
#   - permitLogArea           -- log(lot area), permit's own scale choice
#   - logBcaLandPPSF          -- BCA 2019 assessed land value per sqft
#                                of lot (at the permit's own parcel).
#                                Replaces the three sales-GWR localPPSF
#                                surfaces previously used. One measure
#                                of local land value, not three; built
#                                by appraisers rather than from sales;
#                                universal coverage; matched in time
#                                to the permit window (BCA 2019 vs
#                                permits 2019-2023).
#   - slope_singleFamilyPre   -- local SF elasticity surface
#   - slope_strataPre         -- local strata elasticity surface
#   - slope_duplexPremium33   -- local duplex log-price premium on 33' lots
#   - avgSlope                -- composite of the three slopes (sign-
#                                aligned, z-scored, averaged). Z-scoring
#                                is REQUIRED here to give each surface
#                                equal weight in the composite, and
#                                sign-flipping is required to put the
#                                duplex premium on the same "positive
#                                predicts duplex" axis as the other two
#                                slopes. Other regressors enter in
#                                their natural units.
#
# Horse races: nine specs total. Model column order in the etable
# output:
#   (1) A      : BCA only
#   (2) B-SF   : SF slope only
#   (3) B-S    : strata slope only
#   (4) B-D    : duplex premium only
#   (5) B-avg  : avg slope only
#   (6) C-SF   : BCA + SF slope
#   (7) C-S    : BCA + strata slope
#   (8) C-D    : BCA + duplex premium
#   (9) C-avg  : BCA + avg slope
#
# Hypothesis: BCA land value alone (spec A) should be a strong predictor;
# adding any one elasticity surface (C specs) should add little once
# land value is conditioned on.
#
# Inference: Conley SEs at cutoff = 2 * GWR median effective radius.
#
# Tom Davidoff
# 05/18/26
# 05/20/26 (rev: BCA land value replaces the three sales-GWR localPPSF
#           surfaces; z-scoring confined to the avgSlope construction;
#           etable calls stripped to defaults to avoid fixest 0.13.2
#           argument-parsing quirks; user can hand-edit the TeX output
#           for the paper)

library(data.table)
library(sf)
library(fixest)
library(ggplot2)
library(ggspatial)
library(RSQLite)

# ---------------------------------------------------------------------------
# Config -- paths must match vancouverElasticityPrice.R
# ---------------------------------------------------------------------------
cfg <- list(
  crsProj           = 26910,
  permitFile        = "~/DropboxExternal/dataRaw/issued-building-permits.csv",
  zoningGeojson    = "~/DropboxExternal/dataRaw/vancouver_zoning.geojson",
  targetZone        = "R1-1",
  permitYearRange   = c(2019, 2023),
  parcelRDSDir      = "~/DropboxExternal/dataProcessed/",
  surfacesRDS       = "~/DropboxExternal/dataProcessed/covElasticityPrice_permitSurfaces.rds",
  sqliteFile        = "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3",
  conleyCutoffKM    = 3.0,
  xlim              = c(-123.23, -123.02),
  ylim              = c( 49.20,   49.32),
  tileZoom          = 12,
  outDir            = "text/",
  width33           = 33,
  width50           = 50,
  excludeBox        = list(
    lonMin = -123.130,
    lonMax = -123.101,
    latMax =   49.258,
    latMin = -Inf
  )
)

# ---------------------------------------------------------------------------
# 1. Load helpers
# ---------------------------------------------------------------------------
loadPermitsTagged <- function(cfg) {
  dp <- fread(cfg$permitFile,
    select = c("PermitNumber","ProjectValue","TypeOfWork",
               "PropertyUse","SpecificUseCategory",
               "geo_point_2d","YearMonth"))
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
  sfP <- st_as_sf(dp[, .(permitLat, permitLon, SpecificUseCategory)],
                  coords = c("permitLon","permitLat"), crs = 4326)
  dZ <- st_read(cfg$zoningGeojson, quiet = TRUE)
  dZ <- dZ[dZ$zoning_district == cfg$targetZone, "zoning_district"]
  sfP <- st_transform(sfP, st_crs(dZ))
  sfP <- st_join(sfP, dZ, join = st_within)
  sfP <- sfP[!is.na(sfP$zoning_district), ]
  sfP <- st_transform(sfP, 4326)
  data.table(lon    = st_coordinates(sfP)[, 1],
             lat    = st_coordinates(sfP)[, 2],
             useCat = sfP$SpecificUseCategory)
}

attachOwnLotArea <- function(permits, dtSF, cfg) {
  sfB <- dtSF[!is.na(dtSF$landWidth) & !is.na(dtSF$landDepth) &
              dtSF$landWidth > 0 & dtSF$landDepth > 0, ]
  sfB$landArea <- as.numeric(sfB$landWidth) * as.numeric(sfB$landDepth)
  sfB <- sfB[sfB$landArea > 0, ]
  sfB <- st_transform(sfB, cfg$crsProj)
  sfB <- st_make_valid(sfB)

  sfP <- st_transform(
            st_as_sf(copy(permits), coords = c("lon","lat"),
                     crs = 4326, remove = FALSE),
            cfg$crsProj)
  idx <- st_within(sfP, sfB)
  hit <- lengths(idx) > 0
  first <- vapply(idx, function(x) if (length(x) > 0) x[1] else NA_integer_,
                  integer(1))

  permits[, permitLandArea  := NA_real_]
  permits[, permitLandWidth := NA_real_]
  permits[hit, permitLandArea  := sfB$landArea[first[hit]]]
  permits[hit, permitLandWidth := as.numeric(sfB$landWidth[first[hit]])]
  permits[, permitLogArea   := log(permitLandArea)]

  cat(sprintf("Permits on a former-SF parcel: %d / %d (%.1f%%)\n",
              sum(hit), length(hit), 100 * mean(hit)))
  permits[]
}

# Attach BCA 2019 land value per sqft of lot. Pulls landValue from the
# SQLite `valuation` table, merges onto dtSF by folioID, attaches to
# permits via point-in-polygon. Adds bcaLandPPSF and logBcaLandPPSF
# columns to `permits` in place.
attachBcaLandPPSF <- function(permits, dtSF, cfg) {
  con <- dbConnect(SQLite(), path.expand(cfg$sqliteFile))
  dtVal <- as.data.table(dbGetQuery(con,
    "SELECT folioID, landValue FROM valuation"))
  dbDisconnect(con)
  dtVal[, folioID   := as.character(folioID)]
  dtVal[, landValue := as.numeric(landValue)]
  dtVal <- dtVal[!is.na(landValue) & landValue > 0]
  dtVal <- unique(dtVal, by = "folioID")

  dtSFwithLand <- copy(dtSF)
  dtSFwithLand$folioID <- as.character(dtSFwithLand$folioID)
  dtSFwithLand <- merge(dtSFwithLand, dtVal, by = "folioID", all.x = TRUE)

  sfB <- dtSFwithLand[!is.na(dtSFwithLand$landWidth) &
                      !is.na(dtSFwithLand$landDepth) &
                      dtSFwithLand$landWidth > 0 &
                      dtSFwithLand$landDepth > 0, ]
  sfB$landArea     <- as.numeric(sfB$landWidth) * as.numeric(sfB$landDepth)
  sfB$bcaLandValue <- as.numeric(sfB$landValue)
  sfB <- sfB[sfB$landArea > 0 & !is.na(sfB$bcaLandValue) &
             sfB$bcaLandValue > 0, ]
  sfB$bcaLandPPSF <- sfB$bcaLandValue / sfB$landArea
  sfB <- st_transform(sfB, cfg$crsProj)
  sfB <- st_make_valid(sfB)

  sfP <- st_transform(
            st_as_sf(copy(permits), coords = c("lon","lat"),
                     crs = 4326, remove = FALSE),
            cfg$crsProj)
  idx   <- st_within(sfP, sfB)
  hit   <- lengths(idx) > 0
  first <- vapply(idx, function(x) if (length(x) > 0) x[1] else NA_integer_,
                  integer(1))

  permits[, bcaLandPPSF    := NA_real_]
  permits[hit, bcaLandPPSF := sfB$bcaLandPPSF[first[hit]]]
  permits[, logBcaLandPPSF := log(bcaLandPPSF)]

  cat(sprintf("Permits with BCA land-ppsf: %d / %d (%.1f%%)\n",
              sum(!is.na(permits$bcaLandPPSF)),
              nrow(permits),
              100 * mean(!is.na(permits$bcaLandPPSF))))
  permits[]
}

applyExcludeBox <- function(dt, cfg) {
  bx <- cfg$excludeBox
  if (is.null(bx)) return(dt)
  in_box <- dt$lon >= bx$lonMin & dt$lon <= bx$lonMax &
            dt$lat >= bx$latMin & dt$lat <= bx$latMax
  cat(sprintf("applyExcludeBox: dropping %d / %d rows in Cambie box\n",
              sum(in_box, na.rm = TRUE), nrow(dt)))
  dt[!in_box | is.na(in_box)]
}

# ---------------------------------------------------------------------------
# 2. Load surfaces
# ---------------------------------------------------------------------------
if (!file.exists(path.expand(cfg$surfacesRDS)))
  stop(sprintf("%s not found. Run vancouverElasticityPrice.R first.",
               cfg$surfacesRDS))
surfaces <- as.data.table(readRDS(path.expand(cfg$surfacesRDS)))
cat(sprintf("\nSurfaces export: %d rows, columns:\n", nrow(surfaces)))
print(names(surfaces))

# ---------------------------------------------------------------------------
# 3. Load permits, attach lot area + BCA land ppsf, classify isDuplex
# ---------------------------------------------------------------------------
permits <- loadPermitsTagged(cfg)
cat(sprintf("\nPermits in R1-1, %d-%d: %d total\n",
            cfg$permitYearRange[1], cfg$permitYearRange[2], nrow(permits)))
print(permits[, .N, by = useCat][order(-N)])

permits <- applyExcludeBox(permits, cfg)

if (nrow(permits) != nrow(surfaces))
  warning(sprintf(paste("Permit count (%d) != surfaces row count (%d).",
                        "Check that vancouverElasticityPrice.R was run",
                        "with the same cfg$permitYearRange, zoning filter,",
                        "AND excludeBox."),
                  nrow(permits), nrow(surfaces)))

choiceDT <- cbind(permits, surfaces[, !c("lon","lat"), with = FALSE])

dtSF <- readRDS(file.path(path.expand(cfg$parcelRDSDir),
                          "bca_vancouver_singleFamily.rds"))
choiceDT <- attachOwnLotArea(choiceDT, dtSF, cfg)
choiceDT <- attachBcaLandPPSF(choiceDT, dtSF, cfg)

hasMultiple <- grepl("Multiple", choiceDT$useCat, ignore.case = TRUE)
hasDuplex   <- grepl("uplex|Two-Family|Two Family", choiceDT$useCat,
                     ignore.case = TRUE)
hasSF       <- grepl("Single Detached", choiceDT$useCat, ignore.case = TRUE)
choiceDT[, isDuplex := NA_integer_]
choiceDT[!hasMultiple & hasDuplex & !hasSF, isDuplex := 1L]
choiceDT[!hasMultiple & hasSF     & !hasDuplex, isDuplex := 0L]
cat("\nisDuplex classification (post-Multiple/mixed drop):\n")
print(choiceDT[, .N, by = .(isDuplex,
            ifelse(grepl("Suite", useCat), "with suite", "no suite"))][
            order(isDuplex)])

choiceDT[, formerSF := !is.na(permitLandArea)]
cat(sprintf("\nformer-SF restriction: %d / %d permits retained\n",
            sum(choiceDT$formerSF), nrow(choiceDT)))

# ---------------------------------------------------------------------------
# 4. Estimation sample + avgSlope construction
# ---------------------------------------------------------------------------
needed <- c("isDuplex","permitLogArea","logBcaLandPPSF",
            "slope_singleFamilyPre",
            "slope_strataPre",
            "slope_duplexPremium33")
est <- choiceDT[formerSF == TRUE &
                complete.cases(choiceDT[, ..needed]) &
                is.finite(permitLogArea) &
                is.finite(logBcaLandPPSF)]
cat(sprintf("\nEstimation sample: n = %d  (duplex = %d, SF = %d)\n",
            nrow(est), sum(est$isDuplex == 1L), sum(est$isDuplex == 0L)))

# ---- avgSlope: the ONE place z-scoring is used ---------------------------
# Sign-align each slope so positive = predicts duplex, z-score on est,
# then average. avgSlope is the only constructed regressor that uses
# z-scoring. All other regressors enter in natural units.
zscore <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x,   na.rm = TRUE)
  (x - m) / s
}
est[, avgSlope := rowMeans(cbind(
  zscore( slope_singleFamilyPre),
  zscore( slope_strataPre),
  zscore(-slope_duplexPremium33)),
  na.rm = FALSE)]

cat("\nCorrelations among regressors on est (natural units, except avgSlope):\n")
diagCols <- c("logBcaLandPPSF",
              "slope_singleFamilyPre",
              "slope_strataPre",
              "slope_duplexPremium33",
              "avgSlope")
print(round(cor(est[, ..diagCols], use = "complete.obs"), 3))

# ---------------------------------------------------------------------------
# 5. Horse races
# ---------------------------------------------------------------------------
mkFml <- function(rhs) as.formula(sprintf("isDuplex ~ permitLogArea + %s", rhs))

fitLogit <- function(fml, dat)
  feglm(fml, data = dat, family = binomial(link = "logit"))

ame <- function(model) {
  X    <- model.matrix(model)
  b    <- coef(model)
  eta  <- as.vector(X %*% b)
  dens <- dlogis(eta)
  vapply(colnames(X)[-1],
         function(v) mean(dens) * b[v], numeric(1))
}

conleyVcov <- conley(cutoff = cfg$conleyCutoffKM, distance = "spherical")

dir.create(path.expand(cfg$outDir), showWarnings = FALSE, recursive = TRUE)

# A spec
mA <- fitLogit(mkFml("logBcaLandPPSF"), est)

# B specs (each elasticity alone)
mB_SF  <- fitLogit(mkFml("slope_singleFamilyPre"), est)
mB_S   <- fitLogit(mkFml("slope_strataPre"),       est)
mB_D   <- fitLogit(mkFml("slope_duplexPremium33"), est)
mB_avg <- fitLogit(mkFml("avgSlope"),              est)

# C specs (BCA + each elasticity)
mC_SF  <- fitLogit(mkFml("logBcaLandPPSF + slope_singleFamilyPre"), est)
mC_S   <- fitLogit(mkFml("logBcaLandPPSF + slope_strataPre"),       est)
mC_D   <- fitLogit(mkFml("logBcaLandPPSF + slope_duplexPremium33"), est)
mC_avg <- fitLogit(mkFml("logBcaLandPPSF + avgSlope"),              est)

allFits <- list(mA,
                mB_SF, mB_S, mB_D, mB_avg,
                mC_SF, mC_S, mC_D, mC_avg)

cat("\n========== HORSE RACE: BCA land value vs elasticities ==========\n")

cat("\n-- Default SEs --\n")
print(etable(mA, mB_SF, mB_S, mB_D, mB_avg, mC_SF, mC_S, mC_D, mC_avg))

cat("\n-- Conley SEs --\n")
print(etable(mA, mB_SF, mB_S, mB_D, mB_avg, mC_SF, mC_S, mC_D, mC_avg,
             vcov = conleyVcov))

# TeX dump -- everything, default formatting. Hand-edit for the paper.
etable(mA, mB_SF, mB_S, mB_D, mC_D, mC_avg,
       vcov = conleyVcov,
       tex = TRUE,
       file = file.path(cfg$outDir, "horseRace_all.tex"),
       replace = TRUE)

cat("\n-- Average marginal effects --\n")
for (i in seq_along(allFits)) {
  cat(sprintf("Model %d:\n", i))
  print(round(ame(allFits[[i]]), 4))
}

cat("\nDone with estimation.\n")

# ---------------------------------------------------------------------------
# 6. Heatmap helpers
# ---------------------------------------------------------------------------
plotChoiceHeatmap <- function(dt, cfg, exactWidth, label, outPath,
                              size = 1.4) {
  dtSub <- dt[formerSF == TRUE &
              !is.na(isDuplex) &
              !is.na(permitLandWidth) &
              round(permitLandWidth) == exactWidth]
  cat(sprintf("\n%s heatmap (round(width)==%g ft): n=%d  duplex=%d  SF=%d\n",
              label, exactWidth, nrow(dtSub),
              sum(dtSub$isDuplex == 1L), sum(dtSub$isDuplex == 0L)))
  dtSub[, choice := factor(isDuplex, levels = c(0L, 1L),
                           labels = c("Single Detached", "Duplex"))]
  p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = cfg$tileZoom) +
    geom_point(data = dtSub, aes(x = lon, y = lat, color = choice),
               size = size, alpha = 0.85) +
    scale_color_manual(values = c("Single Detached" = "#1f77b4",
                                  "Duplex"          = "#d62728")) +
    coord_sf(crs = 4326, xlim = cfg$xlim, ylim = cfg$ylim) +
    theme_void() +
    labs(color = NULL,
         title = sprintf("Permit choice on %s lots (round(width)==%g ft)",
                         label, exactWidth))
  ggsave(outPath, plot = p, width = 8, height = 7, dpi = 200)
  invisible(p)
}

# Continuous-surface heatmap. Viridis gradient. Legend title rotated
# vertically beside the color bar to save horizontal space. No plot title.
plotSurfaceHeatmap <- function(dt, valueCol, cfg, legendTitle, outPath,
                               size = 1.0, option = "plasma") {
  dtSub <- dt[!is.na(get(valueCol))]
  cat(sprintf("\nSurface heatmap [%s]: n=%d  range=[%.3g, %.3g]\n",
              valueCol, nrow(dtSub),
              min(dtSub[[valueCol]]), max(dtSub[[valueCol]])))
  p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = cfg$tileZoom) +
    geom_point(data = dtSub,
               aes(x = lon, y = lat, color = .data[[valueCol]]),
               size = size, alpha = 0.85) +
    scale_color_viridis_c(option = option) +
    coord_sf(crs = 4326, xlim = cfg$xlim, ylim = cfg$ylim) +
    theme_void() +
    theme(legend.title = element_text(angle = 90),
          legend.title.position = "left") +
    labs(color = legendTitle)
  ggsave(outPath, plot = p, width = 8, height = 7, dpi = 200)
  invisible(p)
}

# Choice heatmaps
plotChoiceHeatmap(choiceDT, cfg, cfg$width33, "33-ft",
                  file.path(cfg$outDir, "duplexChoice_w33.png"))
plotChoiceHeatmap(choiceDT, cfg, cfg$width50, "50-ft",
                  file.path(cfg$outDir, "duplexChoice_w50.png"))

# Surface heatmaps
plotSurfaceHeatmap(choiceDT, "logBcaLandPPSF", cfg,
                   "BCA log(land $/sqft)",
                   file.path(cfg$outDir, "surface_logBcaLandPPSF.png"))
plotSurfaceHeatmap(choiceDT, "slope_singleFamilyPre", cfg,
                   "SF slope",
                   file.path(cfg$outDir, "surface_slope_singleFamilyPre.png"))
plotSurfaceHeatmap(choiceDT, "slope_strataPre", cfg,
                   "Strata slope",
                   file.path(cfg$outDir, "surface_slope_strataPre.png"))
plotSurfaceHeatmap(choiceDT, "slope_duplexPremium33", cfg,
                   "Duplex premium slope (33-ft)",
                   file.path(cfg$outDir, "surface_slope_duplexPremium33.png"))

cat("\nDone.\n")
