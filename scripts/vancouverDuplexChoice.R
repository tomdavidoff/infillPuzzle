# vancouverDuplexChoice.R
# Downstream of vancouverElasticityPrice.R.
# Permit-level discrete choice: does a permit build a duplex or a single
# detached house? Regressors are the local price level and local
# elasticity surfaces estimated at each permit's location.
#
# Spec strategy (per Tom): three horse races, one per elasticity surface.
# In each race, three nested logits:
#   A) localPPSF only
#   B) the elasticity surface only
#   C) both together
# All specs include the permit's OWN log lot area (developer's scale
# choice variable, separate from neighborhood surfaces).
#
# Hypothesis: localPPSF should beat each elasticity in its horse race,
# because ppsf and the regulation-induced slope are observationally
# (nearly) equivalent to builders. The "C" columns tell us whether the
# elasticity surface carries any incremental information once the price
# level is conditioned on.
#
# Inference: Conley SEs at cutoff = 2 * GWR median effective radius,
# to handle spatial autocorrelation in the constructed regressors.
#
# Tom Davidoff
# 05/18/26

library(data.table)
library(sf)
library(fixest)
library(ggplot2)
library(ggspatial)

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
  # Conley cutoff in km. Set roughly to 2 * median GWR radius, to be
  # tightened/loosened below after we see the median radius from the
  # surfaces export. 3 km is a defensible default for k~750 GWR in COV.
  conleyCutoffKM    = 3.0,
  # Plotting (match vancouverLotPlex.R conventions)
  xlim              = c(-123.23, -123.02),
  ylim              = c( 49.20,   49.32),
  tileZoom          = 12,
  outDir            = "text/",
  # Width filter: exact landWidth (rounded). 33 = "33-ft lot", 50 = "50-ft lot".
  width33           = 33,
  width50           = 50,
  # Cambie corridor exclusion: Oak St (W) to Main St (E), south of W
  # 16th Ave, southern edge open. Drops sales/permits where SF
  # transactions reflect redevelopment-option value from the Cambie
  # corridor upzoning (and its bleed into adjacent west-side blocks)
  # rather than the SF-density structural pricing the model expects.
  # Coordinates: Oak ~-123.130, Main ~-123.101, W 16th lat ~49.258.
  excludeBox        = list(
    lonMin = -123.130,
    lonMax = -123.101,
    latMax =   49.258,
    latMin = -Inf
  )
)

# ---------------------------------------------------------------------------
# 1. Load permits with use category (need isDuplex AND lon/lat in the
#    SAME order as the surfaces export -- which means using the SAME
#    loadPermits filter chain, but retaining SpecificUseCategory).
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

# Attach each permit's OWN log lot area AND lot width by point-in-polygon
# join: permit centroid must fall within a BCA single-family parcel
# polygon. Permits that don't land on an SF parcel (e.g. on an existing
# duplex, multi-family, vacant non-residential, or off-parcel) get NA
# and are dropped downstream. Enforces "former single family" sample.
attachOwnLotArea <- function(permits, dtSF, cfg) {
  # dtSF is an sf object with polygon geometry (see bcaVancouverParcelExtract.R).
  # Filter to parcels with usable width/depth.
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

# Drop rows inside cfg$excludeBox (Cambie corridor). Applied to any
# data.table with lon/lat. NULL or missing box -> no-op.
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
# 2. Load surfaces from the unified export
# ---------------------------------------------------------------------------
if (!file.exists(path.expand(cfg$surfacesRDS)))
  stop(sprintf("%s not found. Run vancouverElasticityPrice.R first.",
               cfg$surfacesRDS))
surfaces <- as.data.table(readRDS(path.expand(cfg$surfacesRDS)))
cat(sprintf("\nSurfaces export: %d rows, columns:\n", nrow(surfaces)))
print(names(surfaces))

# ---------------------------------------------------------------------------
# 3. Load permits, tag isDuplex, attach own lot area
# ---------------------------------------------------------------------------
permits <- loadPermitsTagged(cfg)
cat(sprintf("\nPermits in R1-1, %d-%d: %d total\n",
            cfg$permitYearRange[1], cfg$permitYearRange[2], nrow(permits)))
print(permits[, .N, by = useCat][order(-N)])

# Cambie corridor exclusion. Must be applied here (BEFORE the cbind
# with surfaces) because vancouverElasticityPrice.R applies the same
# exclusion to its permits before estimating surfaces. With both sides
# excised in the same order, the row counts and order match.
permits <- applyExcludeBox(permits, cfg)

if (nrow(permits) != nrow(surfaces))
  warning(sprintf(paste("Permit count (%d) != surfaces row count (%d).",
                        "Check that vancouverElasticityPrice.R was run",
                        "with the same cfg$permitYearRange, zoning filter,",
                        "AND excludeBox."),
                  nrow(permits), nrow(surfaces)))

# Permits and surfaces are row-aligned by construction (same loadPermits
# filter chain + same Cambie exclusion in both scripts). Bind column-wise.
choiceDT <- cbind(permits, surfaces[, !c("lon","lat"), with = FALSE])

# Attach permit's own log lot area from nearest SF parcel.
dtSF <- readRDS(file.path(path.expand(cfg$parcelRDSDir),
                          "bca_vancouver_singleFamily.rds"))
choiceDT <- attachOwnLotArea(choiceDT, dtSF, cfg)

# Binary outcome: 1 = Duplex (any flavor, including w/ secondary suite,
# lock-off, infill duplex, two-family), 0 = Single Detached House (any
# flavor, including w/ secondary suite).
#
# Rules:
#   - Anything containing "Multiple" (Multiple Dwelling, Multiple
#     Conversion Dwelling, Infill Multiple Dwelling) -> NA. These are
#     3+ unit structures, not the SF-vs-duplex binary.
#   - Mixed permits naming BOTH "Single Detached" and "uplex"/"Two-
#     Family" -> NA (ambiguous; very few).
#   - "uplex" or "Two-Family" in useCat and not mixed -> 1.
#   - "Single Detached" in useCat and not mixed -> 0.
#   - Everything else (Laneway House alone, etc.) -> NA.
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

# Sample restriction: permit's own parcel was a former single-family
# parcel (point-in-polygon hit in attachOwnLotArea).
choiceDT[, formerSF := !is.na(permitLandArea)]
cat(sprintf("\nformer-SF restriction: %d / %d permits retained\n",
            sum(choiceDT$formerSF), nrow(choiceDT)))

# ---------------------------------------------------------------------------
# 4. Estimation sample
# ---------------------------------------------------------------------------
# Build the composite avgSlope surface (mean of the three local slope
# surfaces) BEFORE constructing the estimation sample, so it can be
# required and included in the cor matrix.
choiceDT[, avgSlope := rowMeans(.SD, na.rm = FALSE),
         .SDcols = c("slope_singleFamilyPre",
                     "slope_strataPre",
                     "slope_duplexPremium33")]

needed <- c("isDuplex","permitLogArea",
            "localPPSF_singleFamilyPre",
            "localPPSF_strataPre",
            "localPPSF_duplexPremium33",
            "slope_singleFamilyPre","slope_strataPre",
            "slope_duplexPremium33","avgSlope")
est <- choiceDT[formerSF == TRUE &
                complete.cases(choiceDT[, ..needed]) &
                is.finite(permitLogArea)]
cat(sprintf("\nEstimation sample: n = %d  (duplex = %d, SF = %d)\n",
            nrow(est), sum(est$isDuplex == 1L), sum(est$isDuplex == 0L)))

# Diagnostic: correlations among the constructed regressors.
cat("\nCorrelations among constructed regressors:\n")
regCols <- c("localPPSF_singleFamilyPre",
             "localPPSF_strataPre",
             "localPPSF_duplexPremium33",
             "slope_singleFamilyPre","slope_strataPre",
             "slope_duplexPremium33","avgSlope")
print(round(cor(est[, ..regCols], use = "complete.obs"), 3))

# ---------------------------------------------------------------------------
# 5. Horse races
# ---------------------------------------------------------------------------
# Four races. In each race three specs:
#   A) isDuplex ~ permitLogArea + <local_ppsf>
#   B) isDuplex ~ permitLogArea + <elasticity>
#   C) isDuplex ~ permitLogArea + <local_ppsf> + <elasticity>
# The ppsf paired with each elasticity is the sector-specific local
# price level (so e.g. the strata race uses strata localPPSF, not SF
# localPPSF). The fourth race uses SF localPPSF and the three-slope
# average -- a single composite "average elasticity" specification.

races <- list(
  SF      = list(ppsf  = "localPPSF_singleFamilyPre",
                 elast = "slope_singleFamilyPre"),
  strata  = list(ppsf  = "localPPSF_strataPre",
                 elast = "slope_strataPre"),
  duplex  = list(ppsf  = "localPPSF_duplexPremium33",
                 elast = "slope_duplexPremium33"),
  avgSlope = list(ppsf  = "localPPSF_singleFamilyPre",
                  elast = "avgSlope")
)

mkFmlA <- function(ppsf) as.formula(sprintf(
            "isDuplex ~ permitLogArea + %s", ppsf))
mkFmlB <- function(elast) as.formula(sprintf(
            "isDuplex ~ permitLogArea + %s", elast))
mkFmlC <- function(ppsf, elast) as.formula(sprintf(
            "isDuplex ~ permitLogArea + %s + %s", ppsf, elast))

fitLogit <- function(fml, dat)
  feglm(fml, data = dat, family = binomial(link = "logit"))

# AMEs via mean-of-derivatives (continuous regressors)
ame <- function(model) {
  X    <- model.matrix(model)
  b    <- coef(model)
  eta  <- as.vector(X %*% b)
  dens <- dlogis(eta)
  vapply(colnames(X)[-1],
         function(v) mean(dens) * b[v], numeric(1))
}

# Conley SEs at the configured cutoff.
conleyVcov <- conley(cutoff = cfg$conleyCutoffKM, distance = "spherical")

for (raceName in names(races)) {
  rc    <- races[[raceName]]
  ppsf  <- rc$ppsf
  elast <- rc$elast
  cat(sprintf(
    "\n========== HORSE RACE [%s]: %s  vs  %s ==========\n",
    raceName, ppsf, elast))

  mA <- fitLogit(mkFmlA(ppsf), est)
  mB <- fitLogit(mkFmlB(elast), est)
  mC <- fitLogit(mkFmlC(ppsf, elast), est)

  cat("\n-- Default (model-based) SEs --\n")
  print(etable(mA, mB, mC, digits = 3,
               headers = c(sprintf("A: %s only", ppsf),
                           sprintf("B: %s only", elast),
                           "C: both")))

  cat(sprintf("\n-- Conley SEs (cutoff = %.1f km) --\n", cfg$conleyCutoffKM))
  print(etable(mA, mB, mC, digits = 3, vcov = conleyVcov,
               headers = c(sprintf("A: %s only", ppsf),
                           sprintf("B: %s only", elast),
                           "C: both")))

  cat("\n-- Average marginal effects (mean-of-derivatives) --\n")
  cat("A: "); print(round(ame(mA), 4))
  cat("B: "); print(round(ame(mB), 4))
  cat("C: "); print(round(ame(mC), 4))
}

cat("\nDone with estimation.\n")

# ---------------------------------------------------------------------------
# 6. Permit-level heatmaps: duplex vs SF by lot-width bin
# ---------------------------------------------------------------------------
# Discrete 2-color scale rather than viridis_c, because isDuplex is binary.
# One PNG per width bin (33-ft and 50-ft).

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

# Continuous-surface heatmap. Used for localPPSF and the three slope
# surfaces. Drops NA values; the colour scale spans the observed range.
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
    labs(color = legendTitle, title = legendTitle)
  ggsave(outPath, plot = p, width = 8, height = 7, dpi = 200)
  invisible(p)
}

dir.create(path.expand(cfg$outDir), showWarnings = FALSE, recursive = TRUE)

# Choice heatmaps: exact 33-ft and 50-ft lots.
plotChoiceHeatmap(choiceDT, cfg, cfg$width33, "33-ft",
                  file.path(cfg$outDir, "duplexChoice_w33.png"))
plotChoiceHeatmap(choiceDT, cfg, cfg$width50, "50-ft",
                  file.path(cfg$outDir, "duplexChoice_w50.png"))

# Surface heatmaps: localPPSF + the three slope surfaces, at all
# permit locations (formerSF gate not applied; these are the same
# surfaces values the regression uses, plotted in space).
plotSurfaceHeatmap(choiceDT, "localPPSF", cfg,
                   "Local PPSF (single family, pre-2019)",
                   file.path(cfg$outDir, "surface_localPPSF.png"))
plotSurfaceHeatmap(choiceDT, "slope_singleFamilyPre", cfg,
                   "SF slope (log price ~ log sqft, pre-2019)",
                   file.path(cfg$outDir, "surface_slope_singleFamilyPre.png"))
plotSurfaceHeatmap(choiceDT, "slope_strataPre", cfg,
                   "Strata slope (log price ~ log sqft, pre-2019)",
                   file.path(cfg$outDir, "surface_slope_strataPre.png"))
plotSurfaceHeatmap(choiceDT, "slope_duplexPremium33", cfg,
                   "Duplex premium slope (33-ft lots)",
                   file.path(cfg$outDir, "surface_slope_duplexPremium33.png"))

cat("\nDone.\n")
