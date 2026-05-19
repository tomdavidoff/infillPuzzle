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
  conleyCutoffKM    = 3.0
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

# Attach each permit's OWN log lot area by nearest-parcel join to BCA
# single-family parcels. Lifted from vancouverLandAssessment.R.
attachOwnLotArea <- function(permits, dtSF, cfg) {
  bca <- dtSF[!is.na(lon) & !is.na(lat) &
              !is.na(landWidth) & !is.na(landDepth) &
              landWidth > 0 & landDepth > 0]
  bca[, landArea := as.numeric(landWidth) * as.numeric(landDepth)]
  bca <- bca[landArea > 0]
  sfP <- st_transform(
            st_as_sf(copy(permits), coords = c("lon","lat"), crs = 4326),
            cfg$crsProj)
  sfB <- st_transform(
            st_as_sf(bca[, .(landArea, lon, lat)],
                     coords = c("lon","lat"), crs = 4326),
            cfg$crsProj)
  nn <- st_nearest_feature(sfP, sfB)
  permits[, permitLandArea := sfB$landArea[nn]]
  permits[, permitLogArea  := log(permitLandArea)]
  permits[]
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

if (nrow(permits) != nrow(surfaces))
  warning(sprintf("Permit count (%d) != surfaces row count (%d). ",
                  "Check that vancouverElasticityPrice.R was run with the ",
                  "same cfg$permitYearRange and zoning filter.",
                  nrow(permits), nrow(surfaces)))

# Permits and surfaces are row-aligned by construction (same loadPermits
# filter chain). Bind them column-wise.
choiceDT <- cbind(permits, surfaces[, !c("lon","lat"), with = FALSE])

# Attach permit's own log lot area from nearest SF parcel.
dtSF <- as.data.table(readRDS(file.path(path.expand(cfg$parcelRDSDir),
                                        "bca_vancouver_singleFamily.rds")))
choiceDT <- attachOwnLotArea(choiceDT, dtSF, cfg)

# Binary outcome: 1 = Duplex, 0 = Single Detached House. Drop others.
choiceDT[, isDuplex := fifelse(useCat == "Duplex", 1L,
                       fifelse(useCat == "Single Detached House", 0L,
                               NA_integer_))]

# ---------------------------------------------------------------------------
# 4. Estimation sample
# ---------------------------------------------------------------------------
needed <- c("isDuplex","permitLogArea","localPPSF",
            "slope_singleFamilyPre","slope_strataPre","slope_duplexPremium33")
est <- choiceDT[complete.cases(choiceDT[, ..needed]) &
                is.finite(permitLogArea)]
cat(sprintf("\nEstimation sample: n = %d  (duplex = %d, SF = %d)\n",
            nrow(est), sum(est$isDuplex == 1L), sum(est$isDuplex == 0L)))

# Diagnostic: correlations among the constructed regressors. Useful
# context for reading the horse-race coefficients.
cat("\nCorrelations among constructed regressors:\n")
regCols <- c("localPPSF","slope_singleFamilyPre",
             "slope_strataPre","slope_duplexPremium33")
print(round(cor(est[, ..regCols], use = "complete.obs"), 3))

# ---------------------------------------------------------------------------
# 5. Horse races
# ---------------------------------------------------------------------------
# Spec A (common across races): isDuplex ~ permitLogArea + localPPSF
# Spec B (per race):             isDuplex ~ permitLogArea + <elasticity>
# Spec C (per race):             isDuplex ~ permitLogArea + localPPSF + <elasticity>

races <- list(
  SF      = "slope_singleFamilyPre",
  strata  = "slope_strataPre",
  duplex  = "slope_duplexPremium33"
)

mkFmlA <- as.formula("isDuplex ~ permitLogArea + localPPSF")
mkFmlB <- function(elast) as.formula(sprintf(
            "isDuplex ~ permitLogArea + %s", elast))
mkFmlC <- function(elast) as.formula(sprintf(
            "isDuplex ~ permitLogArea + localPPSF + %s", elast))

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

mA <- fitLogit(mkFmlA, est)

# Conley SEs at the configured cutoff.
conleyVcov <- conley(cutoff = cfg$conleyCutoffKM, distance = "spherical")

for (raceName in names(races)) {
  elast <- races[[raceName]]
  cat(sprintf(
    "\n========== HORSE RACE: localPPSF vs %s (%s) ==========\n",
    elast, raceName))

  mB <- fitLogit(mkFmlB(elast), est)
  mC <- fitLogit(mkFmlC(elast), est)

  cat("\n-- Default (model-based) SEs --\n")
  print(etable(mA, mB, mC, digits = 3,
               headers = c("A: ppsf only", sprintf("B: %s only", raceName),
                           "C: both")))

  cat(sprintf("\n-- Conley SEs (cutoff = %.1f km) --\n", cfg$conleyCutoffKM))
  print(etable(mA, mB, mC, digits = 3, vcov = conleyVcov,
               headers = c("A: ppsf only", sprintf("B: %s only", raceName),
                           "C: both")))

  cat("\n-- Average marginal effects (mean-of-derivatives) --\n")
  cat("A: "); print(round(ame(mA), 4))
  cat("B: "); print(round(ame(mB), 4))
  cat("C: "); print(round(ame(mC), 4))
}

cat("\nDone.\n")
