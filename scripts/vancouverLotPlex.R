# vancouverLandAssessment.R
# Spatial heterogeneity in the elasticity of land value w.r.t. lot size.
# Local-linear (bisquare, adaptive-k) GWR evaluated at building-permit locations.
# Modular: city config + generic loaders/plotters so Minneapolis / Portland slot in.
# Tom Davidoff
# 05/13/26  (rev 05/15/26: discrete-choice block + bbox exclusion)

library(data.table)
library(ggplot2)
library(ggspatial)
library(RSQLite)
library(sf)

# ===========================================================================
# 1. GENERIC TOOLS
# ===========================================================================

# Hand-rolled adaptive-bandwidth GWR (bisquare kernel, k-nearest-neighbour bw)
gwrLocal <- function(y, x, dataCoords, fitCoords, k = 1000, mcCores = 6) {
  bisquare <- function(d, h) ifelse(d < h, (1 - (d/h)^2)^2, 0)
  fitOne <- function(s) {
    d  <- sqrt((dataCoords[,1] - s[1])^2 + (dataCoords[,2] - s[2])^2)
    h  <- sort(d, partial = k)[k]
    w  <- bisquare(d, h)
    keep <- w > 0
    Xm <- cbind(1, x[keep]); ym <- y[keep]; wm <- w[keep]
    fit <- lm.wfit(Xm, ym, wm)
    resid <- ym - Xm %*% fit$coefficients
    sigma2 <- sum(wm * resid^2) / (sum(wm) - 2)
    XtWX_inv <- solve(crossprod(Xm * sqrt(wm)))
    se <- sqrt(diag(sigma2 * XtWX_inv))
    c(intercept = fit$coefficients[1], slope = fit$coefficients[2],
      seInt = se[1], seSlope = se[2], nEff = sum(wm))
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

# Intercept-only adaptive-bandwidth local mean (bisquare, k-NN bw).
# Returns a locally-smoothed level of `y` at each fit point -- i.e. a
# nonparametric surface that does NOT condition on lot size. This is the
# right "local ppsf" regressor for the choice model: it is the ambient
# price level a developer faces, not a value extrapolated through the
# log-log slope to some arbitrary lot size.
gwrLocalMean <- function(y, dataCoords, fitCoords, k = 1000, mcCores = 6) {
  bisquare <- function(d, h) ifelse(d < h, (1 - (d/h)^2)^2, 0)
  fitOne <- function(s) {
    d <- sqrt((dataCoords[,1] - s[1])^2 + (dataCoords[,2] - s[2])^2)
    h <- sort(d, partial = k)[k]
    w <- bisquare(d, h)
    keep <- w > 0
    sum(w[keep] * y[keep]) / sum(w[keep])     # weighted mean = intercept-only WLS
  }
  unlist(parallel::mclapply(seq_len(nrow(fitCoords)),
                            function(i) fitOne(fitCoords[i,]),
                            mc.cores = mcCores))
}

# Trim a data.table by symmetric quantile filter on a set of columns
trimByQuantile <- function(dt, cols, q) {
  for (c in cols) {
    lo <- quantile(dt[[c]], q,     na.rm = TRUE)
    hi <- quantile(dt[[c]], 1 - q, na.rm = TRUE)
    dt <- dt[get(c) %between% c(lo, hi)]
  }
  dt
}

# Drop rows in the exclusion STRIP: west-bounded by Oak, east-bounded by
# Ontario, and SOUTH of W 16th Ave -- the southern edge is open (no lower
# latitude limit). A row is excluded iff
#     lon in [lonMin, lonMax]  AND  lat <= latMax
# cfg$excludeBox is list(lonMin, lonMax, latMax) or NULL.
# Operates on any data.table with numeric `lon`/`lat`; applied globally
# to BCA parcels, sales, and permit fit points so every downstream
# pipeline (appraisal GWR, sales GWR, choice model) sees the same cut.
# NOTE: northern boundary is INclusive of points exactly at 16th and the
# strip extends arbitrarily far south; this is the intended half-plane.
applyExcludeBox <- function(dt, cfg) {
  bx <- cfg$excludeBox
  if (is.null(bx)) return(dt)
  inStrip <- dt$lon %between% c(bx$lonMin, bx$lonMax) &
             dt$lat <= bx$latMax
  dt[!inStrip]
}

# Resolve the exclusion strip bounds from named street references by
# reading the City zoning GeoJSON (already WGS84, vertices on street
# centrelines) -- avoids hardcoding longitudes/latitudes that cannot be
# verified without the actual street grid. Strategy: take the bounding
# box of the zoning layer's vertices and locate the requested streets by
# matching against the layer extent is NOT reliable; instead this helper
# expects the caller to have supplied either:
#   (a) cfg$excludeBox already populated (manual override -- used as is), OR
#   (b) cfg$excludeBoxRef = list(west=, east=, north=) giving longitudes
#       for the west/east streets and a latitude for the north street,
#       which are simply copied into excludeBox.
# It performs no geocoding; it only normalizes whichever the user gave.
# If both are NULL the strip is disabled.
resolveExcludeBox <- function(cfg) {
  if (!is.null(cfg$excludeBox)) return(cfg)        # explicit override wins
  ref <- cfg$excludeBoxRef
  if (is.null(ref)) { cfg$excludeBox <- NULL; return(cfg) }
  cfg$excludeBox <- list(
    lonMin = min(ref$west, ref$east),
    lonMax = max(ref$west, ref$east),
    latMax = ref$north
  )
  cfg
}

# ---------------------------------------------------------------------------
# Derive the exclusion-strip reference bounds from real street geometry
# (OpenStreetMap via osmdata), built deliberately as an OUTER bound.
#
# Asymmetry assumption: over-inclusion costs only sample, under-inclusion
# leaves contamination -> we take the EXTREME vertex of each street over
# the relevant span, then push it OUTWARD by `slackMeters`. The strip is
# intentionally too wide.
#
# Segments (not whole streets):
#   * Oak     -- only the portion SOUTH of 16th
#   * Ontario -- only the portion SOUTH of 16th
#   * 16th    -- only the portion BETWEEN Oak and Ontario
# 16th's latitude is read first (its vertices near the Oak/Ontario
# longitude band), then used to clip Oak/Ontario southward.
#
# Returns list(west=, east=, north=) suitable for cfg$excludeBoxRef.
# Network: osmdata hits the Overpass API. Result is cached to
# `cachePath`; delete the file to force a refresh. If osmdata is
# unavailable or the query fails, returns NULL and the caller keeps
# whatever excludeBoxRef was already set (the approximate fallback).
#
# slackMeters is converted to degrees locally: ~1 deg lat = 111320 m;
# 1 deg lon = 111320*cos(lat) m at ~49.26 N. Outward = west pushed
# more-negative, east pushed more-positive, north pushed up.
deriveBoundsFromOSM <- function(cfg,
                                cachePath  = "~/DropboxExternal/dataProcessed/exclStripBounds.rds",
                                slackMeters = 150) {
  cachePath <- path.expand(cachePath)
  if (file.exists(cachePath)) {
    message("Exclusion bounds: using cached ", cachePath)
    return(readRDS(cachePath))
  }
  if (!requireNamespace("osmdata", quietly = TRUE)) {
    warning("osmdata not installed; falling back to cfg$excludeBoxRef")
    return(NULL)
  }
  ok <- tryCatch({
    # Bounding box generous enough to contain all three streets near the
    # corner; osmdata returns full named ways, we clip below.
    bb <- c(cfg$xlim[1], cfg$ylim[1], cfg$xlim[2], cfg$ylim[2])

    pull <- function(streetName) {
      q <- osmdata::opq(bbox = bb)
      q <- osmdata::add_osm_feature(q, key = "name", value = streetName)
      res <- osmdata::osmdata_sf(q)
      ls  <- res$osm_lines
      if (is.null(ls) || nrow(ls) == 0)
        stop("no OSM lines for '", streetName, "'")
      # union all matching segments, densify-free vertex extraction
      cm <- st_coordinates(st_geometry(ls))
      data.table(lon = cm[, "X"], lat = cm[, "Y"])
    }

    oak <- pull("Oak Street")
    ont <- pull("Ontario Street")
    s16 <- pull("West 16th Avenue")

    # Longitude band of the corner = rough Oak..Ontario span, used to
    # isolate the relevant stretch of 16th. Use medians as robust anchors.
    lonOak <- median(oak$lon); lonOnt <- median(ont$lon)
    loB <- min(lonOak, lonOnt); hiB <- max(lonOak, lonOnt)

    # 16th between Oak and Ontario -> its representative latitude
    s16seg <- s16[lon %between% c(loB, hiB)]
    if (nrow(s16seg) == 0) stop("16th has no vertices between Oak and Ontario")
    lat16 <- median(s16seg$lat)

    # Oak / Ontario, only SOUTH of 16th
    oakS <- oak[lat <= lat16]
    ontS <- ont[lat <= lat16]
    if (nrow(oakS) == 0 || nrow(ontS) == 0)
      stop("Oak/Ontario have no vertices south of 16th in bbox")

    # OUTER extremes over the clipped spans
    westRaw  <- min(oakS$lon, ontS$lon)   # westernmost of the two
    eastRaw  <- max(oakS$lon, ontS$lon)   # easternmost of the two
    northRaw <- max(s16seg$lat)           # northern (16th) edge

    # Push outward by slack (metres -> degrees at ~49.26 N)
    dLat <- slackMeters / 111320
    dLon <- slackMeters / (111320 * cos(lat16 * pi / 180))
    out <- list(
      west  = westRaw  - dLon,            # more negative = further west
      east  = eastRaw  + dLon,            # more positive = further east
      north = northRaw + dLat             # further north
    )
    saveRDS(out, cachePath)
    message(sprintf(
      "Exclusion bounds from OSM (+%dm slack): W=%.5f E=%.5f N=%.5f  [16th lat ~ %.5f]",
      slackMeters, out$west, out$east, out$north, lat16))
    out
  }, error = function(e) {
    warning("deriveBoundsFromOSM failed (", conditionMessage(e),
            "); falling back to cfg$excludeBoxRef")
    NULL
  })
  ok
}

# Project lon/lat columns of a data.table to a planar CRS; return coord matrix
projectCoords <- function(dt, crsOut) {
  sfTmp <- st_as_sf(dt[, .(lon, lat)], coords = c("lon","lat"), crs = 4326)
  sfTmp <- st_transform(sfTmp, crsOut)
  st_coordinates(sfTmp)
}

# Run GWR + attach lon/lat for plotting.
# dtData must contain numeric columns named in yCol/xCol, plus lon/lat.
# fitPointsLL is a data.table with lon/lat for fit locations.
runGWR <- function(dtData, yCol, xCol, fitPointsLL, cfg, k = 1000) {
  dataCoords <- projectCoords(dtData, cfg$crsProj)
  fitCoords  <- projectCoords(fitPointsLL, cfg$crsProj)
  dtLocal <- gwrLocal(y          = dtData[[yCol]],
                      x          = dtData[[xCol]],
                      dataCoords = dataCoords,
                      fitCoords  = fitCoords,
                      k          = k)
  dtLocal[, `:=`(lon = fitPointsLL$lon, lat = fitPointsLL$lat)]
  dtLocal[]
}

# Local smoothed level of yCol at fit points (intercept-only GWR).
runGWRMean <- function(dtData, yCol, fitPointsLL, cfg, k = 1000) {
  dataCoords <- projectCoords(dtData, cfg$crsProj)
  fitCoords  <- projectCoords(fitPointsLL, cfg$crsProj)
  gwrLocalMean(y = dtData[[yCol]], dataCoords = dataCoords,
               fitCoords = fitCoords, k = k)
}

# Heatmap on a basemap. Pass `valueCol` as a string.
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

# ===========================================================================
# 2. CITY CONFIG
# ===========================================================================
# To add Minneapolis / Portland: copy this list, swap paths / CRS / bbox /
# trim thresholds. Everything downstream is config-driven.

cfgVancouver <- list(
  city      = "Vancouver",
  crsProj   = 26910,                              # UTM 10N (meters)
  xlim      = c(-123.23, -123.02),
  ylim      = c( 49.20,   49.32),
  tileZoom  = 12,
  # data paths
  sqliteFile     = "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3",
  bcaRDS         = "~/DropboxExternal/dataProcessed/bca_vancouver_residential.rds",
  permitFile     = "~/DropboxExternal/dataRaw/issued-building-permits.csv",
  zoningGeojson  = "~/DropboxExternal/dataRaw/vancouver_zoning.geojson",
  targetZone     = "R1-1",
  # filtering
  trimDimsQ      = 0.05,                          # winsorize landWidth & landDepth
  trimYQ         = 0.01,                          # winsorize the response (ppsf or price)
  salesYearRange = c(2014, 2018),
  permitYearRange = c(2019, 2023),
  # ---- GLOBAL EXCLUSION STRIP ---------------------------------------------
  # Half-plane strip: west-bounded by Oak St, east-bounded by Ontario St,
  # SOUTH of W 16th Ave (no southern limit). Dropped everywhere (BCA,
  # sales, permits) before any GWR / choice estimation.
  #
  # Two ways to set it (resolveExcludeBox picks one):
  #   * excludeBox = list(lonMin=, lonMax=, latMax=)  -- explicit override,
  #     used verbatim; OR
  #   * excludeBoxRef = list(west=, east=, north=)    -- longitudes of the
  #     west/east streets + latitude of the north street; normalized into
  #     excludeBox at runtime.
  # Set BOTH to NULL to disable the strip entirely.
  #
  # The reference values below are APPROXIMATE Vancouver street
  # coordinates and should be replaced with values read off the City
  # zoning GeoJSON (same WGS84 CRS as the rest of the pipeline) before
  # any production run. They are good enough to eyeball the cut on the
  # heatmaps but NOT to trust for a final sample definition.
  excludeBox    = NULL,
  excludeBoxRef = list(
    west  = -123.1283,   # Oak St      -- APPROX, verify against zoning layer
    east  = -123.1037,   # Ontario St  -- APPROX, verify against zoning layer
    north =   49.2575    # W 16th Ave  -- APPROX, verify against zoning layer
  )
)

# ===========================================================================
# 3. LOAD BCA PARCEL CONTEXT (used by both appraisal and sales pipelines)
# ===========================================================================
# Returns a data.table keyed on rollStart with lat, lon, and the MB_* columns
# needed for sales-side hedonics.

loadBCAParcels <- function(cfg) {
  sfBCA <- st_as_sf(readRDS(cfg$bcaRDS))
  sfBCA <- st_transform(sfBCA, 4326)
  cent  <- st_centroid(st_geometry(sfBCA))
  sfBCA$lon <- st_coordinates(cent)[, 1]
  sfBCA$lat <- st_coordinates(cent)[, 2]
  keepCols <- intersect(c("folioID","rollStart","landWidth","landDepth",
                          "MB_effective_year","MB_total_finished_area",
                          "neighbourhoodDescription","lat","lon"),
                        names(sfBCA))
  dtBCA <- as.data.table(st_drop_geometry(sfBCA))[, ..keepCols]
  dtBCA <- applyExcludeBox(dtBCA, cfg)            # GLOBAL bbox exclusion
  dtBCA
}

# ===========================================================================
# 4. APPRAISAL LOADER (BCA valuation table)
# ===========================================================================
# Returns dtValuation with columns: folioID, landValue, landWidth, landDepth,
# landArea, ppsf, logPPSF, logArea, lat, lon.

loadAppraisal <- function(cfg, dtBCA) {
  con <- dbConnect(SQLite(), cfg$sqliteFile)
  qVal  <- "SELECT folioID, landValue FROM valuation"
  qInv  <- "SELECT roll_number, land_width, land_depth, zoning FROM residentialInventory
            WHERE jurisdiction==200 and zoning LIKE 'RS%'"
  qFol  <- "SELECT folioID, rollNumber FROM folio WHERE jurisdictionCode == 200"
  qDes  <- "SELECT folioID, actualUseDescription, landWidth, landDepth FROM folioDescription"
  valuation   <- as.data.table(dbGetQuery(con, qVal))
  inventory   <- as.data.table(dbGetQuery(con, qInv))
  folio       <- as.data.table(dbGetQuery(con, qFol))
  description <- as.data.table(dbGetQuery(con, qDes))
  dbDisconnect(con)

  setkey(valuation, folioID);     setkey(folio, folioID);        valuation <- valuation[folio]
  setkey(inventory, roll_number); setkey(valuation, rollNumber); valuation <- valuation[inventory]
  setkey(description, folioID);   setkey(valuation, folioID);    valuation <- valuation[description]

  numericCols <- c("landValue","land_width","land_depth","landWidth","landDepth")
  valuation[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
  valuation[is.na(landWidth) & !is.na(land_width), landWidth := land_width]
  valuation[is.na(landDepth) & !is.na(land_depth), landDepth := land_depth]

  valuation[, rollStart := floor(as.numeric(rollNumber)/1000)]
  setkey(valuation, rollStart)
  setkey(dtBCA,     rollStart)
  dt <- valuation[dtBCA, nomatch = 0]   # inner join: must have coords

  # Derive analytical variables
  dt[, landArea := landWidth * landDepth]
  dt[, ppsf     := landValue / landArea]
  dt <- dt[!is.na(lat) & !is.na(lon) & !is.na(ppsf) & !is.na(landArea) &
           landArea > 0 & ppsf > 0]
  dt <- trimByQuantile(dt, c("landWidth","landDepth"), cfg$trimDimsQ)
  dt <- trimByQuantile(dt, "ppsf", cfg$trimYQ)
  dt[, logPPSF := log(ppsf)]
  dt[, logArea := log(landArea)]
  dt[]
}

# ===========================================================================
# 5. SALES LOADER (BCA sales table)
# ===========================================================================
# Returns dtSales with columns: folioID, year, conveyancePrice, logPrice,
# landWidth, landDepth, landArea, logArea, ppsfSale, logPPSFSale, age, FSR,
# logFSR, lat, lon.

loadSales <- function(cfg, dtBCA) {
  con <- dbConnect(SQLite(), cfg$sqliteFile)
  dt  <- as.data.table(dbGetQuery(con,
    "SELECT folioID, conveyanceDate, conveyancePrice, conveyanceTypeDescription
     FROM sales"))
  dbDisconnect(con)

  dt <- dt[conveyanceTypeDescription == "Improved Single Property Transaction",
           .(folioID, conveyanceDate, conveyancePrice)]
  dt <- merge(dt, dtBCA, by = "folioID")

  dt[, conveyancePrice := as.numeric(conveyancePrice)]
  dt[, landWidth       := as.numeric(landWidth)]
  dt[, landDepth       := as.numeric(landDepth)]
  dt <- dt[!is.na(lat) & !is.na(lon) & !is.na(conveyancePrice) &
           !is.na(landWidth) & !is.na(landDepth) &
           conveyancePrice > 0 & landWidth > 0 & landDepth > 0]

  dt[, year      := as.numeric(substring(conveyanceDate, 1, 4))]
  dt[, logPrice  := log(conveyancePrice)]
  dt[, landArea  := landWidth * landDepth]
  dt[, logArea   := log(landArea)]
  dt[, ppsfSale  := conveyancePrice / landArea]
  dt[, logPPSFSale := log(ppsfSale)]
  if ("MB_effective_year" %in% names(dt))
    dt[, age := year - as.numeric(MB_effective_year)]
  if ("MB_total_finished_area" %in% names(dt)) {
    dt[, FSR    := as.numeric(MB_total_finished_area) / landArea]
    dt[, logFSR := log(FSR)]
  }

  dt <- trimByQuantile(dt, c("landWidth","landDepth"), cfg$trimDimsQ)
  dt <- trimByQuantile(dt, "ppsfSale", cfg$trimYQ)
  if (!is.null(cfg$salesYearRange)) {
    dt <- dt[year %between% cfg$salesYearRange]
  }
  dt[]
}

# ===========================================================================
# 6. PERMIT LOADER (fit-point locations)
# ===========================================================================
# Returns a data.table with lon/lat for permits in the target zone PLUS the
# use category, so the duplex/single choice can be modeled. `useCat` retains
# the raw SpecificUseCategory; `isDuplex` is the binary choice variable
# (1 = Duplex, 0 = Single Detached House; other categories -> NA and dropped
# in the choice model but retained as GWR fit points).

loadPermits <- function(cfg) {
  dtPermit <- fread(cfg$permitFile,
    select = c("PermitNumber","ProjectValue","TypeOfWork",
               "PropertyUse","SpecificUseCategory",
               "GeoLocalArea","geo_point_2d","YearMonth"))
  dtPermit <- dtPermit[PropertyUse == "Dwelling Uses" &
                       TypeOfWork %in% c("New Building","Addition / Alteration")]
  dtPermit[, c("permitLat","permitLon") := tstrsplit(geo_point_2d, ", ")]
  dtPermit[, `:=`(permitLat = as.numeric(permitLat),
                  permitLon = as.numeric(permitLon))]
  dtPermit <- dtPermit[!is.na(permitLat) & !is.na(permitLon)]
  qCrit <- quantile(dtPermit[TypeOfWork == "New Building" &
                             SpecificUseCategory %in%
                               c("Duplex","Single Detached House") &
                             ProjectValue > 0, ProjectValue], 0.25)
  dtPermit <- dtPermit[TypeOfWork == "New Building" | ProjectValue >= qCrit]
  dtPermit <- dtPermit[grepl("Duplex|Single Detached House|Laneway House|Multiple Dwelling",
                             SpecificUseCategory)]
  dtPermit[,Year:= as.numeric(substring(YearMonth, 1, 4))]
  dtPermit <- dtPermit[Year >= cfg$permitYearRange[1] & Year <= cfg$permitYearRange[2]]
  sfPermit <- st_as_sf(dtPermit[, .(permitLat, permitLon, SpecificUseCategory)],
                       coords = c("permitLon","permitLat"), crs = 4326)
  dZ <- st_read(cfg$zoningGeojson, quiet = TRUE)
  dZ <- dZ[dZ$zoning_district == cfg$targetZone, "zoning_district"]
  sfPermit <- st_transform(sfPermit, st_crs(dZ))
  sfPermit <- st_join(sfPermit, dZ, join = st_within)
  sfPermit <- sfPermit[!is.na(sfPermit$zoning_district), ]
  sfPermit <- st_transform(sfPermit, 4326)
  # convert YearMongth to numeric year and only use in PermitYearRange to be added to cfg
  print(head(sfPermit))

  dtOut <- data.table(lon    = st_coordinates(sfPermit)[, 1],
                       lat    = st_coordinates(sfPermit)[, 2],
                       useCat = sfPermit$SpecificUseCategory)
  dtOut[, isDuplex := fifelse(useCat == "Duplex", 1L,
                       fifelse(useCat == "Single Detached House", 0L,
                               NA_integer_))]
  dtOut <- applyExcludeBox(dtOut, cfg)            # GLOBAL bbox exclusion
  dtOut[]
}

# Attach each permit's OWN parcel lot size by nearest-parcel spatial join
# to BCA. The permit CSV carries no lot area, so the discrete-choice
# regressor "log lot size" must be sourced from the matched parcel.
# Nearest-feature join (st_nearest_feature) rather than st_within because
# permit geo_point_2d is a rooftop/centroid that need not fall inside the
# parcel polygon's planar footprint after reprojection.
attachPermitLot <- function(permits, dtBCA, cfg) {
  bca <- dtBCA[!is.na(lon) & !is.na(lat) &
               !is.na(landWidth) & !is.na(landDepth)]
  bca[, landArea := as.numeric(landWidth) * as.numeric(landDepth)]
  bca <- bca[landArea > 0]
  sfP <- st_transform(
            st_as_sf(copy(permits), coords = c("lon","lat"), crs = 4326),
            cfg$crsProj)
  sfB <- st_transform(
            st_as_sf(bca[, .(landArea, lon, lat)],
                     coords = c("lon","lat"), crs = 4326),
            cfg$crsProj)
  nn  <- st_nearest_feature(sfP, sfB)
  permits[, permitLandArea := sfB$landArea[nn]]
  permits[, permitLogArea  := log(permitLandArea)]
  permits[]
}

# ===========================================================================
# 7. PIPELINE
# ===========================================================================

cfg     <- cfgVancouver
osmRef  <- deriveBoundsFromOSM(cfg, slackMeters = 150)
if (!is.null(osmRef)) cfg$excludeBoxRef <- osmRef   # else keep approx fallback
cfg     <- resolveExcludeBox(cfg)   # populate cfg$excludeBox from refs/override
if (!is.null(cfg$excludeBox))
  cat(sprintf("Exclusion strip ACTIVE: lon in [%.4f, %.4f], lat <= %.4f\n",
              cfg$excludeBox$lonMin, cfg$excludeBox$lonMax,
              cfg$excludeBox$latMax)) else
  cat("Exclusion strip DISABLED\n")
dtBCA   <- loadBCAParcels(cfg)
permits <- loadPermits(cfg)

# ---- 7a. Appraisal: log(ppsf) ~ log(landArea) -----------------------------
dtVal <- loadAppraisal(cfg, dtBCA)
print(summary(dtVal))

plotHeatmap(dtVal, "ppsf", cfg,
            legendTitle = "Price per sqft (appraisal)",
            outPath     = "text/landPPSFHeatMapVancouver.png")

dtLocalVal <- runGWR(dtVal, yCol = "logPPSF", xCol = "logArea",
                     fitPointsLL = permits, cfg = cfg, k = 1000)
print(summary(dtLocalVal))

plotHeatmap(dtLocalVal, "slope", cfg,
            legendTitle = "Local elasticity\nlog(ppsf) ~ log(landArea)\nappraisal",
            outPath     = "text/landElasticityHeatMapVancouver.png")

# ---- 7b. Sales: log(ppsfSale) ~ log(landArea) -----------------------------
dtSales <- loadSales(cfg, dtBCA)
print(summary(dtSales))

plotHeatmap(dtSales, "ppsfSale", cfg,
            legendTitle = "Price per sqft (sales)",
            outPath     = "text/salesPPSFHeatMapVancouver.png")

dtLocalSales <- runGWR(dtSales, yCol = "logPPSFSale", xCol = "logArea",
                       fitPointsLL = permits, cfg = cfg, k = 1000)
print(summary(dtLocalSales))

plotHeatmap(dtLocalSales, "slope", cfg,
            legendTitle = "Local elasticity\nlog(ppsf) ~ log(landArea)\nsales",
            outPath     = "text/salesElasticityHeatMapVancouver.png")

# ===========================================================================
# 8. DISCRETE CHOICE: DUPLEX vs SINGLE DETACHED
# ===========================================================================
# Dependent var: isDuplex (1 = Duplex permit, 0 = Single Detached House).
# Regressors:
#   permitLogArea  -- log lot size of the permit's OWN matched parcel
#   localSlope     -- locally-estimated elasticity at the permit (appraisal GWR)
#   localLogPPSF   -- locally-smoothed log price/sqft level at the permit
#                     (intercept-only GWR -- ambient price, not slope-extrapolated)
# Estimated three ways: LPM (lm), logit, probit.
#
# Note on the ppsf regressor: using the GWR *intercept* would mean the
# fitted log(ppsf) at log(landArea)=0 -- a wildly out-of-sample lot of 1
# sqft -- which is exactly the geometric artifact already discussed in the
# infill work. The intercept-only local mean of logPPSF is the clean
# ambient-price control and is what enters here.

# 8.1 Own-parcel lot size for each permit (nearest BCA parcel)
permits <- attachPermitLot(permits, dtBCA, cfg)

# 8.2 Local elasticity at permits (already have dtLocalVal: row-aligned to
#     `permits` by construction -- fitPointsLL == permits, same order,
#     bbox exclusion applied to both before GWR).
permits[, localSlope := dtLocalVal$slope]

# 8.3 Local ambient log-ppsf level at permits (intercept-only GWR on the
#     appraisal sample). Independent of any lot-size assumption.
permits[, localLogPPSF := runGWRMean(dtVal, yCol = "logPPSF",
                                     fitPointsLL = permits,
                                     cfg = cfg, k = 1000)]

# 8.4 Estimation sample: duplex/single only, complete cases
choiceDT <- permits[!is.na(isDuplex) &
                     is.finite(permitLogArea) &
                     is.finite(localSlope) &
                     is.finite(localLogPPSF)]
cat(sprintf("\nChoice sample: n = %d  (duplex = %d, single = %d)\n",
            nrow(choiceDT), sum(choiceDT$isDuplex == 1L),
            sum(choiceDT$isDuplex == 0L)))

fmlChoiceSlopeOnly <- isDuplex ~ permitLogArea + localSlope
fmlChoicePPSFOnly  <- isDuplex ~ permitLogArea + localLogPPSF
fmlChoice <- isDuplex ~ permitLogArea + localSlope + localLogPPSF

mLPM   <- lm(fmlChoice, data = choiceDT)
mLogit <- glm(fmlChoice, data = choiceDT, family = binomial(link = "logit"))
mProbit<- glm(fmlChoice, data = choiceDT, family = binomial(link = "probit"))
mLogitSlopeOnly <- glm(fmlChoiceSlopeOnly, data = choiceDT, family = binomial(link = "logit"))
mLogitPPSFOnly  <- glm(fmlChoicePPSFOnly, data = choiceDT, family = binomial(link = "logit"))

cat("\n================ LPM  (isDuplex) ================\n")
print(summary(mLPM))
cat("\n================ LOGIT  (isDuplex) ==============\n")
print(summary(mLogit))
print(summary(mLogitSlopeOnly))
print(summary(mLogitPPSFOnly))
cat("\n================ PROBIT  (isDuplex) =============\n")
print(summary(mProbit))

# 8.5 Average marginal effects for logit/probit (so coefficients are
#     comparable in scale to the LPM). Numerical AME via finite differences
#     on the linear predictor scale would be exact for continuous
#     regressors; here we use the simple mean-of-derivatives form.
ame <- function(model) {
  X    <- model.matrix(model)
  b    <- coef(model)
  eta  <- as.vector(X %*% b)
  dens <- switch(model$family$link,
                 logit  = dlogis(eta),
                 probit = dnorm(eta))
  vapply(colnames(X)[-1],
         function(v) mean(dens) * b[v], numeric(1))
}
cat("\n---- Avg marginal effects ----\n")
cat("logit :\n");  print(ame(mLogit))
cat("logit :\n");  print(ame(mLogitSlopeOnly))
cat("logit :\n");  print(ame(mLogitPPSFOnly))
cat("probit:\n");  print(ame(mProbit))
cat("LPM (slopes are the marginal effects):\n")
print(coef(mLPM)[-1])
