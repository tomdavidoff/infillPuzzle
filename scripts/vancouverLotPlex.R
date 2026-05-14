# vancouverLandAssessment.R
# Spatial heterogeneity in the elasticity of land value w.r.t. lot size.
# Local-linear (bisquare, adaptive-k) GWR evaluated at building-permit locations.
# Modular: city config + generic loaders/plotters so Minneapolis / Portland slot in.
# Tom Davidoff
# 05/13/26

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

# Trim a data.table by symmetric quantile filter on a set of columns
trimByQuantile <- function(dt, cols, q) {
  for (c in cols) {
    lo <- quantile(dt[[c]], q,     na.rm = TRUE)
    hi <- quantile(dt[[c]], 1 - q, na.rm = TRUE)
    dt <- dt[get(c) %between% c(lo, hi)]
  }
  dt
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
  trimYQ         = 0.01,                           # winsorize the response (ppsf or price)
  salesYearRange = c(2014, 2018)
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
# Returns a data.table with lon/lat for permits in the target zone.

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
  sfPermit <- st_as_sf(dtPermit[, .(permitLat, permitLon)],
                       coords = c("permitLon","permitLat"), crs = 4326)
  dZ <- st_read(cfg$zoningGeojson, quiet = TRUE)
  dZ <- dZ[dZ$zoning_district == cfg$targetZone, "zoning_district"]
  sfPermit <- st_transform(sfPermit, st_crs(dZ))
  sfPermit <- st_join(sfPermit, dZ, join = st_within)
  sfPermit <- sfPermit[!is.na(sfPermit$zoning_district), ]
  sfPermit <- st_transform(sfPermit, 4326)
  data.table(lon = st_coordinates(sfPermit)[, 1],
             lat = st_coordinates(sfPermit)[, 2])
}

# ===========================================================================
# 7. PIPELINE
# ===========================================================================

cfg     <- cfgVancouver
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
