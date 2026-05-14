# vancouverLandAssessment.R
# Spatial heterogeneity in the elasticity of land value with respect to lot size.
# Local-linear (bisquare, adaptive-k) GWR evaluated at building-permit locations.
# Tom Davidoff
# 05/13/26

library(data.table)
library(ggplot2)
library(ggspatial)
library(RSQLite)
library(sf)

# ---------------------------------------------------------------------------
# Hand (by Claude) -rolled adaptive-bandwidth GWR -- GWR was slow and annoying in either sp or model versions.
# ---------------------------------------------------------------------------
# y, x          : numeric response and (single) predictor vectors, length n
# dataCoords    : n x 2 matrix of projected (meter) coordinates for the data
# fitCoords     : m x 2 matrix of projected coordinates at which to fit
# k             : adaptive bandwidth = number of nearest neighbours
# Returns a data.table with m rows: intercept, slope, SEs, effective n.
# ---------------------------------------------------------------------------
gwrLocal <- function(y, x, dataCoords, fitCoords, k = 2000) {
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
  resList <- parallel::mclapply(seq_len(nrow(fitCoords)), function(i) fitOne(fitCoords[i,]), mc.cores = 6)
  res <- do.call(rbind, resList)
  dt  <- as.data.table(res)
  setnames(dt, c("intercept","slope","seInt","seSlope","nEff"))
  dt[, `:=`(fitX = fitCoords[,1], fitY = fitCoords[,2])]
  dt[]
}

# ---------------------------------------------------------------------------
# Load 2019 BC Assessment data
# ---------------------------------------------------------------------------
filename <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
con <- dbConnect(SQLite(), filename)

queryValuation   <- "SELECT folioID, landValue FROM valuation"
queryInventory   <- "SELECT roll_number, land_width, land_depth, zoning FROM residentialInventory WHERE jurisdiction==200 and zoning LIKE 'RS%'"
queryFolio       <- "SELECT folioID, rollNumber FROM folio WHERE jurisdictionCode == 200"
queryDescription <- "SELECT folioID, actualUseDescription, landWidth, landDepth FROM folioDescription"

valuation   <- data.table(dbGetQuery(con, queryValuation))
inventory   <- data.table(dbGetQuery(con, queryInventory))
folio       <- data.table(dbGetQuery(con, queryFolio))
description <- data.table(dbGetQuery(con, queryDescription))
dbDisconnect(con)

# Merge
setkey(valuation, folioID)     
setkey(folio, folioID)        
valuation <- valuation[folio]
setkey(inventory, roll_number)
setkey(valuation, rollNumber)
valuation <- valuation[inventory]
setkey(description, folioID)
setkey(valuation, folioID)
valuation <- valuation[description]

# Coerce numerics, fill in missing dimensions from inventory
numericCols <- c("landValue","land_width","land_depth","landWidth","landDepth")
dtValuation <- as.data.table(valuation)
dtValuation[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
print(summary(dtValuation[!is.na(land_width) & !is.na(land_depth), land_width == landWidth]))
dtValuation[is.na(landWidth) & !is.na(land_width), landWidth := land_width]
dtValuation[is.na(landDepth) & !is.na(land_depth), landDepth := land_depth]

# ---------------------------------------------------------------------------
# Attach BCA centroid coordinates
# ---------------------------------------------------------------------------
sfBCA <- st_as_sf(readRDS("~/DropboxExternal/dataProcessed/bca_vancouver_residential.rds"))
sfBCA <- st_transform(sfBCA, 4326)
cent  <- st_centroid(st_geometry(sfBCA))
sfBCA$lon <- st_coordinates(cent)[, 1]
sfBCA$lat <- st_coordinates(cent)[, 2]
dtBCA <- as.data.table(st_drop_geometry(sfBCA))[, .(lat, lon, rollStart)]

dtValuation[, rollStart := floor(as.numeric(rollNumber)/1000)]
setkey(dtValuation, rollStart); setkey(dtBCA, rollStart)
dtValuation <- dtValuation[dtBCA]

# ---------------------------------------------------------------------------
# Sample selection: RS, plausible dimensions, plausible ppsf
# ---------------------------------------------------------------------------
dtValuation[, landArea := landWidth * landDepth]
dtValuation[, ppsf     := landValue / landArea]
dtValuation <- dtValuation[!is.na(lat) & !is.na(lon) & !is.na(ppsf) & !is.na(landArea)]

critQuant <- 0.05
dtValuation <- dtValuation[landWidth %between% quantile(landWidth, c(critQuant, 1 - critQuant))]
dtValuation <- dtValuation[landDepth %between% quantile(landDepth, c(critQuant, 1 - critQuant))]

critQuant <- 0.01
dtValuation <- dtValuation[ppsf %between% quantile(ppsf, c(critQuant, 1 - critQuant))]
print(summary(dtValuation))

# Raw ppsf heatmap
ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 12) +
  geom_point(data = dtValuation, aes(x=lon, y = lat, color = ppsf), size = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(crs = 4326, xlim = c(-123.23, -123.02), ylim = c(49.20, 49.32)) +
  theme_void() +
  labs(color = "Local elasticity")
ggsave("text/landPPSFHeatMapVancouver.png")

# ---------------------------------------------------------------------------
# Permit locations (restricted to R1-1 zoning)
# ---------------------------------------------------------------------------
permitFile <- "~/DropboxExternal/dataRaw/issued-building-permits.csv"
dtPermit <- fread(permitFile, select = c("PermitNumber","ProjectValue","TypeOfWork",
                                         "PropertyUse","SpecificUseCategory",
                                         "GeoLocalArea","geo_point_2d","YearMonth"))
dtPermit <- dtPermit[PropertyUse == "Dwelling Uses" &
                     TypeOfWork %in% c("New Building","Addition / Alteration")]
dtPermit[, c("permitLat","permitLon") := tstrsplit(geo_point_2d, ", ")]
dtPermit[, `:=`(permitLat = as.numeric(permitLat),
                permitLon = as.numeric(permitLon))]
dtPermit <- dtPermit[!is.na(permitLat) & !is.na(permitLon)]

CRITQUANT <- 0.25
qCrit <- quantile(dtPermit[TypeOfWork == "New Building" &
                           SpecificUseCategory %in% c("Duplex","Single Detached House") &
                           ProjectValue > 0, ProjectValue], CRITQUANT)
dtPermit <- dtPermit[TypeOfWork == "New Building" | ProjectValue >= qCrit]
dtPermit <- dtPermit[grepl("Duplex|Single Detached House|Laneway House|Multiple Dwelling",
                           SpecificUseCategory)]

sfPermit <- st_as_sf(dtPermit[, .(permitLat, permitLon)],
                     coords = c("permitLon","permitLat"), crs = 4326)
dZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson")
dZ <- dZ[dZ$zoning_district == "R1-1", "zoning_district"]
sfPermit <- st_transform(sfPermit, st_crs(dZ))
sfPermit <- st_join(sfPermit, dZ, join = st_within)
sfPermit <- sfPermit[!is.na(sfPermit$zoning_district), ]

# ---------------------------------------------------------------------------
# GWR: project to UTM 10N (meters) and fit
# ---------------------------------------------------------------------------
sfData       <- st_transform(st_as_sf(dtValuation, coords = c("lon","lat"), crs = 4326), 26910)
sfPermitProj <- st_transform(sfPermit, 26910)

dataCoords <- st_coordinates(sfData)
fitCoords  <- st_coordinates(sfPermitProj)

dtLocal <- gwrLocal(y          = log(dtValuation$ppsf),
                    x          = log(dtValuation$landArea),
                    dataCoords = dataCoords,
                    fitCoords  = fitCoords,
                    k          = 1000)
print(summary(dtLocal))

# Attach lat/lon back to fit points for plotting
fitLL <- st_coordinates(st_transform(sfPermitProj, 4326))
dtLocal[, `:=`(lon = fitLL[,1], lat = fitLL[,2])]

# ---------------------------------------------------------------------------
# Heatmap of local elasticity
# ---------------------------------------------------------------------------
ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 12) +
  geom_point(data = dtLocal, aes(lon, lat, color = slope), size = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  coord_sf(crs = 4326, xlim = c(-123.23, -123.02), ylim = c(49.20, 49.32)) +
  theme_void() +
  labs(color = "Local elasticity")

#ggplot(dtLocal, aes(x = lon, y = lat, color = slope)) +
#  geom_point(size = 0.8) +
#  scale_color_viridis_c(option = "plasma") +
#  theme_void() +
#  labs(color = "Local elasticity\nlog(ppsf) ~ log(landArea)")
#
ggsave("text/landElasticityHeatMapVancouver.png")

# ============================================================================
# ---- Sales -----------------------------------------------------------------
# ============================================================================
con <- dbConnect(SQLite(), cfg$sqliteFile)
dtSalesRaw <- dbGetQuery(con, "SELECT folioID, conveyanceDate, conveyancePrice,
                               conveyanceTypeDescription FROM sales") |> setDT()
dbDisconnect(con)

dtSalesRaw <- dtSalesRaw[conveyanceTypeDescription == "Improved Single Property Transaction"]
dtSalesRaw <- dtSalesRaw[, .(folioID, conveyanceDate, conveyancePrice)]

dtSalesAll <- merge(dtSalesRaw, dtBCA, by = "folioID")
dtSalesAll[, year       := as.numeric(substring(conveyanceDate, 1, 4))]
dtSalesAll[, logPrice   := log(as.numeric(conveyancePrice))]
dtSalesAll[, landWidth  := as.numeric(landWidth)]
dtSalesAll[, landDepth  := as.numeric(landDepth)]
dtSalesAll[, roundWidth := round(landWidth)]
dtSalesAll[, w50        := as.integer(roundWidth == 50)]
dtSalesAll[, w33        := as.integer(roundWidth == 33)]
dtSalesAll[, age        := year - as.numeric(MB_effective_year)]
dtSalesAll[, FSR        := as.numeric(MB_total_finished_area) / (landWidth * landDepth)]
dtSalesAll[, logFSR     := log(FSR)]
dtSalesAll[, logFSR2    := logFSR^2]
dtSalesAll[, logFSR3    := logFSR^3]
dtSalesAll[, logWidth   := log(landWidth)]


