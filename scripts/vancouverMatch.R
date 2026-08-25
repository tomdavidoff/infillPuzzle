
# Now do sales within x KM of permit# vancouverMatch.R
# match permits to neighbourhoods, assessments, etc
# Objective -- correlation and plots: local price levels vs duplex / multiplex choice.
# Tom Davidoff
# 08/13/26
#

SALE_MINYEAR <- 2015    # comp sales window start (pre-rezoning)
SALE_MAXYEAR <- 2017    # comp sales window end
CRITVAL         <- 0.5  # ProjectValue percentile threshold for non-new-SFD permits
MIN_PERMIT_YEAR <- 2018 # keep permit years strictly greater than this
USE_MIN_N       <- 100  # SpecificUseCategory kept if it has more than this many permits
MATCHDIST <- 7.5 # somewhat arbitrary, lots at 1 or below

library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)
library(RSQLite)
library(fixest)
library(parallel)
library(units)
library(scales)
library(patchwork)


sf_use_s2(TRUE)

# step 1: merge 2026 roll Number (start) geometry to 2018 sales and property characteristics
fGeo     <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
dirGpkg  <- "~/bigFiles/latestSpatialBCA/"
gpkgFiles <- list.files(dirGpkg, pattern = "\\.gpkg$", full.names = TRUE)
stopifnot(length(gpkgFiles) == 1)
fileGpkg <- gpkgFiles

if (!file.exists(fGeo)) {
  dfG <- st_read(fileGpkg, query =
    "SELECT ROLL_NUMBER, geom
     FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
     WHERE JURISDICTION_CODE = '200'")            # City of Vancouver

  # centroid lon/lat in WGS84 (parcel polygons -> single point per folio)
  dfG <- st_make_valid(dfG)
  dfG <- st_transform(dfG, 4326)
  ctr <- st_coordinates(st_centroid(st_geometry(dfG)))
  dtGeo <- as.data.table(st_drop_geometry(dfG))
  dtGeo[, `:=`(longitude = ctr[, 1], latitude = ctr[, 2])]
  dtGeo[, rollStart := floor(as.numeric(ROLL_NUMBER) / 1000)]
  dtGeo <- unique(dtGeo[, .(rollStart, longitude, latitude)])
  saveRDS(dtGeo, fGeo)
}
dtGeo <- readRDS(fGeo)
dtGeo[,rr:=frank(rollStart), by=rollStart]
dtGeo <- dtGeo[rr==1, .(rollStart, longitude, latitude)]

# BCA 2019 sales/inventory. Comp pool ground-oriented (condos purged in SQL).

fnameSingles <- "~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds"
fnameComps   <- "~/DropboxExternal/dataProcessed/bca19VancouverCompValuation.rds"
if (!file.exists(fnameSingles) | !file.exists(fnameComps)) {
	bca19 <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
	con <- dbConnect(RSQLite::SQLite(), bca19)
	dtBCA19 <- data.table(dbGetQuery(con, "
	  SELECT d.folioID, f.rollNumber, i.zoning, CAST(i.MB_effective_year AS NUMERIC) AS MB_effective_year, i.MB_total_finished_area,
		 CAST(i.land_width AS NUMERIC) AS land_width, CAST(i.land_depth AS NUMERIC) AS land_depth,
		 d.actualUseDescription, d.neighbourhoodDescription, CAST(d.landWidth AS NUMERIC) AS landWidth, CAST(d.landDepth AS NUMERIC) AS landDepth, CAST(v.landValue AS NUMERIC) AS landValue, CAST(v.improvementValue AS NUMERIC) AS improvementValue
	  FROM folio f
	  JOIN residentialInventory i ON i.roll_number = f.rollNumber
	  JOIN folioDescription d      ON d.folioID     = f.folioID
	  JOIN valuation v            ON v.folioID  = f.folioID
	  WHERE f.jurisdictionCode = '200'
	"))
	dtBCA19[, rollStart := floor(as.numeric(rollNumber) / 1000)]
	dtBCA19[is.na(landWidth), landWidth := land_width]
	dtBCA19[is.na(landDepth), landDepth := land_depth]
	dtBCA19 <- merge(dtBCA19, dtGeo, by = "rollStart", all.x = TRUE)
	dtSingles <- dtBCA19[actualUseDescription %in% c("Single Family Dwelling", "Residential Dwelling with Suite"),.(landWidth, landDepth, MB_total_finished_area, MB_effective_year, rollNumber, folioID, longitude, latitude,landValue)]
	dtSingles <- dtSingles[!is.na(longitude) & !is.na(latitude)]
	saveRDS(dtSingles, fnameSingles)
	dtComps <- dtBCA19[, .(MB_total_finished_area, MB_effective_year, rollNumber, folioID, longitude, latitude,landValue, improvementValue)]


	for (v in c("landWidth", "landDepth", "MB_total_finished_area", "MB_effective_year")) {
	  dtBCA19[, (v) := as.numeric(get(v))]
	}

	dtComps[, structAge := 2018 - MB_effective_year]
	dtComps[structAge < 0, structAge := 0]
	dtComps <- dtComps[!is.na(longitude) & !is.na(latitude)	& !is.na(MB_total_finished_area) & !is.na(MB_effective_year)  & !is.na(landValue) & !is.na(improvementValue)]
	dtComps[,totalValue := landValue + improvementValue]
	saveRDS(dtComps, fnameComps)
}
dtSales <- readRDS(fnameComps)
cat("comp sales after filters (pre-coordinate merge):", nrow(dtSales), "\n")
dtSingles <- readRDS(fnameSingles)
print(head(dtSales))
dtSales[,MB_total_finished_area := as.numeric(MB_total_finished_area)]

# Now read permits and find matching single family dwelling where it exist
dtP <- fread(
  "~/DropboxExternal/dataRaw/issued-building-permitsDwellingUses.csv",
  select = c("SpecificUseCategory", "PermitNumber", "geo_point_2d",
             "PermitNumberCreatedDate", "TypeOfWork", "ProjectValue")
)
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]

# ---- zoning: R1-1 shapes (used for permits AND for the comp inR1 flag) -------
dgZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson")
dgZ <- dgZ[dgZ$zoning_district == "R1-1", ]

dtPgeo <- st_as_sf(dtP[!is.na(lat), .(PermitNumber, lat, lon)],
                   coords = c("lon", "lat"), crs = 4326)
dtPgeo <- st_join(dtPgeo, dgZ, join = st_within)
r1Permits <- unique(as.data.table(dtPgeo)$PermitNumber)
cat("R1-1 permits:", length(r1Permits), "\n")

# DEFINE THE ANALYSIS PERMIT SAMPLE 

dtP <- dtP[PermitNumber %in% r1Permits]
dtP[, year   := as.numeric(substr(PermitNumberCreatedDate, 1, 4))]
dtP <- dtP[year > MIN_PERMIT_YEAR]
useList  <- dtP[, .N, by = SpecificUseCategory][order(-N)][N > USE_MIN_N, SpecificUseCategory]
dtP <- dtP[SpecificUseCategory %in% useList]
dtP[, single := grepl("Single", SpecificUseCategory)]
minSpend <- quantile(dtP[TypeOfWork=="New Building" & single==1, ProjectValue], CRITVAL)
dtP <- dtP[TypeOfWork == "New Building"  | (ProjectValue > minSpend)]
dtP <- dtP[!is.na(lat) & !is.na(lon)]
dtP[, duplex := grepl("Duplex", SpecificUseCategory)]
dtP[, multiPlex := SpecificUseCategory == "Multiple Dwelling"]
cat("analysis permits after all filters:", nrow(dtP), "\n")
print(useList)

# try to match with a close dtSingle obs
# ---- lot geometry from nearest folio (for landWidth/landDepth splits) --------
dtPgeoNN <- st_as_sf(dtP[, .(PermitNumber, lat, lon)], coords = c("lon", "lat"), crs = 4326)
dfSingle <- st_as_sf(dtSingles, coords = c("longitude", "latitude"), crs = 4326)

idx  <- st_nearest_feature(dtPgeoNN, dfSingle)
dist <- as.numeric(st_distance(dtPgeoNN, dfSingle[idx, ], by_element = TRUE))  # meters, plain numeric
print(summary(dist))

matched <- as.data.table(st_drop_geometry(dfSingle))[idx]   # nearest folio's attrs, one row per permit
matched[dist > MATCHDIST, names(matched) := NA]                    # data.table-correct gate at 10 m
matched[, PermitNumber := dtP$PermitNumber]                 # attach the key (order-safe: idx is in dtP order)
matched[, dist := dist]
print(matched)
matched <- matched[dist<MATCHDIST, .(PermitNumber, rollNumber, dist,MB_total_finished_area, MB_effective_year, landWidth, landDepth,landValue)]
print(nrow(dtP))
dtP <- merge(dtP, matched, by = "PermitNumber")
print(head(dtP))
print(nrow(dtP))

# Now do sales within x KM of permit
# ---- 2D kernel price estimates at 1250 & 2500 sqft for each permit --------
# distance dims: geographic (km) and floor area (sqft). Product of Epanechnikov kernels.
dtSales <- dtSales[!is.na(longitude) & !is.na(latitude) &
                   !is.na(MB_total_finished_area) & MB_total_finished_area > 0]
print(summary(dtSales[,totalValue/MB_total_finished_area]))
quants <- quantile(dtSales[,totalValue/MB_total_finished_area], c(0.01, 0.99))
dtSales <- dtSales[totalValue/MB_total_finished_area > quants[1] &
		   totalValue/MB_total_finished_area < quants[2]]
sfSales <- st_transform(st_as_sf(dtSales, coords = c("longitude","latitude"), crs = 4326), 3005)
sfPerm  <- st_transform(st_as_sf(dtP,     coords = c("lon","lat"),           crs = 4326), 3005)


MAXAGE  <- 10                                   # A: max comp structure age at sale
KM      <- 1.0                                  # d: geographic radius, km
AREABW  <- sd(dtSales$MB_total_finished_area)/2   # size bandwidth = 1 SD of area (sd appx 1Ksf)
TARGETS <- c(1250, 2500)

cat("area bandwidth (1 SD sqft):", round(AREABW), "\n")

pc <- st_coordinates(sfPerm)
sc <- st_coordinates(sfSales)
p  <- dtSales$totalValue
a  <- dtSales$MB_total_finished_area
age <- dtSales$structAge

epan <- function(u) 0.75 * (1 - u*u) * (abs(u) < 1)   # zero outside [-1,1]

estOne <- function(i) {
  dx <- sc[,1] - pc[i,1]
  dy <- sc[,2] - pc[i,2]
  dgeo <- sqrt(dx*dx + dy*dy) / 1000                  # km
  wgeo <- epan(dgeo / KM) * (age <= MAXAGE)           # geo kernel + age gate

  out <- numeric(length(TARGETS))
  nn  <- integer(length(TARGETS))
  for (k in seq_along(TARGETS)) {
    warea <- epan((a - TARGETS[k]) / AREABW)          # size kernel around this target
    w <- wgeo * warea
    ok <- w > 0
    nn[k]  <- sum(ok)
    out[k] <- if (any(ok)) weighted.mean(p[ok]/a[ok], w[ok]) else NA_real_
  }
  c(out, nn)
}

res <- t(vapply(seq_len(nrow(sfPerm)), estOne, numeric(2 * length(TARGETS))))
dtP[, `:=`(kEst1250 = res[,1], kEst2500 = res[,2],
           kN1250   = as.integer(res[,3]), kN2500 = as.integer(res[,4]))]

cat("permits with >=1 comp @1250:", sum(dtP$kN1250 > 0),
    " @2500:", sum(dtP$kN2500 > 0), "of", nrow(dtP), "\n")
print(summary(dtP[, .(kEst1250, kEst2500, kN1250, kN2500)]))

# ---- cartolight heatmap of fitted price for each target -------------------
dtP[,landArea := landWidth * landDepth]
dtP <- dtP[landDepth %between% c(100,150) & landWidth %between% c(30,60)]
dtP[, landValue := landValue / (landWidth * landDepth)]
sfP <- st_as_sf(dtP, coords = c("lon","lat"), crs = 4326)

mapOne <- function(col, ttl)
  ggplot(sfP) +
    annotation_map_tile(type = "cartolight", zoomin = -1, progress = "none") +
    geom_sf(aes(colour = .data[[col]]), size = 0.9, alpha = 0.85) +
    scale_colour_viridis_c(option = "magma", direction = -1,
                           labels = label_dollar(scale = 1e-6, suffix = "M"), name = NULL) +
    labs(title = ttl) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.01))

pp <- mapOne("kEst1250", "Fitted price — 1250 sqft") +
      mapOne("kEst2500", "Fitted price — 2500 sqft")+
      mapOne("landValue", "Land value (BC Assessment)") 

ggsave("text/fittedKernelPriceMaps.png", pp,
       width = 14, height = 7, dpi = 200, bg = "white")
print(pp)

# plot simple xy fiited KEst1250 and KEst2500
ggplot(dtP, aes(x = kEst1250, y = kEst2500)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 2, intercept = 0, colour = "red", linetype = "dashed") +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(x = "Fitted price @1250 sqft", y = "Fitted price @2500 sqft",
       title = "Kernel-fitted prices at two target sizes") +
  theme_minimal(base_size = 12)
ggsave("text/fittedKernelPriceXY.png", width = 7, height = 7, dpi = 200, bg = "white")


# land value per square foot
print(summary(feols(duplex ~ log(kEst2500/kEst1250) + landWidth+log(kEst1250), data = dtP[year<2024 ],vcov=vcov_conley(lat="lat",lon="lon",cutoff=2.0))))
print(summary(feols(duplex ~ log(kEst2500/kEst1250) + landWidth+log(landValue), data = dtP[year<2024 ],vcov=vcov_conley(lat="lat",lon="lon",cutoff=2.0))))
print(summary(feols(duplex ~ log(kEst1250) + log(kEst2500) + landWidth+log(landValue), data = dtP[year<2024 ],vcov=vcov_conley(lat="lat",lon="lon",cutoff=2.0))))
print(summary(feols(multiPlex ~ log(kEst1250) + log(kEst2500) + landWidth, data = dtP[year>=2024 ],vcov=vcov_conley(lat="lat",lon="lon",cutoff=2.0))))

# ---- sub-1500 comps coloured by price/sqft --------------------------------
sfSmall <- st_as_sf(dtSales[MB_total_finished_area < 1500 &
                            !is.na(longitude) & !is.na(latitude) &
                            & MB_total_finished_area > 0 & structAge<=MAXAGE],
                    coords = c("longitude","latitude"), crs = 4326)

ggplot(sfSmall) +
  annotation_map_tile(type = "cartolight", zoomin = -1, progress = "none") +
  geom_sf(aes(colour = MB_total_finished_area), size = 0.7, alpha = 0.7) +
  scale_colour_viridis_c(option = "magma", direction = -1,
                         limits = lims, oob = scales::squish,
                         labels = scales::label_dollar(),
                         name = "sqft") +
  labs(title = "Sub-1500 sqft comps — sqft") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.01),
        legend.position = "right")
ggsave("text/vancouverSub1500.png")

# size distribution by use — is sub-1500 detached actually rare?
dtSales[, .N, by = actualUseDescription][order(-N)]
dtSales[MB_total_finished_area < 1500, .N, by = actualUseDescription][order(-N)]
dtSales[MB_total_finished_area < 1250, .N, by = actualUseDescription][order(-N)]
print(quantile(dtSales$MB_total_finished_area, seq(.05,1,.05), na.rm = TRUE))
q("no")
# ---- kernel price estimates at 1250 & 2500 sqft for each permit ----------
# comp sales -> sf, project both to a metric CRS for planar distance
dtSales <- dtSales[!is.na(longitude) & !is.na(latitude)]
sfSales  <- st_transform(st_as_sf(dtSales, coords = c("longitude","latitude"), crs = 4326), 3005)  # BC Albers, metres
sfPerm   <- st_transform(st_as_sf(dtP,     coords = c("lon","lon"),           crs = 4326), 3005)

# NOTE: your dtP has lat/lon; guard against the c("lon","lon") typo:
sfPerm   <- st_transform(st_as_sf(dtP, coords = c("lon","lat"), crs = 4326), 3005)

MAXAGE   <- 30      # A: max comp structure age at sale
KM       <- 1.0     # d: radius in km
TARGETS  <- c(1250, 2500)

pc <- st_coordinates(sfPerm)
sc <- st_coordinates(sfSales)

estOne <- function(i) {
  dx <- sc[,1] - pc[i,1]
  dy <- sc[,2] - pc[i,2]
  d  <- sqrt(dx*dx + dy*dy) / 1000                      # km
  ok <- d <= KM & dtSales$structAge <= MAXAGE
  if (!any(ok)) return(c(NA_real_, NA_real_, 0L))
  u  <- d[ok] / KM
  w  <- 0.75 * (1 - u*u)                                # Epanechnikov
  p  <- dtSales$conveyancePrice[ok]
  a  <- dtSales$MB_total_finished_area[ok]
  # price per sqft, kernel-weighted, then scale to each target size
  pps <- weighted.mean(p / a, w)
  c(pps * TARGETS[1], pps * TARGETS[2], sum(ok))
}

res <- t(vapply(seq_len(nrow(sfPerm)), estOne, numeric(3)))
dtP[, `:=`(kEst1250 = res[,1], kEst2500 = res[,2], kN = as.integer(res[,3]))]

cat("permits with >=1 comp:", sum(dtP$kN > 0), "of", nrow(dtP), "\n")
print(summary(dtP[, .(kEst1250, kEst2500, kN)]))

q("no")


#shana
dtPgeoNN <- st_as_sf(dtP[, .(PermitNumber, lat, lon)], coords = c("lon", "lat"), crs = 4326)
dfSingle <- st_as_sf(dtSingles, coords = c("longitude", "latitude"), crs = 4326)
idx  <- st_nearest_feature(dtPgeoNN, dfSingle)
dist <- st_distance(dtPgeoNN, dfSingle[idx, ], by_element = TRUE)
cols    <- setdiff(names(dfSingle), attr(dfSingle, "sf_column"))
matched <- st_drop_geometry(dfSingle)[idx]
matched[dist > set_units(10, "m"), ] <- NA
dtPgeoNN$dist <- dist
dtPgeoNN <- as.data.table(dtPgeoNN)[, .(PermitNumber, rollNumber)]
print(dtPgeoNN)
q("no")
# lot attributes: exact folio join on ROLL_NUMBER via dtGeo->dtBCA19 would fan out on
# rollStart, so pull them from dtBCA19 keyed to the matched folio's rollStart, taking
# ONE row per rollStart to avoid duplication.
dtPgeoNN[, rollStart := floor(as.numeric(ROLL_NUMBER) / 1000)]
lotByRoll <- unique(dtBCA19[, .(rollStart, landWidth, landDepth, MB_total_finished_area,
                                MB_effective_year, neighbourhoodDescription)],
                     by = "rollStart")
dtPgeoNN <- merge(dtPgeoNN, lotByRoll, by = "rollStart", all.x = TRUE)
dtP <- merge(dtP, dtPgeoNN, by = "PermitNumber")
print(table(dtP[, .(SpecificUseCategory, multiPlex)]))


#
# ---- kernel / prediction tuning knobs ---------------------------------------
AGE_DECAY    <- 20      # years; age weight = exp(-structAge / AGE_DECAY)
SIZE_BW      <- log(1.5)# log-sqft; size weight = exp(-0.5 * (dlogsqft / SIZE_BW)^2)
KNN_TARGET   <- 100     # global radius set from distance to this-th nearest comp
KNN_PCTL     <- 0.80    # ...taken at this percentile across permits
TARGET_SQFT  <- c(1250, 2500)   # sizes to predict
MIN_EFFN     <- 8       # local fit requires this much Kish effective N (else NA)
CRS_M        <- 3005    # BC Albers, meters
MC_CORES     <- 4       # cores for the per-permit local fits

# ---- analysis-sample knobs (define which permits enter the kernel) ----------
# -----------------------------------------------------------------------------

# ============================ permits ========================================

# get Vancouver sales with lat/lon from 2018 # 
# merge from current geom #
# coordinate source: 2026 unified BCA GeoPackage #
# Pull folio parcel geometry only; hedonics/prices come from the 2018 extract.
# One row per folio: FOLIO_ID, ROLL_NUMBER, rollStart, lon, lat.

dtGeo <- readRDS(fGeo)
print(summary(dtGeo[,var(longitude),by=rollStart]))
print(summary(dtGeo[,var(latitude),by=rollStart]))


 ============================================================================
# comp geometry: ground-oriented improved residential folios (coords for comp pool).
# Cache carries folioID (if present in the layer), ROLL_NUMBER, coords, and inR1.
# NOTE: this cache now stores inR1 -- if the previous .rds lacks it, DELETE the cache:
#   file.remove("~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds")
# ============================================================================
fTract <- "~/DropboxExternal/dataProcessed/censusTractsBC.rds"
if (!file.exists(fTract)) {
  dfT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
  dfT <- dfT[dfT$PRUID == "59", ]
  dfT <- st_make_valid(dfT)
  dfT <- st_transform(dfT, crs = 4326)
  saveRDS(dfT, fTract)
}
dfT <- readRDS(fTract)

fGeo     <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
dirGpkg  <- "~/bigFiles/latestSpatialBCA/"
gpkgFiles <- list.files(dirGpkg, pattern = "\\.gpkg$", full.names = TRUE)
stopifnot(length(gpkgFiles) == 1)
fileGpkg <- gpkgFiles

if (!file.exists(fGeo)) {
  # pull folioID too (layer has it); keep ROLL_NUMBER for the R1 rollStart path
  dfG <- st_read(fileGpkg, query =
    "SELECT FOLIO_ID, ROLL_NUMBER, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD, geom
     FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
     WHERE JURISDICTION = 'City of Vancouver'
       AND (ACTUAL_USE_DESCRIPTION IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
            OR ACTUAL_USE_DESCRIPTION LIKE '%plex%'
            OR ACTUAL_USE_DESCRIPTION LIKE '%Row Hous%')"
  )

  # census tract tag (unchanged purpose)
  dCT <- dfT[dfT$PRUID == "59", ]
  dCT <- st_transform(dCT, st_crs(dfG))
  dfG <- st_join(dfG, dCT, join = st_within)

  # inR1 flag: point-in-polygon of each folio centroid against R1-1 shapes.
  dfGpt  <- st_transform(st_centroid(st_geometry(dfG)), st_crs(dgZ))
  inR1   <- lengths(st_within(dfGpt, dgZ)) > 0
  dfG$inR1 <- inR1

  dfG <- st_transform(dfG, crs = 4326)
  dfG$centroid  <- st_centroid(st_geometry(dfG))
  dfG$longitude <- st_coordinates(dfG$centroid)[, 1]
  dfG$latitude  <- st_coordinates(dfG$centroid)[, 2]
  dfG$centroid  <- NULL
  saveRDS(dfG, fGeo)
}
dfG <- readRDS(fGeo)
dtGeo <- as.data.table(dfG)[, .(FOLIO_ID, ROLL_NUMBER, CTNAME, ACTUAL_USE_DESCRIPTION,
                                NEIGHBOURHOOD, inR1, longitude, latitude, geom)]
print(table(dtGeo$ACTUAL_USE_DESCRIPTION)[order(-table(dtGeo$ACTUAL_USE_DESCRIPTION))])
cat("geometry folios inR1:", sum(dtGeo$inR1), "of", nrow(dtGeo), "\n")
dtGeo[, rollStart := floor(as.numeric(ROLL_NUMBER) / 1000)]

# ============================================================================
# (C) REGION-SPLIT COORDINATE MERGE -- kills rollStart fan-out.
#   A sale is tagged inR1 by whether its folio has an R1 geometry match. We route:
#     inR1  -> merge on rollStart, geometry collapsed to ONE point per rollStart
#     !inR1 -> merge on folioID (FOLIO_ID), exact one-to-one
# ============================================================================

# folioID-keyed geometry (non-R1 path): one row per folio, exact.
dtGeoFolio <- unique(dtGeo[!is.na(FOLIO_ID) & !is.na(longitude) & !is.na(latitude),
                           .(FOLIO_ID, longitude, latitude)], by = "FOLIO_ID")

# rollStart-collapsed geometry (R1 path): one representative point per rollStart,
# built ONLY from folios that are inR1, so non-R1 folios can't pollute the centroid.
dtGeoRoll <- dtGeo[inR1 == TRUE & !is.na(longitude) & !is.na(latitude),
                   .(longitude = mean(longitude), latitude = mean(latitude),
                     nFolio = .N),
                   by = rollStart]
cat("R1 rollStart fan-out: median", dtGeoRoll[, median(nFolio)],
    " p95", dtGeoRoll[, quantile(nFolio, .95)],
    " max", dtGeoRoll[, max(nFolio)], "\n")

# which sales are R1? a sale's folio is R1 if it appears in the inR1 geometry set.
r1Folios <- dtGeo[inR1 == TRUE, unique(FOLIO_ID)]
dtSales[, inR1 := folioID %in% r1Folios]
cat("comp sales inR1:", dtSales[, sum(inR1)], "of", nrow(dtSales), "\n")

# R1 comps -> rollStart (collapsed); non-R1 comps -> folioID (exact)
xyR1 <- merge(
  dtSales[inR1 == TRUE, .(folioID, rollStart, conveyancePrice, MB_total_finished_area, structAge)],
  dtGeoRoll[, .(rollStart, longitude, latitude)],
  by = "rollStart"
)
xyOut <- merge(
  dtSales[inR1 == FALSE, .(folioID, conveyancePrice, MB_total_finished_area, structAge)],
  dtGeoFolio, by.x = "folioID", by.y = "FOLIO_ID"
)

dtSalesXY <- rbindlist(list(
  xyR1[,  .(folioID, conveyancePrice, MB_total_finished_area, structAge, longitude, latitude)],
  xyOut[, .(folioID, conveyancePrice, MB_total_finished_area, structAge, longitude, latitude)]
))
dtSalesXY <- dtSalesXY[!is.na(longitude) & !is.na(latitude)]
cat("comp sales with coordinates:", nrow(dtSalesXY),
    "(should be <=", nrow(dtSales), ")\n")
print(summary(dtSalesXY[, .(conveyancePrice, MB_total_finished_area, structAge)]))

# ================= per-permit three-way (space x age x size) kernel ==========
sfComp <- st_transform(st_as_sf(dtSalesXY, coords = c("longitude", "latitude"), crs = 4326), CRS_M)
compXY <- st_coordinates(sfComp)

dtPermPts <- unique(dtP[, .(PermitNumber, lat, lon)])
sfPerm <- st_transform(st_as_sf(dtPermPts, coords = c("lon", "lat"), crs = 4326), CRS_M)
permXY <- st_coordinates(sfPerm)

if (!requireNamespace("FNN", quietly = TRUE))
  install.packages("FNN", repos = "https://cloud.r-project.org")
kk    <- min(KNN_TARGET, nrow(compXY))
knn   <- FNN::get.knnx(compXY, permXY, k = kk)
distK <- knn$nn.dist[, kk]
R     <- as.numeric(quantile(distK, KNN_PCTL, na.rm = TRUE))
cat(sprintf("global radius R = %.0f m (%.2f km); %d permits, %d comps, k=%d\n",
            R, R / 1000, nrow(permXY), nrow(compXY), kk))

SPACE_BW <- R / 2
nbList   <- st_is_within_distance(sfPerm, sfComp, dist = R)

price   <- dtSalesXY$conveyancePrice
sqft    <- dtSalesXY$MB_total_finished_area
logSqft <- log(sqft)
ppsf    <- price / sqft
logPpsf <- log(ppsf)
wAge    <- exp(-dtSalesXY$structAge / AGE_DECAY)
logTgt  <- log(TARGET_SQFT)

# For each permit we return, per target size s*:
#   priceFlat  = mean_w(PPSF) * s*     [w = space x age x size(s*)]
#   priceSlope = exp( a + b*log(s*) )  from local logPPSF ~ log(sqft), then * s*... but
#     since logPPSF = log(price) - log(sqft), we fit log(price) ~ log(sqft) locally and
#     read at s* directly (identical target, avoids double-logging).
# Two separate size-kernels (one per s*) => two independent local fits; no slope carried.

TARGET_N <- length(TARGET_SQFT)
naRes <- list(flat = rep(NA_real_, TARGET_N), slope = rep(NA_real_, TARGET_N),
              effN = rep(0, TARGET_N), n = 0L)

rowOp <- function(i) {
  ii <- nbList[[i]]
  if (length(ii) < MIN_EFFN) return(naRes)
  d   <- sqrt((compXY[ii, 1] - permXY[i, 1])^2 + (compXY[ii, 2] - permXY[i, 2])^2)
  wS  <- pmax(0, 1 - (d / SPACE_BW)^2)^2          # spatial bisquare
  wA  <- wAge[ii]                                  # age
  ls  <- logSqft[ii]
  lp  <- logPpsf[ii]
  lpr <- lp + ls                                   # = log(price)
  flat <- rep(NA_real_, TARGET_N); slope <- rep(NA_real_, TARGET_N); eff <- rep(0, TARGET_N)
  for (t in seq_len(TARGET_N)) {
    wZ <- exp(-0.5 * ((ls - logTgt[t]) / SIZE_BW)^2)   # size gaussian around s*
    w  <- wS * wA * wZ
    sw <- sum(w)
    if (!is.finite(sw) || sw <= 0) next
    eff[t] <- sw^2 / sum(w^2)                          # Kish effN in this size band
    if (eff[t] < MIN_EFFN) next
    # FLAT: weighted mean PPSF -> price at s*
    mLogPpsf <- sum(w * lp) / sw
    flat[t]  <- exp(mLogPpsf) * TARGET_SQFT[t]
    # SLOPE: local log(price) ~ log(sqft), read at s* (needs sqft variation)
    if (length(unique(ls[w > 0])) >= 2) {
      fit <- tryCatch(stats::lm.wfit(cbind(1, ls), lpr, w), error = function(e) NULL)
      if (!is.null(fit) && !any(is.na(fit$coefficients))) {
        b <- fit$coefficients
        slope[t] <- exp(b[1] + b[2] * logTgt[t])
      }
    }
  }
  list(flat = flat, slope = slope, effN = eff, n = length(ii))
}

res <- mclapply(seq_len(nrow(permXY)), rowOp, mc.cores = MC_CORES)
res <- lapply(res, function(r) if (is.list(r) && !is.null(r$flat)) r else naRes)
nBad <- sum(vapply(res, function(r) all(is.na(r$flat)), logical(1)))
cat("permits with no flat price (any size):", nBad, "of", length(res), "\n")

flatMat  <- do.call(rbind, lapply(res, `[[`, "flat"))
slopeMat <- do.call(rbind, lapply(res, `[[`, "slope"))
effMat   <- do.call(rbind, lapply(res, `[[`, "effN"))
nComp    <- vapply(res, `[[`, integer(1), "n")

dtPred <- data.table(
  PermitNumber   = dtPermPts$PermitNumber,
  price1250      = flatMat[, 1],    # FLAT PPSF (primary)
  price2500      = flatMat[, 2],
  price1250_sl   = slopeMat[, 1],   # SLOPE (robustness)
  price2500_sl   = slopeMat[, 2],
  effN1250       = effMat[, 1],
  effN2500       = effMat[, 2],
  nCompKernel    = nComp
)
i <- which(!is.na(dtPred$price1250))[1:5]
for (k in i) {
  ii <- nbList[[k]]
  cat("permit", k, ": n=", length(ii),
      " sqft IQR=", paste(round(quantile(dtSalesXY$MB_total_finished_area[ii], c(.25,.75))), collapse="-"),
      "\n")
}
cat("permits with non-NA price1250 (flat):", dtPred[!is.na(price1250), .N],
    "  price2500 (flat):", dtPred[!is.na(price2500), .N], "of", nrow(dtPred), "\n")
print(summary(dtPred[, .(price1250, price2500, price1250_sl, price2500_sl,
                         effN1250, effN2500, nCompKernel)]))
# flat vs slope agreement (should be close where both exist)
print(dtPred[!is.na(price1250) & !is.na(price1250_sl),
             .(cor1250 = cor(log(price1250), log(price1250_sl)),
               cor2500 = cor(log(price2500), log(price2500_sl), use = "complete.obs"))])

dtP <- merge(dtP, dtPred, by = "PermitNumber", all.x = TRUE)
# ============================================================================

# correlations: duplex vs the two local price levels (flat primary)
print(cor(dtP[, .(duplex, price1250, price2500)], use = "complete.obs"))

print("CLAUDE!")
dtP[!is.na(price1250_sl) & !is.na(price2500_sl), cor(log(price1250_sl), log(price2500_sl))]
ggplot(dtP[!is.na(price1250)], aes(x = log(price1250), y = log(price2500))) +
  geom_point(aes(color = nCompKernel), alpha = 0.5) + theme_minimal()
ggsave("text/kernelPrices.png", width = 8, height = 6)

# ================================ regressions ================================
dtD <- dtP[(single == TRUE | duplex == TRUE) & year < 2024]

# FLAT (primary): duplex vs local low-end and high-end price levels, by lot width
print(summary(feols(duplex ~ log(price2500) + landDepth | year,
                    data = dtD[round(landWidth) == 33], cluster = "neighbourhoodDescription")))
print(summary(feols(duplex ~ log(price2500) + landDepth | year,
                    data = dtD[round(landWidth) == 50], cluster = "neighbourhoodDescription")))

print(summary(feols(duplex ~ log(price1250) + log(price2500) + landDepth | year,
                    data = dtD[round(landWidth) == 33], cluster = "neighbourhoodDescription")))
print(summary(feols(duplex ~ log(price1250) + log(price2500) + landDepth | year,
                    data = dtD[round(landWidth) == 50], cluster = "neighbourhoodDescription")))

# SLOPE variant (robustness): same spec on the slope-based prices
print(summary(feols(duplex ~ log(price1250_sl) + log(price2500_sl) + landDepth | year,
                    data = dtD[round(landWidth) == 33], cluster = "neighbourhoodDescription")))
print(summary(feols(duplex ~ log(price1250_sl) + log(price2500_sl) + landDepth | year,
                    data = dtD[round(landWidth) == 50], cluster = "neighbourhoodDescription")))

dtM <- dtP[year >= 2024 & (single == TRUE | duplex == TRUE | multiPlex == TRUE)]

print(summary(feols(multiPlex ~ log(price1250) + log(price2500) + landDepth | year,
                    data = dtM[round(landWidth) == 33], cluster = "neighbourhoodDescription")))
print(summary(feols(multiPlex ~ log(price1250) + log(price2500) + landDepth | year,
                    data = dtM[round(landWidth) == 50], cluster = "neighbourhoodDescription")))

print(summary(dtD[, landWidth]))
print(summary(dtM[, landWidth]))

q("no")
    AND (d.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
         OR d.actualUseDescription LIKE '%plex%'
         OR d.actualUseDescription LIKE '%Row Hous%')

