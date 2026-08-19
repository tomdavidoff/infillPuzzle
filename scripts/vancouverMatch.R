# vancouverMatch.R
# match permits to neighbourhoods, assessments, etc
# Objective -- correlation and plots elasticity, price, duplex share. Then do multiplex share
# Tom Davidoff
# 08/13/26
#
# 08/19/26 revision: replace the RS-only / single-family hedonic-by-tract block with a
# per-permit distance-weighted local kernel price prediction.
#   - Comp pool: ALL improved-property Vancouver sales STARTYEAR..ENDYEAR, agnostic on
#     zoning and property type, ignoring land area entirely (sqft of structure only).
#   - For each R1 permit (permit years 2019-2023), local LINEAR fit of
#     log(price) ~ log(sqft) over in-radius comps, weighted by structure age at sale
#     (exp(-age/AGE_DECAY)), predicting price at 1250 and 2500 sqft.
#   - Spatial weighting: HARD truncation at a single global radius R, with NO distance
#     taper. R = 80th percentile across permits of each permit's distance to its
#     KNN_TARGET-th (100th) nearest comp.
#   - All distances in projected meters (EPSG:3005, BC Albers).

library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)
library(RSQLite)
library(fixest)
library(units)

sf_use_s2(TRUE) # (5) ensure s2 for fast lat/lon st_within

# ---- kernel / prediction tuning knobs ---------------------------------------
SALE_MINYEAR <- 2015    # comp sales window start (pre-rezoning, avoids supply endogeneity)
SALE_MAXYEAR <- 2017    # comp sales window end
AGE_DECAY    <- 10      # years; weight = exp(-(saleYear - effectiveYear)/AGE_DECAY)
KNN_TARGET   <- 100     # global radius set from distance to this-th nearest comp
KNN_PCTL     <- 0.80    # ...taken at this percentile across permits
TARGET_SQFT  <- c(1250, 2500)   # sizes to predict
MIN_COMPS    <- 10      # permits with fewer in-radius comps -> NA prediction
CRS_M        <- 3005    # BC Albers, meters
# -----------------------------------------------------------------------------

# permits
# (7) only read the columns actually used downstream
dtP <- fread(
  "~/DropboxExternal/dataRaw/issued-building-permitsDwellingUses.csv",
  select = c("SpecificUseCategory", "PermitNumber", "geo_point_2d","PermitNumberCreatedDate","TypeOfWork","ProjectValue")
) # pre-filtered for dwelling uses on download from Vancouver Open Data
print(head(dtP))

# grab zoning and census tracts to make spatial
dgZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson") # vancouver_zoning.geojson
print(head(dgZ))
dgZ <- dgZ[dgZ$zoning_district=="R1-1",]

# use data below on dtP variables to convert to sf with key PermitNumber -- just export geo variables and PermitNumber
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]

dtPgeo <- st_as_sf(dtP[!is.na(lat),.(SpecificUseCategory,PermitNumber,lat,lon)], coords = c("lon", "lat"), crs = 4326)
print(head(dtPgeo))

# (4) pre-filter permits to the zoning bbox before the join to shrink the join input
dtPgeo <- dtPgeo[st_intersects(dtPgeo, st_as_sfc(st_bbox(dgZ)), sparse = FALSE)[,1], ]

# merge as point within shapes to dgZ
dtPgeo <- st_join(dtPgeo, dgZ, join = st_within) # spatial join to zoning shapes
print(head(dtPgeo))
print(table(dtPgeo$zoning_district)) # check zoning districts
write.table(table(is.na(dtPgeo$zoning_district),dtPgeo$SpecificUseCategory),"text/sanityCheckVancouverZoning.txt") # check zoning districts
dtPgeo$SpecificUseCategory <- NULL
dtPgeo <- dtPgeo[!is.na(dtPgeo$zoning_district),] # drop NA zoning districts

# census tract shapes
# (2,3) build a Vancouver-only, validated, reprojected tract subset once and cache it.
# (3) filter to BC (PRUID 59) BEFORE make_valid/transform so we don't validate all of Canada.
fTract <- "~/DropboxExternal/dataProcessed/censusTractsBC.rds"
if (!file.exists(fTract)) {
  dfT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
  dfT <- dfT[dfT$PRUID == "59", ]        # (3) BC only, before validate/transform
  dfT <- st_make_valid(dfT)
  dfT <- st_transform(dfT, crs = 4326)
  saveRDS(dfT, fTract)
}
dfT <- readRDS(fTract)


dtPgeo <- st_join(dtPgeo, dfT, join = st_within) # spatial join to census tract shapes
print(head(dtPgeo))

dtPgeo <- as.data.table(dtPgeo)[,.(PermitNumber,DGUID,CTNAME,geometry)]

# now get appraised values and sales for single family homes by tract for 2017, pre re-zonings. Do current appraisals and 2017 sales to show correlations. Can get all this from current data

# get census tract from lat/lon
fGeo <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
dirGpkg <- "~/bigFiles/latestSpatialBCA/"
fGpkg <- list.files(dirGpkg)
fileGpkg <- paste0(dirGpkg,fGpkg)

if (!file.exists(fGeo)) {
  # Note: Extracting NEIGHBOURHOOD (BCA code) and joining with Census Tracts for Income

  dfG <- st_read(fileGpkg,query =
	"SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD, geom
	FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
	WHERE JURISDICTION = 'City of Vancouver' AND (ACTUAL_USE_DESCRIPTION IN ('Single Family Dwelling', 'Residential Dwelling with Suite') OR ACTUAL_USE_DESCRIPTION LIKE '%plex%') "
  )

  dCT <- dfT[dfT$PRUID == "59", ]
  dCT <- st_transform(dCT, st_crs(dfG))

  dfG <- st_join(dfG, dCT, join = st_within)
  # now return to normal lat lon crs and record longitude and latitude of parcel geom (not geom is a polygon)
  dfG <- st_transform(dfG, crs = 4326)
  # Need a single coordinate pair, as geom is a polygon()
  dfG$centroid <- st_centroid(dfG)
  dfG$longitude <- st_coordinates(dfG$centroid)[, 1]
  dfG$latitude <- st_coordinates(dfG$centroid)[, 2]

  saveRDS(dfG, fGeo)
}
dfG <- readRDS(fGeo)
dtGeo <- as.data.table(dfG)[, .(ROLL_NUMBER, CTNAME, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD, longitude, latitude, geom)]
# use ggspatial with street map
checkGEOPlotName <- "text/checkGEO.png"
if (!file.exists(checkGEOPlotName)) {
ggplot(dtGeo, aes(x = longitude, y = latitude, color = NEIGHBOURHOOD)) +
  annotation_map_tile(type = "cartolight", zoom = 12) +
  geom_point(size = 0.5) +
  coord_sf(crs = 4326) +
  theme(legend.position = "none") +
  theme_minimal()
ggsave(checkGEOPlotName, width = 8, height = 6)
}

print(summary(dtGeo))
dtGeo[,rollStart:=floor(as.numeric(ROLL_NUMBER)/1000)]

# Merge with BCA 2019 sales/inventory.
# NOTE (08/19/26): comp pool is now AGNOSTIC on zoning and property type. We keep the
# use-description filter OUT of SQL and pull effective year + finished area for every
# improved sale, then apply only the sqft/price/age sanity filters below.
bca19 <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
con <- dbConnect(RSQLite::SQLite(), bca19)
dtBCA19 <- data.table(dbGetQuery(con, "
  SELECT d.folioID, f.rollNumber, i.zoning, i.MB_effective_year, i.MB_total_finished_area,
         i.land_width, i.land_depth,
         d.actualUseDescription, d.neighbourhoodDescription, d.landWidth, d.landDepth
  FROM folio f
  JOIN residentialInventory i ON i.roll_number = f.rollNumber
  JOIN folioDescription d      ON d.folioID     = f.folioID
  WHERE f.jurisdictionCode = '200'
"))
dfSales <- dbGetQuery(con, "SELECT folioID, conveyanceDate, conveyancePrice FROM sales WHERE conveyanceTypeDescription = 'Improved Single Property Transaction' ")
print(head(dtBCA19))
print(table(dtBCA19[,actualUseDescription])[order(-table(dtBCA19[,actualUseDescription]))])
print(nrow(dtBCA19))

dtBCA19[,rollStart:=floor(as.numeric(rollNumber)/1000)]
dtBCA19[is.na(landWidth),landWidth:=land_width]
dtBCA19[is.na(landDepth),landDepth:=land_depth]
for (v in c("landWidth","landDepth","MB_total_finished_area","MB_effective_year")) {
	dtBCA19[,(v):=as.numeric(get(v))]
}

# ---- assemble comp sales (agnostic on zoning + use; structure sqft only) -----
dtSales <- merge(data.table(dfSales), dtBCA19, by="folioID")
dtSales[,conveyancePrice := as.numeric(conveyancePrice)]
dtSales[,year := as.numeric(substr(conveyanceDate,1,4))]
dtSales[,structAge := year - MB_effective_year]
dtSales[structAge < 0, structAge := 0]   # clamp reno-updated effective years to new-ish
dtSales <- dtSales[
  year %between% c(SALE_MINYEAR, SALE_MAXYEAR) &
  !is.na(MB_total_finished_area) & MB_total_finished_area > 0 &
  !is.na(conveyancePrice)        & conveyancePrice > 0 &
  !is.na(structAge)
]
cat("comp sales after filters:", nrow(dtSales), "\n")
print(summary(dtSales[,.(conveyancePrice, MB_total_finished_area, structAge)]))

# comp coordinates: use the folio geometry (dfG) roll match for lat/lon of each sale.
dtSales[, rollStart := floor(as.numeric(rollNumber)/1000)]
dtSalesXY <- merge(
  dtSales[, .(folioID, rollStart, conveyancePrice, MB_total_finished_area, structAge)],
  dtGeo[, .(rollStart, longitude, latitude)],
  by = "rollStart"
)
dtSalesXY <- dtSalesXY[!is.na(longitude) & !is.na(latitude)]
cat("comp sales with coordinates:", nrow(dtSalesXY), "\n")

# ================= per-permit distance-weighted local kernel =================
# project comps and permits to meters (BC Albers) for honest distances.
sfComp <- st_transform(
  st_as_sf(dtSalesXY, coords = c("longitude","latitude"), crs = 4326),
  CRS_M
)
compXY <- st_coordinates(sfComp)

# permit points: one row per permit from dtP (all R1 permits; we predict everywhere,
# and subset to the analysis permit-years later, same as the original flow).
dtPermPts <- unique(dtP[!is.na(lat) & !is.na(lon), .(PermitNumber, lat, lon)])
sfPerm <- st_transform(
  st_as_sf(dtPermPts, coords = c("lon","lat"), crs = 4326),
  CRS_M
)
permXY <- st_coordinates(sfPerm)

# --- global radius R: 80th pctl across permits of distance to KNN_TARGET-th comp ----
# use FNN for a fast exact kNN in projected meters.
if (!requireNamespace("FNN", quietly = TRUE)) install.packages("FNN", repos="https://cloud.r-project.org")
kk <- min(KNN_TARGET, nrow(compXY))
knn <- FNN::get.knnx(compXY, permXY, k = kk)
distK <- knn$nn.dist[, kk]                 # each permit's distance to its kk-th comp (m)
R <- as.numeric(quantile(distK, KNN_PCTL, na.rm = TRUE))
cat(sprintf("global radius R = %.0f m (%.2f km); %d permits, %d comps, k=%d\n",
            R, R/1000, nrow(permXY), nrow(compXY), kk))

# --- per-permit local linear fit, hard-truncated at R, age-weighted ----------
# True fixed-radius neighbour query: for each permit, every comp within R metres.
# st_is_within_distance returns exactly those (no k to guess, no silent truncation).
# sfPerm/sfComp are already in projected metres (EPSG:CRS_M), so R is metres directly.
nbList <- st_is_within_distance(sfPerm, sfComp, dist = R)   # list, one vector of comp idx per permit

logComp <- log(dtSalesXY$conveyancePrice)
logSqft <- log(dtSalesXY$MB_total_finished_area)
wAge    <- exp(-dtSalesXY$structAge / AGE_DECAY)
lt      <- log(TARGET_SQFT)

predMat <- matrix(NA_real_, nrow = nrow(permXY), ncol = length(TARGET_SQFT))
nComp   <- integer(nrow(permXY))

for (i in seq_len(nrow(permXY))) {
  ii <- nbList[[i]]
  nComp[i] <- length(ii)
  if (length(ii) < MIN_COMPS) next
  x <- logSqft[ii]; y <- logComp[ii]; w <- wAge[ii]
  if (length(unique(x)) < 2) next            # need sqft variation for a slope
  fit <- tryCatch(stats::lm.wfit(cbind(1, x), y, w), error = function(e) NULL)
  if (is.null(fit) || any(is.na(fit$coefficients))) next
  b <- fit$coefficients                      # intercept, slope
  predMat[i, ] <- exp(b[1] + b[2] * lt)
}

dtPred <- data.table(
  PermitNumber = dtPermPts$PermitNumber,
  price1250    = predMat[, 1],
  price2500    = predMat[, 2],
  nCompKernel  = nComp
)
dtPred[, logRatio2500_1250 := log(price2500) - log(price1250)]
dtPred[, kernelElasticity  := logRatio2500_1250 / (log(2500) - log(1250))]
cat("permits with non-NA kernel prediction:", dtPred[!is.na(price1250), .N], "of", nrow(dtPred), "\n")
print(summary(dtPred[, .(price1250, price2500, kernelElasticity, nCompKernel)]))

# merge kernel predictions onto permits
dtP <- merge(dtP, dtPred, by = "PermitNumber", all.x = TRUE)
# ============================================================================

useList <- dtP[, .N, by = SpecificUseCategory][order(-N)][N > 100]
useList <- useList[,SpecificUseCategory]
dtP[,year:=as.numeric(substr(PermitNumberCreatedDate,1,4))]
for (y in 2017:2026) {
	print(dtP[year==y & SpecificUseCategory %in% useList, .N, by = SpecificUseCategory][order(-N)])
}

CRITVAL <- .5
dtP[,single:=grepl("Single",SpecificUseCategory)]
dtP[,duplex:=grepl("Duplex",SpecificUseCategory)]
dtP <- dtP[single==TRUE | duplex==TRUE] # for now
minSpend <- quantile(dtP[,ProjectValue],CRITVAL)
dtP <- dtP[TypeOfWork=="New Building" & single==1| ProjectValue>minSpend]
dtP <- dtP[year>2018 & year<2024]
dtP <- dtP[SpecificUseCategory %in% useList]
print(useList)

# correlations: duplex share vs local kernel price level & local elasticity
print(cor(dtP[,.(duplex, kernelElasticity, price1250, price2500)], use="complete.obs"))
print(cor(dtP[nCompKernel>KNN_TARGET,.(duplex, kernelElasticity, price1250, price2500)], use="complete.obs"))

ggplot(dtP[!is.na(price1250)], aes(x=log(price1250), y=kernelElasticity)) +
  geom_point(aes(color=nCompKernel), alpha=0.5) + theme_minimal()
ggsave("text/kernelPriceElasticity.png", width=8, height=6)

# ---- lot geometry from nearest folio (unchanged: for landWidth/landDepth splits) ----
crr <- st_crs(dfG)
dtP <- dtP[!is.na(lat) & !is.na(lon)]
dtPgeoNN <- st_as_sf(
  dtP[, .(PermitNumber,lat, lon)],
  coords = c("lon", "lat"),
  crs = 4326
)
dtPgeoNN <- st_transform(dtPgeoNN, st_crs(dfG))
idx  <- st_nearest_feature(dtPgeoNN, dfG)
dist <- st_distance(dtPgeoNN, dfG[idx, ], by_element = TRUE)
print("post-geo merge to R1 only")
for (y in 2017:2026) {
	print(y)
	print(dtP[year==y & SpecificUseCategory %in% useList, .N, by = SpecificUseCategory][order(-N)])
}
cols <- setdiff(names(dfG), attr(dfG, "sf_column"))
matched <- st_drop_geometry(dfG)[idx, cols, drop = FALSE]
matched[dist > set_units(10, "m"), ] <- NA
dtPgeoNN <- cbind(dtPgeoNN, matched)
dtPgeoNN$dist <- dist
dtPgeoNN <- as.data.table(dtPgeoNN)[,.(PermitNumber,ROLL_NUMBER)]
dtPgeoNN[,rollStart:=floor(as.numeric(ROLL_NUMBER)/1000)]
dtPgeoNN <- merge(dtPgeoNN, dtBCA19[,.(rollStart,landWidth,landDepth,MB_total_finished_area,MB_effective_year)], by="rollStart", all.x=TRUE)
dtP <- merge(dtP, dtPgeoNN, by="PermitNumber")

# baseline
print(summary(feols(duplex ~  log(price2500) + landDepth | year, data=dtP[round(landWidth)==33], cluster="PermitNumber")))
print(summary(feols(duplex ~  log(price2500) + landDepth | year, data=dtP[round(landWidth)==50], cluster="PermitNumber")))

# duplex ~ local kernel elasticity + local price level, by lot width
print(summary(feols(duplex ~ kernelElasticity + log(price1250) + landDepth | year, data=dtP[round(landWidth)==33], cluster="PermitNumber")))
print(summary(feols(duplex ~ kernelElasticity + log(price1250) + landDepth | year, data=dtP[round(landWidth)==50], cluster="PermitNumber")))

# do differently
print(summary(feols(duplex ~ log(price2500) + log(price1250) + landDepth | year, data=dtP[round(landWidth)==33], cluster="PermitNumber")))
print(summary(feols(duplex ~ log(price2500) + log(price1250) + landDepth | year, data=dtP[round(landWidth)==50], cluster="PermitNumber")))


q("no")
q("no")
# vancouverMatch.R
# match permits to neighbourhoods, assessments, etc
# Objective -- correlation and plots elasticity, price, duplex share. Then do multiplex share
# Tom Davidoff
# 08/13/26

library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)
library(RSQLite)
library(fixest)
library(units)

sf_use_s2(TRUE) # (5) ensure s2 for fast lat/lon st_within

# permits
# (7) only read the columns actually used downstream
dtP <- fread(
  "~/DropboxExternal/dataRaw/issued-building-permitsDwellingUses.csv",
  select = c("SpecificUseCategory", "PermitNumber", "geo_point_2d","PermitNumberCreatedDate","TypeOfWork","ProjectValue")
) # pre-filtered for dwelling uses on download from Vancouver Open Data
print(head(dtP))

# grab zoning and census tracts to make spatial
dgZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson") # vancouver_zoning.geojson
print(head(dgZ))
dgZ <- dgZ[dgZ$zoning_district=="R1-1",]

# use data below on dtP variables to convert to sf with key PermitNumber -- just export geo variables and PermitNumber
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]

dtPgeo <- st_as_sf(dtP[!is.na(lat),.(SpecificUseCategory,PermitNumber,lat,lon)], coords = c("lon", "lat"), crs = 4326)
print(head(dtPgeo))

# (4) pre-filter permits to the zoning bbox before the join to shrink the join input
dtPgeo <- dtPgeo[st_intersects(dtPgeo, st_as_sfc(st_bbox(dgZ)), sparse = FALSE)[,1], ]

# merge as point within shapes to dgZ
dtPgeo <- st_join(dtPgeo, dgZ, join = st_within) # spatial join to zoning shapes
print(head(dtPgeo))
print(table(dtPgeo$zoning_district)) # check zoning districts
write.table(table(is.na(dtPgeo$zoning_district),dtPgeo$SpecificUseCategory),"text/sanityCheckVancouverZoning.txt") # check zoning districts
dtPgeo$SpecificUseCategory <- NULL
dtPgeo <- dtPgeo[!is.na(dtPgeo$zoning_district),] # drop NA zoning districts

# census tract shapes
# (2,3) build a Vancouver-only, validated, reprojected tract subset once and cache it.
# (3) filter to BC (PRUID 59) BEFORE make_valid/transform so we don't validate all of Canada.
fTract <- "~/DropboxExternal/dataProcessed/censusTractsBC.rds"
if (!file.exists(fTract)) {
  dfT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
  dfT <- dfT[dfT$PRUID == "59", ]        # (3) BC only, before validate/transform
  dfT <- st_make_valid(dfT)
  dfT <- st_transform(dfT, crs = 4326)
  saveRDS(dfT, fTract)
}
dfT <- readRDS(fTract)


dtPgeo <- st_join(dtPgeo, dfT, join = st_within) # spatial join to census tract shapes
print(head(dtPgeo))

dtPgeo <- as.data.table(dtPgeo)[,.(PermitNumber,DGUID,CTNAME,geometry)]

# now get appraised values and sales for single family homes by tract for 2017, pre re-zonings. Do current appraisals and 2017 sales to show correlations. Can get all this from current data

# get census tract from lat/lon
fGeo <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
dirGpkg <- "~/bigFiles/latestSpatialBCA/"
fGpkg <- list.files(dirGpkg)
fileGpkg <- paste0(dirGpkg,fGpkg)

if (!file.exists(fGeo)) {
  # Note: Extracting NEIGHBOURHOOD (BCA code) and joining with Census Tracts for Income

  dfG <- st_read(fileGpkg,query =
	"SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD, geom
	FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
	WHERE JURISDICTION = 'City of Vancouver' AND (ACTUAL_USE_DESCRIPTION IN ('Single Family Dwelling', 'Residential Dwelling with Suite') OR ACTUAL_USE_DESCRIPTION LIKE '%plex%') "
  )

  dCT <- dfT[dfT$PRUID == "59", ]
  dCT <- st_transform(dCT, st_crs(dfG))

  dfG <- st_join(dfG, dCT, join = st_within)
  # now return to normal lat lon crs and record longitude and latitude of parcel geom (not geom is a polygon)
  dfG <- st_transform(dfG, crs = 4326)
  # Need a single coordinate pair, as geom is a polygon()
  dfG$centroid <- st_centroid(dfG)
  dfG$longitude <- st_coordinates(dfG$centroid)[, 1]
  dfG$latitude <- st_coordinates(dfG$centroid)[, 2]

  saveRDS(dfG, fGeo)
}
dfG <- readRDS(fGeo)
dtGeo <- as.data.table(dfG)[, .(ROLL_NUMBER, CTNAME, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD, longitude, latitude, geom)]
# use ggspatial with street map
checkGEOPlotName <- "text/checkGEO.png"
if (!file.exists(checkGEOPlotName)) {
ggplot(dtGeo, aes(x = longitude, y = latitude, color = NEIGHBOURHOOD)) +
  annotation_map_tile(type = "cartolight", zoom = 12) +
  geom_point(size = 0.5) +
  coord_sf(crs = 4326) +
  theme(legend.position = "none") +
  theme_minimal()
ggsave(checkGEOPlotName, width = 8, height = 6)
}

print(summary(dtGeo))
dtGeo[,rollStart:=floor(as.numeric(ROLL_NUMBER)/1000)]

# Merge with BCA 2019 Vancouver single family R1 -zoned parcels
bca19 <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
con <- dbConnect(RSQLite::SQLite(), bca19)
# (1) one join across folio (jurisdiction) + residentialInventory (zoning/area) + folioDescription
# (use description) -- replaces the three separate pulls and the two R merges, and filters
# in SQL before pulling into R. INNER JOINs reproduce the old merge()-without-all=TRUE
# behavior (rows without a match in every table are dropped, same as before).
dtBCA19 <- data.table(dbGetQuery(con, "
  SELECT d.folioID, f.rollNumber, i.zoning, i.MB_effective_year, i.MB_total_finished_area, i.land_width, i.land_depth,
         d.actualUseDescription, d.neighbourhoodDescription, d.landWidth, d.landDepth
  FROM folio f
  JOIN residentialInventory i ON i.roll_number = f.rollNumber
  JOIN folioDescription d      ON d.folioID     = f.folioID
  WHERE f.jurisdictionCode = '200'
    AND (d.actualUseDescription IN ('Single Family Dwelling', 'Residential Dwelling with Suite')
         OR d.actualUseDescription LIKE '%plex%')
"))
dfSales <- dbGetQuery(con, "SELECT folioID, conveyanceDate, conveyancePrice FROM sales WHERE     conveyanceTypeDescription  = 'Improved Single Property Transaction' ")
print(head(dtBCA19))
print(table(dtBCA19[,actualUseDescription])[order(-table(dtBCA19[,actualUseDescription]))])
print(nrow(dtBCA19))

dtBCA19[,rollStart:=floor(as.numeric(rollNumber)/1000)]
print(head(dtBCA19))
dtBCA19[is.na(landWidth),landWidth:=land_width]
dtBCA19[is.na(landDepth),landDepth:=land_depth]

setkey(dtBCA19,rollStart)
setkey(dtGeo,rollStart)
print(table(dtBCA19[,zoning]))
dtBCA19 <- dtBCA19[dtGeo]
print(table(dtBCA19[,zoning]))
dtBCA19 <- dtBCA19[substring(zoning,1,2)=="RS"]
for (v in c("landWidth","landDepth","MB_total_finished_area")) {
	dtBCA19[,(v):=as.numeric(get(v))]
}

dtSales <- merge(data.table(dfSales), dtBCA19, by="folioID")
print(head(dtSales))
print(table(dtSales[,actualUseDescription]))
# almost no duplex sales
dtSales <- dtSales[actualUseDescription %in% c("Single Family Dwelling", "Residential Dwelling with Suite")]
dtSales[,year:=as.numeric(substr(conveyanceDate,1,4))]
dtSales[,yearMonth:=substr(conveyanceDate,1,7)]
print(summary(dtSales[year==2017,.N,by=CTNAME]))
print(summary(dtSales[year %between% c(2016,2017),.N,by=CTNAME]))
print(summary(dtSales[year %between% c(2015,2017),.N,by=CTNAME]))
MINYEAR <- 2015
MAXYEAR <- 2017
for (v in c("conveyancePrice")) {
	dtSales[,(v):=as.numeric(get(v))]
}
dtSales <- dtSales[year %between% c(MINYEAR,MAXYEAR) & !is.na(landWidth) & !is.na(landDepth) & !is.na(MB_total_finished_area) & !is.na(conveyancePrice) & conveyancePrice>0 & MB_total_finished_area>0]

regH <- feols(log(conveyancePrice) ~ log(MB_total_finished_area) + log(landWidth) + (landDepth),data=dtSales)
dtSales[,hedonicResidual:=residuals(regH)]
dtHedonicC <- dtSales[,.(hedonicResidualMean=mean(hedonicResidual)),by=CTNAME]
print(summary(dtHedonicC))
dtElasticityC <- dtSales[,.(hedonicElasticity=cov(log(MB_total_finished_area),log(conveyancePrice))/var(log(MB_total_finished_area)),count=.N),by=CTNAME]
dtHedonicN <- dtSales[,.(hedonicResidualMean=mean(hedonicResidual)),by=NEIGHBOURHOOD]
print(summary(dtHedonicN))
dtElasticityN <- dtSales[,.(hedonicElasticity=cov(log(MB_total_finished_area),log(conveyancePrice))/var(log(MB_total_finished_area)),count=.N),by=NEIGHBOURHOOD]
dtCT <- merge(dtHedonicC,dtElasticityC,by="CTNAME")
dtN <- merge(dtHedonicN,dtElasticityN,by="NEIGHBOURHOOD")
for (k in c(0,.05,.1,.5)) {
	print(k)
	print(quantile(dtCT[,count],k))
	print(dtCT[count>quantile(count,k),cor(hedonicResidualMean,hedonicElasticity,use="complete.obs")])
	print(quantile(dtN[,count],k))
	print(dtN[count>quantile(count,k),cor(hedonicResidualMean,hedonicElasticity,use="complete.obs")])
}
ggplot(dtCT,aes(x=hedonicResidualMean,y=hedonicElasticity)) + geom_point(aes(size=count,color=count))
ggsave("text/ctElasticityFE.png",width=8,height=6)
ggplot(dtN,aes(x=hedonicResidualMean,y=hedonicElasticity)) + geom_point(aes(size=count,color=count))
ggsave("text/neighbourhoodElasticityFE.png",width=8,height=6)

print(head(dtPgeo))
dtPgeo <- merge(dtPgeo,dtCT,by="CTNAME",all.x=TRUE)
dtP <- merge(dtP,dtPgeo,by="PermitNumber")
print(head(dtP))
useList <- dtP[, .N, by = SpecificUseCategory][order(-N)][N > 100]
useList <- useList[,SpecificUseCategory]

CRITVAL <- .5
dtP[,single:=grepl("Single",SpecificUseCategory)]
dtP[,duplex:=grepl("Duplex",SpecificUseCategory)]
dtP <- dtP[single==TRUE | duplex==TRUE] # for now
minSpend <- quantile(dtP[,ProjectValue],CRITVAL)
dtP <- dtP[TypeOfWork=="New Building" & single==1| ProjectValue>minSpend]
dtP[,year:=as.numeric(substr(PermitNumberCreatedDate,1,4))]
dtP <- dtP[year>2018 & year<2024]
dtP <- dtP[SpecificUseCategory %in% useList]
print(cor(dtP[,.(duplex,hedonicElasticity,hedonicResidualMean)],use="complete.obs"))
print(cor(dtP[count>100,.(duplex,hedonicElasticity,hedonicResidualMean)],use="complete.obs"))

# match dtP with dfGeo by lat lon distance nearest neighbour
# make a spatial data frame
crr <- st_crs(dfG)
dtPgeo <- st_as_sf(
  dtP[, .(PermitNumber,lat, lon)],
  coords = c("lon", "lat"),
  crs = 4326                    # what lat/lon actually are
)
dtPgeo <- st_transform(dtPgeo, st_crs(dfG))   # now match dfG
idx  <- st_nearest_feature(dtPgeo, dfG)
dist <- st_distance(dtPgeo, dfG[idx, ], by_element = TRUE)

cols <- setdiff(names(dfG), attr(dfG, "sf_column"))
matched <- st_drop_geometry(dfG)[idx, cols, drop = FALSE]
matched[dist > set_units(10, "m"), ] <- NA
dtPgeo <- cbind(dtPgeo, matched)
dtPgeo$dist <- dist
print(head(dtPgeo))
print(summary(dtPgeo$dist))
dtPgeo <- as.data.table(dtPgeo)
dtPgeo <- dtPgeo[,.(PermitNumber,ROLL_NUMBER)]
dtPgeo[,rollStart:=floor(as.numeric(ROLL_NUMBER)/1000)]
dtPgeo <- merge(dtPgeo,dtBCA19[,.(rollStart,landWidth,landDepth,MB_total_finished_area,MB_effective_year)],by="rollStart",all.x=TRUE)
dtP <- merge(dtP,dtPgeo,by="PermitNumber")
print(summary(feols(duplex ~ hedonicElasticity + hedonicResidualMean + landDepth| year,data=dtP[round(landWidth)==33],cluster="CTNAME")))
print(summary(feols(duplex ~ hedonicElasticity + hedonicResidualMean + landDepth| year,data=dtP[round(landWidth)==33 & count>100],cluster="CTNAME")))
print(summary(feols(duplex ~ hedonicElasticity + hedonicResidualMean + landDepth| year,data=dtP[round(landWidth)==50],cluster="CTNAME")))
print(summary(feols(duplex ~ hedonicElasticity + hedonicResidualMean + landDepth| year,data=dtP[round(landWidth)==50 & count>100],cluster="CTNAME")))
print(summary(feols(duplex ~ hedonicElasticity +  landDepth| year,data=dtP[round(landWidth)==33],cluster="CTNAME")))


q("no")
