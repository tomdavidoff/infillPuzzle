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

sf_use_s2(TRUE) # (5) ensure s2 for fast lat/lon st_within

# permits
# (7) only read the columns actually used downstream
dtP <- fread(
  "~/DropboxExternal/dataRaw/issued-building-permitsDwellingUses.csv",
  select = c("SpecificUseCategory", "PermitNumber", "geo_point_2d")
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
useList <- dtP[, .N, by = SpecificUseCategory][order(-N)][N > 100, "SpecificUseCategory"]
dtP <- dtP[SpecificUseCategory %in% useList]
dtP[,single:=grepl("Single",SpecificUseCategory)]
dtP[,duplex:=grepl("Duplex",SpecificUseCategory)]
dtP <- dtP[single==TRUE | duplex==TRUE]
print(dtP[,cor(Duplex,hedonicElasticity,hedonicResidualMean,use="complete.obs")])
print(dtP[count>100,cor(Duplex,hedonicElasticity,hedonicResidualMean,use="complete.obs")])

q("no")
