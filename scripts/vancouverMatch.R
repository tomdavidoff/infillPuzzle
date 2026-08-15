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

# permits
dtP <- fread("~/DropboxExternal/dataRaw/issued-building-permitsDwellingUses.csv") # pre-filtered for dwelling uses on download from Vancouver Open Data
print(head(dtP))

# grab zoning and census tracts to make spatial
dgZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson") # vancouver_zoning.geojson
print(head(dgZ))
dgZ <- dgZ[dgZ$zoning_district=="R1-1",]

# use data below on dtP variables to convert to sf with key PermitNumber -- just export geo variables and PermitNumber
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]

dtPgeo <- st_as_sf(dtP[!is.na(lat),.(SpecificUseCategory,PermitNumber,lat,lon)], coords = c("lon", "lat"), crs = 4326)
print(head(dtPgeo))
# merge as point within shapes to dgZ
dtPgeo <- st_join(dtPgeo, dgZ, join = st_within) # spatial join to zoning shapes
print(head(dtPgeo))
print(table(dtPgeo$zoning_district)) # check zoning districts
write.table(table(is.na(dtPgeo$zoning_district),dtPgeo$SpecificUseCategory),"text/sanityCheckVancouverZoning.txt") # check zoning districts
dtPgeo$SpecificUseCategory <- NULL
dtPgeo <- dtPgeo[!is.na(dtPgeo$zoning_district),] # drop NA zoning districts

# census tract shapes
dfT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
dfT <- st_transform(dfT, crs = 4326)
dfT <- st_make_valid(dfT)


dtPgeo <- st_join(dtPgeo, dfT, join = st_within) # spatial join to census tract shapes
print(head(dtPgeo))

dtPgeo <- as.data.table(dtPgeo)[,.(PermitNumber,DGUID,geometry)]

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
	WHERE JURISDICTION = 'City of Vancouver' AND (ACTUAL_USE_DESCRIPTION IN ('Single Family Dwelling', 'Residential Dwelling with Suite') OR ACTUAL_USE_DESCRIPTION LIKE '%plex') "
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
# get zoning, rollnumber, folioID from bca19
con <- dbConnect(RSQLite::SQLite(), bca19)
dfFolio <- dbGetQuery(con, "SELECT folioID, rollNumber FROM folio WHERE jurisdictionCode=='200'")
dfInventory <- dbGetQuery(con, "SELECT roll_number, zoning, MB_effective_year, MB_total_finished_area FROM residentialInventory") 
dfDescription <- dbGetQuery(con, "SELECT folioID, actualUseDescription, neighbourhoodDescription, landWidth, landDepth FROM folioDescription")
dfSales <- dbGetQuery(con, "SELECT folioID, conveyanceDate, conveyancePrice, conveyanceTypeDescription FROM conveyance")
print(nrow(dfFolio))
dfBCA19 <- merge(dfFolio, dfInventory, by.x="rollNumber", by.y="roll_number")
dtBCA19 <- data.table(merge(dfBCA19, dfDescription, by="folioID"))
print(head(dtBCA19))
print(table(dtBCA19[,actualUseDescription])[order(-table(dtBCA19[,actualUseDescription]))])
dtBCA19 <- dtBCA19[actualUseDescription %in% c("Single Family Dwelling","Residential Dwelling with Suite")]
print(nrow(dtBCA19))

dtBCA19[,rollStart:=floor(as.numeric(rollNumber)/1000)]
print(head(dtBCA19))




q("no")

                                                                 Geom YearMonth
                                                               <char>    <char>
1: {""coordinates"": [-123.1204926, 49.2473731], ""type"": ""Point""}   2025-02
2: {""coordinates"": [-123.1206837, 49.2473735], ""type"": ""Point""}   2024-09
3: {""coordinates"": [-123.0501588, 49.2850388], ""type"": ""Point""}   2023-05
4:   {""coordinates"": [-123.04458, 49.2392977], ""type"": ""Point""}   2021-10
5: {""coordinates"": [-123.0595101, 49.2741246], ""type"": ""Point""}   2025-05
6: {""coordinates"": [-123.2064534, 49.2718579], ""type"": ""Point""}   2022-06
               geo_point_2d
                     <char>
1: 49.2473731, -123.1204926
2: 49.2473735, -123.1206837
3: 49.2850388, -123.0501588
4:   49.2392977, -123.04458
5: 49.2741246, -123.0595101
6: 49.2718579, -123.2064534
