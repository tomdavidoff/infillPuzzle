# vancouverMatch.R
# match permits to neighbourhoods, assessments, etc
# Objective -- correlation and plots elasticity, price, duplex share. Then do multiplex share
# Tom Davidoff 
# 08/13/26

library(data.table)
library(sf)

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

# now get appraised values and sales for single family homes by tract for 2017, pre re-zonings. Do current appraisals and 2017 sales to show correlations.
"~/OneDrive - UBC/Documents/data/bca/REVD18_and_inventory_extracts_CSV_files/REVD18_and_inventory_extracts_CSV_files"


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
