# mergePermitsSpatial.R
# merge permit data with single family lots as of 2019 BCA (2018 assessment) and then census tracts
# Tom Davidoff
# 01/16/26

library(data.table)
library(sf)

dtBCA <- readRDS("~/OneDrive - UBC/dataProcessed/bca_vancouver_residential.rds")
 #[1] "permitnumber"              "permitnumbercreateddate"  [3] "issuedate"                 "permitelapseddays"        [5] "projectvalue"              "typeofwork"               [7] "address"                   "projectdescription"       [9] "permitcategory"            "applicant"                [11] "applicantaddress"          "propertyuse"              [13] "specificusecategory"       "buildingcontractor"       [15] "buildingcontractoraddress" "issueyear"                [17] "geolocalarea"              "geom"                     [19] "yearmonth"                 "geo_point_2d"             
dtP <- fread("~/OneDrive - UBC/dataRaw/vancouver_permits_full.csv",select=c("geom","geo_point_2d","permitnumber","permitnumbercreateddate","applicant","typeofwork","projectvalue","specificusecategory"))
dtP <- dtP[typeofwork %in% c("Addition / Alteration","New Building")]
dtP[,yearMoCreated:=substr(permitnumbercreateddate,1,7)]
dtP <- dtP[specificusecategory %in% c("Single Detached House","Laneway House","Single Detached House w/ Sec Suite","Multiple Dwelling","Duplex","Duplex w/Secondary Suite","address")]
print(quantile(dtP[typeofwork=="Addition / Alteration",projectvalue]))
MINVAL <- 125000 # appx 75th percentile for Addition / Alteration
dtP <- dtP[projectvalue>=MINVAL]
print(head(dtP[,.(geo_point_2d)]))
print(head(dtBCA[,.(geom)]))
# 1. Parse the point string and create sf geometry
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]
dtP <- dtP[!is.na(lat)]
sfP <- st_as_sf(dtP, coords = c("lon", "lat"), crs = 4326)

# 2. Transform to match BCA's CRS (check with st_crs(dtBCA))
sfBCA <- st_as_sf(dtBCA)
sfP <- st_transform(sfP, st_crs(sfBCA)) #st_crs(dtBCA)) from ogrinfo of original BCA .gpkg
print(st_crs(sfBCA))


# 3. Spatial join (points within polygons)
merged <- st_join(sfP, sfBCA, join = st_within)
print(head(merged))
print(nrow(sfP))
print(nrow(dtBCA))
print(nrow(sfP))

# now get census tracts
fCT <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
dCT <- st_read(fCT)
print(head(dCT))
# swap crs of census tracts to that of merged)
print(st_crs(dCT))
print(st_crs(merged))
dCT <- st_transform(dCT,st_crs(merged))

mergedCT <- st_join(merged,dCT,join=st_within)
print(head(mergedCT))

##
merged[,matched:=!is.na(zoning)]
print(table(merged[matched==0,specificusecategory]))
print(table(merged[matched==1,specificusecategory]))

merged <- merged[matched==1]


