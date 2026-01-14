
# permitsZoning.R
# Identify the zone for each permit via spatial join
# Tom Davidoff 
# 01/13/26
# Now from chat

library(sf)
library(data.table)

zoning_path  <- "~/OneDrive - UBC/dataRaw/vancouver_zoning.geojson"
permits_path <- "~/OneDrive - UBC/dataRaw/vancouver_permits_full.csv"
out_path     <- "~/OneDrive - UBC/dataProcessed/vancouver_permits_with_zone.csv"

# 1) Zoning polygons
sZoning <- st_read(zoning_path, quiet = TRUE)

# 2) Permits
dt <- fread(permits_path,drop="projectdescription")  # drop to save memory

# 3) Parse "lat, lon" -> sf points
# geo_point_2d example: "49.2862375, -123.1123788"
ll <- tstrsplit(gsub("\\s+", "", dt$geo_point_2d), ",", fixed = TRUE)
dt[, `:=`(lat = as.numeric(ll[[1]]), lon = as.numeric(ll[[2]]))]

dt <- dt[!is.na(lat) & !is.na(lon)]
sPermits <- st_as_sf(dt, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# 4) CRS alignment + join
sPermits <- st_transform(sPermits, st_crs(sZoning))
sOut <- st_join(sPermits, sZoning, join = st_within)


# --- City of Vancouver boundary (polygon) ---
cov_url <- "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/city-boundary/exports/geojson"
sCoV <- st_read(cov_url, quiet = TRUE)
sCov <- st_make_valid(sCoV)
sCov <- st_union(sCov)

# --- 2021 Census Tracts (cartographic) via download
ct_file <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
sCT <- st_read(ct_file, quiet = TRUE)

# --- Clip CTs to City of Vancouver only ---
sCT <- st_make_valid(sCT)
sCT <- st_transform(sCT, st_crs(sCoV))
sCT_cov <- st_intersection(sCT, st_make_valid(sCoV))

# --- Add CT to your permits (points) ---
# sPermitsWithZone should already be sf and already joined to zoning
sPermitsWithZone <- st_transform(sOut, st_crs(sCT_cov))
sPermitsWithZone <- st_join(sPermitsWithZone, sCT_cov, join = st_within)

# 5) Save
#permitnumber,permitnumbercreateddate,issuedate,permitelapseddays,projectvalue,typeofwork,address,permitcategory,applicant,applicantaddress,propertyuse,specificusecategory,buildingcontractor,buildingcontractoraddress,issueyear,geolocalarea,geom,yearmonth,geo_point_2d.x,lat,lon,object_id,zoning_classification,zoning_category,zoning_district,cd_1_number,geo_point_2d.y,CTUID,DGUID,CTNAME,LANDAREA,PRUID,name,geo_point_2d
goodCols <- c("permitnumber","typeofwork","projectvalue","address","specificusecategory","applicant","geolocalarea","permitnumbercreateddate","yearmonth","zoning_district","CTUID","DGUID","CTNAME")
#fwrite(as.data.table(st_drop_geometry(sOut)), out_path)
fwrite(as.data.table(st_drop_geometry(sPermitsWithZone))[, ..goodCols], out_path)

# Note multiplex appears for sure to be specificusecategory "Multiple Dwelling"
#> print(table(x[grepl("ultiplex",projectdescription),specificusecategory]))
#
#                      Duplex                Dwelling Unit
#                          12                           12
#Infill Single Detached House               Micro Dwelling
#                           1                            1
#           Multiple Dwelling        Single Detached House
#                         303                            9
