# mergePermitsSpatial.R
# Merge permit data with single family lots (2019 BCA), zoning, and census tracts
# Tom Davidoff
# 01/16/26

library(data.table)
library(sf)

# Load data
dtBCA <- readRDS("~/OneDrive - UBC/dataProcessed/bca_vancouver_residential.rds")
dtP <- fread("~/OneDrive - UBC/dataRaw/vancouver_permits_full.csv",
             select = c("geom", "geo_point_2d", "permitnumber", "permitnumbercreateddate",
                        "applicant", "typeofwork", "projectvalue", "specificusecategory", "address"))

# Filter to new builds and high-value alterations
dtP <- dtP[typeofwork %in% c("Addition / Alteration", "New Building")]
dtP[, yearCreated := year(permitnumbercreateddate)]
dtP <- dtP[yearCreated >= 2017]

print(table(dtP[grepl("laneway", specificusecategory),specificusecategory]))
# Keep residential categories only
dtP <- dtP[specificusecategory %in% c("Single Detached House", "Laneway House", "Single Detached House w/Sec Suite", "Multiple Dwelling", "Duplex", "Duplex w/Secondary Suite")]
dtP <- dtP[projectvalue >= 250000 | typeofwork=="New Building"]

# Create use type flags
dtP[, duplex := grepl("uplex", specificusecategory)]
dtP[, laneway := grepl("aneway", specificusecategory)]
dtP[, multi := grepl("ultiple", specificusecategory)]
dtP[, single := grepl("Single Detached", specificusecategory)]

# Classify use hierarchy: multi > duplex > laneway > single
dtP[, use := fcase(
  multi == TRUE, "multi",
  duplex == TRUE, "duplex",
  laneway == TRUE, "laneway",
  default = "single"
)]

# Convert to sf and transform CRS
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]
dtP <- dtP[!is.na(lat)]

# Calculate nearest neighbor distances BEFORE spatial joins (on all permits)
sfP_meters <- st_as_sf(dtP, coords = c("lon", "lat"), crs = 4326)
sfP_meters <- st_transform(sfP_meters, 32610)
coords <- st_coordinates(sfP_meters)
dtP[, `:=`(x = coords[, 1], y = coords[, 2])]

# Nearest neighbor: laneway <-> single
library(RANN)
dtLaneway <- dtP[use == "laneway"]
dtSingle <- dtP[use == "single"]

if (nrow(dtLaneway) > 0 & nrow(dtSingle) > 0) {
  # Nearest single for each laneway
  knn_lane <- nn2(dtSingle[, .(x, y)], dtLaneway[, .(x, y)], k = 1)
  dtLaneway[, distToSingle := knn_lane$nn.dists[, 1]]
  dtLaneway[, nearestSinglePermit := dtSingle$permitnumber[knn_lane$nn.idx[, 1]]]
  dtLaneway[, nearestSingleApplicant := dtSingle$applicant[knn_lane$nn.idx[, 1]]]
  dtLaneway[, nearestSingleAddress := dtSingle$address[knn_lane$nn.idx[, 1]]]

  # Nearest laneway for each single
  knn_single <- nn2(dtLaneway[, .(x, y)], dtSingle[, .(x, y)], k = 1)
  dtSingle[, distToLaneway := knn_single$nn.dists[, 1]]
  dtSingle[, nearestLanewayPermit := dtLaneway$permitnumber[knn_single$nn.idx[, 1]]]
  dtSingle[, nearestLanewayApplicant := dtLaneway$applicant[knn_single$nn.idx[, 1]]]
  dtSingle[, nearestLanewayAddress := dtLaneway$address[knn_single$nn.idx[, 1]]]


}

# Combine back
dtOther <- dtP[!use %in% c("laneway", "single")]
dtP <- rbindlist(list(dtLaneway, dtSingle, dtOther), fill = TRUE)

# Now do spatial joins
sfP <- st_as_sf(dtP, coords = c("lon", "lat"), crs = 4326)
sfBCA <- st_as_sf(dtBCA)
sfP <- st_transform(sfP, st_crs(sfBCA))

# Spatial join: permits to BCA parcels (gets roll number, land area, etc.)
merged <- st_join(sfP, sfBCA, join = st_within)

# Spatial join: add zoning from city zoning shapefile
sfZoning <- st_read("~/OneDrive - UBC/dataRaw/vancouver_zoning.geojson")
sfZoning <- st_transform(sfZoning, st_crs(merged))
merged <- st_join(merged, sfZoning[, c("zoning_classification", "zoning_category", "zoning_district")],
                  join = st_within)

# Spatial join: add census tracts
dCT <- st_read("~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
dCT <- st_transform(dCT, st_crs(merged))
merged <- st_join(merged, dCT, join = st_within)

# Convert to data.table and finalize
dtSpatial <- as.data.table(merged)
dtSpatial[, matched := !is.na(ROLL_NUMBER)]

# Select final columns
dtSpatial <- dtSpatial[matched == TRUE,
                        .(CTUID, ROLL_NUMBER, zoning_classification, zoning_category,
                          zoning_district, folioID, permitnumber, permitnumbercreateddate,
                          use, MB_effective_year, MB_total_finished_area,
                          neighbourhoodDescription, address, LANDAREA,
                          landWidth, landDepth, geo_point_2d, typeofwork,applicant,
                          distToSingle, distToLaneway, nearestSinglePermit, nearestLanewayPermit) ]

saveRDS(dtSpatial, "~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")

print(summary(dtLaneway$distToSingle))
print(summary(dtSingle$distToLaneway))
