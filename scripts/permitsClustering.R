# permitsClustering.R
# R to identify permit clusters
# Tom Davidoff
# 02/02/26

library(data.table)
library(sf)
library(units)
library(RANN)

dt <- fread("~/OneDrive - UBC/dataRaw/vancouver_permits_full.csv")
print(head(dt))

# Convert to sf object where longitude and lat are not degrees, but meters for Vancouver, BC
# make lon/lat from geo_point_2d, e.g. 49.2888, -123.1234. There is not long/lat in data, just the point. Also there is geom
# Use geom column to create sf object
dt <- dt[!is.na(geo_point_2d)]
dt <- dt[typeofwork %chin% c("New Building", "Addition / Alteration")]
dt <- dt[,.(geo_point_2d,specificusecategory,address,permitnumber,applicant)]
dt[,longitude:=tstrsplit(geo_point_2d, ",")[[2]]]
dt[,latitude:=tstrsplit(geo_point_2d, ",")[[1]]]
dt <- dt[!is.na(longitude) & !is.na(latitude)]
dt_sf <- st_as_sf(dt, coords = c("longitude", "latitude"), crs = 4326)


# 0) Stable ID (DO THIS ONCE, before any sorting/filtering)
dt_sf$id <- seq_len(nrow(dt_sf))

# 1) Work in meters (Vancouver-ish; change CRS if needed)
x <- st_transform(dt_sf, 32610)

# 2) Get points to run kNN on
pts <- if (any(st_geometry_type(x) != "POINT")) st_point_on_surface(x) else x
xy  <- st_coordinates(pts)

# 3) 3 nearest neighbours + self  => k = 4
knn <- nn2(xy, xy, k = 4)

# For each row i, pick the first neighbour index that isn't i
i <- seq_len(nrow(dt_sf))
pick1 <- function(nn_row, self) nn_row[ nn_row != self ][1]

nn1_idx  <- mapply(pick1, split(knn$nn.idx, row(knn$nn.idx)), i)

x <- st_transform(dt_sf, 32610)          # meters
pts <- if (any(st_geometry_type(x) != "POINT")) st_point_on_surface(x) else x

xy <- st_coordinates(pts)
knn <- nn2(xy, xy, k = 3)

dt_sf$nn1   <- knn$nn.idx[, 2]
dt_sf$dist1 <- knn$nn.dists[, 2]
dt_sf$nn2   <- knn$nn.idx[, 3]
dt_sf$dist2 <- knn$nn.dists[, 3]

# only keep if laneway or single
dt_DT <- as.data.table(dt_sf)[dist1 < 40]
#dt_DT <- dt_DT[grepl("Single Detached",specificusecategory) | grepl("aneway",specificusecategory)]

# get a histogram of dist1, focusing on less than 40m
print(dt_DT[, .N, by = dist1][order(dist1)][1:20])

# Direct index-based merge (faster)
neighbor_data <- as.data.table(dt_sf)[dt_DT$nn1,
    .(nn1_specificusecategory = specificusecategory,
      nn1_address = address,
      nn1_permitnumber = permitnumber,
      nn1_applicant = applicant)]

# Bind columns
dt_merged <- cbind(dt_DT, neighbor_data)

# get histogram of distances of likely pairs
print(quantile(dt_merged[grepl("aneway",specificusecategory) & applicant==nn1_applicant & grepl("Single",nn1_specificusecategory), dist1],c(.25,.5,.7,.8,.9)))
print(quantile(dt_merged[grepl("aneway",specificusecategory) & applicant!=nn1_applicant , dist1],c(.25,.5,.7,.8,.9)))
## looks like about 20 meters
