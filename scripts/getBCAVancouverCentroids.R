# getBCAVancouverCentroids.csv
# Extract Vancouver single family / duplex parcels from BCA 2026 GeoPackage
# and save locally for faster loading
# Tom Davidoff / Claude
# 01/15/26

library(sf)
library(RSQLite)
library(data.table)

# ============================================================================
# Configuration
# ============================================================================
bcaFile<- "/Volumes/T7Office/bigFiles/bca_folios_spatial_file_20251103/bca_folios.gpkg.gpkg"
outFile   <- "~/OneDrive - UBC/dataProcessed/bca25Centroids.rds"

# ============================================================================
# Extract Vancouver residential parcels
# ============================================================================

cat("Loading Vancouver residential parcels from BCA \n")
cat("Source:", bcaFile, "\n")

sBCA<- st_read(
  bcaFile,
  layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
  query = "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
           WHERE JURISDICTION_CODE='200'",
  quiet = TRUE
)
           #AND (ACTUAL_USE_DESCRIPTION IN ('Residential Dwelling with Suite','Single Family Dwelling')

cat("Parcels loaded:", nrow(sBCA), "\n")
cat("CRS:", st_crs(sBCA)$epsg, "\n")
# 1) keep only the ID + polygon geometry
cent_sf <- sBCA[, "ROLL_NUMBER"]              # sf with POLYGON geom, CRS 3005

# 2) compute centroids in 3005
cent_sf$centroid <- st_centroid(st_geometry(cent_sf))

# 3) switch active geometry to centroid
cent_sf <- st_set_geometry(cent_sf, "centroid")

# 4) drop the old polygon geometry column (now just points + ROLL_NUMBER)
cent_sf$geom <- NULL                          # if your polygon column is named 'geom'
# (If it's named differently, remove that column instead.)

# 5) transform the point geometry to lon/lat
cent_ll <- st_transform(cent_sf, 4326)

# 6) extract lon/lat
xy <- st_coordinates(cent_ll)

dt_out <- data.table(
  ROLL_NUMBER = cent_ll$ROLL_NUMBER,
  lon = xy[,1],
  lat = xy[,2]
)

print(dt_out)

saveRDS(dt_out,outFile)
print("foo")

q("no")
