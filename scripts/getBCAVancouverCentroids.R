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
sBCA$centroid <- st_centroid(sBCA$geom)
dtBCA <- as.data.table(sBCA)[,.(ROLL_NUMBER,centroid)]
dtBCA[,rollStart:=floor(as.numeric(ROLL_NUMBER)/1000)]
saveRDS(dtBCA,outFile)
print("foo")
q("no")
