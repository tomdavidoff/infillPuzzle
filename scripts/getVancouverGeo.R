# extractBCA26VancouverResidential.R
# Extract Vancouver single family / duplex parcels from BCA 2026 GeoPackage
# and save locally for faster loading
# Tom Davidoff / Claude
# 01/15/26

library(sf)

# ============================================================================
# Configuration
# ============================================================================

bcaFile<- "/Volumes/T7Office/bigFiles/bca_folios.gpkg"
outFile   <- "~/OneDrive - UBC/dataProcessed/bca_vancouver_residential.gpkg"

# ============================================================================
# Extract Vancouver residential parcels
# ============================================================================

cat("Loading Vancouver residential parcels from BCA \n")
cat("Source:", bcaFile, "\n")

sBCA<- st_read(
  bcaFile,
  layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
  query = "SELECT ROLL_NUMBER, geom 
           FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
           WHERE JURISDICTION_CODE='200'
           AND (ACTUAL_USE_DESCRIPTION IN ('Residential Dwelling with Suite','Single Family Dwelling')
                OR INSTR(ACTUAL_USE_DESCRIPTION, 'uplex') > 0)",
  quiet = TRUE
)

cat("Parcels loaded:", nrow(sBCA), "\n")
cat("CRS:", st_crs(sBCA)$epsg, "\n")

# ============================================================================
# Save locally
# ============================================================================

cat("\nSaving to:", outFile, "\n")
st_write(sBCA, outFile, delete_dsn = TRUE)

cat("Done!\n")
cat("File size:", round(file.size(outFile) / 1e6, 1), "MB\n")

