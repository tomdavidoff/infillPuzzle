# extractBCA26VancouverResidential.R
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
  query = "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
           WHERE JURISDICTION_CODE='200'",
  quiet = TRUE
)
           #AND (ACTUAL_USE_DESCRIPTION IN ('Residential Dwelling with Suite','Single Family Dwelling')

cat("Parcels loaded:", nrow(sBCA), "\n")
cat("CRS:", st_crs(sBCA)$epsg, "\n")
dtBCA <- as.data.table(sBCA)
dtBCA[,rollStart:=floor(as.numeric(ROLL_NUMBER)/1000)]

# ============================================================================
# Save locally
# ============================================================================


# Merge with BCA 2019 Vancouver single family R1 -zoned parcels
bca19 <- "~/OneDrive - UBC/dataRaw/REVD19_and_inventory_extracts.sqlite3"
# get the schema
# get zoning, rollnumber, folioID from bca19
con <- dbConnect(RSQLite::SQLite(), bca19)
dfFolio <- dbGetQuery(con, "SELECT folioID, rollNumber FROM folio WHERE jurisdictionCode=='200'")
dfInventory <- dbGetQuery(con, "SELECT roll_number, zoning, MB_effective_year, MB_total_finished_area FROM residentialInventory") 
dfDescription <- dbGetQuery(con, "SELECT folioID, actualUseDescription, neighbourhoodDescription, landWidth, landDepth FROM folioDescription")
dfBCA19 <- merge(dfFolio, dfInventory, by.x="rollNumber", by.y="roll_number")
dtBCA19 <- data.table(merge(dfBCA19, dfDescription, by="folioID"))
print(head(dtBCA19))
print(table(dtBCA19[,actualUseDescription])[order(-table(dtBCA19[,actualUseDescription]))])
dtBCA19 <- dtBCA19[actualUseDescription %in% c("Single Family Dwelling","Residential Dwelling with Suite")]

dtBCA19[,rollStart:=floor(as.numeric(rollNumber)/1000)]
dtBCA19 <- merge(dtBCA19, dtBCA, by="rollStart",all.x=TRUE, suffixes=c("_2019","_2026"))
print(head(dtBCA19))
print(mean(dtBCA19[,is.na(geom)])) # should be small
# how many duplex/multi on same rollStart?
dtBCA19[,Nlot:=.N, by=.(rollStart)]
print(table(dtBCA19[,Nlot]))
print(table(dtBCA19[Nlot>5,neighbourhoodDescription])) # nontrivial but small
dtBCA19 <- dtBCA19[Nlot==1 ]
print(table(dtBCA19[,zoning]))
dtBCA19 <- dtBCA19[grepl("RS",zoning)]
saveRDS(dtBCA19, file="~/OneDrive - UBC/dataProcessed/bca_vancouver_residential.rds")


q("no")

