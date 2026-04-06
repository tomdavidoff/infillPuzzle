# testSQFT.R
# R to test whether square feet are different assessor history vs flat
# Tom Davidoff 
# 04/03/26
library(data.table)
library(duckdb)
library(sf)
cf <-  list(SALES_FILE = "~/DropboxExternal/dataProcessed/27053.txt",
	ASSESSOR_PARQUET = "~/DropboxExternal/dataProcessed/AH_state_MN.parquet",
	TRACT_PARQUET = "~/DropboxExternal/dataProcessed/attomTract.parquet",
	ASSESSOR_PARQUET_2 = "~/DropboxExternal/dataProcessed/by_property_states/tax_assessor_state_MN.parquet",
	FIPS_MUNI = "053",
	LON_RANGE = c(-95.5, -91.0),
	LAT_RANGE = c(43.0, 47.0),
	YEARS = 2016:2020,
	ZIP_START = "55",
	MIN_SQFT = 400
)
MIN_OBS <- 20
con <- dbConnect(duckdb())
# --- Assessor History: filtered parquet read via DuckDB ---
dtA <- as.data.table(dbGetQuery(con, sprintf(
	"SELECT CAST(\"[ATTOM ID]\" AS VARCHAR) AS attom_id,
	SA_FIN_SQFT_1, SA_FIN_SQFT_2, SA_FIN_SQFT_3, SA_FIN_SQFT_4, SA_BSMT_FIN_SQFT,
	CAST(SA_FIN_SQFT_TOT AS DOUBLE) AS sqft, CAST(SA_SQFT AS DOUBLE) AS sqft_alt,
	SA_SQFT_DQ,
	CAST(SA_BLDG_SQFT AS DOUBLE) AS bldgSqft,
	CAST(SA_LOTSIZE AS DOUBLE) AS lotSize,
	ASSR_YEAR
	FROM read_parquet('%s')
	WHERE MM_FIPS_MUNI_CODE = '%s'
	AND USE_CODE_STD = 'RSFR'
	AND CAST(SA_FIN_SQFT_TOT AS DOUBLE) > %d",
	cf$ASSESSOR_PARQUET, cf$FIPS_MUNI, cf$MIN_SQFT
)))
dtA[, maxYear := max(ASSR_YEAR), by = attom_id]
dtA <- dtA[ASSR_YEAR == maxYear]

# --- Tax Assessor (flat file): AreaBuilding measure ---
dtA2 <- as.data.table(dbGetQuery(con, sprintf(
	"SELECT CAST(\"[ATTOM ID]\" AS VARCHAR) AS attom_id,
	CAST(AreaBuilding AS DOUBLE) AS AreaBuilding,
	AreaBuildingDefinitionCode,
	CAST(AreaGross AS DOUBLE) AS AreaGross,
	CAST(Area1stFloor AS DOUBLE) AS Area1stFloor,
	CAST(Area2ndFloor AS DOUBLE) AS Area2ndFloor,
	CAST(AreaUpperFloors AS DOUBLE) AS AreaUpperFloors,
	PropertyUseMuni, PropertyUseGroup, PropertyUseStandardized,
	CAST(StoriesCount AS INT) AS StoriesCount,
	CAST(PropertyLongitude AS DOUBLE) AS Longitude,
	CAST(PropertyLatitude AS DOUBLE) AS Latitude,
	ZonedCodeLocal
	FROM read_parquet('%s')
	WHERE PropertyUseStandardized = '385'",
	cf$ASSESSOR_PARQUET_2
)))

dbDisconnect(con)

# --- Merge ---
setkey(dtA, attom_id)
setkey(dtA2, attom_id)
dtM <- dtA[dtA2, nomatch = 0]

# --- The key comparison ---
message("=== Definition codes (what does each sqft field actually mean?) ===")
message("SA_SQFT_DQ from history file:")
print(table(dtM$SA_SQFT_DQ, useNA = "ifany"))
message("AreaBuildingDefinitionCode from flat file:")
print(table(dtM$AreaBuildingDefinitionCode, useNA = "ifany"))

message("=== All sqft measures side by side ===")
print(summary(dtM[, .(sqft, sqft_alt, bldgSqft, AreaBuilding, AreaGross,
	Area1stFloor, Area2ndFloor, AreaUpperFloors)]))

message("=== AreaBuilding (flat) vs SA_FIN_SQFT_TOT (history) ===")
print(summary(dtM[, .(sqft, AreaBuilding, diff = sqft - AreaBuilding, ratio = sqft / AreaBuilding)]))

message("=== By StoriesCount ===")
print(dtM[, .(medianHistory = median(sqft, na.rm = TRUE),
	medianFlat = median(AreaBuilding, na.rm = TRUE),
	medianGross = median(AreaGross, na.rm = TRUE),
	medianRatio = median(sqft / AreaBuilding, na.rm = TRUE),
	N = .N), by = StoriesCount][order(StoriesCount)])

message("=== By AreaBuildingDefinitionCode ===")
print(dtM[, .(medianHistory = median(sqft, na.rm = TRUE),
	medianFlat = median(AreaBuilding, na.rm = TRUE),
	medianGross = median(AreaGross, na.rm = TRUE),
	N = .N), by = AreaBuildingDefinitionCode][order(AreaBuildingDefinitionCode)])

# --- Convert floor sqft to numeric and decompose ---
dtM[, SA_FIN_SQFT_1 := as.numeric(SA_FIN_SQFT_1)]
dtM[, SA_FIN_SQFT_2 := as.numeric(SA_FIN_SQFT_2)]
dtM[, SA_FIN_SQFT_3 := as.numeric(SA_FIN_SQFT_3)]
dtM[, SA_FIN_SQFT_4 := as.numeric(SA_FIN_SQFT_4)]
dtM[, SA_BSMT_FIN_SQFT := as.numeric(SA_BSMT_FIN_SQFT)]

message("=== sqft - sum of floors (should be ~0 if sqft = sum of floors) ===")
print(summary(dtM[, sqft - SA_FIN_SQFT_1 - SA_FIN_SQFT_2 - SA_FIN_SQFT_3 - SA_FIN_SQFT_4]))

message("=== sqft - sum of floors - basement ===")
print(summary(dtM[, sqft - SA_FIN_SQFT_1 - SA_FIN_SQFT_2 - SA_FIN_SQFT_3 - SA_FIN_SQFT_4 - SA_BSMT_FIN_SQFT]))

message("=== AreaBuilding vs 1st floor only (footprint check) ===")
print(summary(dtM[, .(AreaBuilding, SA_FIN_SQFT_1, diff = AreaBuilding - SA_FIN_SQFT_1)]))

# --- Zoning ---
zoning_file <- "~/DropboxExternal/dataRaw/Planning_Primary_Zoning/Planning_Primary_Zoning.shp"
message("Reading Minneapolis zoning data...")
sfZoning <- st_read(zoning_file)
names(sfZoning) <- tolower(names(sfZoning))
sfZoning <- st_transform(sfZoning, 26915)

message("Converting property locations to sf points...")
sfPoints <- st_as_sf(dtM, coords = c("Longitude", "Latitude"), crs = 4326)
sfPoints <- st_transform(sfPoints, 26915)

message("Performing spatial join...")
sfPointsZoned <- st_join(sfPoints, sfZoning, left = TRUE)
dtMZoned <- as.data.table(sfPointsZoned)

message("=== Zoning land use codes ===")
print(table(dtMZoned$land_use_c))
message(sprintf("Rows before join: %d, after: %d", nrow(dtM), nrow(dtMZoned)))

message("=== Summary for UN1/UN2/UN3 zones ===")
print(summary(dtMZoned[land_use_c %in% c("UN1", "UN2", "UN3"),
	.(sqft, AreaBuilding, StoriesCount,AreaGross)]))

message("=== Comparison by stories, UN1/UN2/UN3 only ===")
print(dtMZoned[land_use_c %in% c("UN1", "UN2", "UN3"),
	.(medianHistory = median(sqft, na.rm = TRUE),
	  medianFlat = median(AreaBuilding, na.rm = TRUE),
	  N = .N), by = StoriesCount][order(StoriesCount)])

