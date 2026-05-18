# greaterVancouverElasticity.R
# R code to go census tract -> neighbourhood -> elasticity of circa 2018 ppsf and elasticity
# Tom Davidoff 
# 05/16/26

library(sf)
library(data.table)
library(DBI)
library(RSQLite)
library(fixest)

# helpers
trimByQuantile <- function(dt, cols, q) {
  for (c in cols) {
    lo <- quantile(dt[[c]], q,     na.rm = TRUE)
    hi <- quantile(dt[[c]], 1 - q, na.rm = TRUE)
    dt <- dt[get(c) %between% c(lo, hi)]
  }
  dt
}


# ============================================================================
# Extract Greater residential parcels
# ============================================================================

bcaGeoFolder <- "~/bigFiles/latestSpatialBCA/"
bcaFiles <- list.files(bcaGeoFolder, pattern="*.gpkg", full.names=FALSE) # will only be one
geoInFile <- paste0(bcaGeoFolder, bcaFiles[1])
geoOutFile <- "~/DropboxExternal/dataProcessed/greaterVancouverSingle.rds"

if (!file.exists(geoOutFile)) {
    cat("Loading Greater Vancouver residential parcels from BCA \n")
    cat("Source:", geoInFile, "\n")
    
    sBCA <- st_read(
        geoInFile,
        layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
        query = "SELECT ROLL_NUMBER, NEIGHBOURHOOD, JURISDICTION_CODE, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV WHERE REGIONAL_DISTRICT_CODE='15' AND ACTUAL_USE_DESCRIPTION IN ('Residential Dwelling with Suite','Single Family Dwelling') ", 
        quiet = TRUE
    )
    print(head(sBCA))
    
    cat("Parcels loaded:", nrow(sBCA), "\n")
    cat("CRS:", st_crs(sBCA)$epsg, "\n")
    
   
    

    # Spatial join: add census tracts
    dCT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
    dCT <- st_transform(dCT, st_crs(sBCA))
    sBCA <- st_join(sBCA, dCT, join = st_within)
    dtBCA <- as.data.table(sBCA)
    # FIX: Clean strings and align column names right at the source layer
    dtBCA[, rollNumber   := trimws(as.character(ROLL_NUMBER))]
    dtBCA[, jurisdiction := trimws(as.character(JURISDICTION_CODE))]
    dtBCA <- dtBCA[!is.na(rollNumber) & rollNumber != "" & !is.na(jurisdiction) & jurisdiction != ""]
    
    saveRDS(dtBCA, geoOutFile)
}
dtBCA <- as.data.table(readRDS(geoOutFile))
print(head(dtBCA))

cfgVancouver <- list(
    city           = "Vancouver",
    crsProj        = 26910,                              # UTM 10N (meters)
    xlim           = c(-123.23, -123.02),
    ylim           = c( 49.20,   49.32),
    tileZoom       = 12,
    sqliteFile     = "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3",
    bcaRDS         = "~/DropboxExternal/dataProcessed/bca_vancouver_residential.rds",
    permitFile     = "~/DropboxExternal/dataRaw/issued-building-permits.csv",
    zoningGeojson  = "~/DropboxExternal/dataRaw/vancouver_zoning.geojson",
    targetZone     = "R1-1",
    trimDimsQ      = 0.05,                          # winsorize landWidth & landDepth
    trimYQ         = 0.01,                          # winsorize the response (ppsf or price)
    salesYearRange = c(2014, 2018),
    permitYearRange = c(2019, 2023),
    excludeBox     = NULL,
    excludeBoxRef  = list(
        west  = -123.1283,   # Oak St
        east  = -123.1037,   # Ontario St
        north =   49.2575    # W 16th Ave
    )
)

# ===========================================================================
# APPRAISAL LOADER (BCA valuation table)
# ===========================================================================
cfg <- cfgVancouver

con <- dbConnect(SQLite(), cfg$sqliteFile)
qVal  <- "SELECT folioID, landValue FROM valuation"
qInv  <- "SELECT roll_number, land_width, land_depth, zoning, MB_effective_year, MB_total_finished_area, jurisdiction FROM residentialInventory"
qFol  <- "SELECT folioID, rollNumber, jurisdictionCode FROM folio "
qDes  <- "SELECT folioID, actualUseDescription, landWidth, landDepth FROM folioDescription WHERE actualUseDescription IN ('Residential Dwelling with Suite','Single Family Dwelling')"

valuation   <- as.data.table(dbGetQuery(con, qVal))
inventory   <- as.data.table(dbGetQuery(con, qInv))
folio       <- as.data.table(dbGetQuery(con, qFol))
description <- as.data.table(dbGetQuery(con, qDes))
dbDisconnect(con)
    
setkey(valuation, folioID);     setkey(folio, folioID);        valuation <- valuation[folio]
setkey(description, folioID);   setkey(valuation, folioID);    valuation <- valuation[description]
setnames(valuation, "jurisdictionCode", "jurisdiction")

# FIX 1: Keep IDs safely as trimmed characters to avoid precision loss & truncation
valuation[, rollNumber   := trimws(as.character(rollNumber))]
inventory[, rollNumber   := trimws(as.character(roll_number))]
valuation[, jurisdiction := trimws(as.character(jurisdiction))]
inventory[, jurisdiction := trimws(as.character(jurisdiction))]
    
# FIX 2: Clear missing keys *before* doing any uniqueness testing
valuation <- valuation[!is.na(rollNumber) & rollNumber != "" & !is.na(jurisdiction) & jurisdiction != ""]
inventory <- inventory[!is.na(rollNumber) & rollNumber != "" & !is.na(jurisdiction) & jurisdiction != ""]

# FIX 3: Drop duplicate database records using safe 1-to-1 string checks
valuation <- unique(valuation, by = c("rollNumber", "jurisdiction"))
inventory <- unique(inventory, by = c("rollNumber", "jurisdiction"))

# First inner join (Database cross-match)
setkey(inventory, rollNumber, jurisdiction)
setkey(valuation, rollNumber, jurisdiction)
valuation <- valuation[inventory, nomatch = NULL]

# Numeric formatting transformations
numericCols <- c("landValue","land_width","land_depth","landWidth","landDepth")
valuation[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
valuation[is.na(landWidth) & !is.na(land_width), landWidth := land_width]
valuation[is.na(landDepth) & !is.na(land_depth), landDepth := land_depth]

# FIX 4: Exact string match instead of lossy numeric rollStart math
setkey(valuation, rollNumber, jurisdiction)
setkey(dtBCA,      rollNumber, jurisdiction)
dt <- valuation[dtBCA, nomatch = 0]   # Precise spatial intersection

# Extract coordinates directly from sf shape layer if not present as flat variables
if (!"lat" %in% names(dt) || !"lon" %in% names(dt)) {
	coords <- sf::st_coordinates(sf::st_centroid(sf::st_as_sf(dt)))
	dt[, lon := coords[, 1]]
	dt[, lat := coords[, 2]]
}
    
# Derive analytical variables
dt[, landArea := landWidth * landDepth]
dt[, ppsf     := landValue / landArea]
dt <- dt[!is.na(lat) & !is.na(lon) & !is.na(ppsf) & !is.na(landArea) &
	 landArea > 0 & ppsf > 0]

dt <- trimByQuantile(dt, c("landWidth","landDepth"), cfg$trimDimsQ)
dt <- trimByQuantile(dt, "ppsf", cfg$trimYQ)
dt[, logPPSF := log(ppsf)]
dt[, logArea := log(landArea)]
dtMerge <- dt
dt <- NULL

# ===========================================================================
# 5. SALES LOADER (BCA sales table)
# ===========================================================================

con <- dbConnect(SQLite(), cfg$sqliteFile)
dt  <- as.data.table(dbGetQuery(con, "SELECT folioID, conveyanceDate, conveyancePrice, conveyanceTypeDescription FROM sales"))
dbDisconnect(con)
    
dt <- dt[conveyanceTypeDescription == "Improved Single Property Transaction", .(folioID, conveyanceDate, conveyancePrice)]
dt <- merge(dt, dtMerge, by = "folioID")
    
dt[, conveyancePrice := as.numeric(conveyancePrice)]
dt[, landWidth       := as.numeric(landWidth)]
dt[, landDepth       := as.numeric(landDepth)]
dt <- dt[!is.na(lat) & !is.na(lon) & !is.na(conveyancePrice) &
	 !is.na(landWidth) & !is.na(landDepth) &
	 conveyancePrice > 0 & landWidth > 0 & landDepth > 0]

dt[, year      := as.numeric(substring(conveyanceDate, 1, 4))]
dt[, logPrice  := log(conveyancePrice)]
dt[, landArea  := landWidth * landDepth]
dt[, logArea   := log(landArea)]
dt[, ppsfSale  := conveyancePrice / landArea]
dt[, logPPSFSale := log(ppsfSale)]
if ("MB_effective_year" %in% names(dt))
dt[, age := year - as.numeric(MB_effective_year)]
if ("MB_total_finished_area" %in% names(dt)) {
	dt[, FSR    := as.numeric(MB_total_finished_area) / landArea]
	dt[, logFSR := log(FSR)]
}

dt <- trimByQuantile(dt, c("landWidth","landDepth"), cfg$trimDimsQ)
dt <- trimByQuantile(dt, "ppsfSale", cfg$trimYQ)
if (!is.null(cfg$salesYearRange)) {
	dt <- dt[year %between% cfg$salesYearRange]
}
print(head(dt))
print(summary(dt))

# for each level: CTNAME, NEIGHBOURHOOD, jurisdiction
# only do sales
# calculate mean ppsf residuals from reg log PPSFSale ~ log(year - MB_effective_year) + dummy at relevant level -- collect geographic residuals -- or do with i(term) in fixest, better
# then calculate elasticity of ppsf with respect to landArea for appraisal 2019 <- just do for year is max year by rollNUmber/jurisdiction pair log PPSFSale ~ log(year - MB_effective_year) + log(MB_total_finished_area) do for each area a separate regression and collect reg coefficient on log(MB_total_finished_area), then create a data.table of ppsf residual and of elasiticity by area and calculate the correlation for non-missing. Do for SALESRANGEYEARS or whatever

MINREGOBS <- 10

levelsToRun <- c("CTNAME","NEIGHBOURHOOD","jurisdiction")
dt[, age := (year - as.numeric(MB_effective_year))]
dtReg <- dt[age>=1 & !is.na(age) & year %between% cfg$salesYearRange]
dtReg[,sqft:=as.numeric(MB_total_finished_area)]
dtReg[,ppsf := conveyancePrice / as.numeric(MB_total_finished_area)]
out <- list()

for (level in levelsToRun) {

  regFE <- feols(
    as.formula(paste0("ppsf ~ log(age) +i(year)| ", level)),
    data = dtReg
  )

  fe_dt <- data.table(
    level = level,
    level_value = names(fixef(regFE)[[1]]),
    ppsf_fe = as.numeric(fixef(regFE)[[1]])
  )

  elas_dt <- dtReg[
    !is.na(get(level)) & !is.na(FSR) & FSR > 0,
    {
      if (.N >= MINREGOBS) {
	reg <- feols(log(ppsf) ~ log(age) + i(year) + log(sqft), data = .SD)
	.(elasticity = coef(reg)["log(sqft)"], n = .N)
      } else {
	.(elasticity = NA_real_, n = .N)
      }
    },
    by = .(level_value = get(level))
  ]

  out[[level]] <- merge(fe_dt, elas_dt, by = "level_value", all.x = TRUE)

  cat("\n", level, "\n")
  print(out[[level]][!is.na(elasticity), cor(ppsf_fe, elasticity)])
}

