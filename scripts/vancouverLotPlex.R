# vancouverLotPlex.R
# R code to see if 33 vs 50 price gap explains duplex takeup
# And correlation with price per square foot
# And Conley stuff
# Tom Davidoff 
# 05/05/26

library(DBI)
library(RSQLite)
library(data.table)
library(fixest)
library(ggplot2)
library(duckdb)
library(sf)

# Strategy: get single family and duplex transactions, get 33 vs 50 price delta pre-2016, then duplex premiums and permitting...
# permit data
permitFile <- "~/DropboxExternal/dataRaw/issued-building-permits.csv"
dtPermit <- fread(permitFile,select=c("PermitNumber","PermitNumberCreatedDate","ProjectValue","TypeOfWork","Address","Applicant","PropertyUse","SpecificUseCategory","BuildingContractor","GeoLocalArea","geo_point_2d","YearMonth"))
print(head(dtPermit))
dtPermit <- dtPermit[PropertyUse=="Dwelling Uses"]
print(table(dtPermit[,TypeOfWork]))
dtPermit <- dtPermit[TypeOfWork %in% c("New Building","Addition / Alteration")]
print(dtPermit[,summary(ProjectValue),by="TypeOfWork"])
print(dtPermit[ProjectValue>0 & SpecificUseCategory %in% c("Duplex","Single Detached House","Laneway House"),quantile(ProjectValue,c(.01,.05,.1,.5,.75,.9,.95)),by=c("TypeOfWork","SpecificUseCategory")])
CRITQUANT <- .25
qCrit <- quantile(dtPermit[TypeOfWork=="New Building" & SpecificUseCategory %in% c("Duplex","Single Detached House") & ProjectValue>0,ProjectValue],CRITQUANT)
dtPermit <- dtPermit[TypeOfWork=="New Building" | ProjectValue<=qCrit]
# order by count
dtPermit[,nCat:=.N,by=SpecificUseCategory]
# doesn't work dtPermit <- dtPermit[grepl("Duplex"|"Single Detached House"| "Laneway House",SpecificUseCategory)]
dtPermit <- dtPermit[grepl("Duplex",SpecificUseCategory) | grepl("Single Detached House",SpecificUseCategory) | grepl("Laneway House",SpecificUseCategory) | grepl("Multiple Dwelling",SpecificUseCategory)]
# categorize by first word
dtPermit[,useCat:=tstrsplit(SpecificUseCategory," ")[[1]]]
dtPermit[,useCat:=tstrsplit(useCat,",")[[1]]]
print(table(dtPermit[,useCat]))

# limit to R zones in Vancouver
dZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson")
print(table(dZ$zoning_district))
dZ <- dZ[dZ$zoning_district=="R1-1",]
print(head(dZ))

# convert dtPermit to sf
print(dtPermit[1:5,geo_point_2d])
#] "49.2704922, -123.2100666" "49.2402197, -123.1899453" [3] "49.2263643, -123.1221317" "49.2200871, -123.0847553" [5] "49.2706651, -123.1858476"
dtPermit[,c("lat","lon"):=tstrsplit(geo_point_2d,", ")]
dtPermit[,lat:=as.numeric(lat)]
dtPermit[,lon:=as.numeric(lon)]
dtPermit <- dtPermit[!is.na(lat) & !is.na(lon)]
dtPermitSF <- st_as_sf(dtPermit,coords=c("lon","lat"),crs=4326)

# intersect with zoning
dtPermitSF <- st_transform(dtPermitSF,st_crs(dZ))
dtPermitSF <- st_join(dtPermitSF,dZ,join=st_intersects)
print(table(dtPermitSF$zoning_district))
print(nrow(dtPermitSF))
print(nrow(dtPermit))
print(summary(is.na(dtPermitSF$zoning_district)))
dtPermitSF <- dtPermitSF[!is.na(dtPermitSF$zoning_district),]

# Now get sales, etc from BC Assessment, use 2019 so as complete as feasible. Can get appx lat-lon from pccf at 6-digit postal code
library(DBI); library(RSQLite); library(data.table)

con <- dbConnect(SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")

# --- Read each table, filter where cheap ---

dtFolio <- dbGetQuery(con, "
  SELECT folioID, rollNumber, jurisdictionCode, assessmentAreaCode
  FROM folio WHERE jurisdictionCode = '200'
") |> setDT()

dtFolioDesc <- dbGetQuery(con, "
  SELECT folioID, neighbourhoodDescription, actualUseDescription,
         landWidth, landDepth
  FROM folioDescription
") |> setDT()

dtResInv <- dbGetQuery(con, "
  SELECT roll_number, jurisdiction, area,
         MB_effective_year, MB_year_built, MB_total_finished_area,
         zoning, num_full_baths, land_width, land_depth
  FROM residentialInventory WHERE jurisdiction = '200'
") |> setDT()

dtAddress <- dbGetQuery(con, "
  SELECT folioID, postalCode, primaryFlag, streetNumber, streetName
  FROM address WHERE primaryFlag = 'true'
") |> setDT()


# --- Cast roll numbers to numeric for clean joining ---
dtFolio[,  rollNumber_n := as.numeric(rollNumber)]
dtResInv[, rollNumber_n := as.numeric(roll_number)]

# Sanity: any NAs introduced by coercion? (non-numeric roll numbers)
cat("Folio roll NAs:",  sum(is.na(dtFolio$rollNumber_n)), "/", nrow(dtFolio), "\n")
cat("ResInv roll NAs:", sum(is.na(dtResInv$rollNumber_n)), "/", nrow(dtResInv), "\n")

# --- Cast numeric inventory fields ---
num_cols <- c("MB_effective_year","MB_year_built","MB_total_finished_area",
              "num_full_baths","land_width","land_depth")
dtResInv[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

dtFolioDesc[, `:=`(landWidth = as.numeric(landWidth),
                   landDepth = as.numeric(landDepth))]

# --- Merges ---

# 1. folio + folioDescription on folioID
setkey(dtFolio,     folioID)
setkey(dtFolioDesc, folioID)
dtH <- dtFolioDesc[dtFolio]   # right join: keeps all Vancouver folios

cat("After folioDesc merge:", nrow(dtH), "\n")

# 2. add residentialInventory on (rollNumber_n, jurisdiction, area)
dtH <- merge(dtH, dtResInv,
             by.x = c("rollNumber_n","jurisdictionCode","assessmentAreaCode"),
             by.y = c("rollNumber_n","jurisdiction","area"),
             all.x = TRUE)

cat("After resInv merge:", nrow(dtH),
    " | with MB_total:", sum(!is.na(dtH$MB_total_finished_area)), "\n")

# 3. add address on folioID
setkey(dtH, folioID)
setkey(dtAddress, folioID)
dtH <- dtAddress[, .(folioID, postalCode, streetNumber, streetName)][dtH, on = "folioID"]

cat("After address merge:", nrow(dtH),
    " | with postalCode:", sum(!is.na(dtH$postalCode) & dtH$postalCode != ""), "\n")

# --- Filter to RS hedonic sample ---
dtHedonic19 <- dtH[!is.na(MB_effective_year) &
                   !is.na(MB_year_built) &
                   !is.na(MB_total_finished_area) &
                   grepl("RS", zoning)]

# Normalize postal code for PCCF merge
dtHedonic19[, postalCode := toupper(gsub("\\s+", "", postalCode))]

cat("Final dtHedonic19:", nrow(dtHedonic19),
    " | with postalCode:", sum(!is.na(dtHedonic19$postalCode) & dtHedonic19$postalCode != ""), "\n")

# sales
dtSales <- dbGetQuery(con, "SELECT folioID, conveyanceDate, conveyancePrice, conveyanceTypeDescription FROM sales ") |> setDT()
dbDisconnect(con)
print(head(dtSales))

setkey(dtHedonic19, folioID)
setkey(dtSales, folioID)
dtSales <- dtSales[dtHedonic19]
print(head(dtSales))

print(summary(dtSales[,.(landWidth,landDepth)]))
dtSales[is.na(landWidth),landWidth:=land_width]
dtSales[is.na(landDepth),landDepth:=land_depth]
print(summary(dtSales[,.(landWidth,landDepth)]))
dtSales <- dtSales[!is.na(landWidth) & !is.na(landDepth)]

# get lat and lon from pccf
pccfFile <- "~/DropboxExternal/dataRaw/pccfNat_fccpNat_062017.txt"
# grab postal code and lat-lon, note fixed width per below
#A0A1A0A0A1010011001144Aquaforte                                                             T  21499979910.001099101000100009100107450003  47.011133   -52.93662115AQUAFORTE                     WW19830801190000012BNN30
#A0A1B0A0A1010011001464Avondale                                                              T  47299649910.001099101000100009100105620003  47.400311   -53.22148513AVONDALE                      WW19830401190000012BNN30
# read as fixed width
library(readr); library(data.table)

pccfFile <- "~/DropboxExternal/dataRaw/pccfNat_fccpNat_062017.txt"

# PCCF Reference Guide column positions (1-indexed, inclusive)
# Pulling just what you need for postal → CT/DA/lat-lon merge
pccf_cols <- fwf_cols(
  postalCode = c(1,   6),    # 6-char postal
  fsa        = c(7,   9),    # forward sortation area  
  pr         = c(10, 11),    # province
  cdUid      = c(12, 15),    # census division
  csdUid     = c(16, 22),    # census subdivision
  csdName    = c(23, 92),    # CSD name (70 chars, padded)
  csdType    = c(93, 95),
  ccsCode    = c(96, 98),
  saCode     = c(99,101),
  edUid      = c(102,104),
  ed_year    = c(105,108),
  daUid      = c(109,116),   # dissemination area (8-digit)
  dmt        = c(117,117),
  demoCode   = c(118,121),
  ctUid      = c(122,128),   # census tract (7-char, fmt: CMA + 4-digit base + 2-digit decimal)
  pcType     = c(129,129),
  lat        = c(130,140),
  lon        = c(141,152),
  sli        = c(153,153),   # single link indicator — KEY for dedup
  pcAssign   = c(154,156),
  cmaPuid    = c(157,159),
  birthDate  = c(160,167),
  retireDate = c(168,175),
  pos        = c(176,177),   # 'WP' weighted, etc.
  comm_name  = c(178,207),
  dmtName    = c(208,208),
  hLevel     = c(209,210),
  qi         = c(211,212)
)

dtPCCF <- read_fwf(pccfFile, pccf_cols, 
                   col_types = cols(.default = "c"),
                   trim_ws = TRUE) |> setDT()

cat("PCCF rows:", nrow(dtPCCF), "\n")

# Keep only single-link records — gives you 1:1 postal → geography
dtPCCFlink <- dtPCCF[sli == "1"]

# BC only, since you're merging to Vancouver
dtPCCFlink <- dtPCCFlink[pr == "59"]   # 59 = BC

# Cast lat/lon
dtPCCFlink[, `:=`(lat = as.numeric(lat), lon = as.numeric(lon))]

# Verify uniqueness
dtPCCFlink[, .N, by = postalCode][N > 1]   # should be empty (or near-empty)

cat("BC SLI records:", nrow(dtPCCFlink),
    " | unique postals:", uniqueN(dtPCCFlink$postalCode), "\n")

# Postal codes already normalized in dtHedonic19 (toupper + no spaces)
# PCCF stores them the same way — but verify:
head(dtPCCFlink$postalCode, 3)

dtHedonic19 <- merge(
  dtHedonic19,
  dtPCCFlink[, .(postalCode, ctUid, daUid, lat_pccf = lat, lon_pccf = lon)],
  by = "postalCode",
  all.x = TRUE
)

cat("With CT:", sum(!is.na(dtHedonic19$ctUid)), "/", nrow(dtHedonic19), "\n")
