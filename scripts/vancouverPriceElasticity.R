# vancouverPriceElasticity.R
# price and elasticity by geography
# Tom Davidoff 
# 10/29/26

library(sf)
library(DBI)
library(RSQLite)
library(data.table)

# 2019 data with sales through 2018
sql2019 <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
# open with 
con <- dbConnect(SQLite(), sql2019)
q <- "SELECT s.conveyancePrice, s.conveyanceDate, s.conveyanceTypeDescription, f.rollNumber, i.MB_effective_year, i.MB_total_finished_area, i.land_width, i.land_depth, d.actualUseDescription , d.neighbourhoodDescription
FROM folio f
JOIN sales s ON s.folioID=f.folioID 
JOIN folioDescription d ON d.folioID=f.folioID
JOIN residentialInventory i ON i.roll_number=f.rollNumber
WHERE f.jurisdictionCode= '200' AND s.conveyanceTypeDescription=='Improved Single Property Transaction' and d.actualUseDescription IN ('Residential Dwelling with Suite',  'Single Family Dwelling')"
# do q1 with con
df <- as.data.table(dbGetQuery(con, q))
print(head(df))
print(table(df$actualUseDescription))
df[,rollStart:=floor(as.numeric(rollNumber)/1000)]

# get tract etc
# ---- tracts; 2026 parcels -> tract (centroid) ----
fTract    <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
ct <- st_read(fTract, quiet = TRUE)
ct <- st_transform(st_make_valid(ct[ct$PRUID == "59", "CTNAME"]), 3005)

fdir <- "~/bigFiles/latestSpatialBCA/"
gpkg <- paste0(fdir,list.files(path="~/bigFiles/latestSpatialBCA")[1])
print(gpkg)
g <- st_read(gpkg, quiet = TRUE, query =
  "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
g <- st_transform(st_make_valid(g), 3005)
g$rollStart <- floor(as.numeric(g$ROLL_NUMBER)/ 1000)
geo <- as.data.table(st_drop_geometry(st_join(st_centroid(g), ct, join = st_within)))
geo <- unique(geo[!is.na(CTNAME), .(rollStart, CTNAME)], by = "rollStart")

print(head(geo))

dfM <- merge(df, geo, by = "rollStart", all.x = TRUE)
print(table(dfM[,CTNAME]))
print(table(is.na(dfM[,CTNAME])))
print(table(dfM[is.na(CTNAME),neighbourhoodDescription]))


