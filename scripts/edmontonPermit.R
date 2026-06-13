# edmontonPermit.R
# edmonton building permit data
# Tom Davidoff
# learned via websearch linking here https://data.edmonton.ca/Urban-Planning-Economy/General-Building-Permits/24uj-dj8v/about_data

library(data.table)
library(sf)
FILENAME <- "~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv"
if (!file.exists(FILENAME)) {
  dp <- fread("https://data.edmonton.ca/api/views/24uj-dj8v/rows.csv?accessType=DOWNLOAD")
  fwrite(dp, FILENAME)
} else dp <- fread(FILENAME)

print(head(dp))

print(table(dp[,ZONING]))
newZone <- c("RS")
formerZones  <- c("RF1","RF2","RF3","RF4","RF4t","RF4T")
dp <- dp[ZONING %in% formerZones| ZONING %in% newZone]
print(table(dp[,ZONING]))
dp[,permitMonth:=substring(PERMIT_DATE,1,7)]
for (m in unique(dp[order(permitMonth),permitMonth])) {
  print(m)
  print(table(dp[permitMonth==m,ZONING]))
  print(table(dp[permitMonth==m,BUILDING_TYPE]))
  print(dp[permitMonth==m,mean(CONSTRUCTION_VALUE,na.rm=TRUE),by=BUILDING_TYPE])
}

# merge with census tract data
dCT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
print(head(dCT))

print(summary(is.na(dp[,LATITUDE]))) # small minority
dps <- st_as_sf(dp[!is.na(LATITUDE)], coords=c("LONGITUDE","LATITUDE"), crs=4326)

dps <- st_transform(dps, st_crs(dCT))
# join on within
print(nrow(dps))
dps <- st_join(dps, dCT[,c("CTUID","CTNAME")], join=st_within)
print(nrow(dps))

dtIncome <- fread("~/DropboxExternal/dataRaw/9810005801_databaseLoadingData.csv") #select = c("GEO", "DGUID", "Household income statistics (6)", "VALUE"))
dtIncome <- dtIncome[`Household income statistics (6)` == "Median household total income (2020) (2020 constant dollars)"]

## from Gemini
#2021 (Vintage year)
#S (Spatial type)
#0507 (The specific geographic boundary type code, which is why it always ends in 507 for this dataset).
dtIncome[,CTUID:=gsub("2021S0507","",DGUID)]

print(nrow(dps))
dps <- merge(dps, dtIncome[,c("CTUID","VALUE")], by="CTUID")
print(nrow(dps))
print(summary(dps))

dps <- data.table(dps)[YEAR>2023]
print(summary(dps))
dps[,meanRow:=mean(grepl("Row House",BUILDING_TYPE)),by=CTUID]
dps[,meanSingle:=mean(grepl("Single Detached House",BUILDING_TYPE)),by=CTUID]
dps[,shareRow:=meanRow/(meanSingle+meanRow)]
print(cor(dps[,.(meanRow,shareRow,VALUE)],use="complete.obs"))
print(dps[,mean(UNITS_ADDED,na.rm=TRUE),by=BUILDING_TYPE])

# verify what's feasible with building inventory 2026
dtI <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv",select=c("Latitude","Longitude","Point Location","zoning","year_built","lot_size","House Number","Street Name","Total Gross Area"))
print(table(dtI[,zoning]))
dtI <- dtI[zoning=="RS" & year_built>2023]
print(summary(dtI))
# print(table(dtI[,is.na(Suite) | Suite==""])) # suite almost always missing or ""

# do permit and inventory merge as an alternative test

dtP <- fread(FILENAME,select=c("YEAR","LATITUDE","LONGITUDE","Geometry Point","ADDRESS","ZONING","WORK_TYPE","BUILDING_TYPE","UNITS_ADDED","CONSTRUCTION_VALUE"))
dtP <- dtP[YEAR>=2023 & ZONING=="RS"]
dtP <- dtP[grepl("Row House",BUILDING_TYPE) | grepl("Single Detached House",BUILDING_TYPE)]
print(table(dtP[,.(BUILDING_TYPE,WORK_TYPE)]))
print(dtP[,mean(UNITS_ADDED,na.rm=TRUE),by=BUILDING_TYPE])
print(dtP[,mean(CONSTRUCTION_VALUE,na.rm=TRUE),by=BUILDING_TYPE])
print(dtP[,mean(is.na(UNITS_ADDED)),by=BUILDING_TYPE])

## nearest-neighbour join ----------------------------------------------
sfP <- st_as_sf(dtP[!is.na(LATITUDE)], coords = c("LONGITUDE","LATITUDE"),
                crs = 4326, remove = FALSE)
sfI <- st_as_sf(dtI[!is.na(Latitude)], coords = c("Longitude","Latitude"),
                crs = 4326, remove = FALSE)

dtM <- st_join(sfI, sfP, join = st_nearest_feature)
print(head(dtM))

dtM$dist <- st_distance(st_geometry(dtM), st_as_sfc(dtM[["Geometry Point"]], crs = 4326), by_element = TRUE) |> as.numeric()

dtM <- as.data.table(dtM)
print(summary(dtM[,dist]))
print(dtM[,summary(dist),by=.(BUILDING_TYPE)])
print(dtM[,summary(as.numeric(lot_size)),by=.(BUILDING_TYPE)])
print(dtM[,mean(`Total Gross Area`,na.rm=TRUE),by=BUILDING_TYPE])
print(dtM[grepl("Row House",BUILDING_TYPE)][order(ADDRESS)])
print(head(dtM))
print(head(dtM[dist<60]))
print(head(dtM[dist<30]))


