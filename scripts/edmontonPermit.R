# edmontonPermit.R
# edmonton building permit data
# Tom Davidoff
# learned via websearch linking here https://data.edmonton.ca/Urban-Planning-Economy/General-Building-Permits/24uj-dj8v/about_data

library(data.table)
library(sf)
FILENAME <- "~/DropboxExternal/dataRaw/edmontonBuildingPermits.csv"
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

print(names(dtIncome))
print(names(dps))

q("no")
