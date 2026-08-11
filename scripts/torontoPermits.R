# torontoPermits.R
# look at location, etc of appropriate permits
# Tom Davidoff
# 08/10/26

library(duckdb)
library(data.table)
library(sf)

# read in the file
con <- dbConnect(duckdb())
permitPath <- path.expand("~/DropboxExternal/dataRaw/torontoBuildingPermits.csv")
dfP <- dbGetQuery(con, sprintf("
  SELECT *
  FROM read_csv_auto('%s')
  WHERE PERMIT_TYPE LIKE '%%esidentia%%' 
", permitPath))
dtP <- as.data.table(dfP)
print(summary(dtP))
dtP[,nunits:=as.numeric(DWELLING_UNITS_CREATED)]
print(summary(dtP[,nunits]))
dtP <- dtP[nunits<10 | is.na(nunits)]

# merge with address csv to get lat lon by GEO_ID
dtA <- fread("~/DropboxExternal/dataRaw/torontoAddressPoints.csv",select=c("ADDRESS_POINT_ID","geometry"))
setindex(dtP, GEO_ID)
setindex(dtA, ADDRESS_POINT_ID)
dtM <- dtP[dtA, on=c(GEO_ID="ADDRESS_POINT_ID"),nomatch=0]
print(dtM[is.na(geometry)])
print(nrow(dtP))
print(nrow(dtM))
# Note small loss
# print rows in dtP that don't exist in dtM
noMatch <- setdiff(dtP[,GEO_ID], dtM[,GEO_ID])
notHas <- dtP[GEO_ID %in% noMatch,.N,by=STRUCTURE_TYPE][order(-N)]
hasAddress <- dtM[,.N,by=STRUCTURE_TYPE][order(-N)]
setnames(hasAddress, "N", "nHasAddress")
hasAddress <- merge(hasAddress, notHas, by="STRUCTURE_TYPE")
hasAddress[,hasShare:=nHasAddress/(nHasAddress+N)]
print(hasAddress[order(-N)])

dtM[,saysPlex:=grepl("plex", PROPOSED_USE, ignore.case=TRUE)]
print(table(dtM[saysPlex==1,STRUCTURE_TYPE]))
print(table(dtM[saysPlex==1,PROPOSED_USE]))

# get top 10 and share of total for each of the above
print(dtP[, .N, by=PERMIT_TYPE][order(-N)][1:20])
print(dtP[, .N, by=STRUCTURE_TYPE][order(-N)][1:20])
print(dtP[, .N, by=PROPOSED_USE][order(-N)][1:20])
print(names(dtP))
print(dtP[,summary(APPLICATION_DATE)])
dtP[,appYear:=substring(APPLICATION_DATE,1,4)]
dtP <- dtP[appYear>2016]
for (y in 2016:2026) {
	print(y)
	print(dtP[appYear==y, .N, by=PERMIT_TYPE][order(-N)][1:20])
	print(dtP[appYear==y, .N, by=STRUCTURE_TYPE][order(-N)][1:20])
	print(dtP[appYear==y, .N, by=PROPOSED_USE][order(-N)][1:20])
}
# close connection

dbDisconnect(con)

# zoning
zoningPath <- path.expand("~/DropboxExternal/dataRaw/torontoZoningArea.zip")
dfZ <- st_read(paste0("/vsizip/", zoningPath, "/Zoning Area - 4326.shp"))
print(head(dfZ))



