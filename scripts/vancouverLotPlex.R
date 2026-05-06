# vancouverLotPlex.R
# R code to see if 33 vs 50 price gap explains duplex takeup
# And correlation with price per square foot
# And Conley stuff
# Tom Davidoff 
# 05/05/26

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

# Now get sales, etc from BC Assessment

