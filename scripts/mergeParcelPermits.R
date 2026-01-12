# mergeParcelPermits.R
# R code to do the discrete task: Merge parcel data with permit data based on addresses.

# Load necessary libraries
library(data.table)
library(sf)

# Read headers
directory <- "~/OneDrive - UBC/dataRaw"
f_permit <- "vancouver_permits_full.csv"
f_parcel <- "property-parcel-polygons.geojson"

# Read permit data
dtPermits <- fread(file.path(directory,f_permit))#,select=c("permitnumber","address","))
# Read parcel data
parcels <- st_read(file.path(directory,f_parcel)) # is thjis best way to read geojson?
dtParcels <- as.data.table(parcels) # convert to data.table for easier manipulation
dtPermits[,grouper:=paste0(yearmonth,applicant)]
# convert geo_point_2d to lon, lat and then compute a simple within group distance
dtPermits[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]
dtPermits[, groupDistance := { lon_adj <- cos(mean(lat) * pi/180) 
          sqrt(var(lat) + var(lon * lon_adj)) * 111000
        }, by = grouper] # meters
print(summary(dtPermits[,groupDistance]))
print(length(unique(dtPermits[,grouper])))
dtPermits[,nGroup:=.N,by=grouper]
dtPermits[,group2:=grouper]
dtPermits[groupDistance>25,group2:=paste0(grouper,runif(length(grouper)))]
print(length(unique(dtPermits[,grouper])))
print(nrow(dtPermits))

dtParcels[,address:=paste(civic_number,streetname)]
dtParcels[,address:=toupper(address)] # make uppercase to match permit data
# How to these subsequent shifts recognizing we dont want "STREET" -> "STREETEET"? But also recognizing "ST" often ends word, so gsub(" ST "," STREET ") wont catch those
# What's below for parcels is good, but let's make a nice data.table or something and apply to both parcels and permits. Maybe a function? Your call
streetRename <- data.table(
  short = c(" ST"," AVE"," AV"," RD"," DR"," BLVD"," CRES"," HWY"," PL"," PLZ"),
  long = c(" STREET"," AVENUE"," AVENUE"," ROAD"," DRIVE"," BOULEVARD"," CRESCENT"," HIGHWAY"," PLACE"," PLAZA")
)
# Apply renaming to parcels
for (i in 1:nrow(streetRename)) {
  short <- streetRename$short[i]
  long <- streetRename$long[i]
  dtParcels[,address:=gsub(paste0(short,"$"), long, address)] # standardize street type at end of string
  dtParcels[,address:=gsub(paste0(short," "), paste0(long," "), address)] # standardize street type
  dtPermits[,address:=gsub(paste0(short,"$"), long, address)] # standardize street type at end of string
  dtPermits[,address:=gsub(paste0(short," "), paste0(long," "), address)] # standardize street type
}
dtPermits[,address:=gsub(", Vancouver, BC","",address)] # remove city and province
# delete last 7 characters (postal code) if present
# detect a postal code of form " V#V #V#" at end of string
# This failed dtPermits[,address:=gsub(" V[0-9][A-Z][0-9] [0-9][A-Z][0-9]$","",address)]
dtPermits[,address:=gsub(" V[0-9][A-Z] [0-9][A-Z][0-9]$","",address)]


print(head(dtParcels))
print("PP")
print(head(dtPermits))
dtMerge <- merge(dtParcels,dtPermits,by="address",all.y=TRUE)
print(table(is.na(dtMerge$site_id))) # check how many permits did not find a matching parcel
dtMerge[,maxMerge:=max(!is.na(site_id)),by=grouper]
print(table(dtMerge$maxMerge)) # check how many permit groups had at least one successful merge
print(dtMerge[maxMerge==0,mean(propertyuse=="Dwelling Uses",na.rm=TRUE)]) # fraction of permits in groups with no successful merges that are dwelling uses
print(dtMerge[maxMerge==1,mean(propertyuse=="Dwelling Uses",na.rm=TRUE)]) # fraction of permits in groups with no successful merges that are dwelling uses
print(names(dtMerge))
print(table(dtMerge[maxMerge==0 & propertyuse=="Dwelling Uses",specificusecategory]))
print(table(dtMerge[maxMerge==1 & propertyuse=="Dwelling Uses",specificusecategory]))
print(table(dtMerge[maxMerge==0 & propertyuse=="Dwelling Uses",mean(grepl("uplex",specificusecategory))]))
print(table(dtMerge[maxMerge==1 & propertyuse=="Dwelling Uses",mean(grepl("uplex",specificusecategory))]))
print(table(dtMerge[maxMerge==0 & propertyuse=="Dwelling Uses",mean(grepl("Single Detached",specificusecategory))]))
print(table(dtMerge[maxMerge==1 & propertyuse=="Dwelling Uses",mean(grepl("Single Detached",specificusecategory))]))
# Note: looks like no selection!
q("no")
