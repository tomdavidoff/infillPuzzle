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
dtPermits <- fread(file.path(directory,f_permit),select=c("permitnumber","address"))
# Read parcel data
parcels <- st_read(file.path(directory,f_parcel)) # is thjis best way to read geojson?
dtParcels <- as.data.table(parcels) # convert to data.table for easier manipulation
print(head(dtParcels))
#   civic_number     streetname tax_coord   site_id
#         <char>         <char>    <char>    <char>
# 1:         4875     HEATHER ST  73015907 032529686
print(head(dtPermits))
    # addres: 126 W 3RD AVENUE, Vancouver, BC # permits

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
print(head(dtMerge))
print(head(dtMerge[is.na(site_id)])) # inspect permits that did not find a matching parcel
print(head(dtMerge[!is.na(site_id)])) # inspect permits that found a matching parcel
N <- 200
# print N rows of randomly selected failed merge addresses for manual inspection
set.seed(123)
print(sample(dtMerge[is.na(site_id)]$address,N))

# see if something systematic about last word of missing addresses,e.g. street type
failedAddresses <- dtMerge[is.na(site_id) & address!="",.(address)]
print(head(failedAddresses))
failedAddresses[,lastWord:=sapply(strsplit(failedAddresses," "),"[",lengths(strsplit(failedAddresses," ")))]
print(table(failedAddresses$lastWord))

# take that sample and somehow find closest matches in parcel data?
# perhaps using stringdist package?
# Too slow for large data, but ok for small sample
# Not super useful 
if (0) {
	library(stringdist)
	failed_addresses <- dtMerge[is.na(site_id) & address!="",address][1:N]
	matched_site_ids <- sapply(failed_addresses, function(addr) {
	  distances <- stringdist(addr, dtParcels$address, method = "jw")
	  closest_index <- which.min(distances)
	  return(dtParcels$site_id[closest_index])
	})
	# Create a data.table of failed addresses, their initial failed address and the associated  matched address from the parcel data
	dtMatched <- data.table(
	  failed_address = failed_addresses,
	  matched_site_id = matched_site_ids
	)
	print(dtMatched) # but I need to see the parcel info too
	dtMatched <- merge(dtMatched, dtParcels[, .(site_id, address)], by.x = "matched_site_id", by.y = "site_id", all.x = TRUE)
	print(dtMatched)
}
