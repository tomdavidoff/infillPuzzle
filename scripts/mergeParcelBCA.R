# mergeParcelBCA.R merge parcel vancouver data with BC assessment data
# Tom Davidoff 
# 01/11/26

library(data.table)
library(RSQLite)
library(sf)

# read BCA data from 201901, pre-duplex zoning benchmark, when avaialble. Proof of concept with 201..

bcaDir <- "~/OneDrive - UBC/Documents/data/bca/REVD18_and_inventory_extracts_CSV_files"
dtAddress <- fread(file.path(bcaDir,"address.csv"))
dtAddress <- dtAddress[city=="VANCOUVER"]
dtDescription <- fread(file.path(bcaDir,"folioDescription.csv"),select=c("folioID","actualUseDescription","landDimensionType","landDimensionTypeDescription","landDimension","landWidth","landDepth"))
dtAddress <- merge(dtAddress,dtDescription,by="folioID")
print(table(dtAddress[,actualUseDescription])[order(-table(dtAddress[,actualUseDescription]))])
dtAddress <- dtAddress[actualUseDescription %in% c("Single Family Dwelling","Residential Dwelling with Suite")]
print(head(dtAddress))
parcelDir <- "~/OneDrive - UBC/dataRaw"
parcels <- st_read(file.path(parcelDir,"property-parcel-polygons.geojson")) # is this best way to read geojson?
dtParcels <- as.data.table(parcels) # convert to data.table for easier manipulation
print(head(dtParcels))
dtAddress[,streetType:=gsub("AVE","AV",streetType)]
# missing street types
#        AV  BOULEVARD  BRIDGEWAY   BROADWAY     CIRCLE      CLOSE  CONNECTOR 
#      1666         28          1        138         11          5         11 
#     COURT       COVE   CRESCENT   CROSSING  DIVERSION      DRIVE       FOOT 
#        20          1        931         10         22       4870          1 
#GREENCHAIN    HIGHWAY   KINGSWAY    LANDING       LANE       MEWS         NA 
#         1        109         47          2          4          9       1163 
#     NORTH     PARADE      PLACE       ROAD      SOUTH     SQUARE         ST 
#       127          2        357        462         81         10       1815 
#      WALK        WAY 
#         3          6 
# 
# vers street types in BCA
#        ALLEY     AV   BLVD CAMPUS    CIR  CLOSE   COVE   CRES  CROSS    CRT 
#  7439     47  89581   1202      2     11     16      8   3176     10    184 
#DIVERS     DR  GREEN   HILL    HWY  INLET ISLAND LANDNG   LANE   MALL   MEWS 
#    45  10482      1     36    188      1      2     11    849    719   1628 
#PARADE    PKY     PL   QUAY     RD    ROW     SQ     ST  VISTA   WALK    WAY 
#   456    170   3493     29   2894      3    581 103323      1    280    950 
#  WYND 
#    38 
dtAddress[,streetType:=gsub("CRESCENT","CRES",streetType)]
dtAddress[,streetType:=gsub("BOULEVARD","BLVD",streetType)]
dtAddress[,streetType:=gsub("DR","DRIVE",streetType)]
dtAddress[,streetType:=gsub("PLACE","PL",streetType)]

dtAddress[,address:=toupper(paste0(streetNumber," ",streetDirectionPrefix," ",streetDirectionSuffix," ",streetName," ",streetType," "))]
dtAddress[unitNumber!="",address:=toupper(paste0(address," #",unitNumber))]
dtParcels[,address:=toupper(paste0(civic_number," ",streetname))]
dtParcels[,address:=toupper(paste(civic_number,streetname))]
dtParcels[,address:=gsub("  "," ",address)] # remove double spaces if any
dtAddress[,address:=gsub("   "," ",address)] # remove double spaces if any
dtAddress[,address:=gsub("  "," ",address)] # remove double spaces if any
# trim terminal space in dtAddress
dtAddress[,address:=gsub(" $","",address)]
print(head(dtAddress$address))
print(head(dtParcels$address))
print(table(dtAddress[,streetType]))
dtMerge <- merge(dtParcels,dtAddress,by="address",all.x=TRUE,all.y=TRUE)
print(table(is.na(dtMerge[,.(primaryFlag)])))#,is.na(dtMerge[,.(site_id)])))
print(dtMerge[is.na(primaryFlag)])
print(dtMerge[is.na(primaryFlag),address][1:100])
dtMerge[,lastWord:=sub(".* (.*)$","\\1",address)]
print(table(dtMerge[is.na(primaryFlag),lastWord]))
print(mean(is.na(dtMerge[,site_id]))) # missing appx 10% of single family lots in the parcel data
print(names(dtMerge)) # need folioID and address, and land area stuff
print(head(dtMerge))

q("no")
