# mergePermitAddress.Raddress,
# R to merge vancouver permit and bca address Data
# Tom Davidoff
# 01/07/26


# Data examples from permits
#[33782] "2251 COLLINGWOOD STREET, Vancouver, BC V6R 3L1"
#[33783] "459 E 49TH AVENUE, Vancouver, BC V5W 2G8"

#BCA address 
#folioID,addressID,primaryFlag,streetNumber,unitNumber,streetDirectionPrefix,streetName,streetDirectionSuffix,streetType,city,province,postalCode,mapReferenceNumber
#"A000027PMT","A000004S6Z","true","425","","","11TH","E","ST","NORTH VANCOUVER","BC","V7L 2H3",""
#"A000027PMU","A000004S70","true","429","","","11TH","E","ST","NORTH VANCOUVER","BC","V7L 2H3",""
library(data.table)
library(stringr)
library(jsonlite)
library(sf)
# prepare hugedata if needed
ingpkg  <- "/Volumes/T7Office/bigFiles/bca_folios.gpkg"
layer   <- "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_ADDRESSES_SV"
outgpkg <- path.expand("~/OneDrive - UBC/dataProcessed/vancouverFolio2022.gpkg")

if (!file.exists( outgpkg)) { 
  sql <- paste0(
    "SELECT FOLIO_ID, ROLL_NUMBER, BCA_FA_SYSID, geom ",
    "FROM ", layer, " ",
    "WHERE CITY = 'VANCOUVER'"
  )

  x_van <- st_read(ingpkg, query = sql, quiet = TRUE)

  st_write(x_van, outgpkg, delete_dsn = TRUE, quiet = TRUE)
}

geo2022 <-  st_read(outgpkg)
print(head(geo2022))    

# Make nice commands that give file names after directories for the below
dirRaw <- "~/OneDrive - UBC/dataRaw"
dirBCA <- "~/OneDrive - UBC/Documents/data/bca"
f_parc  <- file.path(dirRaw, "property-parcel-polygons.geojson")
fPermit <- file.path(dirRaw, "vancouver_permits_full.csv")
fBCA <- file.path(dirBCA,"REVD18_and_inventory_extracts_CSV_files","address.csv")
fDescription <- file.path(dirBCA,"REVD18_and_inventory_extracts_CSV_files","folioDescription.csv")
dtBCA <- fread(fBCA)[city=="VANCOUVER"]
dtDescription <- fread(fDescription,select=c("folioID","actualUseDescription"))
dtBCA <- merge(dtBCA,dtDescription,by="folioID",all.x=TRUE)
dtBCA <- dtBCA[actualUseDescription=="Residential Dwelling with Suite" | actualUseDescription=="Single Family Dwelling"]
print(table(dtBCA$actualUseDescription))
dtPermit <- fread(fPermit,select=c("permitnumber","address","projectvalue","typeofwork","specificusecategory","permitcategory"))
dtPermit <- dtPermit[permitcategory %in% c("New Build - Low Density Housing", "New Build - Standalone Laneway", "Renovation - Residential - Lower Complexity")]
print(table(dtPermit[,specificusecategory]))
dtBCA[,address:= str_trim(paste(streetNumber,streetDirectionPrefix,streetDirectionSuffix,streetName,streetType,city,province))]
dtBCA[,address:=x <- gsub("\\s+", " ", address)]
# need to expand abbreviations in permit addresses to match BCA, or vice versa
dtBCA[,address:= str_trim(toupper(address))]
dtBCA[,address:=gsub(" ST "," STREET ",address)]
dtBCA[,address:=gsub(" AVE "," AVENUE ",address)]
dtBCA[,address:=gsub(" RD "," ROAD ",address)]
dtBCA[,address:=gsub(" DR "," DRIVE ",address)]
dtBCA[,address:=gsub(" BLVD "," BOULEVARD ",address)]
dtBCA[,address:=gsub(" CT "," COURT ",address)]
dtBCA[,address:=gsub(" PL "," PLACE ",address)]
dtBCA[,address:=gsub(" HWY "," HIGHWAY ",address)]
dtBCA[,address:=gsub(" VANCOUVER BC",", VANCOUVER, BC",address)]
# stop the string at "Vancouver, BC" to remove postal codes
dtPermit[,addressOrig:=address]
dtPermit[,address:= str_trim(toupper(address))]
dtPermit[,address:=sub("(VANCOUVER, BC).*","\\1",address)] 

print(head(dtPermit))

print(head(dtBCA))
dtMerge <- merge(dtPermit,dtBCA,by="address",all.x=TRUE)
print(head(dtMerge[is.na(folioID)]))
print(table(dtMerge[is.na(folioID)]$addressOrig))
print(table(is.na(dtMerge$folioID),is.na(dtMerge$permitnumber)))
print(table(dtMerge[is.na(folioID),permitcategory]))
print(table(dtMerge[!is.na(folioID),permitcategory]))
# NB NOT PERFECT, but pretty good -- resi dominated in yes match
fwrite(dtMerge[is.na(folioID)],"~/Downloads/nono.csv")

# Next, try matching on 2025 floor(rollnumber/1000)
dtNoMerge <- dtMerge[is.na(folioID) & !is.na(permitnumber),.(permitnumber,addressOrig,address,projectvalue,typeofwork,specificusecategory,permitcategory)]
print(head(dtNoMerge))
#dtBCA2025
# plan: merge on floor(rollnumber/1000), 
dtBCA2025 <- fread(file.path(dirBCA,"data_advice_REVD25_20250331","bca_folio_addresses_20250331_REVD25.csv"))
print(head(dtBCA2025))
# Merge on address as previously  
dtBCA2025[,address:= str_trim(paste(STREET_NUMBER,STREET_DIRECTION_PREFIX,STREET_DIRECTION_SUFFIX,STREET_NAME,STREET_TYPE,CITY,PROVINCE))]
print(head(dtBCA2025))
# tweak address to match previous format
dtBCA2025[,address:= str_trim(toupper(address))]
dtBCA2025[,address:=gsub(" ST "," STREET ",address)]
dtBCA2025[,address:=gsub(" AVE "," AVENUE ",address)]
dtBCA2025[,address:=gsub(" RD "," ROAD ",address)]
dtBCA2025[,address:=gsub(" DR "," DRIVE ",address)]
dtBCA2025[,address:=gsub(" BLVD "," BOULEVARD ",address)]
dtBCA2025[,address:=gsub(" CT "," COURT ",address)]
dtBCA2025[,address:=gsub(" PL "," PLACE ",address)]
dtBCA2025[,address:=gsub(" HWY "," HIGHWAY ",address)]
dtBCA2025[,address:=gsub(" VANCOUVER BC",", VANCOUVER, BC",address)]
dtBCA2025[,address:=gsub("\\s+", " ", address)]
dtBCA2025[,address:=sub("(VANCOUVER, BC).*","\\1",address)] 
dtBCA2025[,address:= str_trim(toupper(address))]

dtMerge2 <- merge(dtNoMerge,dtBCA2025,by="address",all.x=TRUE)
print(table(is.na(dtMerge2$FOLIO_ID),is.na(dtMerge2$permitnumber)))
print(head(dtMerge2))
print(table(dtMerge2[is.na(folioID),permitcategory]))
print(table(dtMerge2[!is.na(folioID),permitcategory]))


parc_json <- fromJSON(f_parc, simplifyDataFrame = TRUE)
parc <- as.data.table(parc_json$features$properties)
print(head(parc))
print(length(unique(parc[,site_id])))
print(length(unique(parc[,paste0(geo_point_2d.lat,geo_point_2d.lon)])))
print(nrow(parc))
print(parc[civic_number=="1305" & streetname=="MAPLE ST"])
print(parc[civic_number=="2020" & streetname=="CREELMAN AV"])

q("no")
