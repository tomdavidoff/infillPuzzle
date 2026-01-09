# mergePermitAddress.Raddress,
# R to merge vancouver permit and bca address Data
# Tom Davidoff
# 01/07/26

library(data.table)
library(stringr)
library(jsonlite)
library(sf)

# prepare huge BC 2022 FOLIO -> Address data if needed
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

# STEP: Merge as many 2019-2025 permits to 2018 single family as feasible
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
print(names(dtMerge)) # record for subsequent rbind

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
dtNoMerge2 <- dtMerge2[is.na(FOLIO_ID) & !is.na(permitnumber)]
print(table(is.na(dtMerge2$FOLIO_ID),is.na(dtMerge2$permitnumber)))
print(head(dtMerge2))
print(table(dtMerge2[is.na(FOLIO_ID),permitcategory]))
print(table(dtMerge2[!is.na(FOLIO_ID),permitcategory]))
# note only useful if joined to a 2018 house
dtBCA2025 <- merge(dtMerge2[!is.na(FOLIO_ID)],dtBCA,by.x="FOLIO_ID",by.y="folioID",all.x=TRUE,suffixes=c("_2025","_2018"))


# Now geography stuff
# Get centroids of permit addresses by joining address with city parcel polygons
parc_json <- fromJSON(f_parc, simplifyDataFrame = TRUE)
parc <- as.data.table(parc_json$features$properties)
print(head(parc))
print(length(unique(parc[,site_id])))
print(length(unique(parc[,paste0(geo_point_2d.lat,geo_point_2d.lon)])))
print(nrow(parc))
#civic_number     streetname tax_coord   site_id geo_point_2d.lon
#           <char>         <char>    <char>    <char>            <num>
#  1:         4875     HEATHER ST  73015907 032529686        -123.1221             2:          375      E 33RD AV  73019575  EPS10489        -123.0975

parc[,address:= str_trim(toupper(paste(civic_number,streetname)))]
parc[,address:=gsub(" ST "," STREET ",address)]
parc[,address:=gsub(" AVE "," AVENUE ",address)]
parc[,address:=gsub(" RD "," ROAD ",address)]
parc[,address:=gsub(" DR "," DRIVE ",address)]
parc[,address:=gsub(" BLVD "," BOULEVARD ",address)]
parc[,address:=gsub(" CT "," COURT ",address)]
parc[,address:=gsub(" PL "," PLACE ",address)]
parc[,address:=gsub(" HWY "," HIGHWAY ",address)]
dtPermit[,addressParc:= str_trim(toupper(sub(", VANCOUVER, BC.*","",address)))]

dtPermitAddress <- merge(dtPermit,parc,by.x="addressParc",by.y="address",all.x=TRUE)
print(head(dtPermit))
print(head(dtParc))
print(table(is.na(dtPermitAddress$site_id)))

q("no")


# Now merge on centroids from 2018
# we have a st dataframe geo2022
# we want to merge based on closest centroid, with some tolerance, say 30 ft 10Meters max, so be strict at 5m
geo2022$centroid <- st_centroid(geo2022$geom)
geo2022_centroids <- st_coordinates(geo2022$centroid)
geo2022_dt <- as.data.table(geo2022)
geo2022_dt[,geo_point_2d.lon:=geo2022_centroids[,1]]
geo2022_dt[,geo_point_2d.lat:=geo2022_centroids[,2]]
print(head(geo2022_dt))
# Now find min distance for each of the no merge 2
print(names(dtNoMerge2))
q("no")
dtNoMerge2_sf <- st_as_sf(dtNoMerge2, coords = c("geo_point_2d.lon", "geo_point_2d.lat"), crs = 4326, remove=FALSE)
dtNoMerge2_sf <- st_transform(dtNoMerge2_sf, st_crs(geo2022))
dist_matrix <- st_distance(dtNoMerge2_sf, geo2022)
min_dist_indices <- apply(dist_matrix, 1, which.min)
min_dist_values <- apply(dist_matrix, 1, min)
tolerance_meters <- 5
dtNoMerge2[,min_dist_meters:=as.numeric(min_dist_values)]
dtNoMerge2[,closest_folioID:=geo2022_dt$FOLIO_ID[min_dist_indices]]
dtNoMerge2_matched <- dtNoMerge2[min_dist_meters <= tolerance_meters]
print(head(dtNoMerge2_matched))


q("no")
