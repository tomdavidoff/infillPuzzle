# mergePermitsSpatial.R
# merge permit data with single family lots as of 2019 BCA (2018 assessment) and then census tracts
# Tom Davidoff
# 01/16/26

library(data.table)
library(sf)

dtBCA <- readRDS("~/OneDrive - UBC/dataProcessed/bca_vancouver_residential.rds")
 #[1] "permitnumber"              "permitnumbercreateddate"  [3] "issuedate"                 "permitelapseddays"        [5] "projectvalue"              "typeofwork"               [7] "address"                   "projectdescription"       [9] "permitcategory"            "applicant"                [11] "applicantaddress"          "propertyuse"              [13] "specificusecategory"       "buildingcontractor"       [15] "buildingcontractoraddress" "issueyear"                [17] "geolocalarea"              "geom"                     [19] "yearmonth"                 "geo_point_2d"             
dtP <- fread("~/OneDrive - UBC/dataRaw/vancouver_permits_full.csv",select=c("geom","geo_point_2d","permitnumber","permitnumbercreateddate","applicant","typeofwork","projectvalue","specificusecategory","address"))
# check typofwork and value
print(dtP[,median(projectvalue,na.rm=TRUE),by=typeofwork])
dtP <- dtP[typeofwork %in% c("Addition / Alteration","New Building")]
dtP[,yearCreated:=year(permitnumbercreateddate)]
MINYEAR <- 2017 # might want to see jump in duplex
dtP <- dtP[yearCreated>=MINYEAR]
x <- table(dtP[,specificusecategory])
print(sort(x,decreasing=TRUE))
dtP <- dtP[specificusecategory %in% c("Single Detached House","Laneway House","Single Detached House w/ Sec Suite","Multiple Dwelling","Duplex","Duplex w/Secondary Suite")]
print(quantile(dtP[typeofwork=="Addition / Alteration",projectvalue]))
print(quantile(dtP[typeofwork=="New Building",projectvalue]))
MINVAL <- 250000 #quantile(dtP[typeofwork=="New Building",projectvalue],.10) # only 
print(MINVAL)
dtP <- dtP[projectvalue>=MINVAL]
print(head(dtP[,.(geo_point_2d)]))
print(head(dtBCA[,.(geom)]))
# 1. Parse the point string and create sf geometry
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]
dtP <- dtP[!is.na(lat)]
sfP <- st_as_sf(dtP, coords = c("lon", "lat"), crs = 4326)

# 2. Transform to match BCA's CRS (check with st_crs(dtBCA))
sfBCA <- st_as_sf(dtBCA)
sfP <- st_transform(sfP, st_crs(sfBCA)) #st_crs(dtBCA)) from ogrinfo of original BCA .gpkg
print(st_crs(sfBCA))


# 3. Spatial join (points within polygons)
merged <- st_join(sfP, sfBCA, join = st_within)
print(head(merged))
print(nrow(sfP))
print(nrow(dtBCA))
print(nrow(sfP))

# now get census tracts
fCT <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
dCT <- st_read(fCT)
print(head(dCT))
# swap crs of census tracts to that of merged)
print(st_crs(dCT))
print(st_crs(merged))
dCT <- st_transform(dCT,st_crs(merged))

mergedCT <- st_join(merged,dCT,join=st_within)
print(head(mergedCT))

##
dtSpatial <- as.data.table(mergedCT)
dtSpatial[,matched:=!is.na(zoning)]
dtSpatial[,nLot:=.N,by=ROLL_NUMBER]
dtSpatial[,duplex:=grepl("uplex",specificusecategory)]
dtSpatial[,laneway:=grepl("aneway",specificusecategory)]
dtSpatial[,multi:=grepl("ultiple",specificusecategory)]
dtSpatial[,maxMulti:=max(multi),by=ROLL_NUMBER]
dtSpatial[,maxDuplex:=max(duplex),by=ROLL_NUMBER]
dtSpatial[,maxLaneway:=max(laneway),by=ROLL_NUMBER]
dtSpatial[,use:=fifelse(maxMulti==1,"multi",
                           fifelse(maxDuplex==1,"duplex",
                                   fifelse(maxLaneway==1,"laneway","single")))]
# only one obs per lot
dtSpatial[,uniqueLot:=.N==1,by=ROLL_NUMBER]
dtSpatial <- dtSpatial[uniqueLot==TRUE]
print(table(dtSpatial[matched==1,nLot]))
print(table(dtSpatial[matched==0,specificusecategory]))
print(table(dtSpatial[matched==1,specificusecategory,by=nLot]))
print(table(dtSpatial[matched==1,specificusecategory]))
dtSpatial[,matcha:=!is.na(CTUID)]
print(table(dtSpatial[,matcha]))
print(names(dtSpatial))
# [1] "geom"                     "geo_point_2d"            
# [3] "permitnumber"             "permitnumbercreateddate" 
# [5] "applicant"                "typeofwork"              
# [7] "projectvalue"             "specificusecategory"     
# [9] "address"                  "yearCreated"             
#[11] "rollStart"                "folioID"                 
#[13] "rollNumber"               "zoning"                  
#[15] "MB_effective_year"        "MB_total_finished_area"  
#[17] "actualUseDescription"     "neighbourhoodDescription"
#[19] "ROLL_NUMBER"              "Nlot"                    
#[21] "CTUID"                    "DGUID"                   
#[23] "CTNAME"                   "LANDAREA"                
#[25] "PRUID"                    "geometry"                
#[27] "matched"                  "nLot"                    
#[29] "duplex"                   "laneway"                 
#[31] "multi"                    "maxMulti"                
#[33] "maxDuplex"                "maxLaneway"              
#[35] "use"                      "uniqueLot"               
#[37] "matcha"                  
dtSpatial <- dtSpatial[matched==1,.(CTUID,ROLL_NUMBER,folioID,permitnumbercreateddate,use,MB_effective_year,MB_total_finished_area,neighbourhoodDescription,address,LANDAREA,landWidth,landDepth,geo_point_2d)]
outfile <- "~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds"
saveRDS(dtSpatial,outfile)


