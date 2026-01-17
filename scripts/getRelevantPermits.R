# getRelevantPermits.R
# R to get the relevant permits
# Tom Davidoff
# 01/16/26

library(data.table)
 #[1] "permitnumber"              "permitnumbercreateddate"  [3] "issuedate"                 "permitelapseddays"        [5] "projectvalue"              "typeofwork"               [7] "address"                   "projectdescription"       [9] "permitcategory"            "applicant"                [11] "applicantaddress"          "propertyuse"              [13] "specificusecategory"       "buildingcontractor"       [15] "buildingcontractoraddress" "issueyear"                [17] "geolocalarea"              "geom"                     [19] "yearmonth"                 "geo_point_2d"             
dtP <- fread("~/OneDrive - UBC/dataRaw/vancouver_permits_full.csv",select=c("geom","geo_point_2d","permitnumber","permitnumbercreateddate","applicant","typeofwork","projectvalue","specificusecategory"))
print(head(dtP))
print(table(dtP[,typeofwork]))
print(quantile(dtP[,projectvalue]))
dtP <- dtP[typeofwork %in% c("Addition / Alteration","New Building")]
dtP[,yearMoCreated:=substr(permitnumbercreateddate,1,7)]
dtP[,group:=paste0(applicant,yearMoCreated)]
# order table by frequency descending
  print(table(dtP[,specificusecategory])[order(-table(dtP[,specificusecategory]))])
dtP <- dtP[specificusecategory %in% c("Single Detached House","Laneway House","Single Detached House w/ Sec Suite","Multiple Dwelling","Duplex","Duplex w/Secondary Suite","address")]
print(quantile(dtP[typeofwork=="Addition / Alteration",projectvalue]))
MINVAL <- 125000 # appx 75th percentile for Addition / Alteration
dtP <- dtP[projectvalue>=MINVAL]
print(nrow(dtP))
print(length(unique(dtP[,group])))
# find the centroid of each group. Use 2d geopoint
dtP[,c("lon","lat"):=tstrsplit(geo_point_2d,",")]
dtP[,lon:=as.numeric(lon)]
dtP[,lat:=as.numeric(lat)]
dtP[,medLon:=median(lon,na.rm=TRUE),by=group]
dtP[,medLat:=median(lat,na.rm=TRUE),by=group]
dtP[,distanceToMed:=sqrt((lat-medLat)^2+cos(medLat*pi/180)^2*(lon-medLon)^2)*111000] # rough conversion to meters
print(quantile(dtP$distanceToMed,na.rm=TRUE))
dtP[distanceToMed>15,group2:=paste0(group,permitnumber)]
print(nrow(dtP[is.na(group2)]))
print(length(unique(dtP[is.na(group2),group2])))
dtP[,nGroup2:=.N,by=group2]
print(dtP[nGroup2>1])
