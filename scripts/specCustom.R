# specCustom.R
# R to determine spec/custom nature of permits
# Tom Davidoff
# 03/22/26

library(data.table)

dtChoice <- readRDS("~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")
print(summary(dtChoice))
dtChoice[,year:=year(permitnumbercreateddate)]
dtChoice <- dtChoice[year >= 2018 ] # so finished

dtSales <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",select=c("FOLIO_ID","CONVEYANCE_DATE"))
print(head(dtSales))

dtDesc<- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("FOLIO_ID","ROLL_NUMBER","NEIGHBOURHOOD"))

dtSales <- merge(dtSales,dtDesc,by="FOLIO_ID",all.x=TRUE)
dtSales[,roundRoll:=floor(as.numeric(ROLL_NUMBER)/1000)]

dtChoice[,roundRoll:=floor(as.numeric(ROLL_NUMBER)/1000)]

dtMerge <- merge(dtChoice,dtSales,by="roundRoll",all.x=TRUE)
dtMerge[,saleDate:=as.Date(substring(CONVEYANCE_DATE,1,8),format="%Y%m%d")]
dtMerge[,lastSale:=max(saleDate,na.rm=TRUE),by="roundRoll"]
print(summary(dtMerge))
print(table(dtMerge[,use]))
dtMerge[is.na(lastSale),lastSale:=as.Date("1900-01-01")]
dtMerge[,hasPost:= lastSale > permitnumbercreateddate]
print(summary(dtMerge[,lastSale]))
print(dtMerge[,table(use,hasPost)])
print(dtMerge[year < 2022,table(use,hasPost)])
print(dtMerge[year < 2021,table(use,hasPost)])
print(dtMerge[,table(NEIGHBOURHOOD)])
print(dtMerge[year < 2022 & NEIGHBOURHOOD=="Dunbar",table(use,hasPost)])
print(dtMerge[year < 2022 & NEIGHBOURHOOD=="Kitsilano",table(use,hasPost)])
print(dtMerge[year < 2022 & NEIGHBOURHOOD=="Killarney",table(use,hasPost)])
print(head(dtMerge[,geo_point_2d]))
dtMerge[,c("lat","lon") := tstrsplit(geo_point_2d, ",", fixed=TRUE)]
dtMerge[,lon:=as.numeric(lon)]
dtMerge[,east:=lon > median(lon,na.rm=TRUE)]
print(dtMerge[year < 2022 & east==TRUE,table(use,hasPost)])
print(dtMerge[year < 2022 & east==FALSE,table(use,hasPost)])
print(dtMerge[use=="single",quantile(distToLaneway,na.rm=TRUE),by=east])
print(dtMerge[use=="single",mean(distToLaneway<20,na.rm=TRUE),by=east])
print(summary(dtMerge[,lon]))
print(dtMerge[use=="single",mean(distToLaneway<20,na.rm=TRUE),by=c("east","year")])
print(dtMerge[use!="laneway",mean(use=="duplex"),by=c("east","year")])
print(dtMerge[use!="laneway" & landWidth>30 & landWidth<36,mean(use=="duplex"),by=c("east","year")])
print(dtMerge[use!="laneway" & landWidth>40 ,mean(use=="duplex"),by=c("east","year")])
print(dtMerge[use!="laneway",mean(use=="multi"),by=c("east","year")])
print(dtMerge[use!="laneway" & landWidth>=40,mean(use=="multi"),by=c("east","year")])
print(dtMerge[use=="single" & landWidth>=40,mean(distToLaneway<20,na.rm=TRUE),by=c("east","year")])
print(dtMerge[use=="single" & landWidth>=30 & landWidth<=35,mean(distToLaneway<20,na.rm=TRUE),by=c("east","year")])
