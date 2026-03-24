# specCustom.R
# R to determine spec/custom nature of permits
# Tom Davidoff
# 03/22/26

library(data.table)
library(ggplot2)
library(xtable)

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
print(dtMerge[use!="laneway" & landWidth>=50 ,mean(use=="duplex"),by=c("east","year")])
print(dtMerge[use!="laneway" & year>2023,mean(use=="multi"),by=c("east","year")])
print(dtMerge[use!="laneway"   & year>2023 & landWidth>=50,mean(use=="multi"),by=c("east","year")])
print(dtMerge[use!="laneway"   & year>2023 & landWidth<=35,mean(use=="multi"),by=c("east","year")])
print(dtMerge[use!="laneway" ,mean(use=="single"),by=c("east","year")])
print(dtMerge[use!="laneway"   ,mean(use=="single"),by=c("east","year")])
print(dtMerge[use!="laneway"   ,mean(use=="single"),by=c("east","year")])
print(dtMerge[use=="single" & landWidth>=50,mean(distToLaneway<20,na.rm=TRUE),by=c("east","year")])
print(dtMerge[use=="single" & landWidth>=30 & landWidth<=35,mean(distToLaneway<20,na.rm=TRUE),by=c("east","year")])
# do the above with a stacked bar chart -- 2019-2025 on horizontal, each year two bars with share of single/duplex/multi, and then split by east/west
# but stacked: 31-35 width, and then 50+ width
dtMerge[,lotBin:=ifelse(landWidth>=50,"50+",ifelse(landWidth>=30 & landWidth<=35,"30-35","other"))]
p3 <- ggplot(dtMerge[lotBin=="30-35" & use!="laneway" & year>2017 ],aes(x=as.factor(year),fill=use)) + geom_bar(position="fill") + facet_wrap(~east) + theme_bw() + labs(x="Year",y="Share of Permits",fill="Use")
ggsave("text/permitSharesByYearAndLotWidth33.png",width=10,height=6)
p4 <- ggplot(dtMerge[lotBin=="50+" & use!="laneway" & year>2017 ],aes(x=as.factor(year),fill=use)) + geom_bar(position="fill") + facet_wrap(~east) + theme_bw() + labs(x="Year",y="Share of Permits",fill="Use")
ggsave("text/permitSharesByYearAndLotWidth50.png",width=10,height=6)
# predictions of what gets built
dtNeighbourhood33 <- dtMerge[use!="laneway" & landWidth>30 & landWidth<36 & year>2018 & year<2024,.(shareDuplex=mean(use=="duplex"),shareMulti=mean(use=="multi")),by=c("NEIGHBOURHOOD","year")]
dtPred <- fread("~/OneDrive - UBC/dataProcessed/ppsfElasticityPredictionsByNeighbourhood.csv")
dtNeighbourhood33 <- merge(dtNeighbourhood33,dtPred,by=c("NEIGHBOURHOOD","year"))
dtNeighbourhood33[,duplex_premium:=fittedDuplex-fittedSingle]
print(dtNeighbourhood33)
dtNeighbourhood33 <- dtNeighbourhood33[abs(elasticity)<1]
print(summary(dtNeighbourhood33))
# for the following 3 tables, make xtables for latex, can just print them
c1 <- cor(dtNeighbourhood33[,.(elasticity,intercept,duplex_premium,shareDuplex)])

c2 <- cor(dtNeighbourhood33[nobs>33,.(elasticity,intercept,duplex_premium,shareDuplex)])
c3 <- cor(dtNeighbourhood33[nobs>40,.(elasticity,intercept,duplex_premium,shareDuplex)])
dtNeighbourhood33[,duplexActual:=meanDuplex-meanSingle]
print(cor(dtNeighbourhood33[nobs>33 & !is.na(duplexActual),.(elasticity,duplex_premium,shareDuplex,duplexActual)]))
xtable(c1,digits=2)
xtable(c2,digits=2)
xtable(c3,digits=2)
ggplot(dtNeighbourhood33,aes(x=duplex_premium,y=shareDuplex)) + geom_point() 
ggsave("text/duplexPrediction.png",width=6,height=4)


