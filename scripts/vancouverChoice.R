# VancouverChoice.R
# R to analyze discrete choices of developers, maybe aggregate
# Tom Davidoff
# Feb 5, 2026

library(data.table)
library(ggplot2)
library(fixest)
library(geosphere)
library(nnet)
CUTDISTANCE <- 20 # laneay
MINDEPTH <- 100
MINWIDTH <- 30

# Choices data
dtChoice <- readRDS("~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")
print(head(dtChoice))
dtChoice[,year:=year(permitnumbercreateddate)]
dtChoice[,c("latitude","longitude"):=tstrsplit(geo_point_2d,","  )  ]
# Downtown is Vancouver's primary business district, houses many arts, entertainment, and sports venues, and is close to several vibrant residential communities.
# 49.2827° N, 123.1207° W per google
dtChoice[,latDowntown:=49.2827]
dtChoice[,lonDowntown:=-123.1207]
dtChoice[,latitude:=as.numeric(latitude)]
dtChoice[,longitude:=as.numeric(longitude)]
dtChoice[,distDowntown:=distGeo(dtChoice[,c("longitude","latitude")],dtChoice[,c("lonDowntown","latDowntown")])]
dtChoice[,landWidth:=as.numeric(landWidth)]
dtChoice[,landDepth:=as.numeric(landDepth)]
dtChoice[,landArea:=landWidth*landDepth  ]
dtChoice <- dtChoice[landWidth>=MINWIDTH & landWidth<=MINDEPTH]
dtChoice <- dtChoice[!is.na(landWidth)]
dtPriceCT <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_tract.csv",colClasses=c(CTUID="character"))
setnames(dtPriceCT,c("CTUID","priceCT","nobsCT","elasticityCT"))
dtChoice <- merge(dtChoice,dtPriceCT,by="CTUID")
dtChoice[,w33:=abs(landWidth-33)<=3]
dtChoice <- dtChoice[!is.na(landWidth)]
dtPriceND <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_neighbourhood.csv")
setnames(dtPriceND,c("neighbourhoodDescription","priceND","nobsND","elasticityND"))
a <- unique(dtChoice[,neighbourhoodDescription])
dtChoice <- merge(dtChoice,dtPriceND,by="neighbourhoodDescription")
b <- unique(dtChoice[,neighbourhoodDescription])
print(setdiff(a,b))

# Income by census tract
dtIncome <- fread("~/OneDrive - UBC/dataRaw/9810005801_databaseLoadingData.csv",
  select = c("GEO", "DGUID", "Household income statistics (6)", "VALUE")
)
dtIncome <- dtIncome[grepl("Vancouver", GEO)]
setnames(dtIncome, c("DGUID", "Household income statistics (6)", "VALUE"), c("DGUID", "incomeStat", "medianIncome"))
dtIncome <- dtIncome[incomeStat == "Median household total income (2020) (2020 constant dollars)"]
dtIncome[, tract := substr(DGUID, 13, 19)]

dtChoice[,tract:=substring(CTUID,4,10)]
a <- nrow(dtChoice)
dtChoice <- merge(dtChoice,dtIncome,by="tract")
print(c("LOSS OF OBS?",a - nrow(dtChoice)))
dtChoice <- dtChoice[use!="laneway"] # not laneway only
dtChoice <- dtChoice[!is.na(elasticityCT) & !is.na(elasticityND)]
dtChoice[,laneway:=use=="single" & distToLaneway<CUTDISTANCE]
dtChoice[,mergeZone:=tract] # ND or tract

dtSingle <-dtChoice[use=="single"]
print(cor(dtSingle[,.(laneway,medianIncome,elasticityCT,elasticityND,priceCT,priceND)]))
print(dtSingle[order(landWidth),mean(laneway),by=floor(landWidth)]) # not a ton of action
# toggle for lanewayResidual estimation level
rr <- feols(laneway ~ priceCT + w33 + landWidth +longitude + distDowntown|year  ,data=dtSingle)
print(resid(rr))
dtSingle[,eps:=resid(rr)]
dtLanewayResidual <- dtSingle[,.(residLaneway=mean(eps,na.rm=TRUE)),by="mergeZone"]
dtChoice <- merge(dtChoice,dtLanewayResidual,by="mergeZone")
print(dtLanewayResidual)
print(rr)
# NOTE HUGE R2
print(summary(feols(priceCT ~ distDowntown*longitude,data=dtChoice)))
print(feols(laneway ~ priceCT + w33+landWidth|year,data=dtSingle))
print(feols(use=="duplex" ~ priceCT+landWidth+elasticityND|year,data=dtChoice[year<2024],cluster="tract")) # laneway ambiguous -- substitution
print(feols(use=="duplex" ~ priceCT+landWidth+elasticityND + residLaneway + w33 + distDowntown+longitude|year,data=dtChoice[year<2024],cluster="tract")) # laneway ambiguous -- substitution
print(feols(use=="duplex" | use=="multiplex" ~ priceCT+residLaneway+landWidth+elasticityND|year,data=dtChoice),cluster="tract")
print(feols( use=="multi" ~ priceCT+residLaneway+landArea+elasticityND|year,data=dtChoice[use!="single"],cluster="tract"))
print(feols( use=="multi" ~ priceCT+residLaneway+w33+landArea+elasticityND|year,data=dtChoice[use!="single"],cluster="tract"))
print(feols( use=="multi" ~ priceCT+residLaneway+w33+landArea+elasticityND|year,data=dtChoice,cluster="tract"))
print(feols( use=="single" & laneway==0 ~ priceCT+w33+landArea+elasticityND|year,data=dtChoice,cluster="tract"))
print(feols( use=="single" & laneway==0 ~ priceCT+w33+landArea+elasticityND|year,data=dtChoice,cluster="tract"))
dtChoice[,discreteUse:=ifelse(use=="single"&laneway==0,"singleOnly",ifelse(use=="single"&laneway==1,"singleLaneway",ifelse(use=="duplex","duplex","multiplex")))]
print(summary(multinom(discreteUse ~ log(priceCT)+w33 + landArea+elasticityND+factor(year), data=dtChoice)))
# probit regression for when single==0
print(summary(feglm(use=="multi" ~ priceCT+landArea+w33+ factor(year),data=dtChoice[use!="single" & year>2023],cluster="tract",family=binomial(link="probit"))))
print(summary(feglm(use=="multi" ~ priceCT+residLaneway+landArea+w33+elasticityND + factor(year),data=dtChoice[use!="single" & year>2023],cluster="tract",family=binomial(link="probit"))))
print(summary(feglm(use=="multi" ~ priceCT+landArea+w33+ factor(year),data=dtChoice[use!="single" & year>2023],cluster="tract",family=binomial(link="logit"))))
print(summary(feglm(use=="multi" ~ priceCT+residLaneway+landArea+w33+elasticityND + factor(year),data=dtChoice[use!="single" & year>2023],cluster="tract",family=binomial(link="logit"))))
