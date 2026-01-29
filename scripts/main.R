# main.R
# main analysis script
# Tom Davidoff
# 01/16/26

library(data.table)
library(ggplot2)
library(fixest)

# Vancouver analysis # note neighbourhoodDescription better than tract for analysis level
# get slopes and mean values from BCA sales analysis by census tract merge
nameSlope <- "tables/bca19_mean_ppsf_slope_All_by_neighbourhood.csv"
if (!file.exists(nameSlope)) {
  source("scripts/bcaSalesAll.R")
}
# specify column CTUID is a number
dtSlope <- fread(nameSlope)

# Now get projects merged with BCA and census tracts
nameSpatial <- "~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds"
if (!file.exists(nameSpatial)) {
  source("scripts/mergePermitsSpatial.R")
}
dtSpatial <- readRDS(nameSpatial)

print(head(dtSpatial))
print(head(dtSlope))
dtMerge <- merge(dtSpatial,dtSlope,by="neighbourhoodDescription")
print(head(dtMerge))
print(summary(dtMerge))
print(table(dtMerge[,use]))
# permitnumbercreateddate     use            MB_effective_year 
# Min.   :2017-01-03      Length:3762        Length:3762       

dtMerge[,pYear:=year(permitnumbercreateddate)]
for (y in sort(unique(dtMerge[,pYear]))) {
  print(y)
  print(table(dtMerge[pYear==y,use]))
}


dtMerge[,MB_total_finished_area:=as.numeric(MB_total_finished_area)]
dtMerge[,MB_effective_year:=as.numeric(MB_effective_year)]
dtMerge[,landWidth:=as.numeric(landWidth)]
dtMerge[,landDepth:=as.numeric(landDepth)]
dtMerge[,effectiveAge:=pYear - MB_effective_year]
dtMerge[maxSingle==0 & use=="laneway",use:="lanewayOnly"]
# No duplexes under 25 years old (?)
print(summary(dtMerge[,.(MB_total_finished_area,landWidth,landDepth,effectiveAge)]))

dtMerge[,notSingle:=use!="single" & use!="laneway"]

print(names(dtMerge))
dtMerge[,pDelta:=meanPPSF_single-meanPPSF_duplex]
print(feols(notSingle ~ meanPPSF + pDelta | pYear, data=dtMerge))
print(feols(notSingle ~ meanPPSF_single + meanPPSF_duplex | pYear, data=dtMerge[pYear>2020],cluster="neighbourhoodDescription"))
print(cor(dtMerge[,.(meanPPSF_single,meanPPSF_duplex,pDelta,elasticity)]))
q("no")

## KEY: Drop Laneway only
print(dtMerge[effectiveAge>0,summary(effectiveAge),by="use"])
print(dtMerge[effectiveAge>0,table(typeofwork),by="use"])
dtMerge <- dtMerge[effectiveAge>25]
dtMerge <- dtMerge[use!="lanewayOnly" & typeofwork=="New Building"]
# use geo_point_2d to plot variable notSingle by lon,lat
ggplot(dtMerge,aes(x=as.numeric(tstrsplit(geo_point_2d,", ")[[2]]),
                    y=as.numeric(tstrsplit(geo_point_2d,", ")[[1]]),
                    color=notSingle)) +
  geom_point() +
  theme_minimal() +
  labs(title="Vancouver Permits 2017-2025",
       subtitle="Not Single Family (including laneway) Permits")
  ggsave("text/notSingleLocation.png",width=8,height=6)

  # now plot meanPPSF by lon,lat
ggplot(dtMerge,aes(x=as.numeric(tstrsplit(geo_point_2d,", ")[[2]]),
                    y=as.numeric(tstrsplit(geo_point_2d,", ")[[1]]),
                    color=meanPPSF)) +
  geom_point() +
  theme_minimal() +
  labs(title="Vancouver Permits 2017-2025",
       subtitle="Mean PPSF from BCA Sales 2019-2021 by Census Tract")
  ggsave("text/meanPPSFLocation.png",width=8,height=6)

  # now plot elasticity by lon,lat
ggplot(dtMerge,aes(x=as.numeric(tstrsplit(geo_point_2d,", ")[[2]]),
                    y=as.numeric(tstrsplit(geo_point_2d,", ")[[1]]),
                    color=elasticity)) +
  geom_point() +
  theme_minimal() +
  labs(title="Vancouver Permits 2017-2025",
       subtitle="Elasticity from BCA Sales 2019-2021 by Census Tract")
  ggsave("text/elasticityLocation.png",width=8,height=6)

  # and slope
ggplot(dtMerge,aes(x=as.numeric(tstrsplit(geo_point_2d,", ")[[2]]),
                    y=as.numeric(tstrsplit(geo_point_2d,", ")[[1]]),
                    color=slope)) +
  geom_point() +
  theme_minimal() +
  labs(title="Vancouver Permits 2017-2025",
       subtitle="Slope from BCA Sales 2019-2021 by Census Tract")
  ggsave("text/slopeLocation.png",width=8,height=6)

  # mean notSingle by meanPPSF x-y plot
  dtTract <- dtMerge[pYear>2020,.(notSingle=mean(notSingle,na.rm=TRUE), nobs=mean(nobs),
                          meanPPSF=mean(meanPPSF,na.rm=TRUE), slope=mean(slope,na.rm=TRUE), elasticity=mean(elasticity,na.rm=TRUE)),
                      by=.(neighbourhoodDescription)]
  ggplot(dtTract,aes(x=meanPPSF,y=notSingle)) + geom_point() 
  ggsave("text/notSingle_vs_meanPPSF.png",width=8,height=6)

  # ppsf and slope -- make points proportional to nobs
  ggplot(dtTract,aes(x=meanPPSF,y=slope)) + geom_point(aes(size=nobs)) +
    theme_minimal() 
  ggsave("text/ppsfSlopeTract.png",width=8,height=6)

print(cor(dtTract[,.(notSingle,meanPPSF,slope,elasticity)],use="complete.obs"))
print(summary(lm(notSingle ~ meanPPSF,data=dtTract)))
print(summary(lm(notSingle ~ meanPPSF + slope ,data=dtTract)))
print(summary(lm(notSingle ~ meanPPSF + elasticity ,data=dtTract)))
# combinedera
print(summary(feols(notSingle ~ meanPPSF  | pYear,data=dtMerge[pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ slope  | pYear,data=dtMerge[pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ elasticity  | pYear,data=dtMerge[pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF + elasticity  | pYear,data=dtMerge[pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF + slope  | pYear,data=dtMerge[pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | pYear,data=dtMerge[pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF+slope + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | pYear,data=dtMerge[landWidth %between% c(48,56) & pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF+ log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | pYear,data=dtMerge[landWidth %between% c(48,56) & pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF+elasticity + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | pYear,data=dtMerge[landWidth %between% c(48,56) & pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF+slope + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | pYear,data=dtMerge[landWidth %between% c(30,36) & pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF+elasticity + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | pYear,data=dtMerge[landWidth %between% c(30,36) & pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF+slope + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | pYear,data=dtMerge[pYear %between% c(2021,2025)])))
print(summary(feols(notSingle ~ meanPPSF+elasticity + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | pYear,data=dtMerge[pYear %between% c(2021,2025)])))

print(cor(dtMerge[effectiveAge>2,.(MB_total_finished_area/landWidth,effectiveAge,notSingle,meanPPSF)],use="complete.obs"))
print(dtMerge[order(effectiveAge),.(mean(notSingle,na.rm=TRUE),mean(maxSingle,na.rm=TRUE),.N),by=effectiveAge][1:50])

  ##############

# aggregate
dtAgg <- dtMerge[,.(nPermits=.N,
                      nNotSingle=sum(use!="single" & use!="laneway"),
                      meanPPSF=mean(meanPPSF,na.rm=TRUE),
                      slope=mean(slope,na.rm=TRUE),
                      elasticity=mean(elasticity,na.rm=TRUE),
                      meanFinishedArea=mean(MB_total_finished_area,na.rm=TRUE),
                      meanLandArea=mean(LANDAREA,na.rm=TRUE),
                      meanEffectiveAge=mean(effectiveAge,na.rm=TRUE)),
                  by=.(CTUID)]

print(summary(feols(nNotSingle/nPermits ~ meanPPSF + log(meanLandArea) + log(meanEffectiveAge) | 1,data=dtAgg)))
print(summary(feols(nNotSingle/nPermits ~ meanPPSF + slope + log(meanLandArea) + log(meanEffectiveAge) | 1,data=dtAgg)))
print(summary(feols(nNotSingle/nPermits ~ meanPPSF + elasticity + log(meanLandArea) + log(meanEffectiveAge) | 1,data=dtAgg)))
