# main.vancouver_permits_
# main analysis script
# Tom Davidoff
# 01/16/26

library(data.table)
library(ggplot2)
library(fixest)

# Vancouver analysis
# get slopes and mean values from BCA sales analysis by census tract merge
nameSlope <- "tables/bca19_mean_ppsf_slope_by_tract.csv"
if (!file.exists(nameSlope)) {
  source("scripts/bcaSales.R")
}
# specify column CTUID is a number
dtSlope <- fread(nameSlope,colClasses=list(character="CTUID"))

# Now get projects merged with BCA and census tracts
nameSpatial <- "~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds"
if (!file.exists(nameSpatial)) {
  source("scripts/mergePermitsSpatial.R")
}
dtSpatial <- readRDS(nameSpatial)

print(head(dtSpatial))
print(head(dtSlope))

dtMerge <- merge(dtSpatial,dtSlope,by="CTUID")
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
dtMerge[,effectiveAge:=pYear - MB_effective_year]

print(cor(dtMerge[,meanPPSF],dtMerge[,slope],use="complete.obs"))
dtMerge[,slopeMean:=mean(slope),by=neighbourhoodDescription]

# duplex era
print(summary(feols(use=="duplex" ~ meanPPSF + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2019,2023)])))
print(summary(feols(use=="duplex" ~ meanPPSF+slope + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2019,2023)])))
print(summary(feols(use=="duplex" ~ meanPPSF+slopeMean + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2019,2023)])))

# non-laneway
print(summary(feols(use=="duplex" ~ meanPPSF + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2019,2023) & use!="laneway"])))
print(summary(feols(use=="duplex" ~ meanPPSF+slope + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2019,2023) & use!="laneway"])))
print(summary(feols(use=="duplex" ~ meanPPSF+slopeMean + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2019,2023) & use!="laneway"])))

# non-single
print(summary(feols(use=="duplex" ~ meanPPSF + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2019,2023) & use!="single"])))
print(summary(feols(use=="duplex" ~ meanPPSF+slope + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2019,2023) & use!="single"])))

# multiplex era
print(summary(feols(use=="multi" ~ meanPPSF + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2024,2025)])))
print(summary(feols(use=="multi" ~ meanPPSF+slope + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2024,2025)])))
print(summary(feols(use=="multi" ~ meanPPSF+slopeMean + log(MB_total_finished_area) + log(LANDAREA) + log(effectiveAge) | + MB_effective_year,data=dtMerge[pYear %between% c(2024,2025)])))


