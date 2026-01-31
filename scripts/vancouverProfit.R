# vancouverProfit.R
# estimate profitability of redevelopment choices in Vancouver
# Tom Davidoff 
# 01/31/26

library(data.table)
library(ggplot2)

# load data
dtPrice <- fread("tables/bca19_mean_ppsf_slope_by_tract.csv",colClasses=list(character=c("CTUID")))
dtChoice <- readRDS("~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")

dtMerge <- merge(dtChoice,dtPrice,by="CTUID")
print(head(dtMerge))
print(head(dtMerge))
print(table(dtMerge[,use]))

# some major outliers in slope

print(summary(dtMerge))
print(summary(dtMerge[nobs>quantile(nobs,.05)]))
print(summary(dtMerge[nobs>quantile(nobs,.1)]))
print(unique(dtMerge[elasticity>0,.(nobs,neighbourhoodDescription,elasticity)]))
dtMerge <- dtMerge[nobs>4]
# Economic intuition says elasticity can't be less than -1 or else price falls in square feet. Also, as there is no assembly for larger lots, elasticity must be less than 0
dtMerge <- dtMerge[elasticity>0,elasticity:=0]
dtMerge <- dtMerge[elasticity < -1,elasticity:=-1]

# plot use against lat/long --extract from geo_point_2d
dtMerge[, c("lat", "lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]
ggplot(dtMerge,aes(x=lon,y=lat,color=use)) + geom_point() + theme_minimal()
ggsave("text/vancouverUseSpatial.png",width=6,height=6)

# also do ppsf against lat/long and then do elasticity
ggplot(dtMerge,aes(x=lon,y=lat,color=meanPPSF)) + geom_point() + theme_minimal() + scale_color_viridis_c()
ggsave("text/vancouverPPSFSpatial.png",width=6,height=6)

ggplot(dtMerge,aes(x=lon,y=lat,color=elasticity)) + geom_point() + theme_minimal() + scale_color_viridis_c()
ggsave("text/vancouverElasticitySpatial.png",width=6,height=6)
