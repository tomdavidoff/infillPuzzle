# VancouverChoice.int
# R to analyze discrete choices of developers, maybe aggregate
# Tom Davidoff
# Feb 5, 2026

library(data.table)
library(ggplot2)
library(fixest)
library(geosphere)
library(nnet)
library(stargazer)
library(xtable)
CUTDISTANCE <- 20 # laneay
MINDEPTH <- 100
MINWIDTH <- 30

# Choices data
dtChoice <- readRDS("~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")
print(table(dtChoice[,use]))
print(head(dtChoice))
dtChoice[,landWidth:=as.numeric(landWidth)]
dtChoice <- dtChoice[!is.na(landWidth)]
print(quantile(dtChoice[,landWidth],seq(.1,.9,.05)))
q("no")
dtChoice[,year:=year(permitnumbercreateddate)]
dtChoice[,c("latitude","longitude"):=tstrsplit(geo_point_2d,","  )  ]
# Downtown is Vancouver's primary business district, houses many arts, entertainment, and sports venues, and is close to several vibrant residential communities.
# 49.2827° N, 123.1207° W per google
dtChoice[,latDowntown:=49.2827]
dtChoice[,lonDowntown:=-123.1207]
dtChoice[,latitude:=as.numeric(latitude)]
dtChoice[,longitude:=as.numeric(longitude)]
dtChoice[,distDowntown:=distGeo(dtChoice[,c("longitude","latitude")],dtChoice[,c("lonDowntown","latDowntown")])]
dtChoice[,landDepth:=as.numeric(landDepth)]
dtChoice[,landArea:=landWidth*landDepth  ]
dtChoice <- dtChoice[landWidth>=MINWIDTH & landWidth<=MINDEPTH]
dtPriceCT <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_tract.csv",colClasses=c(CTUID="character"))
dtChoice <- merge(dtChoice,dtPriceCT,by="CTUID")
dtChoice[,w33:=abs(landWidth-33)<=3]
dtChoice <- dtChoice[!is.na(landWidth)]

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

# Mean price, single vs duplex premium and elasticity of ppsf among single/duplex, mean price by neighbourhood

dtElasticityN <- fread("~/OneDrive - UBC/dataProcessed/neighbourhoodInteractionsVancouver.csv")
dtPriceCT <- fread("~/OneDrive - UBC/dataProcessed/tractMeansVancouver.csv",colClasses=c(CTNAME="character"))
print(head(dtElasticityN))
print(head(dtPriceCT))
print(head(dtChoice))
print(nrow(dtChoice))
dtElasticityN[,neighbourhoodDescription:=toupper(NEIGHBOURHOOD)]
dtElasticityN[neighbourhoodDescription=="ARBUTUS RIDGE - MACKENZIE HEIGHTS",neighbourhoodDescription:="ARBUTUS/MACKENZIE HEIGHTS"]
dtElasticityN[neighbourhoodDescription=="MAIN & FRASER",neighbourhoodDescription:="MAIN/FRASER"]
s1 <- unique(dtChoice[,neighbourhoodDescription])
dtChoice <- merge(dtChoice,dtElasticityN,by="neighbourhoodDescription")
print(nrow(dtChoice))
print(setdiff(s1,unique(dtChoice[,neighbourhoodDescription])))

b <- unique(dtChoice[,neighbourhoodDescription])
print(setdiff(a,b))

print(c("LOSS OF OBS?",a - nrow(dtChoice)))


dtChoice <- dtChoice[use!="laneway"] # not laneway only
dtChoice[,elasticity:=interactionlSqft]
print(head(dtChoice))
print(head(dtPriceCT))

dtChoice <- merge(dtChoice,dtPriceCT,by.x="tract",by.y="CTNAME")

DOTRACT <- 0
if (DOTRACT==1) {
  dtChoice[,interactionSqft:=NULL]
  dtInteractionsTract <- fread("~/OneDrive - UBC/dataProcessed/tractInteractionsVancouver.csv",colClasses=c("CTNAME"="character","interactionSqft"="numeric"))
  dtChoice <- merge(dtChoice,dtInteractionsTract,by.x="tract",by.y="CTNAME")
}

dtChoice <- dtChoice[!is.na(interactionSingle) & !is.na(interactionlSqft)]
dtChoice[,laneway:=use=="single" & distToLaneway<CUTDISTANCE]
dtChoice[,mergeZone:=tract] # ND or tract
dtChoice[,lmedianIncome := log(medianIncome)]

dtChoice[,lmedianPPSF:=log(medianPPSF)]
dtChoice[,singleLaneway:=use=="single" & distToLaneway<CUTDISTANCE]
dtChoice[,singleNoLaneway:=use=="single" & distToLaneway>=CUTDISTANCE]
dtChoice[distToLaneway<CUTDISTANCE & use=="single",use:="singleLaneway"]
dtChoice[distToLaneway>=CUTDISTANCE & use=="single",use:="singleOnly"]
dtChoice[,duplex:=use=="duplex"]
dtChoice[,multiplex:=use=="multi"]
cols <- c("laneway","singleNoLaneway","singleLaneway","duplex","multiplex", "medianPPSF","medianIncome","elasticity")
print(dtChoice[,median(landWidth),by=use])

x <- copy(dtChoice)[, (cols) := lapply(.SD, function(v) unname(as.numeric(v))), .SDcols = cols]

print(stargazer(as.data.frame(x[, ..cols]), type = "latex", file = "text/summaryChoice.tex"))

dtLaneway <- dtChoice[use=="singleLaneway" | use=="singleOnly",.(laneway=mean(use=="singleLaneway")),by="tract"]
dtDuplex <- dtChoice[year<2024,.(duplex=mean(duplex)),by="tract"]
dtMultiplex <- dtChoice[year>=2024,.(multiplex=mean(multiplex)),by="tract"]
dtStats <- dtChoice[,.(lmedianIncome = mean(lmedianIncome, na.rm = TRUE), singlePremium = mean(interactionSingle, na.rm = TRUE), elasticity = mean(elasticity, na.rm = TRUE), medianPPSF = mean(medianPPSF, na.rm = TRUE)), by=tract]
print(head(dtStats))
print(head(dtLaneway))
print(summary(dtLaneway))
print(head(dtDuplex))
print(head(dtMultiplex))
dtStats <- merge(dtStats,dtLaneway,by="tract",all.x=TRUE)
dtStats <- merge(dtStats,dtMultiplex,by="tract",all.x=TRUE)
dtStats  <- merge(dtStats,dtDuplex,by="tract",all.x=TRUE)
print(summary(dtChoice))
print(table(dtChoice[,use]))
C <- cor(dtStats[,.(laneway,lmedianIncome,singlePremium,elasticity,duplex,multiplex,medianPPSF) ],use="complete.obs")
# correlation matrix
# convert to data.table (optional, but tidy)
C_dt <- as.data.table(C, keep.rownames = "Variable")
# export to LaTeX
print( xtable(C_dt, digits = 2), file = "text/vancouverCorrelate.tex", floating=FALSE, include.rownames = FALSE )
# toggle for lanewayResidual estimation level

# print summary statistics for tabulating in R
dtSingle <- dtChoice[use=="singleOnly" | use=="singleLaneway"]
rrc <- feols(laneway ~ lmedianPPSF + landWidth |year  ,data=dtSingle,cluster="tract")
rri <- feols(laneway ~ lmedianPPSF + landWidth + lmedianIncome|year  ,data=dtSingle,cluster="tract")
rr <- feols(laneway ~ lmedianPPSF + landWidth +   elasticity|year  ,data=dtSingle,cluster="tract")
print(etable(rrc,rri,rr))
dtSingle[,eps:=resid(rr)]
rdc <- feols(use=="duplex" ~ lmedianPPSF + landWidth|year,data=dtChoice[year<2024],cluster="tract")
rdi <- feols(use=="duplex" ~ lmedianPPSF + landWidth + lmedianIncome|year,data=dtChoice[year<2024],cluster="tract")
rd <- feols(use=="duplex" ~ lmedianPPSF +landWidth+lmedianIncome+ elasticity|year,data=dtChoice[year<2024],cluster="tract")
print(etable(rdc,rdi,rd))

rpc <- feols(use!="singleOnly" ~ lmedianPPSF + landWidth|year,data=dtChoice,cluster="tract")
rpi <- feols(use!="singleOnly" ~ lmedianPPSF + landWidth + lmedianIncome|year,data=dtChoice,cluster="tract")
rp <- feols(use!="singleOnly" ~ lmedianPPSF + landWidth+ lmedianIncome+ elasticity|year,data=dtChoice,cluster="tract")
print(etable(rpc,rpi,rp))

rmc <- feols(use=="multi" ~ lmedianPPSF + landWidth |year,data=dtChoice[year>2023 & use!="single"],cluster="tract")
rmi <- feols(use=="multi" ~ lmedianPPSF + landWidth + lmedianIncome|year,data=dtChoice[year>2023 & use!="single"],cluster="tract")
rm <- feols(use=="multi" ~ lmedianPPSF + landWidth + lmedianIncome +elasticity|year,data=dtChoice[year>2023 & use!="single"],cluster="tract")
rme <- feols(use=="multi" ~ lmedianPPSF + landWidth + lmedianIncome + elasticity|year,data=dtChoice[year>2023 & use!="single"],cluster="tract")
print(etable(rmc,rmi,rm,rme))

rmc <- feols(multiplex ~ lmedianPPSF + landWidth |year,data=dtChoice[year>2023 ],cluster="tract")
rm <- feols(multiplex ~ lmedianPPSF + landWidth + elasticity|year,data=dtChoice[year>2023 ],cluster="tract")
print(etable(rmc,rm ))

dtChoice[use=="single" & distToLaneway < CUTDISTANCE,use:="laneway"]
dtChoice[,use:=factor(use,ordered=FALSE)]
dtChoice[,use := relevel(use, ref = "singleOnly")]
#multinomial logit
mlogitc <- multinom(use ~ lmedianPPSF + landWidth + lmedianIncome + factor(year),data=dtChoice[landWidth<40],cluster="tract")
mlogit <- multinom(use ~ lmedianPPSF + landWidth + lmedianIncome + elasticity + factor(year),data=dtChoice[landWidth<40],cluster="tract")
print(summary(mlogitc))
print(summary(mlogit))

# plot with bubbles proportional to nobs
dPlot <- dtChoice[,.(elasticity=mean(elasticity),medianPPSF=mean(medianPPSF),nobs=.N),by="tract"]
ggplot(dPlot, aes(x=medianPPSF, y=elasticity, size=nobs)) + geom_point()
ggsave("text/elasticityMedianPrice.png")

stop()
