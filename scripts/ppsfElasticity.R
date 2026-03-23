# ppsfElasticity.R
# For each neighbourhood in Vancouver regional district, each year, compute ppsf and elasticity wrt sqft for single, duplex, both
# Then look at correlations
# Tom Davidoff 
# 03/20/26

library(data.table)
library(ggplot2)
library(fixest)

# get lat/lon in case want to do census tracts
dG <- readRDS("~/OneDrive - UBC/dataProcessed/bca25Centroids.rds")
print(head(dG))

# merge sales and square footage
dS <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",select=c("ROLL_NUMBER","FOLIO_ID","CONVEYANCE_TYPE_DESCRIPTION","CONVEYANCE_DATE","CONVEYANCE_PRICE"))
print(head(dS))
dS[,year:=as.numeric(substr(CONVEYANCE_DATE,1,4))]
MINYEAR <- 2016
MAXYEAR <- 2024
dS <- dS[year>=MINYEAR & year<=MAXYEAR & CONVEYANCE_TYPE_DESCRIPTION=="Improved Single Property Transaction"]

dD <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("ROLL_NUMBER","FOLIO_ID","JURISDICTION","JURISDICTION_CODE","NEIGHBOURHOOD","ACTUAL_USE_DESCRIPTION"))
print(summary(dD))
# sort by number of cases
dD[,nUse:=.N,by=ACTUAL_USE_DESCRIPTION]
dD <- dD[ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling","Residential Dwelling with Suite") | grepl("Duplex",ACTUAL_USE_DESCRIPTION)]
VANONLY <- 0
if (VANONLY==1) {
	dD <- dD[JURISDICTION_CODE==200]
	print(paste0("Keeping only Vancouver, ",nrow(dD)," records"))
}

# inventories to get
dtMapper <- fread("~/OneDrive - UBC/dataProcessed/regional_district_assessment_area.csv")
#Regional District,Assessment Area
# Vancouver is 9
print(dtMapper[`Assessment Area`==9])
gV <- as.numeric(dtMapper[`Assessment Area`==9, .(`Regional District`)][1])
print(gV)
needInventory <- unique(dtMapper[`Regional District`==gV, .(AA=`Assessment Area`)]$AA)

# loop through like this with needed inventories, then rbind them together
for (i in 1:length(needInventory)) {
	print(paste0("Reading inventory for assessment area ",needInventory[i]))
	if (needInventory[i]<10) {
		filename <- paste0("~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A0",needInventory[i],"_Residential_Inventory_Extract.txt")
	} else {
		filename <- paste0("~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A",needInventory[i],"_Residential_Inventory_Extract.txt")
	}
	dtTemp <- fread(filename,select=c("Roll_Number","MB_Total_Finished_Area","Zoning","Land_Width_Width","Land_Depth_Depth","MB_Effective_Year","Jurisdiction"),colClasses=c(Roll_Number="character",MB_Total_Finished_Area="numeric",Zoning="character",Land_Width_Width="numeric",Land_Depth_Depth="numeric"))
	if (i==1) {
		dtInventory <- dtTemp
	} else {
		dtInventory <- rbind(dtInventory,dtTemp)
	}
}
print(head(dtInventory))
print(dtInventory[Jurisdiction=="200",Roll_Number])
print(dD[JURISDICTION_CODE==200,ROLL_NUMBER])
print(table(dtInventory[,Jurisdiction]))

# merge on as.numeric(roll_number) and Jurisdiction/JURISDICTION_CODE inventory, description


print(head(dD[,.(ROLL_NUMBER,JURISDICTION_CODE,JURISDICTION)]))
dD[, ROLL_NUMBER := as.numeric(ROLL_NUMBER)]
dtInventory[,ROLL_NUMBER := as.numeric(Roll_Number)]
print(head(dtInventory[,.(ROLL_NUMBER,Roll_Number,Jurisdiction)]))
dD[,id:=paste(ROLL_NUMBER,JURISDICTION_CODE)]
dtInventory <- dtInventory[!is.na(ROLL_NUMBER)]
dD <- dD[!is.na(ROLL_NUMBER)]
dtInventory[,id:=paste(ROLL_NUMBER,Jurisdiction)]
print(length(unique(dD$id)))
print(length(unique(dtInventory$id)))
dtMerge <- merge(dtInventory, dD, by="id")
print(head(dtMerge))

dtMerge <- merge(dtMerge, dS, by=c("FOLIO_ID"), all.x=TRUE)
# start with subset
dtMerge <- dtMerge[ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling","Residential Dwelling with Suite") ]

dtMerge[,ppsf:=CONVEYANCE_PRICE/MB_Total_Finished_Area]
dtMerge[,group:=paste(NEIGHBOURHOOD,year,JURISDICTION)]
dtMerge[,nGroup:=.N, by=group]
# computer within group elasticity of ppsf wrt sqft
dtMerge[,logppsf:=log(ppsf)]
dtMerge[,logsqft:=log(MB_Total_Finished_Area)]
dtMerge <- dtMerge[!is.na(logppsf) & !is.na(logsqft)]
# compute elasticity by group with cov/var
MINAGE <- 5
MAXAGE <- 50
MAXWIDTH <- 90
dtMerge[,age:=year-MB_Effective_Year]
print(head(dtMerge))
dtGroup <- dtMerge[age<=MAXAGE & age>=MINAGE & Land_Width_Width<MAXWIDTH, .(cov= cov(logppsf,logsqft), var=var(logsqft), mppsf=median(log(ppsf)),nGroup=.N), by=c("year","NEIGHBOURHOOD","JURISDICTION")]
print(head(dtGroup))
dtGroup[,elasticity:=cov/var]
dtGroupVanLot <- dtMerge[age<=MAXAGE & age>=MINAGE & Land_Width_Width<35 & Land_Width_Width>31 & JURISDICTION=="City of Vancouver", .(cov= cov(logppsf,logsqft), var=var(logsqft), mppsf=median(log(ppsf)),nGroup=.N), by=c("year","NEIGHBOURHOOD","JURISDICTION")]
dtGroupVanLot[,elasticity:=cov/var]
MINOBS <- 20
dtGroup <- dtGroup[nGroup>=MINOBS]
print(summary(dtGroup))
print(summary(feols(elasticity ~ mppsf|year,data=dtGroup)))
print(summary(feols(elasticity ~ mppsf*i(year),data=dtGroup)))
print(summary(feols(elasticity ~ mppsf|year,data=dtGroup[JURISDICTION=="City of Vancouver"])))
print(summary(feols(elasticity ~ mppsf*i(year),data=dtGroup[JURISDICTION=="City of Vancouver"])))
# One plot per year, bubbles proportionate to obs, xlim -1 to 0
ggplot(dtGroup, aes(x=elasticity, y=mppsf, size=nGroup)) + geom_point() + facet_wrap(~year) + theme_bw() + labs(x="Elasticity of ppsf wrt sqft", y="Median ppsf", size="Number of observations") + xlim(-1.5,.25)
ggsave("text/ppsfElasticityMetro.png", width=10, height=6) 
ggplot(dtGroupVanLot, aes(x=elasticity, y=mppsf, size=nGroup)) + geom_point() + facet_wrap(~year) + theme_bw() + labs(x="Elasticity of ppsf wrt sqft", y="Median ppsf", size="Number of observations")+ xlim(-1.5,.25)
ggsave("text/ppsfElasticityVanLot.png", width=10, height=6) 

#elasticityResults <- dtMerge[ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling","Residential Dwelling with Suite"), .(mppsf:=median(ppsf),elasticity=cov(logppsf,logsqft)/var(logsqft)), by=group]
#print(elasticityResults)


