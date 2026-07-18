# edmontonMatch.R
# Match building permits to assessment
# Strategy: do 2026 and late 2025 permits so old address still valid maybe
# Tom Davidoff
# 07/16/26

library(data.table)
library(fixest)
library(sf)

dtA <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv")
dtP  <- fread("~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv")

# ------------------------------------------------------------------------------
# 2. Process Assessment Data
# ------------------------------------------------------------------------------
modWord <- function(word) {
	# anything after a Space is capitalized -- already true 
	# first set all to lower
	x <- tolower(word)
	x <- gsub("_", " ", x)
	x <- gsub("\\s+([a-z])", "\\U\\1",x, perl = TRUE)
	x <- gsub(" ", "", x)
	x <- gsub("^([A-Za-z])", "\\L\\1", x, perl = TRUE)
	return(x)
}


setnames(dtA,names(dtA),modWord(names(dtA)))
setnames(dtP,names(dtP),modWord(names(dtP)))
dtP <- dtP[year>=2024 & zoning=="RS"] # probably decent N
dtA <- dtA[assessmentClass1=="RESIDENTIAL" & zoning %in% c("RF1","RF2","RF3","RF4","RS")]
dtA[,assessedValue:=gsub(",","",assessedValue)]
dtA[,assessedValue:=as.numeric(gsub("\\$","",assessedValue))]
meanP <- dtA[assessmentYear==2023,.(meanVal=mean(assessedValue,na.rm=TRUE)),by=neighbourhood]
print(meanP)
print(head(dtP))
dtP <- dtP[grepl("Building - New",workType,ignore.case=TRUE)]
dtA[,lotSize:=as.numeric(gsub(",","",lotSize))]
print(summary(dtA[,lotSize]))
print(quantile(dtA[!is.na(lotSize),lotSize],seq(.2,.8,by=.1)))
print(summary(dtA[!is.na(lotSize),lotSize%between%c(500,600)]))

dtA[,legal:=gsub("Block:","Blk",legalDescription)]
dtA[,legal:=gsub(":","",legal)]
dtA[,legal:=gsub("  "," ",legal)] # double spaces
dtP[,legal:=legalDescription] 
dtM <- merge(dtP,dtA,by="legal",all.x=FALSE) # drop non-matched for now
print(head(dtP))
print(head(dtA))
print(head(dtM))
print(dtM[!is.na(lotSize)])
print(dtM[,summary(lotSize),by=buildingType])
print(dtM[year==2026,summary(lotSize),by=buildingType])
print(dtM[lotSize %between% c(500,700),summary(floorArea),by=buildingType])
print(dtM[lotSize %between% c(500,600) & year==2026,summary(floorArea),by=buildingType])
print(dtM[lotSize %between% c(600,700) & year==2026,summary(floorArea),by=buildingType])
print(dtM[lotSize %between% c(500,600) & year==2026,summary(constructionValue),by=buildingType])
print(dtM[lotSize %between% c(600,700) & year==2026,summary(constructionValue),by=buildingType])
print(summary(dtP[,year]))
print(summary(dtM[,year]))
print(table(dtM[,neighbourhood.x==neighbourhood.y]))
dtM[,neighbourhood:=neighbourhood.y]
dtM <- merge(dtM,meanP,by="neighbourhood")
print(dtM[,summary(meanVal),by=buildingType])
regData <- unique(dtM[,.(houseNumber,streetName,neighbourhood,meanVal,lotSize,buildingType,year)])
print(summary(regData))
print(summary(feols(meanVal ~ i(buildingType), data=regData)))
print(summary(feols(meanVal ~ i(buildingType)+lotSize, data=regData)))
print(summary(feols(meanVal ~ i(buildingType)+lotSize, data=regData[year==2026])))
print(summary(feols(meanVal ~ i(buildingType)+lotSize, data=regData[year==2026 & lotSize %between% c(500,600)])))
print(summary(feols(meanVal ~ i(buildingType)+lotSize, data=regData[year==2026 & lotSize %between% c(600,700)])))

fwrite(dtM[assessmentClass1=="RESIDENTIAL",.(houseNumber,streetName)],file="~/Downloads/addressesFun.csv")
q('no')
