# edmontonElasticityPPSF.R
# R to identify the relationship between price per square foot and elasticity of price per square foot wrt elasticity
# Tom Davidoff 
# 07/17/26

library(data.table)
library(fixest)
library(ggplot2)

dtA <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv")
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
dtA <- dtA[assessmentYear==2023]
print(names(dtA))
dtA <- dtA[assessmentClass1=="RESIDENTIAL" & zoning %in% c("RF1","RF2","RF3","RF4","RS")]
dtT <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv")
setnames(dtT,names(dtT),modWord(names(dtT)))
MINYEAR <- 1995
dtT <- dtT[zoning=="RS" & lotSize %between% c(500,600) & yearBuilt<2023,.(accountNumber,totalGrossArea,yearBuilt)]
print(head(dtT))
setkey(dtT,accountNumber)
setkey(dtA,accountNumber)
print(nrow(dtT))
dtM <- dtA[dtT]
print(nrow(dtM))
dtM[,assessedValue:=gsub(",","",assessedValue)]
dtM[,assessedValue:=as.numeric(gsub("\\$","",assessedValue))]
dtM[,ppsf:=assessedValue/totalGrossArea]
for (n in unique(dtM[,neighbourhood])) {
	print(n)
	dX <- dtM[neighbourhood==n & lotSize %between% c(500,600) & !is.na(ppsf) & !is.na(totalGrossArea),.(ppsf,totalGrossArea)]
	if (nrow(dX)>10) {
		print(summary(dX))
		reg <- feols(log(ppsf)~log(totalGrossArea),data=dX)
		print(summary(reg))
	}
}
regPSF <- feols(log(ppsf)~log(totalGrossArea) + i(neighbourhood) | yearBuilt,data=dtM[lotSize %between% c(500,600) & !is.na(ppsf) & !is.na(totalGrossArea)])
regElasticity <- feols(log(ppsf)~i(neighbourhood,log(totalGrossArea)) + log (totalGrossArea)|neighbourhood+yearBuilt ,data=dtM[lotSize %between% c(500,600) & !is.na(ppsf) & !is.na(totalGrossArea)])
print(summary(regElasticity))
# get coefficients for each neighborhood from each regression
dtCoef <- data.table(neighbourhood=character(),coefPSF=numeric(),coefElasticity=numeric())
for (n in sort(unique(dtM[,neighbourhood]))) {
	print(n)
	coefPSF <- coef(regPSF)[paste0("neighbourhood::",n)]
	coefElasticity <- coef(regElasticity)[paste0("neighbourhood::",n,":log(totalGrossArea)")]
	dtCoef <- rbind(dtCoef,data.table(neighbourhood=n,coefPSF=coefPSF,coefElasticity=coefElasticity))
}
print(dtCoef)
print(cor(dtCoef[,coefPSF],dtCoef[,coefElasticity],use="complete.obs"))
ggplot(dtCoef,aes(x=coefPSF,y=coefElasticity)) + geom_point() + geom_smooth(method="lm") + xlab("Coefficient of log(totalGrossArea) in log(ppsf) regression") + ylab("Coefficient of log(totalGrossArea) in elasticity regression") + ggtitle("Relationship between price per square foot and elasticity of price per square foot wrt total gross area")
ggsave("text/edmontonElasticityPPSF.png",width=8,height=6)
