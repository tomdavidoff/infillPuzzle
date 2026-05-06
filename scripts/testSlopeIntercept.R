# testSlopeIntercept.R
# R to see if they are negatively correlated in theory
# Tom Davidoff
# 04/17/26

library(data.table)
library(fixest)

trials <- 1000
betaSlopeIntercept <- NULL
G <- 50 # number of groups
N <- 200 # number of observations per group

for (i in 1:trials) {
	x <- rnorm(N*G)
	group <- rep(1:G, each=N)
	trueBeta <- -1*group # want to find right correlation
	y <- group + trueBeta*x + rnorm(N*G)
	dt <- data.table(group=group, x=x, y=y)
	reg <- feols(y ~ 0 + x*i(group),data=dt)
	# make a data table with the group coefficients alone and the interactive slope coefficients
	coefs <- coef(reg)
	coefNames <- names(coefs)
	# group only are the coefficients without x in the name
	# split the name by ":" and take the last item as the group
	groupCoefs <- coefs[!grepl("x", coefNames)]
	slopeCoefs <- coefs[grepl("x:", coefNames)]
	# get the group for each of groupCoefs and slopeCoefs by taking the last item after the last : in the word
	groupCoefDT <- data.table(coefName=names(groupCoefs), coefValue=groupCoefs)
	groupCoefDT[,groupID:=sub(".*:(.*)", "\\1", coefName)]
	slopeCoefDT <- data.table(coefName=names(slopeCoefs), coefValue=slopeCoefs)
	slopeCoefDT[,groupID:=sub("x:group::", "\\1", coefName)]
	coef_dt <- merge(groupCoefDT, slopeCoefDT, by="groupID", suffixes=c("_intercept", "_slope"))
	betaSlopeIntercept  <- c(betaSlopeIntercept, cov(coef_dt$coefValue_intercept, coef_dt$coefValue_slope)/var(coef_dt$coefValue_intercept))
}
print(summary(betaSlopeIntercept))
