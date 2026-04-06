# ahs.R
# check square footage for Minneapolis with ahs data
# Tom Davidoff 
# 04/03/26

library(data.table)

varnames = c("COUNTY_2010","OMB13CBSA","STATE","JUNITSIZE","JBLD","JNUNITS")
df <- fread("~/DropboxExternal/dataRaw/ahs2023n.csv", select = varnames)
print(table(df[,JBLD]))
#df <- df[JBLD=="'2'"]
print(table(df[,OMB13CBSA]))
df <- df[OMB13CBSA=="33460"] # Minneapolis-St. Paul-Bloomington, MN-WI
print(names(df))
print(head(df))
print(summary(df[,as.numeric(JUNITSIZE)]))

