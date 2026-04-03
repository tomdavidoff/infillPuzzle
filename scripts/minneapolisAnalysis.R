# minneapolisAnalysis.R
# Analyze the ppsf slope in sf and infill, as in Vancouver
# Tom Davidoff
# Adapted from Portland code for Minneapolis 2040 analysis

library(data.table)
library(fixest)
library(ggplot2)
library(xtable)

# 1. Load ------------------------------------------------------------------
permits <- readRDS("~/DropboxExternal/dataProcessed/minneapolis_tract_propensity.rds")
slopes  <- readRDS("~/DropboxExternal/dataProcessed/minneapolis_slopes.rds")

setDT(permits); setDT(slopes)

# 2. Align -----------------------------------------------------------------
# Ensure ZIPs are 5-character strings and types match
print(head(permits))
print(head(slopes))
print(table(slopes$tract))
# last 5 digits
permits[,tractNumeric:=as.numeric(substring(geo_id, 6, 11))] # Extract numeric part of tract, last 5 digits
slopes[,tractNumeric:=as.numeric(tract)]

#permits[, zip := sprintf("%05s", as.character(zip))]
#slopes[,  zip := sprintf("%05s", as.character(zip))]

# Merge on Zip
dt <- merge(permits, slopes, by = "tractNumeric")
dt[,zone:=land_use_c]
print(summary(dt))
print(nrow(dt))
print(head(dt))
print(summary(lm(propensity ~ (lppsf)*zone, dt)))
print(summary(lm(propensity ~ (slope)*zone, dt)))
print(summary(lm(propensity ~ (slope)+lppsf+zone, dt)))
print(dt[,.(mp=mean(propensity),mpl=mean(lppsf),mslope=mean(slope),msqft=mean(medianSqft)),by=zone])
for (z in unique(dt[,zone])) {
	print(z)
	print(summary(lm(propensity ~lppsf, dt[zone==z ])))
	print(summary(lm(propensity ~slope+lppsf, dt[zone==z ])))
	print(summary(lm(propensity ~slope+lppsf, dt[zone==z & slope %between% c(-1,0)])))
}
q("no")




# 2. Align -----------------------------------------------------------------
# Ensure ZIPs are 5-character strings and types match
print(head(permits))
print(head(slopes))


setnames(permits, "geo_id", "zip")
permits[, zip := sprintf("%05s", as.character(zip))]
slopes[,  zip := sprintf("%05s", as.character(zip))]

# Merge on Zip
dt <- merge(permits, slopes, by = "zip")
print(head(dt))
print(summary(dt))
permits[,tractNumeric:=as.numeric(substring(geo_id, 7, 11))] # Extract numeric part of tract, last 5 digits
slopes[,tractNumeric:=as.numeric(tract)]

#permits[, zip := sprintf("%05s", as.character(zip))]
#slopes[,  zip := sprintf("%05s", as.character(zip))]

# Merge on Zip
dt <- merge(permits, slopes, by = "tractNumeric")
print(nrow(dt))
print(head(dt))
print(summary(lm(propensity ~ (lppsf)*zone, dt)))
print(summary(lm(propensity ~ (slope)*zone, dt)))
print(summary(lm(propensity ~ (slope)+lppsf+zone, dt)))
print(dt[,.(mp=mean(propensity),mpl=mean(lppsf)),by=zone])
for (z in unique(dt[,zone])) {
	print(z)
	print(summary(lm(propensity ~slope+lppsf, dt[zone==z])))
}
q("no")


# 3. Clean -----------------------------------------------------------------
# Convert slope to numeric (it was <char>) and take absolute value
# (A larger negative slope = a steeper 'size discount')
dt[, price_level := as.numeric(mean_ppsf)]

# Focus on Zips with enough data to be credible
MINOBS <- 2
dt <- dt[N >= MINOBS & total_lots_active >= MINOBS]

dtCensus <- fread("~/OneDrive - UBC/dataRaw/ACSDT5Y2024/ACSDT5Y2024.B19013-Data.csv", skip=1,select = c("Estimate!!Median household income in the past 12 months (in 2024 inflation-adjusted dollars)","Geographic Area Name"),header=TRUE)
print(head(dtCensus))
setnames(dtCensus, old=c("Estimate!!Median household income in the past 12 months (in 2024 inflation-adjusted dollars)"), new=c("medianIncome"))
dtCensus[, medianIncome := log(as.numeric(medianIncome))]
setnames(dtCensus,"Geographic Area Name","zip")
dtCensus[,zip:=substring(zip,7,11)]
dt <- merge(dt,dtCensus,by="zip")
print(table(dt[,land_use_c]))

print(xtable(cor(dt[, .(slope, price_level, propensity,medianIncome)])))
print(xtable(cor(dt[land_use_c=="UN3", .(slope, price_level, propensity,medianIncome)])))
print(xtable(cor(dt[land_use_c!="UN3", .(slope, price_level, propensity,medianIncome)])))
print(nrow(dt[land_use_c=="UN3"]))
print(nrow(dt[land_use_c="UN1"]))
print(nrow(dt[land_use_c="UN2"]))


# 4. Model -----------------------------------------------------------------
# Does the price-size gradient predict development propensity?
# we use robust standard errors (hc1)
m1 <- feols(propensity ~ slope, dt, vcov = "hc1")
m2 <- feols(propensity ~ slope + price_level, dt, vcov = "hc1")
m3 <- feols(propensity ~ price_level, dt, vcov = "hc1")
m4 <- feols(propensity ~ price_level + medianIncome, dt, vcov = "hc1")

# Zen summary: No complex tables, just the facts
print(etable(m1, m2, m3,m4))

# 5. Plot ------------------------------------------------------------------
ggplot(dt, aes(slope, propensity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", fill = "gray80") +
  theme_minimal() +
  labs(x = "Price-Size Gradient (Absolute)", 
       y = "Permit Propensity",
       title = "Minneapolis Supply Response")

ggsave("~/OneDrive - UBC/dataProcessed/minneapolis_supply_response.png", 
       width = 8, height = 6)

# 6. Export ----------------------------------------------------------------
saveRDS(dt, "~/OneDrive - UBC/dataProcessed/minneapolis_analysis_final.rds")
