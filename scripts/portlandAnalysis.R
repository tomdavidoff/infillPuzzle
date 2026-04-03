# portlandAnalysis.R
# Analyze the ppsf slope in sf and infill, as in Vancouver
# Tom Davidoff
# 12/26/25


library(data.table)
library(fixest)
library(ggplot2)

# 1. Load ------------------------------------------------------------------
permits <- readRDS("~/DropboxExternal/dataProcessed/portland_tract_propensity.rds")
slopes  <- readRDS("~/DropboxExternal/dataProcessed/portland_slopes.rds")
print(nrow(permits))
print(nrow(slopes))

setDT(permits); setDT(slopes)

# 2. Align -----------------------------------------------------------------
# Ensure ZIPs are 5-character strings and types match
print(head(permits))
print(head(slopes))
print(table(slopes$tract))
# last 5 digits
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
print(dt[,.(mp=mean(propensity),mpl=mean(lppsf),msqft=mean(medianSqft)),by=zone])
for (z in unique(dt[,zone])) {
	print(z)
	print(summary(dt[zone==z,.(propensity, slope, lppsf)]))
	print(summary(lm(propensity ~lppsf, dt[zone==z])))
	print(summary(lm(propensity ~slope+lppsf, dt[zone==z])))
	print(summary(lm(propensity ~slope+lppsf, dt[zone==z & slope <0 & slope > -1])))
}
q("no")

# 3. Clean -----------------------------------------------------------------
# Convert slope to numeric (it was <char>) and take absolute value
# (A larger negative slope = a steeper 'size discount')
dt[, size_slope := abs(as.numeric(slope))]
dt[, price_level := as.numeric(mean_ppsf)]

# Focus on Zips with enough data to be credible
MINOBS <- 50
dt <- dt[N >= MINOBS & total_lots_active>=MINOBS]
dtCensus <- fread("~/DropboxExternal/dataRaw/ACSDT5Y2024/ACSDT5Y2024.B19013-Data.csv", skip=1,select = c("Estimate!!Median household income in the past 12 months (in 2024 inflation-adjusted dollars)","Geographic Area Name"),header=TRUE)
print(head(dtCensus))
setnames(dtCensus, old=c("Estimate!!Median household income in the past 12 months (in 2024 inflation-adjusted dollars)"), new=c("medianIncome"))
dtCensus[, medianIncome := log(as.numeric(medianIncome))]
setnames(dtCensus,"Geographic Area Name","zip")
dtCensus[,zip:=substring(zip,7,11)]

print(head(dtCensus))
print(head(dt))
dt <- merge(dt,dtCensus,by="zip",all.x=TRUE)
print(nrow(dt))
print(head(dt))
print(cor(dt[,.(slope,price_level,propensity,medianIncome)],))
print(table(dt[,zone]))
print(cor(dt[zone %chin% c("R2.5","R5"),.(slope,price_level,propensity,medianIncome)],))
print(cor(dt[zone %chin% c("R2.5"),.(slope,price_level,propensity,medianIncome)],))
print(cor(dt[zone %chin% c("R5"),.(slope,price_level,propensity,medianIncome)],))
print(dt[,.(mp=mean(propensity),mpl=mean(price_level)),by=zone])
print(dt[zone=="R2.5" | zone=="R5"])
print(dt[,sum(infill_lot_count),by=zone])



# 4. Model -----------------------------------------------------------------
# Does the price-size gradient predict development propensity?
# we use robust standard errors (hc1)
m1 <- feols(propensity ~ slope, dt, vcov = "hc1")
m2 <- feols(propensity ~ slope + medianIncome, dt, vcov = "hc1")
m3 <- feols(propensity ~ log(price_level), dt, vcov = "hc1")
m4 <- feols(propensity ~ log(price_level) + slope, dt[zone %chin% c("R2.5","R5")], vcov = "hc1")
m5 <- feols(propensity ~ log(price_level) + slope|zone, dt[zone %chin% c("R2.5","R5")], vcov = "hc1")

# Zen summary: No complex tables, just the facts
print(etable(m1, m2,m3,m4,m5))

# 5. Plot ------------------------------------------------------------------
ggplot(dt, aes(size_slope, propensity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", fill = "gray80") +
  theme_minimal() +
  labs(x = "Price-Size Gradient (Absolute)", 
       y = "Permit Propensity",
       title = "Portland Supply Response")

# 6. Export ----------------------------------------------------------------
saveRDS(dt, "~/OneDrive - UBC/dataProcessed/portland_analysis_final.rds")
