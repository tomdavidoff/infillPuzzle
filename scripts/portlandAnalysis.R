# portlandAnalysis.R
# Analyze the ppsf slope in sf and infill, as in Vancouver
# Tom Davidoff
# 12/26/25


library(data.table)
library(fixest)
library(ggplot2)

# 1. Load ------------------------------------------------------------------
permits <- readRDS("data/derived/portland_zip_propensity.rds")
slopes  <- readRDS("data/derived/portland_attom_slopes_by_zip.rds")

setDT(permits); setDT(slopes)

# 2. Align -----------------------------------------------------------------
# Ensure ZIPs are 5-character strings and types match
print(head(permits))
print(head(slopes))
setnames(permits,"geo_id","zip")
permits[, zip := sprintf("%05s", as.character(zip))]
slopes[,  zip := sprintf("%05s", as.character(zip))]

# Merge on Zip
dt <- merge(permits, slopes, by = "zip")

# 3. Clean -----------------------------------------------------------------
# Convert slope to numeric (it was <char>) and take absolute value
# (A larger negative slope = a steeper 'size discount')
dt[, size_slope := abs(as.numeric(slope))]
dt[, price_level := as.numeric(mean_ppsf)]

# Focus on Zips with enough data to be credible
MINOBS <- 50
dt <- dt[N >= MINOBS & total_lots_active>=MINOBS]
print(cor(dt[,.(slope,price_level)]))

# 4. Model -----------------------------------------------------------------
# Does the price-size gradient predict development propensity?
# we use robust standard errors (hc1)
m1 <- feols(propensity ~ size_slope, dt, vcov = "hc1")
m2 <- feols(propensity ~ size_slope + price_level, dt, vcov = "hc1")
m3 <- feols(propensity ~ price_level, dt, vcov = "hc1")

# Zen summary: No complex tables, just the facts
print(etable(m1, m2,m3))

# 5. Plot ------------------------------------------------------------------
ggplot(dt, aes(size_slope, propensity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", fill = "gray80") +
  theme_minimal() +
  labs(x = "Price-Size Gradient (Absolute)", 
       y = "Permit Propensity",
       title = "Portland Supply Response")

# 6. Export ----------------------------------------------------------------
saveRDS(dt, "data/derived/portland_analysis_final.rds")
