# processAttom_Minneapolis.R
# Tom Davidoff
# Purpose: Pricing slopes by ZIP for Minneapolis/Hennepin County (FIPS 27053)
# Adapted from Portland code

library(data.table)
library(arrow)
library(fixest)

options(timeout = 600)

# ==========================================
# 1) LOAD SALES (27053.txt) - Hennepin County, MN
# ==========================================
message("Reading Sales data...")

raw_txt <- readLines("~/OneDrive - UBC/dataProcessed/27053.txt", warn = FALSE, encoding = "latin1")
header_line <- raw_txt[1]
data_body   <- raw_txt[-1]

header_names <- unlist(strsplit(header_line, "|", fixed = TRUE))
header_names <- make.names(tolower(header_names))

message("Splitting pipes...")
dtSales <- data.table(raw = data_body)
dtSales <- dtSales[, tstrsplit(raw, "|", fixed = TRUE, fill = NA)]

names(dtSales)[1:length(header_names)] <- header_names
setnames(dtSales, "X.attom.id.", "attom_id", skip_absent = TRUE)
setnames(dtSales, "transferamount", "price", skip_absent = TRUE)
setnames(dtSales, "transactiondate", "date", skip_absent = TRUE)

# normalize names after setnames
setnames(dtSales, tolower(make.names(names(dtSales))))

# types
dtSales[, attom_id := as.character(attom_id)]
dtSales[, price := as.numeric(price)]
dtSales[, date  := as.IDate(date)]
dtSales[,yearMonth := format(date, "%Y-%m")]

# filter years - around 2018 zoning change
# Minneapolis 2040 plan adopted Dec 2018, implemented Jan 2020
# Adjust as needed for your research question
dtSales <- dtSales[year(date) >= 2016 & year(date) <= 2020]

# ZIP from sales (clean)
if (!"propertyaddresszip" %in% names(dtSales)) stop("propertyaddresszip not found in dtSales.")
dtSales[, zip := gsub("[^0-9]", "", as.character(propertyaddresszip))]
dtSales[nchar(zip) == 9, zip := substr(zip, 1, 5)]
dtSales[zip == "", zip := NA_character_]

# OPTIONAL: Filter to Minneapolis-area ZIPs only (554xx range)
# Uncomment if you want to restrict to Minneapolis proper
# dtSales <- dtSales[grepl("^554", zip)]

message("Sales rows after date filter: ", nrow(dtSales))

# ==========================================
# 2) LOAD ASSESSOR (sqft) from Parquet - Minnesota
# ==========================================
message("Opening Assessor Parquet...")
ds <- open_dataset("~/OneDrive - UBC/dataProcessed/AH_state_MN.parquet")

# Pull only what we need - Hennepin County FIPS muni code is "053"
scanner <- ds$NewScan()$
  Filter(Expression$field_ref("MM_FIPS_MUNI_CODE") == "053")$
  Project(c("[ATTOM ID]", "SA_FIN_SQFT_TOT","SA_LOTSIZE","SA_YR_BLT","USE_CODE_STD"))$
  Finish()

dtAssessor <- as.data.table(scanner$ToTable())
setnames(dtAssessor, c("[ATTOM ID]", "SA_FIN_SQFT_TOT","SA_LOTSIZE","SA_YR_BLT","USE_CODE_STD"), c("attom_id", "sqft","lotSize","yearBuilt","useCode"))

dtAssessor[, attom_id := as.character(attom_id)]
dtAssessor[, sqft := as.numeric(sqft)]
dtAssessor[, lotSize := as.numeric(lotSize)]
dtAssessor[, yearBuilt := as.integer(yearBuilt)]

# Deduplicate: multiple records per ATTOM id
dtAssessor[,randomNum := runif(.N)]
dtAssessor[,maxRandom := max(randomNum+sqft, na.rm=TRUE), by=attom_id]
dtAssessor_Final <- dtAssessor[randomNum+sqft == maxRandom]

dtAssessor_Final[is.infinite(sqft), sqft := NA_real_]

message("Unique assessor ATTOM IDs: ", uniqueN(dtAssessor_Final$attom_id))

# ==========================================
# 3) MERGE + SLOPE ESTIMATION BY ZIP
# ==========================================
message("Merging sales + assessor...")
dtFinal <- merge(dtSales, dtAssessor_Final, by = "attom_id", all = FALSE)
print(table(dtFinal$useCode))
dtFinal <- dtFinal[useCode=="RSFR"]

# clean
dtFinal <- dtFinal[!is.na(price) & !is.na(sqft) & !is.na(zip)]
dtFinal <- dtFinal[price > 100000 & sqft > 400]

dtFinal[, ppsf := price / sqft]

# keep plausible ZIPs
dtFinal <- dtFinal[nchar(zip) == 5]

# Filter to ZIPs with enough observations to estimate slopes
# This avoids collinearity issues from ZIPs with only 1-2 sales
min_obs <- 20  # adjust as needed
zip_counts <- dtFinal[, .N, by = zip]
good_zips <- zip_counts[N >= min_obs, zip]
dtFinal <- dtFinal[zip %in% good_zips]

message("Final rows for regression: ", nrow(dtFinal),
        " | ZIPs: ", uniqueN(dtFinal$zip))

# Slope by ZIP: ZIP-specific intercept and ZIP-specific slope on sqft
# (ppsf = a_zip + b_zip * sqft + error)
dtFinal[, zip := as.factor(zip)]
dtFinal[,lppsf := log(ppsf)]
dtFinal[,lsqft := log(sqft)]
m <- feols(lppsf ~ 0 + zip + zip:lsqft + log(lotSize) |yearMonth, data = dtFinal)
print(summary(m))

# ==========================================
# 4) EXTRACT SLOPES ROBUSTLY (no broom)
# ==========================================
b <- coef(m)
keep <- grepl(":lsqft$", names(b))

dtSlopes <- data.table(
  zip = sub(":lsqft$", "", sub("^zip", "", names(b)[keep])),
  slope = unname(b[keep])
)

zipStats <- dtFinal[, .(N = .N, mean_ppsf = mean(ppsf, na.rm = TRUE)), by = zip]

# add sample size per ZIP (useful for filtering)
dtSlopes <- merge(dtSlopes, zipStats, by = "zip", all.x = TRUE)
setorder(dtSlopes, -N)

saveRDS(dtSlopes, "~/OneDrive - UBC/dataProcessed/minneapolis_attom_slopes_by_zip.rds")

message("Saved slopes: OneDrive - UBC/dataProcessed/minneapolis_attom_slopes_by_zip.rds")
print(dtSlopes[1:15])
print(cor(dtSlopes[,.(slope, mean_ppsf)], use = "complete.obs"))
print(cor(dtSlopes[N>median(N),.(slope, mean_ppsf)], use = "complete.obs"))
print(cor(dtSlopes[N>quantile(N,.75),.(slope, mean_ppsf)], use = "complete.obs"))
