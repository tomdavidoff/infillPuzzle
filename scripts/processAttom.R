# processAttom.R
# Tom Davidoff
# Purpose: Pricing slopes by ZIP for Portland or Minneapolis
# Unified script with city toggle

library(data.table)
library(arrow)
library(fixest)

options(timeout = 600)

# ==========================================
# CONFIGURATION - SET CITY HERE
# ==========================================
# CITY <- "portland"
 CITY <- "minneapolis"
# ==========================================

# City-specific parameters
if (CITY == "portland") {
  SALES_FILE <- "~/OneDrive - UBC/dataProcessed/41051.txt"
  PARQUET_FILE <- "~/OneDrive - UBC/dataProcessed/AH_state_OR.parquet"
  FIPS_MUNI <- "051"
  OUTPUT_FILE <- "~/OneDrive - UBC/dataProcessed/portland_attom_slopes_by_zip.rds"
  YEAR_MIN <- 2018
  YEAR_MAX <- 2021
  MIN_SQFT <- 1200
  MIN_OBS <- 20
} else if (CITY == "minneapolis") {
  SALES_FILE <- "~/OneDrive - UBC/dataProcessed/27053.txt"
  PARQUET_FILE <- "~/OneDrive - UBC/dataProcessed/AH_state_MN.parquet"
  FIPS_MUNI <- "053"
  OUTPUT_FILE <- "~/OneDrive - UBC/dataProcessed/minneapolis_attom_slopes_by_zip.rds"
  YEAR_MIN <- 2016
  YEAR_MAX <- 2020
  MIN_SQFT <- 400
  MIN_OBS <- 20
} else {
  stop("CITY must be 'portland' or 'minneapolis'")
}

message("=== Processing ", toupper(CITY), " ===")

# ==========================================
# 1) LOAD SALES
# ==========================================
message("Reading Sales data from: ", SALES_FILE)

raw_txt <- readLines(SALES_FILE, warn = FALSE, encoding = "latin1")
header_line <- raw_txt[1]
data_body <- raw_txt[-1]

header_names <- unlist(strsplit(header_line, "|", fixed = TRUE))
header_names <- make.names(tolower(header_names))

message("Splitting pipes...")
dtSales <- data.table(raw = data_body)
dtSales <- dtSales[, tstrsplit(raw, "|", fixed = TRUE, fill = NA)]

names(dtSales)[seq_along(header_names)] <- header_names
setnames(dtSales, "X.attom.id.", "attom_id", skip_absent = TRUE)
setnames(dtSales, "transferamount", "price", skip_absent = TRUE)
setnames(dtSales, "transactiondate", "date", skip_absent = TRUE)

# normalize names
setnames(dtSales, tolower(make.names(names(dtSales))))

# types
dtSales[, attom_id := as.character(attom_id)]
dtSales[, price := as.numeric(price)]
dtSales[, date := as.IDate(date)]
dtSales[, yearMonth := format(date, "%Y-%m")]

# filter years
dtSales <- dtSales[year(date) >= YEAR_MIN & year(date) <= YEAR_MAX]

# ZIP from sales (clean)
if (!"propertyaddresszip" %in% names(dtSales)) {
  stop("propertyaddresszip not found in dtSales.")
}
dtSales[, zip := gsub("[^0-9]", "", as.character(propertyaddresszip))]
dtSales[nchar(zip) == 9, zip := substr(zip, 1, 5)]
dtSales[zip == "", zip := NA_character_]

message("Sales rows after date filter: ", nrow(dtSales))

# ==========================================
# 2) LOAD ASSESSOR (sqft) from Parquet
# ==========================================
message("Opening Assessor Parquet: ", PARQUET_FILE)
ds <- open_dataset(PARQUET_FILE)

scanner <- ds$NewScan()$
  Filter(Expression$field_ref("MM_FIPS_MUNI_CODE") == FIPS_MUNI)$
  Project(c("[ATTOM ID]", "SA_FIN_SQFT_TOT", "SA_LOTSIZE", "SA_YR_BLT", "USE_CODE_STD"))$
  Finish()

dtAssessor <- as.data.table(scanner$ToTable())
setnames(dtAssessor,
         c("[ATTOM ID]", "SA_FIN_SQFT_TOT", "SA_LOTSIZE", "SA_YR_BLT", "USE_CODE_STD"),
         c("attom_id", "sqft", "lotSize", "yearBuilt", "useCode"))

dtAssessor[, attom_id := as.character(attom_id)]
dtAssessor[, sqft := as.numeric(sqft)]
dtAssessor[, lotSize := as.numeric(lotSize)]
dtAssessor[, yearBuilt := as.integer(yearBuilt)]

# Deduplicate: keep one record per attom_id
# Use row index for deterministic selection (first occurrence with max sqft, ties broken by row order)
dtAssessor[, sqft_safe := fifelse(is.na(sqft), -Inf, sqft)]
dtAssessor[, row_id := .I]
dtAssessor[, max_sqft := max(sqft_safe), by = attom_id]
dtAssessor_Final <- dtAssessor[sqft_safe == max_sqft, .SD[1], by = attom_id]
dtAssessor_Final[, c("sqft_safe", "row_id", "max_sqft") := NULL]

dtAssessor_Final[is.infinite(sqft), sqft := NA_real_]

message("Unique assessor ATTOM IDs: ", uniqueN(dtAssessor_Final$attom_id))

# ==========================================
# 3) MERGE + SLOPE ESTIMATION BY ZIP
# ==========================================
message("Merging sales + assessor...")
dtFinal <- merge(dtSales, dtAssessor_Final, by = "attom_id", all = FALSE)

message("Use codes in merged data:")
print(table(dtFinal$useCode))

# Filter to single-family residential
dtFinal <- dtFinal[useCode == "RSFR"]

# Clean: require price, sqft, zip; apply minimum thresholds
dtFinal <- dtFinal[!is.na(price) & !is.na(sqft) & !is.na(zip)]
dtFinal <- dtFinal[price > 100000 & sqft > MIN_SQFT]

# Compute price per sqft (keep original for summary stats)
dtFinal[, ppsf := price / sqft]

# Keep plausible ZIPs (5 digits)
dtFinal <- dtFinal[nchar(zip) == 5]

# Filter to ZIPs with enough observations
zip_counts <- dtFinal[, .N, by = zip]
good_zips <- zip_counts[N >= MIN_OBS, zip]
dtFinal <- dtFinal[zip %in% good_zips]

message("Final rows for regression: ", nrow(dtFinal),
        " | ZIPs: ", uniqueN(dtFinal$zip))

# Log transforms for regression (new columns to preserve originals)
dtFinal[, zip := as.factor(zip)]
dtFinal[, lppsf := log(ppsf)]
dtFinal[, lsqft := log(sqft)]
dtFinal[, llotSize := log(lotSize)]

# Regression: ZIP-specific intercept and ZIP-specific slope on log(sqft)
m <- feols(lppsf ~ 0 + zip + zip:lsqft + llotSize | yearMonth, data = dtFinal)
print(summary(m))

# ==========================================
# 4) EXTRACT SLOPES
# ==========================================
b <- coef(m)
keep <- grepl(":lsqft$", names(b))

dtSlopes <- data.table(
  zip = sub(":lsqft$", "", sub("^zip", "", names(b)[keep])),
  slope = unname(b[keep])
)

zipStats <- dtFinal[, .(N = .N, mean_ppsf = mean(ppsf, na.rm = TRUE)), by = zip]
dtSlopes <- merge(dtSlopes, zipStats, by = "zip", all.x = TRUE)
setorder(dtSlopes, -N)

# ==========================================
# 5) SAVE & SUMMARY
# ==========================================
saveRDS(dtSlopes, OUTPUT_FILE)
message("Saved slopes to: ", OUTPUT_FILE)

cat("\n=== RESULTS FOR", toupper(CITY), "===\n")
print(head(dtSlopes, 15))

cat("\nCorrelation between slope and mean PPSF:\n")
print(cor(dtSlopes[, .(slope, mean_ppsf)], use = "complete.obs"))

cat("\nCorrelation (ZIPs above median N):\n")
print(cor(dtSlopes[N > median(N), .(slope, mean_ppsf)], use = "complete.obs"))

cat("\nCorrelation (ZIPs above 75th percentile N):\n")
print(cor(dtSlopes[N > quantile(N, 0.75), .(slope, mean_ppsf)], use = "complete.obs"))
