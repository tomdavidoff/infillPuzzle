# processAttom.R
# processAttom_zip.R
# Tom Davidoff
# 12/26/25
# Purpose: Pricing slopes by ZIP for Portland/Multnomah (FIPS muni 051)
# started with google AI, this version ChatGPT

library(data.table)
library(arrow)
library(fixest)

options(timeout = 600)

# ==========================================
# 1) LOAD SALES (41051.txt)
# ==========================================
message("Reading Sales data...")

raw_txt <- readLines("~/OneDrive - UBC/dataProcessed/41051.txt", warn = FALSE, encoding = "latin1")
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

# filter years
dtSales <- dtSales[year(date) >= 2018 & year(date) <= 2021]

# ZIP from sales (clean)
if (!"propertyaddresszip" %in% names(dtSales)) stop("propertyaddresszip not found in dtSales.")
dtSales[, zip := gsub("[^0-9]", "", as.character(propertyaddresszip))]
dtSales[nchar(zip) == 9, zip := substr(zip, 1, 5)]
dtSales[zip == "", zip := NA_character_]

message("Sales rows after date filter: ", nrow(dtSales))

# ==========================================
# 2) LOAD ASSESSOR (sqft) from Parquet
# ==========================================
message("Opening Assessor Parquet...")
ds <- open_dataset("~/OneDrive - UBC/dataProcessed/AH_state_OR.parquet")

# Pull only what we need
scanner <- ds$NewScan()$
  Filter(Expression$field_ref("MM_FIPS_MUNI_CODE") == "051")$
  Project(c("[ATTOM ID]", "SA_FIN_SQFT_TOT"))$
  Finish()

dtAssessor <- as.data.table(scanner$ToTable())
setnames(dtAssessor, c("[ATTOM ID]", "SA_FIN_SQFT_TOT"), c("attom_id", "sqft"))

dtAssessor[, attom_id := as.character(attom_id)]
dtAssessor[, sqft := as.numeric(sqft)]

# Deduplicate: multiple records per ATTOM id
dtAssessor_Final <- dtAssessor[, .(
  sqft = max(sqft, na.rm = TRUE)
), by = attom_id]

dtAssessor_Final[is.infinite(sqft), sqft := NA_real_]

message("Unique assessor ATTOM IDs: ", uniqueN(dtAssessor_Final$attom_id))

# ==========================================
# 3) MERGE + SLOPE ESTIMATION BY ZIP
# ==========================================
message("Merging sales + assessor...")
dtFinal <- merge(dtSales, dtAssessor_Final, by = "attom_id", all = FALSE)

# clean
dtFinal <- dtFinal[!is.na(price) & !is.na(sqft) & !is.na(zip)]
MINSQFT <- 1200 # want mostly houses
dtFinal <- dtFinal[price > 100000 & sqft > MINSQFT]
# transform everything to logs
dtFinal[, ppsf := price / sqft]
dtFinal[,ppsf:=log(ppsf)]
dtFinal[,sqft:=log(sqft)]


# keep plausible ZIPs
dtFinal <- dtFinal[nchar(zip) == 5]

message("Final rows for regression: ", nrow(dtFinal),
        " | ZIPs: ", uniqueN(dtFinal$zip))

# Slope by ZIP: ZIP-specific intercept and ZIP-specific slope on sqft
# (ppsf = a_zip + b_zip * sqft + error)
dtFinal[, zip := as.factor(zip)]
m <- feols(ppsf ~ 0 + zip + zip:sqft, data = dtFinal)

# ==========================================
# 4) EXTRACT SLOPES ROBUSTLY (no broom)
# ==========================================
b <- coef(m)
keep <- grepl(":sqft$", names(b))

dtSlopes <- data.table(
  zip = sub(":sqft$", "", sub("^zip", "", names(b)[keep])),
  slope = unname(b[keep])
)

zipStats <- dtFinal[, .(N = .N, mean_ppsf = mean(ppsf, na.rm = TRUE)), by = zip]

# add sample size per ZIP (useful for filtering)
dtSlopes <- merge(dtSlopes, zipStats, by = "zip", all.x = TRUE)
setorder(dtSlopes, -N)

saveRDS(dtSlopes, "~/OneDrive - UBC/dataProcessed/portland_attom_slopes_by_zip.rds")

message("Saved slopes: data/derived/portland_attom_slopes_by_zip.rds")
print(dtSlopes[1:10])

