# processAttomLean.R
# Tom Davidoff
# Purpose: Per-tract entry-level single-family price and price-size elasticity,
#          for Portland and Minneapolis (Attom), in the spirit of the Vancouver
#          level-at-LREF + elasticity estimator.
#
# Per tract, two SEPARATE estimates:
#   entryPrice  = Epanechnikov-weighted mean of price, weight = K(|lsqft - LREF|/H),
#                 LREF = log(citywide 20th-pctile sqft). Near-entry-size homes dominate;
#                 homes >H (log) from entry size get zero weight. No slope, no space kernel.
#   elasticity  = plain unweighted per-tract slope from lm(log(price) ~ lsqft + llotSize).
# The size weighting touches ONLY the level; the elasticity uses full size variation.
# One knob: H (log-sqft bandwidth), noted inline.

library(data.table)
library(duckdb)

NYEARS <- 10   # slopes need sample; Portland behaves oddly under 5
MIN_OBS <- 15

CITIES <- list(
	portland = list(
		SALES_FILE            = "~/DropboxExternal/dataProcessed/41051.txt",
		ASSESSOR_PARQUET      = "~/DropboxExternal/dataProcessed/AH_state_OR.parquet",
		FLAT_ASSESSOR_PARQUET = "~/DropboxExternal/dataProcessed/by_property_states/tax_assessor_state_OR.parquet",
		FIPS_MUNI = "051",
		LON_RANGE = c(-124.0, -121.0),
		LAT_RANGE = c(  44.0,   47.0),
		LASTYEAR  = 2021,
		CITYNAME  = "PORTLAND",
		ZIP_START = "97",
		MIN_SQFT  = 1200),
	minneapolis = list(
		SALES_FILE            = "~/DropboxExternal/dataProcessed/27053.txt",
		ASSESSOR_PARQUET      = "~/DropboxExternal/dataProcessed/AH_state_MN.parquet",
		FLAT_ASSESSOR_PARQUET = "~/DropboxExternal/dataProcessed/by_property_states/tax_assessor_state_MN.parquet",
		FIPS_MUNI = "053",
		LON_RANGE = c(-95.5, -91.0),
		LAT_RANGE = c( 43.0,  47.0),
		LASTYEAR  = 2020,
		CITYNAME  = "MINNEAPOLIS",
		ZIP_START = "55",
		MIN_SQFT  = 400)
)

con <- dbConnect(duckdb())

for (CITY_NAME in names(CITIES)) {
	cf <- CITIES[[CITY_NAME]]
	cf$YEARS <- (cf$LASTYEAR - NYEARS + 1):cf$LASTYEAR
	message("\n=== ", toupper(CITY_NAME), " ===")

	# --- Sales: pipe-delimited text ---
	raw <- readLines(cf$SALES_FILE, warn = FALSE, encoding = "latin1")
	hdr <- make.names(tolower(unlist(strsplit(raw[1], "|", fixed = TRUE))))
	dtS <- data.table(raw = raw[-1])[, tstrsplit(raw, "|", fixed = TRUE, fill = NA)]
	setnames(dtS, seq_along(hdr), hdr)
	dtS[, attom_id  := as.character(X.attom.id.)]
	dtS[, price     := as.numeric(transferamount)]
	dtS[, sale_date := as.IDate(transactiondate)]
	dtS[, zip       := substr(gsub("[^0-9]", "", as.character(propertyaddresszip)), 1, 5)]
	dtS <- dtS[price > 1e5 & year(sale_date) %in% cf$YEARS,
	           .(attom_id, price, sale_date, zip)]

	# --- Assessor: sqft + lot, latest assessment year per property ---
	dtA <- as.data.table(dbGetQuery(con, sprintf(
		"SELECT CAST(\"[ATTOM ID]\" AS VARCHAR) AS attom_id,
		        CAST(SA_FIN_SQFT_TOT AS DOUBLE) AS sqft,
		        CAST(SA_LOTSIZE      AS DOUBLE) AS lotSize,
		        ASSR_YEAR
		 FROM read_parquet('%s')
		 WHERE MM_FIPS_MUNI_CODE = '%s'
		   AND USE_CODE_STD = 'RSFR'
		   AND sqft > %d",
		cf$ASSESSOR_PARQUET, cf$FIPS_MUNI, cf$MIN_SQFT)))
	dtA[, year    := as.numeric(substr(ASSR_YEAR, 1, 4))]
	dtA[, maxYear := max(year, na.rm = TRUE), by = attom_id]
	dtA <- dtA[year == maxYear]

	# --- Geo: tract + coordinates (pure lookup; structure filter is RSFR on assessor side) ---
	dtT <- as.data.table(dbGetQuery(con, sprintf(
		"SELECT CAST(\"[ATTOM ID]\" AS VARCHAR) AS attom_id,
		        CensusTract,
		        CAST(PropertyLatitude  AS DOUBLE) AS lat,
		        CAST(PropertyLongitude AS DOUBLE) AS lon
		 FROM read_parquet('%s')
		 WHERE PropertyAddressCity = '%s'
		   AND lon BETWEEN %f AND %f
		   AND lat BETWEEN %f AND %f",
		cf$FLAT_ASSESSOR_PARQUET, cf$CITYNAME,
		cf$LON_RANGE[1], cf$LON_RANGE[2],
		cf$LAT_RANGE[1], cf$LAT_RANGE[2])))

	# --- Join sales x assessor x geo ---
	dt <- dtS[dtA, on = "attom_id", nomatch = 0L][dtT, on = "attom_id", nomatch = 0L]
	if (nrow(dt) == 0L) { message("No records for ", CITY_NAME); next }

	dt[, lsqft    := log(sqft)]
	dt[, llotSize := log(pmax(as.numeric(lotSize), 1))]
	dt <- dt[substr(zip, 1, 2) == cf$ZIP_START]

	# --- Per-tract entry price (size-kernel weighted mean) + elasticity (plain slope) ---
	# Confine to the final analysis set (single-detached, adequate-n tracts) BEFORE q20,
	# so LREF is the 20th pctile of exactly the homes the weighted mean runs over.
	tractList <- dt[, .N, by = CensusTract][N >= MIN_OBS, CensusTract]
	dt <- dt[CensusTract %in% tractList]

	LREF <- log(quantile(dt$sqft, 0.20, na.rm = TRUE))   # entry size (log sqft), post-restriction
	H    <- 0.4                                          # size bandwidth, log-sqft; ~e^0.4-1 = +49% -> zero weight
	epan <- function(u) 0.75 * (1 - u * u) * (abs(u) < 1)

	dtSlopes <- rbindlist(lapply(tractList, function(tr) {
		dd <- dt[CensusTract == tr]

		# entry price: Epanechnikov weight on log-sqft distance from citywide q20
		w  <- epan((dd$lsqft - LREF) / H)
		sw <- sum(w)
		entryPrice <- if (sw > 0) sum(w * dd$price) / sw else NA_real_   # all homes beyond H -> NA

		# elasticity: separate, unweighted, full size range
		elasticity <- coef(lm(log(price) ~ lsqft + llotSize, data = dd))[["lsqft"]]

		data.table(tract      = as.character(tr),
		           entryPrice = entryPrice,
		           elasticity = elasticity,
		           nEntry     = sum(w > 0),   # homes actually informing the entry price
		           n          = nrow(dd))
	}))

	message(CITY_NAME, ": ", nrow(dtSlopes), " tracts")
	print(summary(dtSlopes[, .(entryPrice, elasticity, nEntry, n)]))
	saveRDS(dtSlopes, sprintf("~/DropboxExternal/dataProcessed/%s_entryElast.rds", CITY_NAME))
}

dbDisconnect(con)
q("no")
