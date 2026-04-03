# processAttom.R
# Tom Davidoff
# Purpose: Pricing slopes by ZIP for Portland or Minneapolis
# Unified script with city toggle

library(data.table)
library(duckdb)
library(fixest)
library(ggplot2)

CITIES <- list(
	portland = list(
		SALES_FILE = "~/DropboxExternal/dataProcessed/41051.txt",
		ASSESSOR_PARQUET = "~/DropboxExternal/dataProcessed/AH_state_OR.parquet",
		TRACT_PARQUET = "~/DropboxExternal/dataProcessed/attomTract.parquet",
		FIPS_MUNI = "051",
		LON_RANGE = c(-124.0, -121.0),
		LAT_RANGE = c(44.0, 47.0),
		YEARS = 2018:2021,
		ZIP_START = "97",
		MIN_SQFT = 1200),
	minneapolis = list(
		SALES_FILE = "~/DropboxExternal/dataProcessed/27053.txt",
		ASSESSOR_PARQUET = "~/DropboxExternal/dataProcessed/AH_state_MN.parquet",
		TRACT_PARQUET = "~/DropboxExternal/dataProcessed/attomTract.parquet",
		FIPS_MUNI = "053",
		LON_RANGE = c(-95.5, -91.0),
		LAT_RANGE = c(43.0, 47.0),
		YEARS = 2016:2020,
		ZIP_START = "55",
		MIN_SQFT = 400
  )
)

MIN_OBS <- 20
con <- dbConnect(duckdb())

for (CITY_NAME in names(CITIES)) {
	cf <- CITIES[[CITY_NAME]]
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

	# --- Assessor: filtered parquet read via DuckDB ---
	dtA <- as.data.table(dbGetQuery(con, sprintf(
		"SELECT CAST(\"[ATTOM ID]\" AS VARCHAR) AS attom_id,
		SA_FIN_SQFT_1, SA_FIN_SQFT_2, SA_FIN_SQFT_3,SA_FIN_SQFT_4,
		CAST(SA_FIN_SQFT_TOT AS DOUBLE) AS sqft, CAST(SA_LOTSIZE AS DOUBLE) AS lotSize,
		ASSR_YEAR
		FROM read_parquet('%s')
		WHERE MM_FIPS_MUNI_CODE = '%s'
		AND USE_CODE_STD = 'RSFR'
		AND sqft > %d",
		cf$ASSESSOR_PARQUET, cf$FIPS_MUNI, cf$MIN_SQFT
	)))

	# --- Tracts: filtered parquet read via DuckDB ---
	dtT <- as.data.table(dbGetQuery(con, sprintf(
		"SELECT CAST(ATTOM_ID AS VARCHAR) AS attom_id,
		Latitude AS lat, Longitude AS lon, CensusTract
		FROM read_parquet('%s')
		WHERE Longitude BETWEEN %f AND %f
		AND Latitude  BETWEEN %f AND %f",
		cf$TRACT_PARQUET,
		cf$LON_RANGE[1], cf$LON_RANGE[2],
		cf$LAT_RANGE[1], cf$LAT_RANGE[2]
	)))

	# --- Join in data.table ---
	dtA[,year:=as.numeric(substr(ASSR_YEAR,1,4))]
	dtA[,maxYear:=max(year,na.rm=TRUE),by=attom_id]
	dtA <- dtA[year==maxYear]
	dt <- dtS[dtA, on = "attom_id", nomatch = 0L ][dtT, on = "attom_id", nomatch = 0L]

	if (nrow(dt) == 0L) { message("No records for ", CITY_NAME); next }

	dt[, lppsf     := log(price / sqft)]
	dt[,ppsf     := price / sqft]
	dt[, lsqft     := log(sqft)]
	dt[, llotSize  := log(pmax(as.numeric(lotSize), 1))]
	dt[, yearMonth := format(sale_date, "%Y-%m")]
	dt[, zip_n     := .N, by = zip]
	dt <- dt[zip_n >= MIN_OBS]
	# get quantiles of sqft#
	print(quantile(dt[, sqft], probs = seq(0,1,.1)))
	print("SQUARE FOOT WEIRD!")
	print(dt[1:20,.(SA_FIN_SQFT_1, SA_FIN_SQFT_2, SA_FIN_SQFT_3,SA_FIN_SQFT_4, sqft)])

	# --- Regression ---
	print(summary(dt))
	print(nrow(dt))
	#mz <- feols(lppsf ~ 0 + i(zip) + i(zip):lsqft + llotSize | yearMonth, data = dt[substring(zip, 1, 2) == cf$ZIP_START], cluster = "zip")
	mt <- feols(lppsf ~ 0 + i(CensusTract)+ i(CensusTract):lsqft + llotSize | yearMonth , data = dt[substring(zip,1,2)==cf$ZIP_START], cluster = "zip")
	#ml <- feols(ppsf ~ 0 + i(CensusTract) + i(CensusTract):sqft + llotSize | yearMonth, data = dt[substring(zip,1,2)==cf$ZIP_START], cluster = "zip")

	#print(summary(mt))
	#print(summary(ml))
	# bigger Adj R2 at tract level and way bigger in logs

	b <- coef(mt)
	keep <- grepl(":lsqft$", names(b))
	dtSlopes <- data.table(
	tract   = sub(":lsqft$", "", sub("^CensusTract", "", names(b)[keep])),
	slope = unname(b[keep])
	)
	dtSlopes[,tract:=gsub("::","",tract)]
	meanLPPSF <- dt[,.(lppsf=mean(lppsf,na.rm=TRUE),count=.N,medianSqft=median(sqft)),by=CensusTract]
	meanLPPSF[,tract := as.character(CensusTract)]
	print(head(dtSlopes))
	print(head(meanLPPSF))
	dtSlopes <- merge(dtSlopes, meanLPPSF[, .(tract, lppsf, count,medianSqft)], by = "tract")

	saveRDS(dtSlopes, sprintf("~/DropboxExternal/dataProcessed/%s_slopes.rds", CITY_NAME))
	# do a loess fit of price per square foot against square feet
	q99sf <- quantile(dt$sqft, 0.95, na.rm = TRUE)
	q01sf <- quantile(dt$sqft, 0.05, na.rm = TRUE)
	q99p <- quantile(dt$ppsf, 0.95, na.rm = TRUE)
	q01p <- quantile(dt$ppsf, 0.05, na.rm = TRUE)
	dt[,nZip:=.N,by=zip] # have to watch cross-zip big homes, big prices
	ggplot(dt[nZip==max(nZip) & sqft %between% c(q01sf,q99sf) & ppsf %between% c(q01p,q99p)], aes(x = sqft, y = price/sqft)) +
		geom_point(alpha = 0.1, size=.5,color="gray") +
		geom_smooth(method = "loess",color="blue",se=FALSE) +
		labs(title = paste("Price per Square Foot vs. Square Footage in", toupper(CITY_NAME)),
			 x = "Square Footage ",
			 y = "Price per Square Foot ") +
		theme_minimal()
	ggsave(sprintf("text/%sPriceSqftLoess.png", CITY_NAME), width = 8, height = 6)
}

dbDisconnect(con)

q("no")
