# edmontonHorseRace.R
# Row-house choice horse race: census tract median VALUE vs median RENT, logged.
# Tom Davidoff
#
# Both regressors are census tract (2021) levels:
#   VALUE = v_CA21_4311  "Median value of dwellings ($)"        [owner self-report]
#   RENT  = v_CA21_4317  "Median monthly shelter cost, rented"
# Local median assessed value kept as a third, finer-grained value horse.
# Prior finding: rent wins, value (assessed) is null; this adds census value.

library(data.table)
library(fixest)
library(sf)
library(duckdb)
library(cancensus)
set_cancensus_api_key("CensusMapper_4e1a1c2a1c23a336aec2ea4ce443330c")   # one-time; free at censusmapper.ca
library(ggplot2)


sf_use_s2(TRUE)

# ============================================================================
# 1. INGEST
# ============================================================================

assessorHistoricalPath <- "~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv"
assessorCurrentPath    <- "~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv"
permitPath             <- "~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv"
tractShp               <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"

# --- assessment (comp pool for the local median value horse) ---
dtAH <- fread(assessorHistoricalPath,
              select = c("Account Number","Latitude","Longitude",
                         "Assessment Year","Assessed Value"))
dtAC <- fread(assessorCurrentPath,
              select = c("zoning","Account Number","year_built","Total Gross Area"))
dtAC <- dtAC[zoning == "RS"]
setkey(dtAH, "Account Number"); setkey(dtAC, "Account Number")
dtAM <- dtAH[dtAC]

dtAM[, assessed := as.numeric(gsub(",", "", gsub("\\$", "", `Assessed Value`)))]
dtAM[, gross    := as.numeric(`Total Gross Area`)]
dtAM[, maxYear  := max(`Assessment Year`), by = `Account Number`]
dtAM <- dtAM[`Assessment Year` == maxYear]
dtAM <- dtAM[!is.na(assessed) & assessed > 0 & !is.na(gross) & gross > 0 &
             !is.na(Longitude) & !is.na(Latitude)]
dtAM[, appsf := assessed / gross]
qa <- quantile(dtAM$appsf, c(0.01, 0.99), na.rm = TRUE)
dtAM <- dtAM[appsf > qa[1] & appsf < qa[2]]
dtAM[, structAge := 2026 - as.numeric(year_built)]
dtAM <- dtAM[!is.na(structAge) & structAge >= 0 & structAge <= 50]
cat("assessment comps:", nrow(dtAM), "\n")

# --- permits (RS + pre-reform RF; %% escapes for sprintf) ---
con <- dbConnect(duckdb())
q <- sprintf(R"(
  SELECT "Row ID", "YEAR", "BUILDING_TYPE", "WORK_TYPE",
         "CONSTRUCTION_VALUE", "FLOOR_AREA", "ZONING", "LATITUDE", "LONGITUDE"
  FROM read_csv_auto('%s')
  WHERE ZONING = 'RS' OR ZONING LIKE 'RF%%'
)", permitPath)
dtP <- as.data.table(dbGetQuery(con, q))
dbDisconnect(con, shutdown = TRUE)
dtP[, ROW_ID := .I]

dtP[, btype := gsub("\\s*\\(\\d+\\)\\s*$", "", BUILDING_TYPE)]
dtP[, btype := gsub("\\s+", " ", trimws(btype))]
dtP[, isRowHouse := grepl("^Row House", btype, ignore.case = TRUE) &
                    !grepl("Condo", btype, ignore.case = TRUE)]
dtP[, isDetached := grepl("^Single Detached", btype, ignore.case = TRUE)]

QCRIT <- .25
qNew  <- dtP[isRowHouse | isDetached, quantile(CONSTRUCTION_VALUE/FLOOR_AREA, QCRIT, na.rm = TRUE)]
dtP   <- dtP[isRowHouse | isDetached | (CONSTRUCTION_VALUE/FLOOR_AREA) >= qNew]
dtP   <- dtP[(isRowHouse | isDetached) & ZONING == "RS" &
             !is.na(LONGITUDE) & !is.na(LATITUDE)]
dtP[, year := as.numeric(YEAR)]
dtP[, lon  := as.numeric(LONGITUDE)]
dtP[, lat  := as.numeric(LATITUDE)]
cat("analysis permits:", nrow(dtP), "\n")

# ============================================================================
# 2. LOCAL median assessed value (finer-grained value horse; comps within 1km)
# ============================================================================

MATCH_KM  <- 1.0
MIN_COMPS <- 12
sfComp <- st_transform(st_as_sf(dtAM, coords = c("Longitude","Latitude"), crs = 4326), 3776)
sfPerm <- st_transform(st_as_sf(dtP,  coords = c("LONGITUDE","LATITUDE"), crs = 4326), 3776)
sc <- st_coordinates(sfComp); pc <- st_coordinates(sfPerm); av <- dtAM$assessed

localMed <- function(i) {
  d  <- sqrt((sc[,1]-pc[i,1])^2 + (sc[,2]-pc[i,2])^2) / 1000
  ok <- d < MATCH_KM
  if (sum(ok) < MIN_COMPS) return(c(med = NA_real_, n = sum(ok)))
  c(med = median(av[ok]), n = sum(ok))
}
lm_res <- t(vapply(seq_len(nrow(sfPerm)), localMed, c(med = NA_real_, n = NA_real_)))
dtP[, medAssessedLocal := lm_res[, 1]]
cat("permits with local median:", sum(!is.na(dtP$medAssessedLocal)), "of", nrow(dtP), "\n")

# ============================================================================
# 3. CENSUS tract VALUE + RENT
# ============================================================================

ct <- get_census(dataset = "CA21", regions = list(CMA = "48835"),
                 vectors = c(rent = "v_CA21_4317", value = "v_CA21_4311"),
                 level = "CT", geo_format = NA, labels = "short", quiet = TRUE)
setDT(ct)
ct[, CTUID  := as.character(GeoUID)]
ct[, rent   := as.numeric(rent)]
ct[, value  := as.numeric(value)]      # census median owner-estimated dwelling value
cat("\ntracts with rent:", ct[!is.na(rent), .N],
    "| with value:", ct[!is.na(value), .N], "of", nrow(ct), "\n")

dfT <- st_make_valid(st_transform(st_read(tractShp, quiet = TRUE), 4326))
sfP <- st_join(st_as_sf(dtP, coords = c("LONGITUDE","LATITUDE"), crs = 4326),
               dfT[, "CTUID"], join = st_within)
dtPj <- as.data.table(st_drop_geometry(sfP))
dtPj[, CTUID := as.character(CTUID)]
dtPj <- unique(dtPj, by = "ROW_ID")
dtPj <- merge(dtPj, ct[, .(CTUID, rent, value)], by = "CTUID", all.x = TRUE)

# ============================================================================
# 4. HORSE RACE -- log(median VALUE) vs log(median RENT)
# ============================================================================

samp <- dtPj[!is.na(rent) & rent > 0 & !is.na(value) & value > 0 &
             !is.na(medAssessedLocal) & medAssessedLocal > 0]
cat("\nsample:", nrow(samp), " (row houses:", samp[, sum(isRowHouse)], ")\n")
cat("cor(log value, log rent):",
    round(cor(log(samp$value), log(samp$rent)), 3),
    "| cor(log value, log medAssessedLocal):",
    round(cor(log(samp$value), log(samp$medAssessedLocal)), 3), "\n")

cat("\n=== each alone (post-2023) ===\n")
print(summary(feols(isRowHouse ~ log(value) | year, data = samp[year>2023],
                    vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))
print(summary(feols(isRowHouse ~ log(rent) | year, data = samp[year>2023],
                    vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))

cat("\n=== HORSE RACE: log(value) + log(rent), post-2023 ===\n")
print(summary(feols(isRowHouse ~ log(value) + log(rent) | year, data = samp[year>2023],
                    vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))

cat("\n=== HORSE RACE: log(value) + log(rent), full period ===\n")
print(summary(feols(isRowHouse ~ log(value) + log(rent) | year, data = samp,
                    vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))

cat("\n=== all three value/rent measures together, post-2023 ===\n")
print(summary(feols(isRowHouse ~ log(value) + log(medAssessedLocal) + log(rent) | year,
                    data = samp[year>2023],
                    vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))

# anything on value??
samp[, valBin := cut(value, quantile(value, 0:10/10, na.rm=TRUE), include.lowest=TRUE, labels=FALSE)]
samp[, rentBin := cut(rent, quantile(rent, 0:10/10, na.rm=TRUE), include.lowest=TRUE, labels=FALSE)]
samp[year>2023, .(rowShare=mean(isRowHouse), n=.N, nRow=sum(isRowHouse),
                  med_val=round(median(value))), by=valBin][order(valBin)]
samp[year>2023, .(rowShare=mean(isRowHouse), n=.N, nRow=sum(isRowHouse),
                  med_val=round(median(value))), by=rentBin][order(rentBin)]

plotdat <- samp[year > 2023,
  .(rowShare = mean(isRowHouse), n = .N),
  by = .(valBin, rentBin)
]

ggplot(plotdat, aes(x = valBin, y = rentBin, fill = rowShare)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "Value decile", y = "Rent decile", fill = "Row house share") +
  theme_minimal()
ggsave("text/edmontonDecilePlot.png", width=6, height=4, dpi=300)

# plot rent by lat/lon
ggplot(samp[year > 2023], aes(x = lon, y = lat, color = rent)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  labs(x = "Longitude", y = "Latitude", color = "Rent") +
  theme_minimal()
ggsave("text/edmontonRentMap.png", width=6, height=4, dpi=300)
