# edmontonMatchKernel.R
# Match building permits to assessment; per-permit local level + elasticities.
# Port of vancouverMatch.R estimation block to Edmonton.
# Tom Davidoff
# 07/16/26  (rev3: bivariate area+lot elasticities, lot-variation diagnostic)
#
# KEY DIFFERENCES FROM VANCOUVER, forced by the data:
#   - "Comps" are RS ASSESSMENT records, not sales. No transaction date, so the
#     local fit is on the current assessed cross-section, demeaned on age only.
#   - Per permit we now fit log(assessed) ~ log(area) + log(lot), emitting a
#     level, a floor-AREA elasticity, and a LOT-size elasticity.
#   - Choice margin is fee-simple Row House vs Single Detached. Semi-Detached is
#     EXCLUDED (flat/declining ~600-740/yr since 2015 -- established stock, not a
#     reform response). Row House is the reform product: ~180/yr pre-2024 -> 456
#     in 2025.
#   - Elasticities come off ASSESSED values and may reflect the assessor's CAMA
#     functional form. The GLOBAL bivariate slopes (printed) are the test: if the
#     local medians match them, only the cross-permit SPREAD is informative, not
#     the levels. If a LAND VALUE column exists (checked at ingest), the lot
#     elasticity on land value is the clean redevelopment-relevant object.
#
# DIAGNOSTICS inline at each gate. Search "DIAG".

library(data.table)
library(fixest)
library(sf)
library(duckdb)
library(ggplot2)
library(ggspatial)
library(scales)
library(patchwork)

sf_use_s2(TRUE)

# ============================================================================
# PLUMBING (Edmonton-specific) -- assessment + permit ingest and match
# ============================================================================

assessorHistoricalPath <- "~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv"
assessorCurrentPath    <- "~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv"

# DIAG -1: scan the CURRENT file's header for a land / improvement value split.
# If present, the lot elasticity should be run on LAND value (clean), not total
# assessed (land+improvement bundled). This prints the candidate columns; if any
# appear, set LANDVAL_COL below and the script uses it as the LHS.
cat("\n=== DIAG -1: current-file columns matching land/improve/value ===\n")
hdrCur <- names(fread(assessorCurrentPath, nrows = 0))
print(grep("land|improv|value|assess", hdrCur, ignore.case = TRUE, value = TRUE))
LANDVAL_COL <- NA_character_   # <- set to a land-value column name if one exists

dtAH <- fread(assessorHistoricalPath,
              select = c("Lot Size","Account Number","Zoning","Legal Description",
                         "Latitude","Longitude","Assessment Year","Assessed Value"))
dtAC <- fread(assessorCurrentPath,
              select = c("lot_size","zoning","legal_description","Latitude","Longitude",
                         "Account Number","year_built","Total Gross Area"))
dtAC <- dtAC[zoning == "RS"]
setkey(dtAH, "Account Number")
setkey(dtAC, "Account Number")
dtAM <- dtAH[dtAC]

# ---- permit ingest ---------------------------------------------------------
# %% escapes the percent for sprintf; DuckDB receives LIKE 'RF%'. RS + pre-reform
# RF family so the series spans the rezone.
permitPath <- "~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv"
con <- dbConnect(duckdb())
q <- sprintf(R"(
  SELECT
    "PERMIT_DATE", "Row ID", "YEAR", "BUILDING_TYPE", "WORK_TYPE",
    "CONSTRUCTION_VALUE", "FLOOR_AREA", "UNITS_ADDED", "ADDRESS",
    "LEGAL_DESCRIPTION", "ZONING", "NEIGHBOURHOOD", "LATITUDE", "LONGITUDE"
  FROM read_csv_auto('%s')
  WHERE ZONING = 'RS' OR ZONING LIKE 'RF%%'
)", permitPath)
dfP <- dbGetQuery(con, q)
dbDisconnect(con, shutdown = TRUE)
dtP <- as.data.table(dfP)
setnames(dtP, "Row ID", "ROW_ID")   # rename to avoid spaces in column name
# what's actually in the column, and does the name match?
print(head(dtP$ROW_ID))
print(table(dtP$ROW_ID))
print(grep("permit", names(dtP), ignore.case = TRUE, value = TRUE))
print(names(dtP)[1:15])

# ---- normalized building-type classification -------------------------------
dtP[, btype := gsub("\\s*\\(\\d+\\)\\s*$", "", BUILDING_TYPE)]   # drop "(330)"
dtP[, btype := gsub("\\s+", " ", trimws(btype))]
dtP[, isRowHouse := grepl("^Row House", btype, ignore.case = TRUE) &
                    !grepl("Condo", btype, ignore.case = TRUE)]
dtP[, isDetached := grepl("^Single Detached", btype, ignore.case = TRUE)]

cat("\n=== DIAG 1: classification counts (pre-filter) ===\n")
print(dtP[, .N, by = .(isRowHouse, isDetached)][order(-N)])

# new construction OR construction value above critical percentile of new
QCRIT <- .25
qNew  <- dtP[isRowHouse | isDetached, quantile(CONSTRUCTION_VALUE/FLOOR_AREA, probs = QCRIT, na.rm = TRUE)]
dtP   <- dtP[isRowHouse | isDetached | (CONSTRUCTION_VALUE/FLOOR_AREA) >= qNew]
dtP <- dtP[isRowHouse | isDetached]
dtP <- dtP[!is.na(LONGITUDE) & !is.na(LATITUDE)]
dtP[, year := as.numeric(YEAR)]

cat("\n=== DIAG 2: row/detached by year after filters ===\n")
print(dtP[, .(nRow = sum(isRowHouse), nDet = sum(isDetached)), by = year][order(year)])
# having checked, confine to RS
dtP <- dtP[ZONING == "RS"]

# ============================================================================
# COMP POOL: RS assessment records
# ============================================================================

dtAM[, assessed := as.numeric(gsub(",", "", gsub("\\$", "", `Assessed Value`)))]
dtAM[, gross    := as.numeric(`Total Gross Area`)]
dtAM[, lotNum   := as.numeric(lot_size)]

dtAM[, maxYear := max(`Assessment Year`), by = `Account Number`]
dtAM <- dtAM[`Assessment Year` == maxYear]

dtAM <- dtAM[!is.na(assessed) & assessed > 0 &
             !is.na(gross)    & gross    > 0 &
             !is.na(Longitude) & !is.na(Latitude)]
dtAM[, appsf := assessed / gross]
qa <- quantile(dtAM$appsf, c(0.01, 0.99), na.rm = TRUE)
dtAM <- dtAM[appsf > qa[1] & appsf < qa[2]]

MAXAGE <- 50
dtAM[, structAge := 2026 - as.numeric(year_built)]
dtAM[structAge < 0, structAge := 0]
dtAM <- dtAM[!is.na(structAge) & structAge <= MAXAGE]

cat("\nRS assessment comps (pre lot filter):", nrow(dtAM), "\n")

# ============================================================================
# LOCAL LEVEL + BIVARIATE ELASTICITIES  (area AND lot)
#   Per permit: log(assessed) ~ log(area) + log(lot)
#     kLevel     = fitted value at (REF area, REF lot)
#     kElastArea = partial d log(assessed)/d log(area)
#     kElastLot  = partial d log(assessed)/d log(lot)      [NEW]
#   Bivariate, not two univariate fits: area & lot are correlated, so the
#   partials separate them.
# ============================================================================

KM        <- 1.0
REF_PCTL  <- 0.20
MIN_COMPS <- 12          # bivariate fit needs more comps than the scalar version

# lot size is a regressor now -> required in the comp pool
dtAM <- dtAM[!is.na(lotNum) & lotNum > 0]

# OPTIONAL: switch LHS to land value if a column was found at DIAG -1
if (!is.na(LANDVAL_COL)) {
  cat("\nUsing LAND VALUE column as LHS:", LANDVAL_COL, "\n")
  dtAM[, assessed := as.numeric(gsub(",", "", gsub("\\$", "", get(LANDVAL_COL))))]
  dtAM <- dtAM[!is.na(assessed) & assessed > 0]
}

dtAM[, ageBin  := pmin(structAge %/% 5, 9)]
dtAM[, lval    := log(assessed)]
dtAM[, larea   := log(gross)]
dtAM[, llot    := log(lotNum)]
dtAM[, aMean   := mean(lval), by = ageBin]
dtAM[, lvalAdj := lval - aMean]

REF_AREA <- quantile(dtAM$gross,  REF_PCTL, na.rm = TRUE)
REF_LOT  <- quantile(dtAM$lotNum, REF_PCTL, na.rm = TRUE)
LREF_A   <- log(REF_AREA)
LREF_L   <- log(REF_LOT)
cat("comps:", nrow(dtAM),
    " | REF_AREA (p", REF_PCTL*100, "): ", round(REF_AREA),
    " | REF_LOT (p", REF_PCTL*100, "): ", round(REF_LOT), "\n", sep = "")

# geometry
sfComp <- st_transform(st_as_sf(dtAM, coords = c("Longitude","Latitude"), crs = 4326), 3776)
sfPerm <- st_transform(st_as_sf(dtP,  coords = c("LONGITUDE","LATITUDE"), crs = 4326), 3776)
sc <- st_coordinates(sfComp)
pc <- st_coordinates(sfPerm)
yv <- dtAM$lvalAdj
x1 <- dtAM$larea      # log area
x2 <- dtAM$llot       # log lot
stopifnot(length(yv) == nrow(sc), length(x1) == length(yv), length(x2) == length(yv))

epan <- function(u) 0.75 * (1 - u*u) * (abs(u) < 1)

# ---- DIAG 0 (run FIRST): is the lot slope even identified? ------------------
# Local lot variation must be non-trivial and area/lot not too collinear, or
# kElastLot is noise. Probe 300 permits.
cat("\n=== DIAG 0: within-kernel lot variation & area-lot collinearity ===\n")
set.seed(1)
probe <- sample(seq_len(nrow(pc)), min(300, nrow(pc)))
dv <- t(vapply(probe, function(i) {
  d <- sqrt((sc[,1]-pc[i,1])^2 + (sc[,2]-pc[i,2])^2)/1000
  ok <- d < KM
  if (sum(ok) < MIN_COMPS) return(c(sd_llot = NA, cor_al = NA, n = sum(ok)))
  c(sd_llot = sd(x2[ok]), cor_al = cor(x1[ok], x2[ok]), n = sum(ok))
}, numeric(3)))
cat("median within-kernel sd(log lot):", round(median(dv[,"sd_llot"], na.rm=TRUE), 3), "\n")
cat("global  sd(log lot):", round(sd(x2), 3),
    " | global sd(log area):", round(sd(x1), 3), "\n")
cat("median within-kernel cor(log area, log lot):",
    round(median(dv[,"cor_al"], na.rm=TRUE), 3),
    " -- if |cor|>~0.8, partials fragile\n")
cat(">>> if median within-kernel sd(log lot) is tiny vs global, RS lots are too\n",
    "    uniform locally and kElastLot is NOT identified -- stop & reconsider.\n")

# ---- weighted bivariate OLS via 3x3 normal equations, per permit -----------
estOne <- function(i) {
  d  <- sqrt((sc[,1]-pc[i,1])^2 + (sc[,2]-pc[i,2])^2)/1000
  w  <- epan(d / KM)
  ok <- w > 0
  n  <- sum(ok)
  if (n < MIN_COMPS)
    return(c(level = NA_real_, eArea = NA_real_, eLot = NA_real_, n = n))

  ww <- w[ok]; a <- x1[ok]; l <- x2[ok]; yy <- yv[ok]
  Sw  <- sum(ww)
  Sa  <- sum(ww*a);  Sl  <- sum(ww*l);  Sy  <- sum(ww*yy)
  Saa <- sum(ww*a*a); Sll <- sum(ww*l*l); Sal <- sum(ww*a*l)
  Say <- sum(ww*a*yy); Sly <- sum(ww*l*yy)
  XtWX <- matrix(c(Sw, Sa,  Sl,
                   Sa, Saa, Sal,
                   Sl, Sal, Sll), 3, 3, byrow = TRUE)
  XtWy <- c(Sy, Say, Sly)
  b <- tryCatch(solve(XtWX, XtWy), error = function(e) rep(NA_real_, 3))
  if (anyNA(b))
    return(c(level = NA_real_, eArea = NA_real_, eLot = NA_real_, n = n))
  level <- b[1] + b[2]*LREF_A + b[3]*LREF_L
  c(level = level, eArea = b[2], eLot = b[3], n = n)
}

res <- t(vapply(seq_len(nrow(sfPerm)), estOne,
                c(level = NA_real_, eArea = NA_real_, eLot = NA_real_, n = NA_real_)))
dtP[, `:=`(kLevel     = res[, 1],
           kElastArea = res[, 2],
           kElastLot  = res[, 3],
           kN         = as.integer(res[, 4]))]

cat("\npermits with a local fit:", sum(!is.na(dtP$kLevel)), "of", nrow(dtP), "\n")
cat("dropped (kN <", MIN_COMPS, "):", sum(dtP$kN < MIN_COMPS), "\n")
print(summary(dtP[, .(kLevel, kElastArea, kElastLot, kN)]))
cat("cor(area elast, lot elast):",
    round(cor(dtP$kElastArea, dtP$kElastLot, use = "complete.obs"), 3),
    " -- strong negative => collinearity splitting one signal into two\n")

# ---- "is it 1?" distributions for BOTH elasticities ------------------------
cat("\n=== elasticity distributions vs unconstrained null (1.0) ===\n")
cat("AREA: median", round(median(dtP$kElastArea, na.rm=TRUE),3),
    " | share>=1:", round(mean(dtP$kElastArea>=1, na.rm=TRUE),3), "\n")
cat("LOT : median", round(median(dtP$kElastLot,  na.rm=TRUE),3),
    " | share>=1:", round(mean(dtP$kElastLot >=1, na.rm=TRUE),3), "\n")

# ---- global slopes: CAMA functional form vs local variation ----------------
gGlobal <- feols(lvalAdj ~ larea + llot, data = dtAM)
cat("\nGLOBAL bivariate slopes (comp pool):  area =",
    round(coef(gGlobal)["larea"],3), " lot =", round(coef(gGlobal)["llot"],3), "\n")
cat("  -- if local medians ~= these, the LEVELS are the CAMA form; only the\n",
    "     cross-permit SPREAD of each elasticity is informative.\n")

# DIAG 3: fit success by type
cat("\n=== DIAG 3: kernel-fit success by type (should be ~equal) ===\n")
print(dtP[, .(fit_rate = mean(!is.na(kLevel)), n = .N), by = isRowHouse])

# ============================================================================
# CHOICE REGRESSIONS -- now with kElastLot alongside kElastArea
# ============================================================================

dtP[, lon := as.numeric(LONGITUDE)]
dtP[, lat := as.numeric(LATITUDE)]
samp <- dtP[!is.na(kLevel)]

cat("\n=== isRowHouse ~ kLevel + kElastArea + kElastLot | year ===\n")
cat("--- post-2023 (reform era) ---\n")
print(summary(feols(isRowHouse ~ kLevel + kElastArea + kElastLot | year,
                    data = samp[year > 2023],
                    vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2.0))))
cat("--- full period ---\n")
print(summary(feols(isRowHouse ~ kLevel + kElastArea + kElastLot | year,
                    data = samp,
                    vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2.0))))

# lot elasticity ALONE (does it carry signal the area elasticity lacked?)
cat("\n=== isRowHouse ~ kLevel + kElastLot | year  (lot elasticity only) ===\n")
cat("--- post-2023 ---\n")
print(summary(feols(isRowHouse ~ kLevel + kElastLot | year,
                    data = samp[year > 2023],
                    vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2.0))))

# ============================================================================
# PLOTS
# ============================================================================

# level vs LOT elasticity, red (row) over faint blue (detached)
p <- ggplot() +
  geom_point(data = samp[isRowHouse == FALSE],
             aes(kLevel, kElastLot), colour = "blue", size = .4, alpha = 0.18) +
  geom_point(data = samp[isRowHouse == TRUE],
             aes(kLevel, kElastLot), colour = "red",  size = 1.2, alpha = 0.9) +
  geom_hline(yintercept = 1, colour = "grey40", linetype = "dashed") +
  labs(x = "Local log assessed @ (p20 area, p20 lot)", y = "Local LOT elasticity",
       title = "Edmonton: level vs lot-size elasticity",
       subtitle = "dashed = unconstrained null (1.0); red = row house") +
  theme_minimal(base_size = 12)
ggsave("text/edmontonLevelLotElast.png", p, width = 7, height = 7, dpi = 200, bg = "white")

# both elasticity distributions with the 1.0 null marked
pd <- rbind(
  data.table(elast = dtP$kElastArea, which = "area"),
  data.table(elast = dtP$kElastLot,  which = "lot")
)[!is.na(elast)]
p2 <- ggplot(pd, aes(elast, fill = which)) +
  geom_histogram(bins = 60, position = "identity", alpha = 0.5) +
  geom_vline(xintercept = 1, colour = "red", linetype = "dashed") +
  labs(title = "Edmonton local elasticities (assessed): area vs lot",
       subtitle = "red dashed = unconstrained null (1.0)",
       x = "elasticity", fill = NULL) +
  theme_minimal(base_size = 12)
ggsave("text/edmontonElastDistBoth.png", p2, width = 8, height = 5, dpi = 200, bg = "white")

cat("\n=== permit choice by year ===\n")
print(dtP[, .(nRow = sum(isRowHouse), nDet = sum(isDetached),
              rowShare = round(mean(isRowHouse), 3)), by = year][order(year)])

# ============================================================================
# CENSUS-TRACT PRICE/RENT as an alternative local price signal
# Rationale: row houses are largely built-to-RENT, detached built-to-OWN. The
# local rent-to-price ratio (cap rate) is the price of the rental exit. Where
# rents are rich relative to sale/assessed values, the rental exit pencils out
# and we predict MORE row houses. This sidesteps the CAMA elasticity confound
# entirely -- it uses rent, not the assessor's area gradient.
#
# PRICE side: your assessed values (better than census self-reported value),
#             aggregated to tract.
# RENT side:  census median gross rent by tract via cancensus.
# ============================================================================

library(cancensus)
set_cancensus_api_key("CensusMapper_4e1a1c2a1c23a336aec2ea4ce443330c")   # one-time; free at censusmapper.ca
# options(cancensus.cache_path = "~/.cancensus_cache")   # optional but recommended

# ---- find the rent + value vector codes rather than hardcoding --------------
# Vector codes are year-specific; look them up so we use the RIGHT variable.
vecs <- list_census_vectors("CA21")
rentVec <- vecs[grepl("median.*monthly.*shelter.*rent|median.*gross.*rent",
                      vecs$label, ignore.case = TRUE) &
                grepl("rent", vecs$label, ignore.case = TRUE), ]
valVec  <- vecs[grepl("median.*value.*dwelling|average.*value.*dwelling",
                      vecs$label, ignore.case = TRUE), ]
cat("=== candidate RENT vectors (pick the median monthly shelter cost, RENTED) ===\n")
print(rentVec[, c("vector","label")])
cat("\n=== candidate VALUE vectors (median owner-estimated dwelling value) ===\n")
print(valVec[, c("vector","label")])
# >>> INSPECT the printout, then set the two codes explicitly below. The right
#     rent variable is "Median monthly shelter costs for RENTED dwellings";
#     do NOT grab owner shelter costs or a household-income vector by mistake.
RENT_VEC <- rentVec$vector[1]     # <-- verify this is the rented-dwelling one
VAL_VEC  <- valVec$vector[1]      # <-- verify median dwelling value

# ---- pull tract-level rent (+ census value) for the Edmonton CMA -----------
# Edmonton CMA code is 48835 in the 2021 census. Confirm via:
#   list_census_regions("CA21")[grepl("Edmonton", name, ignore.case=TRUE)]
EDM_CMA <- "48835"
ct <- get_census(dataset = "CA21",
                 regions = list(CMA = EDM_CMA),
                 vectors = c(rent = RENT_VEC, censusValue = VAL_VEC),
                 level   = "CT",
                 geo_format = NA,          # attributes only; you already have geometry
                 labels  = "short",
                 quiet   = TRUE)
setDT(ct)
# GeoUID is the tract identifier; standardise to match your dfT CTUID/CTNAME.
ct[, CTUID := as.character(GeoUID)]
ct[, rent  := as.numeric(rent)]
ct[, censusValue := as.numeric(censusValue)]
cat("\ntracts with rent:", ct[!is.na(rent), .N], "of", nrow(ct), "\n")

# ============================================================================
# PRICE side: assessed value to tract, then the ratio
# dtAM already has Longitude/Latitude and assessed. Spatial-join to the tract
# polygons you loaded (dfT) exactly as the original script did, then aggregate.
# ============================================================================

# tract geometry from your existing shapefile load (lct_000b21a_e -> dfT)
# dfT must be in memory from earlier; it carries CTUID (StatCan) per polygon.
dfT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
dfT <- st_transform(dfT, crs = 4326)
dfT <- st_make_valid(dfT)

sfAM <- st_as_sf(dtAM[!is.na(Longitude) & !is.na(Latitude)],
                 coords = c("Longitude","Latitude"), crs = 4326)
sfAM <- st_join(sfAM, dfT[, "CTUID"], join = st_within)   # adjust col name if needed
dtAMct <- as.data.table(st_drop_geometry(sfAM))

# tract median assessed value (detached comp pool) -- robust central tendency
tractPrice <- dtAMct[!is.na(CTUID) & assessed > 0,
                     .(medAssessed = median(assessed),
                       nComp = .N), by = CTUID]

# ---- form price/rent (annualised) and its inverse (rent yield) -------------
tract <- merge(tractPrice, ct[, .(CTUID, rent, censusValue)], by = "CTUID", all.x = TRUE)
tract <- tract[!is.na(rent) & rent > 0 & !is.na(medAssessed)]
tract[, priceRent  := medAssessed / (12 * rent)]     # assessed value / annual rent
tract[, rentYield  := (12 * rent) / medAssessed]     # cap-rate analogue (the mechanism)
# census-internal cross-check ratio (self-reported value / rent)
tract[, priceRentCensus := censusValue / (12 * rent)]

cat("\n=== tract price/rent summary (assessed-based) ===\n")
print(summary(tract[, .(priceRent, rentYield, nComp)]))
cat("cor(assessed price/rent, census price/rent):",
    round(cor(tract$priceRent, tract$priceRentCensus, use = "complete.obs"), 3),
    " -- high => assessed & census ratios agree, ratio is robust\n")

# ============================================================================
# MERGE to permits (tract) and test row-house choice against the yield
# ============================================================================

# permits already have coordinates; join to tracts the same way
sfP <- st_as_sf(dtP[!is.na(LONGITUDE) & !is.na(LATITUDE)],
                coords = c("LONGITUDE","LATITUDE"), crs = 4326)
sfP <- st_join(sfP, dfT[, "CTUID"], join = st_within)
dtPct <- as.data.table(sfP)   # keeps geometry-dropped attrs + CTUID
dtPct[, CTUID := as.character(CTUID)]
dtPct <- merge(dtPct, tract[, .(CTUID, priceRent, rentYield)], by = "CTUID", all.x = TRUE)

# recover lon/lat for Conley
dtPct[, lon := st_coordinates(sfP)[,1]]
dtPct[, lat := st_coordinates(sfP)[,2]]

# bring in kLevel from the kernel step if present (control for price LEVEL, so
# the yield isn't just proxying expensive-area). Merge on ROW_ID.

cat("dup ROW_ID in dtP:",  sum(duplicated(dtP$ROW_ID)), "\n")
cat("dup ROW_ID in dtPct:", sum(duplicated(dtPct$ROW_ID)), "\n")
cat("rows dtPct:", nrow(dtPct), " unique permits:", uniqueN(dtPct$ROW_ID), "\n")
if ("kLevel" %in% names(dtP))
  dtPct <- merge(dtPct, dtP[, .(ROW_ID, kLevel)], by = "ROW_ID", all.x = TRUE)

sampPR <- dtPct[!is.na(rentYield)]

cat("\n=== isRowHouse ~ log(priceRent) | year  (higher price/rent => fewer row?) ===\n")
print(summary(feols(isRowHouse ~ log(priceRent) | year,
                    data = sampPR[year > 2023],
                    vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2.0))))

# with the kernel price LEVEL as control: does the YIELD add signal beyond level?
if ("kLevel" %in% names(sampPR)) {
  cat("\n=== isRowHouse ~ kLevel + log(priceRent) | year  (yield beyond level) ===\n")
  print(summary(feols(isRowHouse ~ kLevel + log(priceRent) | year,
                      data = sampPR[year > 2023 & !is.na(kLevel)],
                      vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2.0))))
}

# tract-level aggregate version (mirrors your ORIGINAL edmontonMatch.R design)
dtShare <- dtPct[year > 2023, .(rowShare = mean(isRowHouse), n = .N), by = CTUID]
dtShare <- merge(dtShare, tract, by = "CTUID")
cat("\n=== tract-level: cor(rowShare, rentYield) and a weighted lm ===\n")
print(cor(dtShare[n >= 5, .(rowShare, rentYield, priceRent, medAssessed)], use = "complete.obs"))
print(summary(feols(rowShare ~ log(priceRent), data = dtShare[n >= 5], weights = ~n)))

ggplot(dtShare[n >= 5], aes(rentYield, rowShare)) +
  geom_point(aes(size = n), alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(x = "Rent yield (annual rent / assessed)", y = "Row-house share (post-2023)",
       title = "Edmonton: row-house share vs local rent yield by tract") +
  theme_minimal(base_size = 12)
ggsave("text/edmontonRowShareRentYield.png", width = 7, height = 6, dpi = 200, bg = "white")

# what's up??
samp[, kLevelBin := cut(kLevel, quantile(kLevel, 0:10/10, na.rm=TRUE),
                        include.lowest=TRUE, labels=FALSE)]
feols(isRowHouse ~ i(kLevelBin) | year, data = samp[year > 2023],
      vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))

# ============================================================================
# GEOGRAPHY SPEC -- test the hump on coordinates we TRUST, not on kLevel.
# Economic model: row houses win in a MIDDLE band --
#   - too cheap / far fringe: not worth densifying, build detached
#   - too prime / river-adjacent: single-family luxury outbids row houses
#   => hump in distance-to-downtown; rising in distance-from-river.
# kLevel bins proved reference-sensitive (U at p20, null at p50) => not a usable
# land-value sort. Geography has no such artifact.
# ============================================================================

# ---- downtown core ---------------------------------------------------------
# Edmonton City Hall / core ~ (-113.4909, 53.5461). Adjust if you prefer a
# different anchor (e.g. the financial core or a row-house centroid).
CORE_LON <- -113.4909
CORE_LAT <-   53.5461

# straight-line (great-circle-ish, fine at city scale) distance to core, km.
# 1 deg lat ~ 111.32 km; 1 deg lon ~ 111.32*cos(lat) km at Edmonton's latitude.
kmPerLat <- 111.32
kmPerLon <- 111.32 * cos(CORE_LAT * pi/180)
samp[, dCore := sqrt((( lat - CORE_LAT)*kmPerLat)^2 +
                     (( lon - CORE_LON)*kmPerLon)^2)]

# ---- river distance, STRAIGHT-LINE APPROX (run-now version) ----------------
# North Saskatchewan through the core runs ~NE-SW. Approximate the central
# reach as a line through two points on the river and take perpendicular
# distance. These two lon/lat points sit on the river channel near downtown;
# nudge them if the fit looks off against your permit map.
#   P1 (SW, near Fort Edmonton / Whitemud):  (-113.55, 53.50)
#   P2 (NE, near Capilano / Rundle):         (-113.42, 53.56)
rP1 <- c(lon = -113.55, lat = 53.50)
rP2 <- c(lon = -113.42, lat = 53.56)
# convert both the line endpoints and each permit to local km coords centred on core
toKm <- function(lon, lat) cbind((lon - CORE_LON)*kmPerLon, (lat - CORE_LAT)*kmPerLat)
A <- toKm(rP1["lon"], rP1["lat"]); B <- toKm(rP2["lon"], rP2["lat"])
P <- toKm(samp$lon, samp$lat)
# perpendicular distance from point P to line AB
AB <- B - A
t  <- as.numeric(((P[,1]-A[1,1])*AB[1,1] + (P[,2]-A[1,2])*AB[1,2]) / sum(AB^2))
proj <- cbind(A[1,1] + t*AB[1,1], A[1,2] + t*AB[1,2])
samp[, dRiver := sqrt((P[,1]-proj[,1])^2 + (P[,2]-proj[,2])^2)]   # km, straight-line approx

cat("=== geography summaries (km) ===\n")
print(summary(samp[, .(dCore, dRiver)]))

# ---- THE SPEC: hump in dCore, monotone in dRiver ---------------------------
cat("\n=== isRowHouse ~ dCore + dCore^2 + dRiver | year (post-2023) ===\n")
m1 <- feols(isRowHouse ~ dCore + I(dCore^2) + dRiver | year,
            data = samp[year>2023],
            vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))
print(summary(m1))
# turning point of the dCore hump (km from core); should be INSIDE the data range
b <- coef(m1)
if (!is.na(b["dCore"]) && !is.na(b["I(dCore^2)"]))
  cat("dCore hump peak at:", round(-b["dCore"]/(2*b["I(dCore^2)"]), 2),
      "km from core (data range:", round(min(samp$dCore,na.rm=TRUE),1), "-",
      round(quantile(samp$dCore,.99,na.rm=TRUE),1), "km)\n")

# ---- add land value to see if geography SUBSUMES it ------------------------
# If dCore hump + dRiver hold and medAssessed goes weak, geography is the story.
cat("\n=== + log(priceRent) as land-value control ===\n")
if ("priceRent" %in% names(samp))
  print(summary(feols(isRowHouse ~ dCore + I(dCore^2) + dRiver + log(priceRent) | year,
                      data = samp[year>2023 & !is.na(priceRent)],
                      vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))

# ---- binned dCore, nonparametric shape (no parabola imposed) ---------------
samp[, dCoreBin := cut(dCore, quantile(dCore, 0:8/8, na.rm=TRUE),
                       include.lowest=TRUE, labels=FALSE)]
cat("\n=== row-house share by distance-to-core octile (raw, with counts) ===\n")
print(samp[year>2023, .(rowShare = round(mean(isRowHouse),3),
                        n = .N, med_dCore = round(median(dCore),1)),
          by = dCoreBin][order(dCoreBin)])

# ---- map to eyeball the river line fit -------------------------------------
p <- ggplot(samp[year>2023]) +
  geom_point(aes(lon, lat, colour = isRowHouse), size=.5, alpha=.5) +
  annotate("segment", x=rP1["lon"], y=rP1["lat"], xend=rP2["lon"], yend=rP2["lat"],
           colour="blue", linewidth=1) +
  annotate("point", x=CORE_LON, y=CORE_LAT, colour="black", size=3, shape=4) +
  scale_colour_manual(values=c(`FALSE`="grey70", `TRUE`="red")) +
  labs(title="Check: blue line = approx river, X = core; red = row house") +
  coord_quickmap() + theme_minimal()
ggsave("text/edmontonRiverCheck.png", p, width=8, height=7, dpi=150, bg="white")
