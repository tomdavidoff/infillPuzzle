# edmontonCondoExist.R
# Tom Davidoff
# 09/03/26
#
# Question: near a MULTI-UNIT ROW HOUSE permit (RS, post-reform), what does the
# current assessor inventory look like?
#   (A) CONDO-EXISTENCE test  -- are there MULTIPLE distinct NEW accounts clustered
#       near the permit? (condominiumized row house = several new titles/accounts)
#   (B) PURPOSE-BUILT-RENTAL-AS-SINGLE test -- is there instead ONE new account
#       sitting essentially on the permit with NO sibling accounts in the tight
#       radius? (whole row house held on one unsubdivided title -> UNITS=1,
#       misclassified as single-detached unless permit form overrides).
#
# Unit of analysis = permit. We loop over tight radii and, for each permit x radius,
# count nearby accounts and nearby NEW accounts, and record the single-big-property flag.
#
# Conventions inherited from edmontonMatch.R / edmontonHorseRace.R:
#   permits  : DuckDB read of edmontonBuildingPermits.csv, ZONING='RS'
#   inventory: current-year assessor (zoning=='RS') joined to historical for vintage/coords
#   distance : EPSG:3776 (Alberta 10-TM), Euclidean on projected metres / 1000 = km
#   rowhouse : ^Row House (not Condo) after stripping trailing "(n)" and squishing ws
#
# Design choices agreed with Tom:
#   NEW account       := year_built >= permit YEAR, and permit YEAR >= REFORMYEAR (2024)
#   radius sweep      := 5, 10, 20, 40 metres (tight -- "on the permit")
#   single-big flag   := exactly ONE account within the tight radius AND no siblings in it

library(data.table)
library(sf)
library(duckdb)

sf_use_s2(TRUE)

REFORMYEAR <- 2024
RADII_M    <- c(5, 10, 20, 40)          # tight radii, metres
CRS_M      <- 3776                       # Alberta 10-TM, metres

assessorHistoricalPath <- "~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv"
assessorCurrentPath    <- "~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv"
permitPath             <- "~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv"

# ============================================================================
# 1. INVENTORY  (current-year RS accounts, with vintage + current coordinates)
# ============================================================================

dtAH <- fread(assessorHistoricalPath,
              select = c("Account Number","Assessment Year"))
dtAH[, maxYear := max(`Assessment Year`), by = `Account Number`]
dtAH <- unique(dtAH[`Assessment Year` == maxYear, .(`Account Number`)])   # just the account roster

dtAC <- fread(assessorCurrentPath,
              select = c("zoning","Account Number","year_built",
                         "Total Gross Area","Latitude","Longitude"))
dtAC <- dtAC[zoning == "RS"]

# inventory = current RS accounts that also appear in the (latest) historical roster
setkey(dtAH, "Account Number"); setkey(dtAC, "Account Number")
dtInv <- dtAC[dtAH, nomatch = 0]

dtInv[, gross    := as.numeric(`Total Gross Area`)]
dtInv[, ybuilt   := as.numeric(year_built)]
dtInv[, invLon   := as.numeric(Longitude)]
dtInv[, invLat   := as.numeric(Latitude)]
dtInv <- dtInv[!is.na(invLon) & !is.na(invLat)]
dtInv <- unique(dtInv, by = "Account Number")     # one row per account

# ============================================================================
# 2. PERMITS  (multi-unit row house, RS, post-reform)
# ============================================================================

con <- dbConnect(duckdb())
q <- sprintf(R"(
  SELECT "Row ID", "YEAR", "BUILDING_TYPE", "WORK_TYPE",
         "CONSTRUCTION_VALUE", "FLOOR_AREA", "ZONING",
         "LATITUDE", "LONGITUDE", "UNITS_ADDED"
  FROM read_csv_auto('%s')
  WHERE ZONING = 'RS'
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
qNew <- dtP[(isRowHouse | isDetached) & WORK_TYPE == "(01) Building - New",
            quantile(CONSTRUCTION_VALUE / FLOOR_AREA, QCRIT, na.rm = TRUE)]
dtP  <- dtP[(isRowHouse | isDetached) & (CONSTRUCTION_VALUE / FLOOR_AREA) >= qNew &
            !is.na(LONGITUDE) & !is.na(LATITUDE)]
dtP[, year := as.numeric(YEAR)]
dtP[, lon  := as.numeric(LONGITUDE)]
dtP[, lat  := as.numeric(LATITUDE)]
dtP <- dtP[year >= REFORMYEAR]
dtP[is.na(UNITS_ADDED), UNITS_ADDED := 1]

# multi-unit row house permits: the objects of interest
dtRH <- dtP[isRowHouse == TRUE & UNITS_ADDED >= 2]

# ============================================================================
# 3. PROJECT + DISTANCE MACHINERY  (EPSG:3776, metres; matches edmontonHorseRace)
# ============================================================================

sfInv <- st_transform(st_as_sf(dtInv, coords = c("invLon","invLat"), crs = 4326), CRS_M)
sfRH  <- st_transform(st_as_sf(dtRH,  coords = c("lon","lat"),       crs = 4326), CRS_M)
ic <- st_coordinates(sfInv)      # inventory account coords (metres)
pc <- st_coordinates(sfRH)       # permit coords (metres)
iy <- dtInv$ybuilt               # inventory vintage, aligned to ic rows

# for permit i: distances (m) to every inventory account
distsTo <- function(i) sqrt((ic[,1] - pc[i,1])^2 + (ic[,2] - pc[i,2])^2)

# ============================================================================
# 4. PER-PERMIT x RADIUS COUNTS
#    nAcct      = distinct accounts within r
#    nNewAcct   = accounts within r with year_built >= permit YEAR (NEW)
#    minDist    = nearest account distance (m)
#    singleBig  = exactly one account within r AND it is NEW  (purpose-built single)
#    isCondo    = >=2 NEW accounts within r                  (condo-existence)
# ============================================================================

oneRadius <- function(i, r) {
  d      <- distsTo(i)
  inR    <- d <= r
  isNew  <- inR & !is.na(iy) & (iy >= dtRH$year[i])   # NEW relative to THIS permit's year
  nAcct  <- sum(inR)
  nNew   <- sum(isNew)
  data.table(
    ROW_ID    = dtRH$ROW_ID[i],
    radius_m  = r,
    year      = dtRH$year[i],
    unitsAdd  = dtRH$UNITS_ADDED[i],
    nAcct     = nAcct,
    nNewAcct  = nNew,
    minDist   = if (nAcct > 0) min(d[inR]) else NA_real_,
    isCondo   = nNew >= 2,
    singleBig = (nAcct == 1 & nNew == 1)
  )
}

res <- rbindlist(lapply(seq_len(nrow(sfRH)),
                        function(i) rbindlist(lapply(RADII_M, function(r) oneRadius(i, r)))))

# ============================================================================
# 5. UNIT TESTS / MERGE SANITY  --- printed FIRST so failures are obvious in .out
# ============================================================================

pass <- function(name, ok) cat(sprintf("[%s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", name))
cat("================ UNIT TESTS ================\n")

# T1: inventory join kept only accounts present in both files (nomatch=0, no NA keys)
pass("T1 inventory has no NA Account Number",
     dtInv[is.na(`Account Number`), .N] == 0)
pass("T1b inventory accounts are unique",
     uniqueN(dtInv$`Account Number`) == nrow(dtInv))

# T2: inventory coords are inside a sane Edmonton bounding box (catches lat/lon swap)
pass("T2 inventory lon in [-114.2,-113.2], lat in [53.3,53.8]",
     dtInv[, all(invLon > -114.2 & invLon < -113.2 & invLat > 53.3 & invLat < 53.8)])
pass("T2b permit coords in same box",
     dtRH[, all(lon > -114.2 & lon < -113.2 & lat > 53.3 & lat < 53.8)])

# T3: projection preserved row counts (no dropped/duplicated geometries)
pass("T3 sfInv rows == dtInv rows", nrow(sfInv) == nrow(dtInv))
pass("T3b sfRH rows == dtRH rows",  nrow(sfRH)  == nrow(dtRH))

# T4: every multi-unit row house permit has UNITS_ADDED >= 2 and is post-reform
pass("T4 all target permits UNITS_ADDED>=2", dtRH[, all(UNITS_ADDED >= 2)])
pass("T4b all target permits year>=REFORMYEAR", dtRH[, all(year >= REFORMYEAR)])

# T5: result grid is complete -- one row per (permit x radius)
pass("T5 result rows == nPermits * nRadii",
     nrow(res) == nrow(dtRH) * length(RADII_M))
pass("T5b every radius present for every permit",
     res[, uniqueN(radius_m), by = ROW_ID][, all(V1 == length(RADII_M))])

# T6: monotonicity -- counts are non-decreasing in radius (nested disks)
monoOK <- res[order(ROW_ID, radius_m),
              all(diff(nAcct) >= 0 & diff(nNewAcct) >= 0), by = ROW_ID][, all(V1)]
pass("T6 nAcct & nNewAcct non-decreasing in radius", monoOK)

# T7: nNewAcct never exceeds nAcct (new is a subset)
pass("T7 nNewAcct <= nAcct everywhere", res[, all(nNewAcct <= nAcct)])

# T8: the two flags are mutually exclusive by construction
pass("T8 singleBig & isCondo never both TRUE", res[, sum(singleBig & isCondo) == 0])

# T9: minDist within radius bound where any account present
pass("T9 minDist <= radius when nAcct>0",
     res[nAcct > 0, all(minDist <= radius_m + 1e-6)])

# ============================================================================
# 6. HEADLINE OUTPUT
# ============================================================================

cat("\n================ COUNTS ================\n")
cat("multi-unit row house permits (RS, post-reform):", nrow(dtRH), "\n")
cat("inventory RS accounts:", nrow(dtInv), "\n\n")

cat("By radius -- how the near field resolves:\n")
summ <- res[, .(
  permits         = .N,
  pct_isCondo     = round(100 * mean(isCondo), 1),
  pct_singleBig   = round(100 * mean(singleBig), 1),
  pct_empty       = round(100 * mean(nAcct == 0), 1),
  mean_nNewAcct   = round(mean(nNewAcct), 2),
  median_minDist  = round(median(minDist, na.rm = TRUE), 1)
), by = radius_m][order(radius_m)]
print(summ)

# permits that are single-big at the tightest radius but gain siblings as r grows:
# these are the strongest purpose-built-rental candidates (one title, then neighbours)
cat("\nPurpose-built-rental candidates (singleBig at smallest radius):\n")
smallest <- min(RADII_M)
cand <- res[radius_m == smallest & singleBig == TRUE, .(ROW_ID, year, unitsAdd, minDist)]
print(cand)
cat("n candidates at", smallest, "m:", nrow(cand), "\n")

# condo candidates: >=2 new accounts even at a tight radius
cat("\nCondo candidates (isCondo at <=20 m):\n")
condo <- res[radius_m <= 20 & isCondo == TRUE, .(ROW_ID = unique(ROW_ID))]
cat("n distinct permits with >=2 new accounts within 20 m:", uniqueN(condo$ROW_ID), "\n")

fwrite(res, "text/edmontonCondoExist.csv")
cat("\nwrote text/edmontonCondoExist.csv (", nrow(res), "rows )\n")
