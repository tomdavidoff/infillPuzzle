# newBuildLotSize.R
# Post-reform (2019-2024 built) duplex vs SFD lot-size analysis, East/West (Ontario St).
# FOUR lot-width estimates per parcel, cross-checked:
#   wGpkgCol  - LAND_WIDTH column, 2026 gpkg folio descriptions
#   wGeom     - width implied by parcel polygon area (st_area or FEATURE_AREA_SQM) / depth
#   wRoll19   - inherited 2019 SFD landWidth via rollStart back-match (pre-redevelopment)
#   wSpatial  - 2019 SFD landWidth via nearest-folio spatial join (fallback for unmatched roll)
# Parcel-level stats dedup to one row per rollStart; sales kept at unit level.
# Reads only; no writes beyond the guarded bca19 singles rds (already exists).
# Tom Davidoff
# 09/08/26

BUILT_MINYEAR <- 2019      # retained: informational only, no longer gates the sample
BUILT_MAXYEAR <- 2024
EFFYEAR_MIN   <- 2000      # vintage gate: MB_Effective_Year strictly greater than this
SALE_MINYEAR  <- 2022      # "recent" price overlay
SALE_MAXYEAR  <- 2026
ONTARIO_LON   <- -123.1036
MATCHDIST     <- 30        # m, spatial fallback gate (looser than permit-match: parcel centroids)
WIDTH_TOL     <- 3         # ft, agreement tolerance across methods

# shared lot-size bins (ft), used by BOTH the sec.7 distribution and sec.8 price tables.
# left-open, right-closed: (30,36]="33", (36,40]="40", (40,50]="50";
# anything <=30 or >50 -> "other", so nothing is silently dropped.
lotBin <- function(width) {
  out <- rep("other", length(width))
  out[width > 30 & width <= 36] <- "33"
  out[width > 36 & width <= 40] <- "40"
  out[width > 40 & width <= 50] <- "50"
  out[is.na(width)] <- NA
  factor(out, levels = c("33","40","50","other"))
}

library(data.table)
library(sf)
library(RSQLite)
library(fixest)

sf_use_s2(TRUE)

dirGpkg   <- "~/bigFiles/latestSpatialBCA/"
fInvTxt   <- "~/DropboxExternal/dataRaw/Residential_inventory_202601/20260101_A09_Residential_Inventory_Extract.txt"
fSingles  <- "~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds"   # has rollNumber, landWidth, lon/lat
fPermits  <- "~/DropboxExternal/dataRaw/issued-building-permitsDwellingUses.csv"

gpkgFiles <- list.files(dirGpkg, pattern = "\\.gpkg$", full.names = TRUE)
stopifnot(length(gpkgFiles) == 1)
fileGpkg <- gpkgFiles

# ============================================================================
# 1. 2026 gpkg: folio descriptions (use, width, depth, area, geom) + values
# ============================================================================
dfD <- st_read(fileGpkg, query =
  "SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD,
          LAND_WIDTH, LAND_DEPTH, LAND_SIZE, FEATURE_AREA_SQM, geom
   FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
dfD <- st_make_valid(dfD)

# geom-based area (m^2) and centroid lon/lat. FEATURE_AREA_SQM is on the table;
# compute st_area too and compare -- if they agree, prefer the column (cheaper).
dfD$areaGeom <- as.numeric(st_area(dfD))                    # m^2 from polygon
dfD4326 <- st_transform(dfD, 4326)
ctr <- st_coordinates(st_centroid(st_geometry(dfD4326)))

dtD <- as.data.table(st_drop_geometry(dfD))
dtD[, `:=`(longitude = ctr[,1], latitude = ctr[,2])]
dtD[, rollNum   := as.numeric(ROLL_NUMBER)]                 # double: 15 digits safe, int would NA
dtD[, rollStart := floor(rollNum / 1000)]                  # parcel stem (strips unit suffix)

cat("=== FEATURE_AREA_SQM vs st_area(geom) agreement (should be ~1:1) ===\n")
print(dtD[!is.na(FEATURE_AREA_SQM) & FEATURE_AREA_SQM > 0 & areaGeom > 0,
          .(n = .N,
            ratio_p10 = quantile(areaGeom/FEATURE_AREA_SQM, .10),
            ratio_p50 = quantile(areaGeom/FEATURE_AREA_SQM, .50),
            ratio_p90 = quantile(areaGeom/FEATURE_AREA_SQM, .90))])
# use the column where present & sane, else the computed area
dtD[, areaM2 := fifelse(!is.na(FEATURE_AREA_SQM) & FEATURE_AREA_SQM > 0,
                        FEATURE_AREA_SQM, areaGeom)]
dtD[, areaFt2 := areaM2 * 10.7639]

# ============================================================================
# 2. inventory .txt: structure (year built, finished area) keyed on Roll_Number
# ============================================================================
dtI <- fread(fInvTxt)
setnames(dtI, make.names(names(dtI)))                      # safe names
dtI[, rollNum := as.numeric(Roll_Number)]
dtI <- dtI[, .(rollNum,
               MB_Year_Built      = as.numeric(MB_Year_Built),
               MB_Effective_Year  = as.numeric(MB_Effective_Year),
               MB_Total_Finished_Area = as.numeric(MB_Total_Finished_Area),
               invZoning = Zoning)]
dtD <- merge(dtD, dtI, by = "rollNum", all.x = TRUE)       # full-roll join (unit level)

# ============================================================================
# 3. width method 1 (column) and method 2 (geom area / depth)
# ============================================================================
dtD[, wGpkgCol := LAND_WIDTH]
# geom width: area / depth, when depth is present & plausible; feet throughout
dtD[, wGeom := fifelse(!is.na(LAND_DEPTH) & LAND_DEPTH > 10, areaFt2 / LAND_DEPTH, NA_real_)]

# ============================================================================
# 4. width methods 3 & 4: inherit 2019 SFD width by rollStart, then spatial
# ============================================================================
dtS19 <- as.data.table(readRDS(fSingles))                  # rollNumber, landWidth, longitude, latitude
dtS19[, rollNum19   := as.numeric(rollNumber)]
dtS19[, rollStart19 := floor(rollNum19 / 1000)]

# 3: rollStart back-match (one 2019 width per stem; dedup 2019 side first)
w19 <- dtS19[!is.na(landWidth) & landWidth > 0,
             .(wRoll19 = median(landWidth)), by = .(rollStart = rollStart19)]
dtD <- merge(dtD, w19, by = "rollStart", all.x = TRUE)
cat("\n=== rollStart back-match rate to 2019 SFD width ===\n")
cat("gpkg parcels:", uniqueN(dtD$rollStart),
    "| with a 2019 rollStart width:", dtD[!is.na(wRoll19), uniqueN(rollStart)], "\n")

# 4: spatial fallback -- nearest 2019 SFD folio, gate at MATCHDIST
#    only computed for parcels lacking a rollStart match, to save the NN cost
needSpatial <- dtD[is.na(wRoll19) & !is.na(longitude) & !is.na(latitude)]
if (nrow(needSpatial) > 0) {
  sfNeed <- st_transform(st_as_sf(needSpatial[, .(rollNum, longitude, latitude)],
                                  coords = c("longitude","latitude"), crs = 4326), 3005)
  sf19   <- st_transform(st_as_sf(dtS19[!is.na(longitude) & !is.na(latitude) &
                                        !is.na(landWidth) & landWidth > 0,
                                        .(landWidth, longitude, latitude)],
                                  coords = c("longitude","latitude"), crs = 4326), 3005)
  idx  <- st_nearest_feature(sfNeed, sf19)
  dNN  <- as.numeric(st_distance(sfNeed, sf19[idx, ], by_element = TRUE))
  wsp  <- as.data.table(st_drop_geometry(sf19))[idx]$landWidth
  needSpatial[, `:=`(wSpatial = fifelse(dNN <= MATCHDIST, wsp, NA_real_), spatDist = dNN)]
  dtD <- merge(dtD, needSpatial[, .(rollNum, wSpatial, spatDist)], by = "rollNum", all.x = TRUE)
} else dtD[, `:=`(wSpatial = NA_real_, spatDist = NA_real_)]

# combined "best" 2019-based width: rollStart if present else spatial
dtD[, w2019 := fifelse(!is.na(wRoll19), wRoll19, wSpatial)]

# ============================================================================
# 5. classify: type, side, new-build vintage (dedup to parcel for lot stats)
# ============================================================================
sfdUse <- c("Single Family Dwelling", "Residential Dwelling with Suite")
dtD[, type := fifelse(grepl("Duplex", ACTUAL_USE_DESCRIPTION), "duplex",
              fifelse(ACTUAL_USE_DESCRIPTION %in% sfdUse,      "SFD", NA_character_))]
dtD[, side := fifelse(longitude < ONTARIO_LON, "West", "East")]
dtD[, newBuild := MB_Effective_Year > EFFYEAR_MIN]   # vintage gate: effective (reno-aware) year

# one row per parcel: for strata duplexes both unit-folios carry identical parcel
# geom/width, so median within rollStart is the parcel value (dedup, no double count)
parcel <- dtD[!is.na(type),
              .(type      = type[1],
                side      = side[1],
                newBuild  = any(newBuild, na.rm = TRUE),
                MB_Year_Built = median(MB_Year_Built, na.rm = TRUE),
                finArea   = median(MB_Total_Finished_Area, na.rm = TRUE),
                wGpkgCol  = median(wGpkgCol,  na.rm = TRUE),
                wGeom     = median(wGeom,     na.rm = TRUE),
                w2019     = median(w2019,     na.rm = TRUE),
                wRoll19   = median(wRoll19,   na.rm = TRUE),
                wSpatial  = median(wSpatial,  na.rm = TRUE),
                nUnits    = .N),
              by = rollStart]

# ============================================================================
# 6. DO THE METHODS AGREE?  (the completeness check you asked for)
# ============================================================================
cat("\n=== coverage: non-missing width by method (new-build parcels) ===\n")
nb <- parcel[newBuild == TRUE & !is.na(type)]
for (m in c("wGpkgCol","wGeom","wRoll19","wSpatial","w2019"))
  cat(sprintf("  %-9s present: %4d / %4d\n", m, sum(!is.na(nb[[m]])), nrow(nb)))

cat("\n=== pairwise agreement on new-build parcels (correlation; median |diff| ft) ===\n")
pairs <- list(c("wGpkgCol","wGeom"), c("wGpkgCol","w2019"),
              c("wGeom","w2019"),    c("wRoll19","wSpatial"))
for (p in pairs) {
  d <- nb[!is.na(get(p[1])) & !is.na(get(p[2]))]
  if (nrow(d) < 5) { cat(sprintf("  %s vs %s: n<5\n", p[1], p[2])); next }
  cat(sprintf("  %-9s vs %-9s  n=%4d  cor=%.3f  med|diff|=%.1f  agree(<%dft)=%.0f%%\n",
              p[1], p[2], nrow(d),
              cor(d[[p[1]]], d[[p[2]]]),
              median(abs(d[[p[1]]] - d[[p[2]]])),
              WIDTH_TOL,
              100*mean(abs(d[[p[1]]] - d[[p[2]]]) < WIDTH_TOL)))
}

cat("\n=== where methods disagree: worst 10 new-build duplex parcels ===\n")
print(nb[type=="duplex"][order(-abs(wGpkgCol - w2019))][1:10,
        .(rollStart, side, wGpkgCol = round(wGpkgCol), wGeom = round(wGeom),
          wRoll19 = round(wRoll19), wSpatial = round(wSpatial), MB_Year_Built)])

# ============================================================================
# 7. THE PUZZLE FOR NEW PRODUCT: lot-width distribution, duplex vs SFD x side
#    Reported under each width method so we can see the answer isn't an artifact
# ============================================================================
widthDist <- function(wcol) {
  cat(sprintf("\n=== new-build lot width [%s]: round-width mix, duplex vs SFD x side ===\n", wcol))
  d <- nb[!is.na(get(wcol))]
  d[, wc := round(get(wcol))]
  # share by lot-size bin (shared taxonomy), by type x side
  d[, wclass := lotBin(wc)]
  print(dcast(d, type + side ~ wclass, fun.aggregate = length, value.var = "rollStart"))
  cat(" median width by type x side:\n")
  print(d[, .(n = .N, p25 = quantile(get(wcol),.25),
              p50 = median(get(wcol)), p75 = quantile(get(wcol),.75)),
          by = .(type, side)][order(type, side)])
}
for (w in c("wGpkgCol","w2019")) widthDist(w)   # column-based and 2019-inherited

# ============================================================================
# 8. RECENT SALES price overlay (unit level -- both half-duplex sales count)
# ============================================================================
dfSale <- st_read(fileGpkg, query =
  "SELECT ROLL_NUMBER, CONVEYANCE_DATE, CONVEYANCE_PRICE, CONVEYANCE_TYPE_DESCRIPTION
   FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_SALES_SV
   WHERE JURISDICTION_CODE = '200'")
dtSale <- as.data.table(st_drop_geometry(dfSale))
cat("\n=== distinct conveyance types (pick the arm's-length one) ===\n")
print(dtSale[, .N, by = CONVEYANCE_TYPE_DESCRIPTION][order(-N)])

dtSale[, rollNum := as.numeric(ROLL_NUMBER)]
dtSale[, price   := as.numeric(CONVEYANCE_PRICE)]
dtSale[, saleYear := as.numeric(substr(CONVEYANCE_DATE, 1, 4))]
dtSale <- dtSale[CONVEYANCE_TYPE_DESCRIPTION == "Improved Single Property Transaction" &
                 price > 0 & saleYear %between% c(SALE_MINYEAR, SALE_MAXYEAR)]

# join unit-level sale to unit-level use/vintage/side (dtD, not parcel), + parcel width
# NEIGHBOURHOOD carried through for the hood-FE spec in sec.8d
dtU <- merge(dtSale, dtD[, .(rollNum, type, side, newBuild, MB_Year_Built,
                             MB_Effective_Year, NEIGHBOURHOOD,
                             MB_Total_Finished_Area, w2019, wGpkgCol)],
             by = "rollNum")
dtUn <- dtU[newBuild == TRUE & !is.na(type)]
cat("\n=== recent (", SALE_MINYEAR, "-", SALE_MAXYEAR,
    ") sales of new-build stock: n by type x side ===\n", sep="")
print(dtUn[, .(n = .N, p50price = median(price)), by = .(type, side)][order(type, side)])

# ---- lot-size bin (same taxonomy as sec.7) + west indicator, on the sale side ----
dtUn[, wUse := fifelse(!is.na(w2019), w2019, wGpkgCol)]      # best width per unit
dtUn[, wc   := round(wUse)]
dtUn[, lotSize := lotBin(wc)]
dtUn[, west := as.integer(side == "West")]

# ============================================================================
# 8b. THE INSIGHT: WEST vs EAST price, by product type x lot size
#     Unconditional means -- no FE, no area control, no logs. Just the gap.
# ============================================================================
cat("\n=== WEST vs EAST price by product type x lot size (unconditional) ===\n")
cell <- dtUn[, .(n = .N, meanP = mean(price), medP = as.numeric(median(price))),
             by = .(type, lotSize, side)]
# wide: one row per type x lotSize, East vs West side by side + ratio
w <- dcast(cell, type + lotSize ~ side,
           value.var = c("n","meanP","medP"))
setnames(w,
  c("n_East","n_West","meanP_East","meanP_West","medP_East","medP_West"),
  c("nE","nW","meanE","meanW","medE","medW"), skip_absent = TRUE)
w[, ratioMean := meanW / meanE]
w[, ratioMed  := medW  / medE]
setorder(w, type, lotSize)
print(w[, .(type, lotSize, nE, nW,
            meanE = round(meanE), meanW = round(meanW), ratioMean = round(ratioMean,3),
            medE  = round(medE),  medW  = round(medW),  ratioMed  = round(ratioMed,3))])

cat("\n=== same, medians only (compact) ===\n")
print(dcast(dtUn, type + lotSize ~ side,
            value.var = "price", fun.aggregate = median)[order(type, lotSize)])

# ============================================================================
# 8c. Lot-size DISTRIBUTION on the sales sample (is 50 even the norm off-33?)
#     Counts + shares by type x side, and the raw width quantiles among non-33.
# ============================================================================
cat("\n=== lot-size mix among SALES, count by type x lotSize x side ===\n")
print(dcast(dtUn, type + side ~ lotSize,
            fun.aggregate = length, value.var = "price")[order(type, side)])

cat("\n=== lot-size mix among SALES, within-cell SHARE (type x side sums to 1) ===\n")
shr <- dtUn[, .N, by = .(type, side, lotSize)]
shr[, share := N / sum(N), by = .(type, side)]
print(dcast(shr, type + side ~ lotSize, value.var = "share")[order(type, side)])

cat("\n=== width quantiles among NON-33 sales (what does 'other'/'50' actually contain) ===\n")
print(dtUn[lotSize != "33" & !is.na(wUse),
           .(n = .N,
             p10 = round(quantile(wUse, .10)),
             p25 = round(quantile(wUse, .25)),
             p50 = round(quantile(wUse, .50)),
             p75 = round(quantile(wUse, .75)),
             p90 = round(quantile(wUse, .90))),
           by = .(type, side)][order(type, side)])

# ============================================================================
# 8d. 33 vs 50 PREMIUM WITHIN NEIGHBOURHOOD (absorb NEIGHBOURHOOD FE)
#     50s sit further south -- arguably worse, certainly different locations --
#     so let hood FE net out location and read the pure 33-vs-50 lot premium.
#     log(price), 33 = reference; SEs clustered on NEIGHBOURHOOD.
# ============================================================================
dtFE <- dtUn[lotSize %in% c("33","50") & !is.na(NEIGHBOURHOOD) & price > 0]
dtFE[, lotSize := droplevels(lotSize)]
dtFE[, lot50   := as.integer(lotSize == "50")]      # 33 = reference category

# how much identifying variation survives the FE: within-hood 33 AND 50 both present?
cat("\n=== within-neighbourhood 33-vs-50 identifying variation (by type) ===\n")
idvar <- dtFE[, .(n33 = sum(lot50 == 0), n50 = sum(lot50 == 1)), by = .(type, NEIGHBOURHOOD)]
print(idvar[, .(hoods              = .N,
                hoodsBothPresent   = sum(n33 > 0 & n50 > 0),
                salesInBothHoods   = sum((n33 + n50)[n33 > 0 & n50 > 0])),
            by = type])

cat("\n=== 33 vs 50 log-price premium, NEIGHBOURHOOD FE, by type ===\n")
for (ty in c("SFD","duplex")) {
  dTy <- dtFE[type == ty]
  m <- feols(log(price) ~ lot50 | NEIGHBOURHOOD, data = dTy, cluster = ~NEIGHBOURHOOD)
  cat("\n---", ty, "  (n =", nrow(dTy),
      "; hoods =", uniqueN(dTy$NEIGHBOURHOOD), ") ---\n")
  print(summary(m))
  cat(sprintf("  50-vs-33 premium: %+.1f%% (exp(b)-1)\n",
              100 * (exp(coef(m)[["lot50"]]) - 1)))
}

# pooled: let the 50 premium differ by type, common hood FE
cat("\n=== pooled: 50 premium interacted with type, NEIGHBOURHOOD FE ===\n")
mAll <- feols(log(price) ~ i(type, lot50) | NEIGHBOURHOOD + type,
              data = dtFE, cluster = ~NEIGHBOURHOOD)
print(summary(mAll))

# ============================================================================
# 8e. 33 vs 50 premium, SEPARATELY by type x side (4 regressions)
#     East SFD, West SFD, East duplex, West duplex. Hood FE, 33 = reference.
# ============================================================================
cat("\n=== 33 vs 50 log-price premium, NEIGHBOURHOOD FE, by type x side ===\n")
for (ty in c("SFD","duplex")) for (sd in c("East","West")) {
  dCell <- dtFE[type == ty & side == sd]
  m <- feols(log(price) ~ lot50 | NEIGHBOURHOOD, data = dCell, cluster = ~NEIGHBOURHOOD)
  cat(sprintf("\n--- %s %s  (n = %d; hoods = %d) ---\n",
              sd, ty, nrow(dCell), uniqueN(dCell$NEIGHBOURHOOD)))
  print(summary(m))
  cat(sprintf("  50-vs-33 premium: %+.1f%% (exp(b)-1)\n",
              100 * (exp(coef(m)[["lot50"]]) - 1)))
}

# ============================================================================
# 8f. THE DOUBLE DIFFERENCE: does the 50-vs-33 frontage premium differ
#     duplex-vs-SFD, and does THAT differ West-vs-East?
#     lot50 x type x side, hood x type FE so each type's gradient is within-hood.
# ============================================================================
dtFE[, dup  := as.integer(type == "duplex")]   # SFD = reference
# main DiD: 50-premium x product, separately by side (read the two 2-way cells)
cat("\n=== 50-vs-33 premium x product (SFD vs duplex), by side ===\n")
for (sd in c("East","West")) {
  m <- feols(log(price) ~ lot50 * dup | NEIGHBOURHOOD^type,
             data = dtFE[side == sd], cluster = ~NEIGHBOURHOOD)
  cat(sprintf("\n--- %s (n = %d) ---\n", sd, nrow(dtFE[side==sd])))
  print(summary(m))
}

# triple diff: is the (duplex-vs-SFD) frontage gap itself different West vs East?
cat("\n=== TRIPLE DIFF: lot50 x dup x west ===\n")
mDDD <- feols(log(price) ~ lot50 * dup * west | NEIGHBOURHOOD^type,
              data = dtFE, cluster = ~NEIGHBOURHOOD)
print(summary(mDDD))

cnt <- dtFE[, .(has = uniqueN(paste(lot50, dup))), by = NEIGHBOURHOOD]
cat("hoods with all 4 lot50xdup cells:", cnt[has == 4, .N], "of", nrow(cnt), "\n")

# ============================================================================
# 8g. PRICE DENSITY OVERLAY: duplex vs SFD, recent new-build stock (DOLLARS)
#     Comparable unit of account: a duplex BUILDING (2 units) vs one SFD.
#     Substitution is whole-house vs whole-duplex, so the half-duplex sale is
#     doubled (priceCmp). Global-substitution prediction: the duplex price
#     DISTRIBUTION is right-truncated relative to SFD -- duplex mass dies off in
#     the price band where SFD is still thick (a whole house stays attainable).
# ============================================================================
TRIM_PCT <- 0.01     # drop top & bottom this fraction of price, within type x side
dirPlot  <- "text"

dtDen <- dtUn[!is.na(price) & price > 0 & type %in% c("SFD","duplex")]
dtDen[, priceCmp := fifelse(type == "duplex", 2 * price, price)]   # 2 units vs 1 SFD

# --- trim outliers within type x side (each product/side gets its own cut) ---
dtDen[, keep := priceCmp > quantile(priceCmp, TRIM_PCT) &
                priceCmp < quantile(priceCmp, 1 - TRIM_PCT), by = .(type, side)]
cat(sprintf("\n=== trimmed %.0f%% each tail within type x side: dropped %d of %d ===\n",
            100*TRIM_PCT, sum(!dtDen$keep), nrow(dtDen)))
dtDen <- dtDen[keep == TRUE]

# --- upper-tail comparison: where does each product's mass sit? ($) ---
cat("\n=== comparable-price quantiles by type (duplex = 2 units, trimmed) ===\n")
print(dtDen[, .(n = .N,
                p50 = round(quantile(priceCmp, .50)),
                p75 = round(quantile(priceCmp, .75)),
                p90 = round(quantile(priceCmp, .90)),
                p95 = round(quantile(priceCmp, .95))),
            by = type][order(type)])

# share above SFD median / p75 -- the "attainable whole house" thresholds
sfdMed <- dtDen[type == "SFD", median(priceCmp)]
sfdP75 <- dtDen[type == "SFD", quantile(priceCmp, .75)]
cat(sprintf("\n=== share of sales above SFD median ($%s) and SFD p75 ($%s) ===\n",
            format(round(sfdMed), big.mark=","), format(round(sfdP75), big.mark=",")))
print(dtDen[, .(n = .N,
                shareAboveSFDmed = round(mean(priceCmp > sfdMed), 3),
                shareAboveSFDp75 = round(mean(priceCmp > sfdP75), 3)),
            by = type][order(type)])

cat("\n=== upper-tail share by type x side ===\n")
print(dtDen[, .(n = .N,
                shareAboveSFDmed = round(mean(priceCmp > sfdMed), 3)),
            by = .(type, side)][order(type, side)])

mfmt <- function(x) paste0("$", formatC(x/1e6, format="f", digits=1), "M")

# --- PNG 1: pooled, two lines (duplex building vs SFD) ---
dS <- density(dtDen[type == "SFD",    priceCmp])
dD <- density(dtDen[type == "duplex", priceCmp])
xlim <- range(dtDen$priceCmp)
ylim <- c(0, max(dS$y, dD$y) * 1.05)

png(file.path(dirPlot, "duplexVsSFD_priceDensity.png"),
    width = 8, height = 5, units = "in", res = 200)
plot(dS, xlim = xlim, ylim = ylim, lwd = 2, col = "black", xaxt = "n",
     main = "New-build price density: SFD vs duplex (2 units)",
     xlab = "sale price", ylab = "density")
axis(1, at = pretty(xlim), labels = mfmt(pretty(xlim)))
lines(dD, lwd = 2, col = "red", lty = 2)
abline(v = sfdMed, col = "grey50", lty = 3)
abline(v = sfdP75, col = "grey50", lty = 3)
legend("topright", bty = "n",
       legend = c("SFD", "duplex (2 units)", "SFD median / p75"),
       col = c("black","red","grey50"), lwd = c(2,2,1), lty = c(1,2,3))
dev.off()
cat("\n  wrote", file.path(dirPlot, "duplexVsSFD_priceDensity.png"), "\n")

# --- PNG 2: split East/West, two lines each ---
png(file.path(dirPlot, "duplexVsSFD_priceDensity_bySide.png"),
    width = 11, height = 5, units = "in", res = 200)
par(mfrow = c(1,2))
for (sd in c("East","West")) {
  dSs <- density(dtDen[type=="SFD"    & side==sd, priceCmp])
  dDs <- density(dtDen[type=="duplex" & side==sd, priceCmp])
  xl  <- range(dtDen[side==sd, priceCmp])
  plot(dSs, xlim = xl, ylim = c(0, max(dSs$y, dDs$y)*1.05), xaxt = "n",
       lwd = 2, col = "black", main = paste(sd, "side"),
       xlab = "sale price", ylab = "density")
  axis(1, at = pretty(xl), labels = mfmt(pretty(xl)))
  lines(dDs, lwd = 2, col = "red", lty = 2)
  legend("topright", bty="n", legend = c("SFD","duplex (2 units)"),
         col = c("black","red"), lwd = 2, lty = c(1,2))
}
dev.off()
cat("  wrote", file.path(dirPlot, "duplexVsSFD_priceDensity_bySide.png"), "\n")

# ============================================================================
# 8h. SAME densities, split by LOT SIZE: 33 vs 50, duplex vs SFD (four lines).
#     Duplex still doubled (priceCmp). The cap story predicts the 50-duplex
#     spike is pinned at the base of the 50-SFD tail (esp. West); 33s overlap.
# ============================================================================
dt4 <- dtUn[!is.na(price) & price > 0 & type %in% c("SFD","duplex") &
            lotSize %in% c("33","50")]
dt4[, priceCmp := fifelse(type == "duplex", 2 * price, price)]   # 2 units vs 1 SFD
dt4[, grp := paste0(type, " ", as.character(lotSize))]           # e.g. "duplex 33"

# trim within each of the four series x side (same rule as 8g)
dt4[, keep := priceCmp > quantile(priceCmp, TRIM_PCT) &
              priceCmp < quantile(priceCmp, 1 - TRIM_PCT), by = .(grp, side)]
dt4 <- dt4[keep == TRUE]

cat("\n=== 33/50 x type series (duplex = 2 units): n by side ===\n")
print(dcast(dt4, grp ~ side, fun.aggregate = length, value.var = "priceCmp"))

grpLev <- c("SFD 33","SFD 50","duplex 33","duplex 50")
grpCol <- c("black",  "grey45","red",      "orange")
grpLty <- c(1,        1,       2,          2)
names(grpCol) <- names(grpLty) <- grpLev

drawLines <- function(dsub, xr) {
  dens <- list(); ymax <- 0
  for (g in grpLev) {
    v <- dsub[grp == g, priceCmp]
    if (length(v) < 5) next
    dd <- density(v); dens[[g]] <- dd; ymax <- max(ymax, dd$y)
  }
  plot(NA, xlim = xr, ylim = c(0, ymax*1.05), xaxt = "n",
       xlab = "sale price", ylab = "density", main = "")
  axis(1, at = pretty(xr), labels = mfmt(pretty(xr)))
  for (g in names(dens))
    lines(dens[[g]], lwd = 2, col = grpCol[g], lty = grpLty[g])
  legend("topright", bty = "n", legend = grpLev,
         col = grpCol, lwd = 2, lty = grpLty)
}

# --- PNG 3: pooled (both sides), four lines ---
png(file.path(dirPlot, "duplexVsSFD_priceDensity_byLot.png"),
    width = 8, height = 5, units = "in", res = 200)
drawLines(dt4, range(dt4$priceCmp))
title(main = "New-build price density: 33 vs 50, duplex (2 units) vs SFD")
dev.off()
cat("\n  wrote", file.path(dirPlot, "duplexVsSFD_priceDensity_byLot.png"), "\n")

# --- PNG 4: split East/West, four lines each ---
png(file.path(dirPlot, "duplexVsSFD_priceDensity_byLot_bySide.png"),
    width = 11, height = 5, units = "in", res = 200)
par(mfrow = c(1,2))
for (sd in c("East","West")) {
  drawLines(dt4[side == sd], range(dt4[side == sd, priceCmp]))
  title(main = paste(sd, "side"))
}
dev.off()
cat("  wrote", file.path(dirPlot, "duplexVsSFD_priceDensity_byLot_bySide.png"), "\n")

# ============================================================================
# 8i. THE SUBSTITUTION, IN ONE PLOT: three unconditional (on lot size) densities
#     - West duplex (raw half-duplex price -- the West buyer's actual outlay)
#     - East SFD    (the whole-house alternative at that budget)
#     - West SFD    (the whole house the West duplex buyer is priced out of)
#     Global-substitution read: West duplex should sit ON TOP OF East SFD (same
#     money, whole house vs half duplex), and well LEFT of West SFD.
# ============================================================================
d3 <- dtUn[!is.na(price) & price > 0 &
           ((type == "duplex" & side == "West") |
            (type == "SFD"    & side == "East") |
            (type == "SFD"    & side == "West"))]
d3[, grp := fifelse(type == "duplex", "West duplex",
             fifelse(side == "East", "East SFD", "West SFD"))]

# trim within each series (same TRIM_PCT rule; raw price, no doubling)
d3[, keep := price > quantile(price, TRIM_PCT) &
             price < quantile(price, 1 - TRIM_PCT), by = grp]
d3 <- d3[keep == TRUE]

cat("\n=== three-series unconditional: n and price quantiles ===\n")
print(d3[, .(n = .N,
             p25 = round(quantile(price,.25)),
             p50 = round(quantile(price,.50)),
             p75 = round(quantile(price,.75)),
             p90 = round(quantile(price,.90))),
         by = grp][order(grp)])

grp3 <- c("West duplex","East SFD","West SFD")
col3 <- c("red",        "black",   "grey45")
lty3 <- c(2,            1,         1)
names(col3) <- names(lty3) <- grp3

den3 <- lapply(grp3, function(g) density(d3[grp == g, price]))
names(den3) <- grp3
xl3  <- range(d3$price)
yl3  <- c(0, max(sapply(den3, function(d) max(d$y))) * 1.05)

png(file.path(dirPlot, "westDuplex_vs_SFD_unconditional.png"),
    width = 8, height = 5, units = "in", res = 200)
plot(NA, xlim = xl3, ylim = yl3, xaxt = "n",
     main = "West duplex vs East / West single family (unconditional)",
     xlab = "sale price", ylab = "density")
axis(1, at = pretty(xl3), labels = mfmt(pretty(xl3)))
for (g in grp3) lines(den3[[g]], lwd = 2, col = col3[g], lty = lty3[g])
legend("topright", bty = "n", legend = grp3,
       col = col3, lwd = 2, lty = lty3)
dev.off()
cat("\n  wrote", file.path(dirPlot, "westDuplex_vs_SFD_unconditional.png"), "\n")
