# duplexPropensity_W_33v50.R
# TIGHT COMBINE of categorizeVancouverSingle.R + vancouverPermitShareByWidth.R + preReformPuzzle.R
# ONE question: on the WEST side, does duplex propensity differ between 33s and 50s?
# Duplex era 2019-2023, R1-1 permits, denominator = single/duplex principal new-build.
# If this isn't significant, the 33-vs-50 West split isn't a main course.
# Everything else from the three files is dropped as scaffolding.
# Tom Davidoff 09/15/26
# =============================================================

library(data.table); library(sf); library(fixest)

dir_path <- "~/DropboxExternal/dataRaw"
gpkg     <- "~/bigFiles/latestSpatialBCA/2026-04-08_bca_folios.gpkg"
inv_path <- file.path(dir_path, "Residential_inventory_202601",
                      "20260101_A09_Residential_Inventory_Extract.txt")
out_dir  <- "text"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

CRITVAL <- 0.25; ONT <- -123.101
desc_tbl <- "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV"
W33 <- 32:34; W50 <- 49:51; DUP_ERA <- 2019:2023
numify   <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", x)))
roll_key <- function(x) suppressWarnings(as.numeric(gsub("[^0-9]", "", x)))

# ---- permits -> R1-1 -> use/value cols ----
permits_dt <- fread(file.path(dir_path, "issued-building-permitsDwellingUses.csv"))
permits_dt <- permits_dt[!is.na(geo_point_2d) & geo_point_2d != ""]
permits_dt[, c("lat","lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = FALSE)]
permits_dt[, `:=`(lat = as.numeric(lat), lon = as.numeric(lon))]
permits_dt <- permits_dt[is.finite(lat) & is.finite(lon) &
                         lat %between% c(49.0,49.4) & lon %between% c(-123.3,-123.0)]
permits_sf <- st_as_sf(permits_dt, coords = c("lon","lat"), crs = 4326)

zoning_sf <- st_read(file.path(dir_path, "vancouver_zoning.geojson"), quiet = TRUE)
zoning_sf <- st_make_valid(st_transform(zoning_sf, st_crs(permits_sf)))["zoning_district"]

dt <- as.data.table(st_join(permits_sf, zoning_sf, join = st_intersects))
id_col <- grep("permit.*number|permitnumber|^id$", names(dt), ignore.case = TRUE, value = TRUE)[1]
dt[, is_r1 := grepl("^R1-1", zoning_district, ignore.case = TRUE)]
setorderv(dt, c(id_col, "is_r1"), c(1, -1))
dt <- unique(dt, by = id_col)[grepl("^R1-1", zoning_district, ignore.case = TRUE)]

use_col <- grep("specific.*use|specificuse", names(dt), ignore.case = TRUE, value = TRUE)
setnames(dt, use_col, "specific_use")
tow_col <- grep("type.*of.*work|typeofwork", names(dt), ignore.case = TRUE, value = TRUE)[1]
val_col <- grep("project.*value|projectvalue|construction.*value|^value$",
                names(dt), ignore.case = TRUE, value = TRUE)[1]
setnames(dt, c(tow_col, val_col), c("type_of_work", "project_value"))
if (!is.numeric(dt$project_value))
  dt[, project_value := as.numeric(gsub("[$,]", "", project_value))]

yr_col <- grep("issue.*year|permit.*year|^year$", names(dt), ignore.case = TRUE, value = TRUE)[1]
if (is.na(yr_col)) {
  dt_col <- grep("issue.*date|permit.*date|applied.*date|date",
                 names(dt), ignore.case = TRUE, value = TRUE)[1]
  dt[, year := as.integer(substr(as.character(get(dt_col)), 1, 4))]
} else dt[, year := as.integer(get(yr_col))]

su <- dt$specific_use
dt[, use_class := fifelse(grepl("Multiple|Multiplex|Conversion", su, ignore.case = TRUE), "Multiplex",
                 fifelse(grepl("Duplex|Two-Family",             su, ignore.case = TRUE), "Duplex",
                 fifelse(grepl("Laneway",                       su, ignore.case = TRUE), "Laneway",
                 fifelse(grepl("Sec Suite|Secondary Suite|Family Suite", su, ignore.case = TRUE), "SFD+suite",
                 fifelse(grepl("^Infill|Dwelling Unit",         su, ignore.case = TRUE), "Infill-other",
                 fifelse(su == "Single Detached House",         "Plain-Jane", NA_character_))))))]

# ---- principal single/duplex, symmetric new-build cost floor (pooled 25th pctile) ----
is_new <- grepl("New Building|New Construction", dt$type_of_work, ignore.case = TRUE)
dt[, principalType := fifelse(use_class == "Duplex", "duplex",
                      fifelse(use_class %in% c("Plain-Jane","SFD+suite"), "single", NA_character_))]
FLOOR <- quantile(dt[is_new & !is.na(principalType), project_value], CRITVAL, na.rm = TRUE)
keep <- dt[!is.na(principalType) & is_new & project_value > FLOOR]
xy <- st_coordinates(st_as_sf(keep)); keep[, `:=`(lon = xy[,1], lat = xy[,2])]

# ---- BCA width (native-CRS bbox) + inventory fallback ----
keep_sf <- st_as_sf(keep, coords = c("lon","lat"), crs = 4326, remove = FALSE)
probe   <- st_read(gpkg, layer = desc_tbl, quiet = TRUE,
                   query = sprintf('SELECT * FROM "%s" LIMIT 1', desc_tbl))
bb <- st_bbox(st_transform(keep_sf, st_crs(probe)))
bb["xmin"] <- bb["xmin"]-300; bb["xmax"] <- bb["xmax"]+300
bb["ymin"] <- bb["ymin"]-300; bb["ymax"] <- bb["ymax"]+300
folio_sf <- st_read(gpkg, layer = desc_tbl, quiet = TRUE,
                    wkt_filter = st_as_text(st_as_sfc(bb)))
stopifnot(nrow(folio_sf) > 0)
folio_keep <- folio_sf[, c("ROLL_NUMBER","LAND_WIDTH","LAND_UNITS")]

keep_sf <- st_transform(keep_sf, st_crs(folio_keep))
kj <- unique(as.data.table(st_join(keep_sf, folio_keep, join = st_intersects)), by = id_col)
kj[, roll := roll_key(ROLL_NUMBER)]

inv <- fread(inv_path, colClasses = "character")
inv[, `:=`(roll = roll_key(Roll_Number), inv_width = numify(Land_Width_Width),
           metric = trimws(Land_Metric_Flag))]
inv[metric %in% c("Y","1","M","T","X") & is.finite(inv_width), inv_width := inv_width/0.3048]
kj <- merge(kj, unique(inv[is.finite(roll), .(roll, inv_width)], by = "roll"), by = "roll", all.x = TRUE)

kj[, gpkg_width := numify(LAND_WIDTH)]
kj[grepl("metre|meter|^m$", LAND_UNITS, ignore.case = TRUE) & is.finite(gpkg_width),
   gpkg_width := gpkg_width/0.3048]
kj[, width_ft := fifelse(is.finite(gpkg_width) & gpkg_width > 0, gpkg_width, as.numeric(inv_width))]
kj[, `:=`(width_bucket = fifelse(round(width_ft) %in% W33, "33",
                          fifelse(round(width_ft) %in% W50, "50", NA_character_)),
          side = fifelse(lon < ONT, "West", "East"))]

# ============================================================
# THE TEST: West-side duplex propensity, 33 vs 50, duplex era
# ============================================================
S <- kj[!is.na(width_bucket) & !is.na(principalType) & year %in% DUP_ERA]
S[, dup   := as.integer(principalType == "duplex")]
S[, lot50 := as.integer(width_bucket == "50")]

cat("=== cell counts (dup / total), by side x width ===\n")
print(S[, .(n = .N, dup = sum(dup), dupShare = round(mean(dup), 3)),
        by = .(side, width_bucket)][order(side, width_bucket)])

W <- S[side == "West"]

# 1. Fisher exact (small-n honest) on the West 2x2: 33/50 x single/duplex
cat("\n=== WEST 33 vs 50: 2x2 and Fisher exact ===\n")
tab <- table(width = W$width_bucket, dup = W$dup)
print(tab)
ft <- fisher.test(tab)
cat(sprintf("Fisher exact: OR=%.3f  p=%.4f  95%% CI [%.3f, %.3f]\n",
            ft$estimate, ft$p.value, ft$conf.int[1], ft$conf.int[2]))

# 2. LPM with heteroskedastic SE (the number you'd report), West only
cat("\n=== WEST 33 vs 50: LPM P(duplex) ~ lot50, hetero SE ===\n")
mW <- feols(dup ~ lot50, data = W, vcov = "hetero")
print(summary(mW))
cat(sprintf("  West 50-minus-33 duplex-share gap = %+.3f (p=%.4f)\n",
            coef(mW)[["lot50"]], pvalue(mW)[["lot50"]]))

# 3. Is the West gap distinguishable from the East gap? (triple-ish interaction)
cat("\n=== Pooled: dup ~ lot50 * west  (is the 33-50 gap West-specific?) ===\n")
S[, west := as.integer(side == "West")]
mI <- feols(dup ~ lot50 * west, data = S, vcov = "hetero")
print(summary(mI))
cat("  lot50:west < 0 & sig  => the West 50-lot duplex fade is real and West-specific\n")
cat("  lot50:west n.s.       => 33-vs-50 split not a main course; write it off\n")

# ============================================================
# 4. Raw single/duplex counts by side x width x year, from 2018.
#    Plus P(duplex | single or duplex) = the conditional odds that IIA
#    predicts flat: if this jumps at 2024, the reform moved the
#    duplex-vs-single margin itself (not just the denominator).
# ============================================================
C <- kj[!is.na(width_bucket) & !is.na(principalType) & year >= 2018]
cnt <- dcast(C[, .N, by = .(side, width_bucket, year, principalType)],
             side + width_bucket + year ~ principalType, value.var = "N", fill = 0)
cnt[, `:=`(tot = single + duplex, dupCond = round(duplex / (single + duplex), 3))]
setorder(cnt, side, width_bucket, year)
cat("\n=== single/duplex counts by side x width x year (>=2018) ===\n")
print(cnt[])

# ============================================================
# 5. New-duplex finished area by year-built x width x side, from the
#    LATEST (Jan 2026) inventory -> post-reform completions appear.
#    MB_Year_Built ~ construction/effective date, so 2024-25 = reform
#    cohort. Basement_Finish_Area breaks out the suite/lock-off mechanism:
#    if the fattened duplex is "same box, more doors", basement area rises
#    faster than total. Duplexes only, keyed to inventory by roll.
# ============================================================
invA <- fread(inv_path, colClasses = "character")
invA[, roll := roll_key(Roll_Number)]
invA[, `:=`(mb_area = numify(MB_Total_Finished_Area),
            mb_year = as.integer(numify(MB_Year_Built)),
            bsmt    = numify(Basement_Finish_Area))]

DUP <- kj[principalType == "duplex" & !is.na(width_bucket)]
DUP <- merge(DUP, unique(invA[is.finite(roll),
             .(roll, mb_area, mb_year, bsmt)], by = "roll"),
             by = "roll", all.x = TRUE)

# match-rate audit FIRST: thin/under-construction 2025 cohort may not have
# a finished area yet -> completed ones are a selected (fast/smaller?) sample.
cat("\n=== duplex rows matched to an inventory finished-area, by width x built-year ===\n")
print(dcast(DUP[, .(matchRate = round(mean(is.finite(mb_area)), 2), n = .N),
                by = .(mb_year, width_bucket)][order(mb_year)],
            mb_year ~ width_bucket, value.var = c("n","matchRate")))

# finished area by built-year x width x side (median = honest stat at small n)
A <- DUP[is.finite(mb_area) & mb_area > 200 & mb_area < 12000 & is.finite(mb_year) & mb_year >= 2015]
cat("\n=== new-duplex finished sqft by year-built x width x side ===\n")
tab <- A[, .(n = .N, medSqft = round(median(mb_area)), meanSqft = round(mean(mb_area)),
             medBsmt = round(median(bsmt, na.rm = TRUE)),
             p25 = round(quantile(mb_area, .25)), p75 = round(quantile(mb_area, .75))),
         by = .(mb_year, width_bucket, side)]
setorder(tab, width_bucket, side, mb_year)
print(tab[])

# compact pivots: median total sqft, and median basement, 33 vs 50 within side
cat("\n=== median duplex TOTAL sqft: (year x side) x width ===\n")
print(dcast(A[, .(med = median(mb_area)), by = .(mb_year, side, width_bucket)],
            mb_year + side ~ width_bucket, value.var = "med"))
cat("\n=== median duplex BASEMENT-finish sqft (suite/lock-off proxy) ===\n")
print(dcast(A[, .(med = round(median(bsmt, na.rm = TRUE))), by = .(mb_year, side, width_bucket)],
            mb_year + side ~ width_bucket, value.var = "med"))

# ============================================================
# 6. rollStart RECOVERY of stratified duplex completions.
#    Permit is filed on parent folio XXXXXX000; on completion the duplex
#    stratifies into XXXXXX001 / XXXXXX002, retiring the parent. The exact-
#    roll join in section 5 therefore MISSES completed duplexes systematically
#    (the newest, most-built ones). rollStart = floor(roll/1000) reunites the
#    parent permit with its child unit rows.
#
#    Storey/floor fields are per-STRUCTURE (both units share them) -> the two
#    children must agree; that's a built-in match-validity check. Area is
#    per-UNIT on the children -> sum for building, median for unit.
# ============================================================
invA[, `:=`(rollStart = floor(roll/1000),
            storeys   = numify(MB_Num_Storeys),
            f2        = numify(Second_Floor_Area),
            f3        = numify(Third_Floor_Area))]
kj[, rollStart := floor(roll/1000)]

# ---- 6a. COLLISION TEST: is rollStart unique per duplex permit? ----
# If distinct duplex permits share a rollStart, the prefix cross-contaminates
# the 33-vs-50 x E/W contrast we care about. Want this near-empty.
dupKj <- kj[principalType == "duplex" & !is.na(width_bucket) & is.finite(rollStart)]
coll <- dupKj[, .(nPermits = uniqueN(get(id_col))), by = rollStart][nPermits > 1]
cat(sprintf("\n=== rollStart collision test: %d prefixes shared by >1 duplex permit (of %d) ===\n",
            nrow(coll), uniqueN(dupKj$rollStart)))
if (nrow(coll) > 0) {
  cat("  collisions exist -> prefix is NOT a clean parcel key; results below suspect.\n")
  print(head(coll[order(-nPermits)], 10))
  # do collisions straddle width buckets? that's the damaging kind
  straddle <- merge(dupKj, coll[, .(rollStart)], by = "rollStart")[
              , .(widths = uniqueN(width_bucket)), by = rollStart][widths > 1]
  cat(sprintf("  ...of which %d straddle >1 width bucket (the harmful kind)\n", nrow(straddle)))
} else cat("  clean: rollStart is unique per duplex permit.\n")

# ---- 6b. Aggregate inventory to one row per parent parcel ----
# storeyRange across children = 0 confirms the child rows are one structure.
invAgg <- invA[is.finite(rollStart), .(
    nUnits      = .N,
    mb_year     = as.integer(median(numify(MB_Year_Built), na.rm = TRUE)),
    storeys     = median(storeys, na.rm = TRUE),
    storeyRange = suppressWarnings(max(storeys, na.rm = TRUE) - min(storeys, na.rm = TRUE)),
    f3_any      = as.integer(any(f3 > 0, na.rm = TRUE)),
    f2med       = median(f2, na.rm = TRUE),
    f3med       = median(f3, na.rm = TRUE),
    areaBuild   = sum(mb_area, na.rm = TRUE),      # both units => whole building
    areaUnit    = median(mb_area, na.rm = TRUE)),  # one unit
  by = rollStart]

DUP2 <- merge(dupKj, invAgg, by = "rollStart", all.x = TRUE)

# ---- 6c. Recovery gain: exact-roll vs rollStart match rates side by side ----
# Compute each rate on its own frame keyed to the same cells (no fragile
# dynamic-id self-merge). exact = section-5 DUP; start = section-6 DUP2.
exactRate <- DUP[is.finite(mb_year) & mb_year >= 2019,
                 .(exactRate = round(mean(is.finite(mb_area)), 2), nE = .N),
                 by = .(width_bucket, mb_year)]
startRate <- DUP2[is.finite(mb_year) & mb_year >= 2019,
                  .(startRate = round(mean(is.finite(areaUnit)), 2), nS = .N),
                  by = .(width_bucket, mb_year)]
cat("\n=== match-rate GAIN: exact-roll (sec.5) vs rollStart (sec.6), by width x built-year ===\n")
print(merge(exactRate, startRate, by = c("width_bucket","mb_year"),
            all = TRUE)[order(width_bucket, mb_year)])

# ---- 6d. CONSISTENCY: do the child units agree on storeys? (want range 0) ----
cat("\n=== child-unit storey agreement (storeyRange>0 => mixed/ bad match) ===\n")
print(DUP2[nUnits > 1 & is.finite(storeyRange),
     .(nMulti = .N, agree = sum(storeyRange == 0),
       agreeRate = round(mean(storeyRange == 0), 2)), by = width_bucket])

# ---- 6e. STOREY table on recovered completions, permit-year clock ----
# permit-year (kj$year) so this aligns with the dupCond series in section 4,
# not the built-year clock. Restrict to clean single-structure matches.
G <- DUP2[storeyRange == 0 & !is.na(width_bucket) & year >= 2019]
cat("\n=== 3-storey incidence on recovered duplexes, PERMIT-year x width x side ===\n")
st <- G[, .(n = .N,
            share3plus = round(mean(storeys >= 3, na.rm = TRUE), 2),
            fullThird  = round(mean(f3med / f2med > 0.8, na.rm = TRUE), 2),   # full flat floor
            tuckThird  = round(mean(f3med > 0 & f3med / f2med <= 0.8, na.rm = TRUE), 2)),  # roof-tuck partial
        by = .(width_bucket, side, year)]
setorder(st, width_bucket, side, year)
print(st[])

# ---- 6f. the money pivot: FULL (flat, above-grade) third-storey share, W33 ----
# If tuckThird was always present but fullThird STEPS UP at a vintage, that step
# dates the height/grade liberalization (tucked partial storey -> full above-grade).
cat("\n=== FULL-third-storey share (flat above-grade), width x side x permit-year ===\n")
print(dcast(G[, .(full = round(mean(f3med / f2med > 0.8, na.rm = TRUE), 2)),
              by = .(year, side, width_bucket)],
            year + side ~ width_bucket, value.var = "full"))
