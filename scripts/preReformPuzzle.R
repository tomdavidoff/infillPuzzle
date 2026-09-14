# duplexPremium3350.R
# Duplex-vs-SFD ppsf premium, vintage-controlled, at BOTH 33 and 50, by side.
# Fills the gap: rev2 ran the premium on 33s only. The 50-ft premium E/W is the
# on-point one for the puzzle -- prediction: West duplex premium collapses toward
# zero (or below) MORE on 50s than on 33s, because the West 50-SFD ceiling is
# highest exactly there (Test 2: West ppsf flat in width).
#
# Reads bca19VancouverSalesDupSFD.rds (~2018 snapshot). land_width; effYear control.
# READ-ONLY except PNG.  Tom Davidoff 09/13/26

SALE_MINYEAR <- 2014
PREYEAR_MAX  <- 2018
ONTARIO_LON  <- -123.1036
W33 <- 32:34; W50 <- 49:51
dirPlot <- "text"

library(data.table); library(fixest)
d <- as.data.table(readRDS("~/DropboxExternal/dataProcessed/bca19VancouverSalesDupSFD.rds"))
dir.create(dirPlot, showWarnings = FALSE, recursive = TRUE)

sfdUse <- c("Single Family Dwelling", "Residential Dwelling with Suite")
d[, price := as.numeric(conveyancePrice)]
d[, saleYear := as.numeric(substring(conveyanceDate,1,4))]
d[, effYear := as.numeric(MB_effective_year)]
d[, finArea := as.numeric(MB_total_finished_area)]
d[, type := fifelse(grepl("Duplex", actualUseDescription), "duplex",
            fifelse(actualUseDescription %in% sfdUse, "SFD", NA_character_))]
d[, side := fifelse(longitude < ONTARIO_LON, "West", "East")]
d[, ppsf := fifelse(!is.na(finArea) & finArea > 0, price/finArea, NA_real_)]
d[, wc := round(as.numeric(land_width))]
d[, wbin := fifelse(wc %in% W33, "33", fifelse(wc %in% W50, "50", "other"))]

pre <- d[!is.na(type) & price>0 & !is.na(longitude) & !is.na(ppsf) & ppsf>0 &
         is.finite(effYear) & effYear>1900 &
         saleYear %between% c(SALE_MINYEAR, PREYEAR_MAX) & wbin %in% c("33","50")]
pre[, dup := as.integer(type=="duplex")]
pre[, west := as.integer(side=="West")]

# ---- premium by width x side, plus dup x west interaction within each width ----
res <- list()
for (w in c("33","50")) {
  cat(sprintf("\n======== %s-ft ========\n", w))
  dw <- pre[wbin==w]
  cat("cells (n):\n"); print(dcast(dw, side~type, fun.aggregate=length, value.var="price"))

  # area-controlled? report BOTH: raw vintage-only, and + log(finArea) to kill the
  # "smaller unit => higher ppsf" mechanical size effect (referee-proofing T1).
  for (sd in c("East","West")) {
    dd <- dw[side==sd]
    if (uniqueN(dd$dup) < 2 || sum(dd$dup) < 10) { cat(sprintf("  %s %s: too thin (dup=%d)\n", sd, w, sum(dd$dup))); next }
    m0 <- feols(log(ppsf) ~ dup | effYear + saleYear, dd, vcov="hetero")
    mA <- feols(log(ppsf) ~ dup + log(finArea) | effYear + saleYear, dd, vcov="hetero")
    cat(sprintf("  %s %s (n=%d,dup=%d):  premium %+.1f%%  | area-ctrl %+.1f%%\n",
                sd, w, nrow(dd), sum(dd$dup),
                100*(exp(coef(m0)[["dup"]])-1), 100*(exp(coef(mA)[["dup"]])-1)))
    b<-coef(m0)[["dup"]]; s<-se(m0)[["dup"]]
    res[[paste(w,sd)]] <- data.table(w=w, side=sd,
        pct=100*(exp(b)-1), lo=100*(exp(b-1.96*s)-1), hi=100*(exp(b+1.96*s)-1))
  }
  # interaction: does the duplex premium differ W vs E at THIS width?
  mI <- feols(log(ppsf) ~ dup*west | effYear + saleYear, dw, vcov="hetero")
  cat(sprintf("\n  [%s] dup:west interaction = %+.4f (t=%.2f)  <0 => West premium lower\n",
              w, coef(mI)[["dup:west"]], coef(mI)[["dup:west"]]/se(mI)[["dup:west"]]))
}

# ---- triple: is the (West premium shortfall) itself bigger on 50 than 33? ----
cat("\n======== TRIPLE: dup x west x lot50 ========\n")
pre[, lot50 := as.integer(wbin=="50")]
mT <- feols(log(ppsf) ~ dup*west*lot50 | effYear + saleYear, pre, vcov="hetero")
print(summary(mT))
cat("  dup:west:lot50 < 0 => West duplex-premium collapse is WORSE on 50s (the puzzle)\n")

# ---- PNG: premium by width x side, 95% CI ----
cm <- rbindlist(res)
png(file.path(dirPlot,"preReform_duplexPremium_33_50.png"),
    width=7.5, height=5, units="in", res=200)
cm[, y := .I]
xr <- range(c(cm$lo, cm$hi, 0))
plot(NA, xlim=xr, ylim=c(0.5, nrow(cm)+0.5), yaxt="n",
     xlab="duplex-vs-SFD ppsf premium (%), vintage-controlled", ylab="",
     main=sprintf("Pre-reform (%d-%d) duplex premium, 33 vs 50", SALE_MINYEAR, PREYEAR_MAX))
axis(2, at=cm$y, labels=paste(cm$side, cm$w), las=1)
abline(v=0, col="grey70", lty=2)
cm[, col := fifelse(side=="West","firebrick","grey30")]
for (i in cm$y) {
  segments(cm$lo[i], i, cm$hi[i], i, col=cm$col[i], lwd=2)
  points(cm$pct[i], i, pch=19, col=cm$col[i], cex=1.3)
}
dev.off()
cat("\n  wrote", file.path(dirPlot,"preReform_duplexPremium_33_50.png"), "\n")
cat("\n=== done (rev 2). ===\n")


# duplexPremium3350.R
# Duplex-vs-SFD ppsf premium, vintage-controlled, at BOTH 33 and 50, by side.
# Fills the gap: rev2 ran the premium on 33s only. The 50-ft premium E/W is the
# on-point one for the puzzle -- prediction: West duplex premium collapses toward
# zero (or below) MORE on 50s than on 33s, because the West 50-SFD ceiling is
# highest exactly there (Test 2: West ppsf flat in width).
#
# Reads bca19VancouverSalesDupSFD.rds (~2018 snapshot). land_width; effYear control.
# READ-ONLY except PNG.  Tom Davidoff 09/13/26

SALE_MINYEAR <- 2014
PREYEAR_MAX  <- 2018
ONTARIO_LON  <- -123.1036
W33 <- 32:34; W50 <- 49:51
dirPlot <- "text"

library(data.table); library(fixest)
d <- as.data.table(readRDS("~/DropboxExternal/dataProcessed/bca19VancouverSalesDupSFD.rds"))
dir.create(dirPlot, showWarnings = FALSE, recursive = TRUE)

sfdUse <- c("Single Family Dwelling", "Residential Dwelling with Suite")
d[, price := as.numeric(conveyancePrice)]
d[, saleYear := as.numeric(substring(conveyanceDate,1,4))]
d[, effYear := as.numeric(MB_effective_year)]
d[, finArea := as.numeric(MB_total_finished_area)]
d[, type := fifelse(grepl("Duplex", actualUseDescription), "duplex",
            fifelse(actualUseDescription %in% sfdUse, "SFD", NA_character_))]
d[, side := fifelse(longitude < ONTARIO_LON, "West", "East")]
d[, ppsf := fifelse(!is.na(finArea) & finArea > 0, price/finArea, NA_real_)]
d[, wc := round(as.numeric(land_width))]
d[, wbin := fifelse(wc %in% W33, "33", fifelse(wc %in% W50, "50", "other"))]

pre <- d[!is.na(type) & price>0 & !is.na(longitude) & !is.na(ppsf) & ppsf>0 &
         is.finite(effYear) & effYear>1900 &
         saleYear %between% c(SALE_MINYEAR, PREYEAR_MAX) & wbin %in% c("33","50")]
pre[, dup := as.integer(type=="duplex")]
pre[, west := as.integer(side=="West")]

# ---- premium by width x side, plus dup x west interaction within each width ----
res <- list()
for (w in c("33","50")) {
  cat(sprintf("\n======== %s-ft ========\n", w))
  dw <- pre[wbin==w]
  cat("cells (n):\n"); print(dcast(dw, side~type, fun.aggregate=length, value.var="price"))

  # area-controlled? report BOTH: raw vintage-only, and + log(finArea) to kill the
  # "smaller unit => higher ppsf" mechanical size effect (referee-proofing T1).
  for (sd in c("East","West")) {
    dd <- dw[side==sd]
    if (uniqueN(dd$dup) < 2 || sum(dd$dup) < 10) { cat(sprintf("  %s %s: too thin (dup=%d)\n", sd, w, sum(dd$dup))); next }
    m0 <- feols(log(ppsf) ~ dup | effYear + saleYear, dd, vcov="hetero")
    mA <- feols(log(ppsf) ~ dup + log(finArea) | effYear + saleYear, dd, vcov="hetero")
    cat(sprintf("  %s %s (n=%d,dup=%d):  premium %+.1f%%  | area-ctrl %+.1f%%\n",
                sd, w, nrow(dd), sum(dd$dup),
                100*(exp(coef(m0)[["dup"]])-1), 100*(exp(coef(mA)[["dup"]])-1)))
    b<-coef(m0)[["dup"]]; s<-se(m0)[["dup"]]
    res[[paste(w,sd)]] <- data.table(w=w, side=sd,
        pct=100*(exp(b)-1), lo=100*(exp(b-1.96*s)-1), hi=100*(exp(b+1.96*s)-1))
  }
  # interaction: does the duplex premium differ W vs E at THIS width?
  mI <- feols(log(ppsf) ~ dup*west | effYear + saleYear, dw, vcov="hetero")
  cat(sprintf("\n  [%s] dup:west interaction = %+.4f (t=%.2f)  <0 => West premium lower\n",
              w, coef(mI)[["dup:west"]], coef(mI)[["dup:west"]]/se(mI)[["dup:west"]]))
}

# ---- triple: is the (West premium shortfall) itself bigger on 50 than 33? ----
cat("\n======== TRIPLE: dup x west x lot50 ========\n")
pre[, lot50 := as.integer(wbin=="50")]
mT <- feols(log(ppsf) ~ dup*west*lot50 | effYear + saleYear, pre, vcov="hetero")
print(summary(mT))
cat("  dup:west:lot50 < 0 => West duplex-premium collapse is WORSE on 50s (the puzzle)\n")

# ---- PNG: premium by width x side, 95% CI ----
cm <- rbindlist(res)
png(file.path(dirPlot,"preReform_duplexPremium_33_50.png"),
    width=7.5, height=5, units="in", res=200)
cm[, y := .I]
xr <- range(c(cm$lo, cm$hi, 0))
plot(NA, xlim=xr, ylim=c(0.5, nrow(cm)+0.5), yaxt="n",
     xlab="duplex-vs-SFD ppsf premium (%), vintage-controlled", ylab="",
     main=sprintf("Pre-reform (%d-%d) duplex premium, 33 vs 50", SALE_MINYEAR, PREYEAR_MAX))
axis(2, at=cm$y, labels=paste(cm$side, cm$w), las=1)
abline(v=0, col="grey70", lty=2)
cm[, col := fifelse(side=="West","firebrick","grey30")]
for (i in cm$y) {
  segments(cm$lo[i], i, cm$hi[i], i, col=cm$col[i], lwd=2)
  points(cm$pct[i], i, pch=19, col=cm$col[i], cex=1.3)
}
dev.off()
cat("\n  wrote", file.path(dirPlot,"preReform_duplexPremium_33_50.png"), "\n")
cat("\n=== done (rev 2). ===\n")

# =============================================================
# R1-1 infill: East/West CONVERGENCE across three eras x lot width
#
# Three eras by permit year:
#   pre-duplex   <=2018        (little duplex; suite/laneway/Plain-Jane world)
#   duplex era   2019-2023     (duplex normalized, pre-R1-1)
#   multiplex era 2025-2026    (R1-1; multiplex arrives; 2024 dropped)
#
# Question: is East/West CONVERGING, and which product drives it?
# Convergence = the East and West type-composition getting more alike over time.
# Measured per era x width with a dissimilarity index over the use mix, plus
# a decomposition of which type closes the most East-West gap.
#
# Plain-Jane (bare Single Detached House) = un-infilled baseline; cost floor
# gates Plain-Jane only. Width from gpkg folio (BC Albers) w/ inventory fallback.
# =============================================================

library(data.table)
library(sf)

dir_path <- "~/DropboxExternal/dataRaw"
gpkg     <- "~/bigFiles/latestSpatialBCA/2026-04-08_bca_folios.gpkg"
inv_path <- file.path(dir_path, "Residential_inventory_202601",
                      "20260101_A09_Residential_Inventory_Extract.txt")
out_dir  <- "text"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

CRITVAL  <- 0.25; RADIUS <- 6; ONT <- -123.101
desc_tbl <- "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV"
W33 <- 32:34; W50 <- 49:51

numify   <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", x)))
roll_key <- function(x) suppressWarnings(as.numeric(gsub("[^0-9]", "", x)))

# ---- 1-4. permits -> zoning -> R1-1 -> use/value cols (as established) ----
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

# ---- 5. Year + THREE eras ----
yr_col <- grep("issue.*year|permit.*year|^year$", names(dt), ignore.case = TRUE, value = TRUE)[1]
if (is.na(yr_col)) {
  dt_col <- grep("issue.*date|permit.*date|applied.*date|date",
                 names(dt), ignore.case = TRUE, value = TRUE)[1]
  dt[, year := as.integer(substr(as.character(get(dt_col)), 1, 4))]
} else dt[, year := as.integer(get(yr_col))]
dt[, era := fifelse(year <= 2018, "1_pre-duplex",
            fifelse(year %in% 2019:2023, "2_duplex",
            fifelse(year %in% 2025:2026, "3_multiplex", NA_character_)))]

# ---- 6. Classify use, most-intensive-first ----
su <- dt$specific_use
dt[, use_class := fifelse(grepl("Multiple|Multiplex|Conversion", su, ignore.case = TRUE), "Multiplex",
                 fifelse(grepl("Duplex|Two-Family",             su, ignore.case = TRUE), "Duplex",
                 fifelse(grepl("Laneway",                       su, ignore.case = TRUE), "Laneway",
                 fifelse(grepl("Sec Suite|Secondary Suite|Family Suite", su, ignore.case = TRUE), "SFD+suite",
                 fifelse(grepl("^Infill|Dwelling Unit",         su, ignore.case = TRUE), "Infill-other",
                 fifelse(su == "Single Detached House",         "Plain-Jane", NA_character_))))))]

# ---- 7. Cost floor, applied SYMMETRICALLY to single AND duplex new-build ----
# Old behaviour gated Plain-Jane only, trimming ~25% of singles while letting every
# duplex through -> mechanically inflated duplex share. Fix: one floor from the POOLED
# single+duplex new-build value distribution, applied to BOTH principal types. Laneway
# and other non-principal types are dropped outright (not a house-vs-duplex choice).
#
# PRINCIPAL_TYPES = the two things that compete for the redevelopment of a lot.
# "single" = Plain-Jane (+ SFD+suite, a single principal building with a suite).
PRINCIPAL_SINGLE <- c("Plain-Jane", "SFD+suite")
PRINCIPAL_DUPLEX <- c("Duplex")

is_new <- grepl("New Building|New Construction", dt$type_of_work, ignore.case = TRUE)
dt[, principalType := fifelse(use_class %in% PRINCIPAL_DUPLEX, "duplex",
                      fifelse(use_class %in% PRINCIPAL_SINGLE, "single", NA_character_))]

# pooled cutoff: 25th pctile of new-build project_value across single+duplex together
minSpendPool <- quantile(dt[is_new & !is.na(principalType), project_value],
                         CRITVAL, na.rm = TRUE)
# for comparison only: the old single-only cutoff
minSpendSingle <- quantile(dt[is_new & principalType == "single", project_value],
                           CRITVAL, na.rm = TRUE)
cat(sprintf("\n=== cost floors (CRITVAL=%.2f): pooled=%s  single-only=%s ===\n",
            CRITVAL, format(round(minSpendPool), big.mark=","),
            format(round(minSpendSingle), big.mark=",")))

FLOOR <- minSpendPool          # <- switch to minSpendSingle for the single-only gate
# keep ONLY principal single/duplex, new-build, above the shared floor.
keep <- dt[!is.na(era) & !is.na(principalType) &
           is_new & project_value > FLOOR]

# report how many of each type the floor drops, so the trim is auditable
cat("=== new-build single/duplex dropped by the shared floor (pre-width) ===\n")
print(dt[!is.na(principalType) & is_new,
         .(n = .N, kept = sum(project_value > FLOOR, na.rm = TRUE),
           dropShare = round(mean(!(project_value > FLOOR) | is.na(project_value), na.rm = TRUE), 3)),
         by = principalType])

xy <- st_coordinates(st_as_sf(keep)); keep[, `:=`(lon = xy[,1], lat = xy[,2])]

# ---- 8-11. BCA width (native-CRS bbox) + inventory fallback ----
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
# 12. CONVERGENCE ENGINE
# For each era x width: composition share of each use_class within East and
# within West; the East-West gap per type; the dissimilarity index
# D = 0.5 * sum|East_share - West_share| (0 = identical mix, 1 = disjoint);
# and which type contributes most to D (and the sign of its gap).
# ============================================================
K <- kj[!is.na(width_bucket) & !is.na(era)]
types <- c("Plain-Jane","SFD+suite","Laneway","Duplex","Multiplex","Infill-other")

comp <- K[, .N, by = .(era, width_bucket, side, use_class)]
comp[, share := N / sum(N), by = .(era, width_bucket, side)]

wide <- dcast(comp, era + width_bucket + use_class ~ side, value.var = "share", fill = 0)
wide[, gap := West - East]                       # + = more West, - = more East

# dissimilarity index per era x width
D <- wide[, .(dissimilarity = round(0.5 * sum(abs(gap)), 3)), by = .(era, width_bucket)]

# largest gap-closing / gap-driving type per cell
lead <- wide[, .SD[which.max(abs(gap))], by = .(era, width_bucket),
             .SDcols = c("use_class","gap","East","West")]
setnames(lead, c("East","West"), c("East_sh","West_sh"))

cat("\n===== East-West DISSIMILARITY by era x width (0=identical mix) =====\n")
print(dcast(D, width_bucket ~ era, value.var = "dissimilarity"))

cat("\n===== Biggest East-West gap driver per era x width =====\n")
print(lead[order(width_bucket, era)][, .(era, width_bucket, use_class,
        gap = round(gap,3), East_sh = round(East_sh,3), West_sh = round(West_sh,3))])

cat("\n===== Full composition gap table (share West - East) =====\n")
gt <- dcast(wide, width_bucket + use_class ~ era, value.var = "gap", fill = 0)
print(gt[order(width_bucket, use_class)])

# ---- 13. Convergence figure: dissimilarity trend by width ----
library(ggplot2)
Dp <- copy(D); Dp[, era := factor(era, levels = c("1_pre-duplex","2_duplex","3_multiplex"))]
p <- ggplot(Dp, aes(era, dissimilarity, group = width_bucket, color = width_bucket)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  ylim(0, NA) + theme_minimal() +
  labs(title = "East-West divergence in R1-1 build mix, by lot width",
       subtitle = "Dissimilarity index over use-type composition (0 = East and West build alike)",
       y = "Dissimilarity index", x = NULL, color = "Lot width (ft)")
ggsave(file.path(out_dir, "ew_convergence.png"), p, width = 8, height = 5, dpi = 150)

# dissimilarity per YEAR x width
comp <- K[, .N, by = .(year, width_bucket, side, use_class)]
comp[, share := N / sum(N), by = .(year, width_bucket, side)]

wide <- dcast(comp, year + width_bucket + use_class ~ side, value.var = "share", fill = 0)
wide[, gap := West - East]

# per-year x width count, computed on K directly (not from the reshaped table)
nby <- K[!is.na(width_bucket), .(n = .N), by = .(year, width_bucket)]

Dy <- wide[, .(D = 0.5 * sum(abs(gap))), by = .(year, width_bucket)]
Dy <- merge(Dy, nby, by = c("year", "width_bucket"))
print(Dy[order(width_bucket, year)])

# =============================================================
# ADDENDUM: 50ft-lot substitution test — suite vs multiplex convergence
# Append after the convergence pipeline (needs K: kj filtered to
# non-NA width_bucket/era, with year, side, use_class). Run r1_infill_
# convergence.R first, or reuse `kj` from it.
#
# Question: on 50ft lots, does the West-East gap converge via SUITES early
# and hand off to MULTIPLEX post-reform (substitution), or does multiplex
# ADD convergence beyond the pre-existing suite trend?
# Gap = West_share - East_share within year (50ft only). Gap -> 0 = converged.
# =============================================================

library(data.table)
library(ggplot2)

out_dir <- "text"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 0. Identifying-assumption check: duplex ~absent on 50s ----
cat("50ft Duplex raw counts (identifying assumption = ~0):\n")
print(kj[width_bucket == "50" & use_class == "Duplex", .N, by = .(year, side)][order(year)])

# ---- 1. 50ft annual composition shares within each side ----
f50 <- kj[width_bucket == "50" & !is.na(year)]
comp <- f50[, .N, by = .(year, side, use_class)]
comp[, share := N / sum(N), by = .(year, side)]
nby  <- f50[, .(n = .N), by = year]            # annual denominator (both sides)

# ---- 2. West - East gap per year per type ----
wide <- dcast(comp, year + use_class ~ side, value.var = "share", fill = 0)
wide[, gap := West - East]
wide <- merge(wide, nby, by = "year")

# focus types for the substitution story
foc <- wide[use_class %in% c("SFD+suite", "Multiplex", "Plain-Jane", "Laneway")]

# ---- 3. Plot: annual W-E gap by type on 50ft lots ----
foc[, use_class := factor(use_class,
      levels = c("Plain-Jane", "SFD+suite", "Multiplex", "Laneway"))]
reform_yr <- 2024

p <- ggplot(foc, aes(year, gap, color = use_class, group = use_class)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_vline(xintercept = reform_yr, linetype = "dashed", linewidth = 0.3, color = "grey40") +
  annotate("text", x = reform_yr, y = Inf, label = "R1-1", vjust = 1.4, hjust = -0.1,
           size = 3, color = "grey40") +
  geom_line(linewidth = 0.9) +
  geom_point(aes(size = n)) +
  scale_size_area(max_size = 4, guide = "none") +
  scale_color_manual(values = c("Plain-Jane" = "#9e9e9e", "SFD+suite" = "#1f78b4",
                                "Multiplex" = "#ff7f00", "Laneway" = "#e31a1c")) +
  theme_minimal() +
  labs(title = "50ft R1-1 lots: West-East gap by build type",
       subtitle = paste0("Gap = West share - East share within year. Toward 0 = sides converge.\n",
                         "Substitution signature: suite gap and multiplex gap cross near reform."),
       y = "West - East share", x = NULL, color = "Build type")
ggsave(file.path(out_dir, "ew_50ft_substitution.png"), p, width = 9, height = 5.5, dpi = 150)

# ---- 4. Print the gap series that the plot draws ----
cat("\n50ft annual West-East gap by type:\n")
print(dcast(foc, year + n ~ use_class, value.var = "gap", fill = 0)[order(year)])
# =============================================================
# ADDENDUM: substitution test, BOTH lot widths, duplex included
# 33ft and 50ft side by side. Needs `kj` (from r1_infill_convergence.R)
# with year, side, use_class, width_bucket.
#
# Gap = West_share - East_share within year x width. Toward 0 = sides converge.
# 33ft: duplex IS a wide part of the mix (the narrow-lot infill vehicle).
# 50ft: duplex ~absent (identifying assumption) -> convergence via suite/multiplex.
# =============================================================

library(data.table)
library(ggplot2)

out_dir <- "text"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
reform_yr <- 2024

# ---- 0. Duplex-by-width check (contrast 33 vs 50) ----
cat("Duplex raw counts by width x year (50 should be ~0, 33 substantial):\n")
print(dcast(kj[use_class == "Duplex" & !is.na(width_bucket) & !is.na(year),
               .N, by = .(width_bucket, year)],
            year ~ width_bucket, value.var = "N", fill = 0)[order(year)])

# ---- 1. Annual within-side composition shares, per width ----
K <- kj[!is.na(width_bucket) & !is.na(year)]
comp <- K[, .N, by = .(width_bucket, year, side, use_class)]
comp[, share := N / sum(N), by = .(width_bucket, year, side)]
nby  <- K[, .(n = .N), by = .(width_bucket, year)]

wide <- dcast(comp, width_bucket + year + use_class ~ side, value.var = "share", fill = 0)
wide[, gap := West - East]
wide <- merge(wide, nby, by = c("width_bucket", "year"))

# five infill types now (duplex included)
foc_types <- c("Plain-Jane", "SFD+suite", "Duplex", "Multiplex", "Laneway")
foc <- wide[use_class %in% foc_types]
foc[, use_class := factor(use_class, levels = foc_types)]
foc[, width_lab := factor(paste0(width_bucket, "ft lots"),
                          levels = c("33ft lots", "50ft lots"))]

pal <- c("Plain-Jane" = "#9e9e9e", "SFD+suite" = "#1f78b4",
         "Duplex" = "#33a02c", "Multiplex" = "#ff7f00", "Laneway" = "#e31a1c")

# ---- 2. Parallel plot: 33ft and 50ft facets ----
p <- ggplot(foc, aes(year, gap, color = use_class, group = use_class)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_vline(xintercept = reform_yr, linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_line(linewidth = 0.9) +
  geom_point(aes(size = n)) +
  scale_size_area(max_size = 4, guide = "none") +
  scale_color_manual(values = pal) +
  facet_wrap(~ width_lab, nrow = 1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "West-East gap by build type, R1-1 lots (33ft vs 50ft)",
       subtitle = paste0("Gap = West share - East share within year. Toward 0 = sides converge. ",
                         "Dashed = R1-1 reform (2024).\n",
                         "50ft: duplex ~absent, so convergence runs through suite/multiplex. ",
                         "33ft: duplex is the narrow-lot vehicle."),
       y = "West - East share", x = NULL, color = "Build type")
ggsave(file.path(out_dir, "ew_substitution_33_50.png"), p, width = 12, height = 5.5, dpi = 150)

# ---- 3. Print both gap series ----
for (w in c("33", "50")) {
  cat("\n", w, "ft annual West-East gap by type:\n", sep = "")
  print(dcast(foc[width_bucket == w], year + n ~ use_class, value.var = "gap", fill = 0)[order(year)])
}



# ============================================================================
# ============================================================================
# APPENDED: duplex propensity by FINE width bin, 2019-2023, duplex/(duplex+single)
# (reuses kj built above; no re-library, no guard)
# ============================================================================
DUP_ERA <- 2019:2023
CUTS <- c(30,34,40,45,49,51,80)
LABS <- c("33","34-40","40-45","45-49","50","50+")

K <- as.data.table(copy(kj))
K <- K[!is.na(width_ft) & width_ft > 30 & width_ft <= 80 & year %in% DUP_ERA]
K[, wbin := cut(width_ft, breaks = CUTS, labels = LABS, right = TRUE)]

# principal single/duplex was already set upstream as `principalType` (section 7),
# where the symmetric new-build cost floor + Laneway/other drop happened. Reuse it.
# `otherShare` here is now structurally 0 (keep excludes non-principal types); the
# real trim audit is the section-7 drop-share table above.
K[, principal := principalType]

cat("=== 2019-2023 single/duplex: per-bin mix (otherShare structurally 0 now) ===\n")
mix <- K[, .(n = .N,
             single = sum(principal=="single", na.rm=TRUE),
             duplex = sum(principal=="duplex", na.rm=TRUE),
             other  = sum(is.na(principal))), by = wbin]
mix[, otherShare := round(other/n, 3)]
setorder(mix, wbin); print(mix[])

# --- duplex share of (duplex+single), by bin x side ---
P <- K[!is.na(principal)]
sh <- P[, .(nDup = sum(principal=="duplex"), n = .N), by = .(side, wbin)]
sh[, dupShare := nDup / n]
setorder(sh, side, wbin)
cat("\n=== duplex share of (duplex+single), 2019-2023, by width bin x side ===\n")
print(dcast(sh, wbin ~ side, value.var = "dupShare"))
cat("\n=== underlying n (duplex+single) by bin x side ===\n")
print(dcast(sh, wbin ~ side, value.var = "n", fill = 0))

# --- pooled share (both sides) for the headline monotonicity read ---
shP <- P[, .(dupShare = mean(principal=="duplex"), n = .N), by = wbin]
setorder(shP, wbin)
cat("\n=== pooled duplex share by width bin (monotonicity check) ===\n")
print(shP[])

# --- PNG: duplex share vs width bin, East vs West ---
sh[, wbin := factor(wbin, levels = LABS)]
p <- ggplot(sh, aes(wbin, dupShare, color = side, group = side)) +
  geom_line(linewidth = 1) + geom_point(aes(size = n)) +
  scale_size_area(max_size = 5, guide = "none") +
  scale_color_manual(values = c(East = "grey30", West = "firebrick")) +
  ylim(0, NA) + theme_minimal() +
  labs(title = "Duplex propensity by lot width, R1 permits 2019-2023",
       subtitle = "Duplex / (duplex + single) principal-building permits. Point size ~ n.",
       x = "lot width (ft)", y = "duplex share", color = NULL)
ggsave(file.path(out_dir, "permit_duplexShare_byWidthBin.png"), p,
       width = 8, height = 5, dpi = 150)
cat("\n  wrote", file.path(out_dir, "permit_duplexShare_byWidthBin.png"), "\n")

# ============================================================================
# APPENDED 2: is the width fade West-SPECIFIC? (wide x west interaction)
#   By-side table shows East flat-to-rising in width, West stepping down above ~45ft.
#   Test it: permit-level P(duplex) on wide(=width>45) x west, and a smooth version.
#   Denominator = single/duplex principal new-build (already filtered upstream).
# ============================================================================
Q <- as.data.table(copy(kj))[!is.na(width_ft) & width_ft > 30 & width_ft <= 80 &
                             year %in% DUP_ERA & !is.na(principalType)]
Q[, dup  := as.integer(principalType == "duplex")]
Q[, west := as.integer(side == "West")]
Q[, wide := as.integer(width_ft > 45)]
Q[, w10  := (width_ft - 33) / 10]          # width in 10-ft units, centred at 33

cat("\n=== P(duplex): wide x west (LPM, threshold at 45ft) ===\n")
mW <- feols(dup ~ wide * west, data = Q, vcov = "hetero")
print(summary(mW))
cat("  wide       = East width effect (expect ~0: East flat)\n")
cat("  wide:west  = EXTRA West width effect (expect <0: West-specific fade)\n")

cat("\n=== P(duplex): continuous width x west (LPM, per +10ft) ===\n")
mC <- feols(dup ~ w10 * west, data = Q, vcov = "hetero")
print(summary(mC))
cat("  w10:west = differential West slope per 10ft (expect <0)\n")

# cell counts behind the interaction, so the reader sees it isn't thin
cat("\n=== n behind wide x west cells ===\n")
print(dcast(Q[, .N, by = .(west, wide)], west ~ wide, value.var = "N"))
