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

# ---- 7. Cost floor on Plain-Jane only ----
is_new   <- grepl("New Building|New Construction", dt$type_of_work, ignore.case = TRUE)
minSpend <- quantile(dt[is_new & use_class == "Plain-Jane", project_value], CRITVAL, na.rm = TRUE)
keep <- dt[!is.na(use_class) & !is.na(era) &
           (use_class != "Plain-Jane" | project_value > minSpend)]
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


