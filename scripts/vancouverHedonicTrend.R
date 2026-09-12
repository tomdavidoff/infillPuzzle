# =============================================================
# Alternative explanation: East/West (and neighbourhood) PRICE convergence
# CoV single-detached sales hedonic from BCA .gpkg SALES + DESCRIPTIONS + A09.
#
#   (1) ln(P) ~ ln(lot) + ln(sqft), MB_Effective_Year FE, run PER SALE-YEAR;
#       plot the East/West premium coefficient over sale years.
#   (2) Same hedonic with NEIGHBOURHOOD dummies instead of E/W, per year;
#       box-and-whisker the distribution of neighbourhood coefficients by year.
#
# If the E/W price premium is itself converging, the building-mix convergence
# may track relative prices rather than a distinct policy/preference shift.
#
# Performance: SALES read is filtered in SQL (price + year) so GDAL returns only
# kept rows; make_valid runs on that small set. DESCRIPTIONS read WITHOUT geom
# (attributes only) so no polygons are parsed. Centroids planar (s2 off).
# Keys: ROLL_NUMBER across gpkg + A09 inventory via numeric coercion.
# =============================================================

library(data.table)
library(sf)
library(fixest)
library(ggplot2)

gpkg     <- "~/bigFiles/latestSpatialBCA/2026-04-08_bca_folios.gpkg"
inv_path <- "~/DropboxExternal/dataRaw/Residential_inventory_202601/20260101_A09_Residential_Inventory_Extract.txt"
out_dir  <- "text"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ONT       <- -123.101
YR_MIN    <- 2011                 # ~15-year window; widen/narrow after seeing coverage
sales_tbl <- "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_SALES_SV"
desc_tbl  <- "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV"

# Arm's-length improved conveyance types — EDIT after the printed counts below.
ARMS_LENGTH <- c("Improved Single Property Transaction")

roll_key <- function(x) suppressWarnings(as.numeric(gsub("[^0-9]", "", x)))
numify   <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", x)))

# ---- 1. SALES: SQL-filtered read (price + year in SQLite, not R) ----
q_sales <- sprintf(
  'SELECT ROLL_NUMBER, CONVEYANCE_PRICE, CONVEYANCE_DATE,
          CONVEYANCE_TYPE_DESCRIPTION, geom
   FROM "%s"
   WHERE CONVEYANCE_PRICE > 100000
     AND CAST(substr(CONVEYANCE_DATE,1,4) AS INTEGER) >= %d', sales_tbl, YR_MIN)
sales <- as.data.table(st_read(gpkg, query = q_sales, quiet = TRUE))
sales[, sale_year := as.integer(substr(as.character(CONVEYANCE_DATE), 1, 4))]

cat("CONVEYANCE_TYPE_DESCRIPTION counts (edit ARMS_LENGTH to match):\n")
print(sales[, .N, by = CONVEYANCE_TYPE_DESCRIPTION][order(-N)])

sales <- sales[CONVEYANCE_TYPE_DESCRIPTION %in% ARMS_LENGTH]
sales[, roll := roll_key(ROLL_NUMBER)]

# ---- 2. East/West from sale geometry (validate small set, planar centroid) ----
sales_sf <- st_transform(st_make_valid(st_as_sf(sales)), 4326)
sf_use_s2(FALSE)
sales[, lon := st_coordinates(st_centroid(st_geometry(sales_sf)))[, 1]]
sf_use_s2(TRUE)
sales[, side := fifelse(lon < ONT, "West", "East")]

# ---- 3. DESCRIPTIONS: attributes only, NO geom (fast), SFD, one row/roll ----
desc <- as.data.table(st_read(gpkg, quiet = TRUE, query = sprintf(
  'SELECT ROLL_NUMBER, LAND_SIZE, NEIGHBOURHOOD_CODE, ACTUAL_USE_DESCRIPTION
   FROM "%s"', desc_tbl)))
desc <- desc[grepl("single", ACTUAL_USE_DESCRIPTION, ignore.case = TRUE)]
desc[, roll := roll_key(ROLL_NUMBER)]
desc_keep <- unique(desc[, .(roll, lot_sqft = as.numeric(LAND_SIZE),
                             nbhd = NEIGHBOURHOOD_CODE)], by = "roll")

# ---- 4. A09 inventory: finished sqft + effective year, one row/roll ----
inv <- fread(inv_path, colClasses = "character")
inv[, `:=`(roll = roll_key(Roll_Number),
           sqft = numify(MB_Total_Finished_Area),
           eff_year = numify(MB_Effective_Year))]
inv_keep <- unique(inv[is.finite(roll), .(roll, sqft, eff_year)], by = "roll")

# ---- 5. Assemble hedonic frame ----
h <- merge(sales[, .(roll, price = CONVEYANCE_PRICE, sale_year, side)], desc_keep, by = "roll")
h <- merge(h, inv_keep, by = "roll")
h <- h[is.finite(lot_sqft) & lot_sqft > 0 & is.finite(sqft) & sqft > 0]
h[, `:=`(lnP = log(price), ln_lot = log(lot_sqft), ln_sqft = log(sqft))]

cat("\nassembled sales:", nrow(h), "| by side:\n"); print(h[, .N, by = side])
cat("price / lot / sqft  5-50-95 pctiles (sanity — check lot units):\n")
print(h[, .(pctile = c("p05","p50","p95"),
            price = round(quantile(price, c(.05,.5,.95))),
            lot   = round(quantile(lot_sqft, c(.05,.5,.95))),
            sqft  = round(quantile(sqft, c(.05,.5,.95))))])

# ---- 6. (1) East/West premium per sale-year ----
yrs <- sort(unique(h[side %in% c("East","West"), sale_year]))
ew <- rbindlist(lapply(yrs, function(y) {
  d <- h[sale_year == y & is.finite(eff_year)]
  if (d[, uniqueN(side)] < 2 || nrow(d) < 40) return(NULL)
  m  <- feols(lnP ~ ln_lot + ln_sqft + i(side, ref = "East") | eff_year, data = d)
  ct <- coeftable(m); r <- grep("^side", rownames(ct))
  data.table(sale_year = y, premium = ct[r, 1], se = ct[r, 2], n = nrow(d))
}), fill = TRUE)

pW <- ggplot(ew, aes(sale_year, premium)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_ribbon(aes(ymin = premium - 1.96*se, ymax = premium + 1.96*se), alpha = 0.15) +
  geom_line(linewidth = 0.9, color = "#1f78b4") + geom_point(aes(size = n)) +
  scale_size_area(max_size = 4, guide = "none") + theme_minimal() +
  labs(title = "West vs East price premium, CoV single-detached (hedonic)",
       subtitle = "ln(P) ~ ln(lot) + ln(sqft) | effective-year FE, per sale year. + = West premium.",
       y = "West premium (log points)", x = NULL)
ggsave(file.path(out_dir, "hedonic_ew_premium.png"), pW, width = 9, height = 5, dpi = 150)

# ---- 7. (2) Neighbourhood-coefficient distribution per sale-year ----
nb <- rbindlist(lapply(yrs, function(y) {
  d <- h[sale_year == y & is.finite(eff_year) & !is.na(nbhd)]
  if (d[, uniqueN(nbhd)] < 5 || nrow(d) < 80) return(NULL)
  m  <- feols(lnP ~ ln_lot + ln_sqft + i(nbhd) | eff_year, data = d)
  ct <- coeftable(m)
  co <- ct[grep("^nbhd::", rownames(ct)), 1]
  data.table(sale_year = y, nbhd_coef = as.numeric(co))
}), fill = TRUE)

pN <- ggplot(nb, aes(factor(sale_year), nbhd_coef)) +
  geom_boxplot(outlier.size = 0.6, fill = "#a6cee3") + theme_minimal() +
  labs(title = "Neighbourhood price-level dispersion by sale year, CoV SFD",
       subtitle = "Distribution of neighbourhood dummy coefficients from per-year hedonic.\nNarrowing boxes = neighbourhoods converging in price level.",
       y = "Neighbourhood coefficient (log points, vs base)", x = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "hedonic_nbhd_dispersion.png"), pN, width = 10, height = 5, dpi = 150)

# ---- 8. Print series ----
cat("\nEast/West premium series:\n"); print(ew[order(sale_year)])
cat("\nNbhd-coef dispersion by year:\n")
print(nb[, .(n_nbhd = .N, iqr = round(IQR(nbhd_coef),3),
             sd = round(sd(nbhd_coef),3)), by = sale_year][order(sale_year)])

# =============================================================
# ADDENDUM: hedonic premia by lot WIDTH x side, over sale years.
# Two questions:
#   (a) the 50-vs-33 width premium (does wide-lot price advantage erode?)
#   (b) the West-vs-East premium computed SEPARATELY within 33 and within 50
#       -> conjecture: (W-E on 50) falls faster than (W-E on 33).
#
# Needs the assembled hedonic frame `h` from r1_sales_hedonic.R (roll, price,
# sale_year, side, lot_sqft, nbhd, sqft, eff_year, lnP, ln_lot, ln_sqft).
# Width bucket here comes from LOT WIDTH; sales lack it, so derive width from
# lot_sqft/depth is unreliable -> instead pull Land_Width from the A09 inventory
# we already loaded (inv_keep2 below), joined on roll.
# =============================================================

library(data.table); library(fixest); library(ggplot2)
out_dir <- "text"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
W33 <- 32:34; W50 <- 49:51

# ---- 0. Attach lot WIDTH to the hedonic frame from A09 inventory ----
# (h already has roll; re-read width from inventory, numeric, metric-aware)
inv <- fread(inv_path, colClasses = "character")
numify <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", x)))
roll_key <- function(x) suppressWarnings(as.numeric(gsub("[^0-9]", "", x)))
inv[, `:=`(roll = roll_key(Roll_Number), w = numify(Land_Width_Width),
           metric = trimws(Land_Metric_Flag))]
inv[metric %in% c("Y","1","M","T","X") & is.finite(w), w := w/0.3048]
wkey <- unique(inv[is.finite(roll) & is.finite(w), .(roll, width_ft = w)], by = "roll")

hw <- merge(h, wkey, by = "roll")
hw[, width_bucket := fifelse(round(width_ft) %in% W33, "33",
                     fifelse(round(width_ft) %in% W50, "50", NA_character_))]
hw <- hw[!is.na(width_bucket)]
cat("hedonic sales with 33/50 width:", nrow(hw), "\n")
print(hw[, .N, by = .(width_bucket, side)][order(width_bucket, side)])

yrs <- sort(unique(hw$sale_year))

# ---- (a) 50-vs-33 WIDTH premium per year, separately by side ----
# ln(P) ~ ln(sqft) + i(width 50 vs 33) | eff_year, within each side x year.
# (lot size dropped as a regressor here since width IS the treatment; keep sqft.)
wp <- rbindlist(lapply(yrs, function(y) rbindlist(lapply(c("East","West"), function(s) {
  d <- hw[sale_year == y & side == s & is.finite(eff_year)]
  if (d[, uniqueN(width_bucket)] < 2 || nrow(d) < 40) return(NULL)
  m  <- feols(lnP ~ ln_sqft + i(width_bucket, ref = "33") | eff_year, data = d)
  ct <- coeftable(m); r <- grep("width_bucket::50", rownames(ct))
  data.table(sale_year = y, side = s, prem50 = ct[r,1], se = ct[r,2], n = nrow(d))
}))), fill = TRUE)

pA <- ggplot(wp, aes(sale_year, prem50, color = side, group = side)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_ribbon(aes(ymin = prem50-1.96*se, ymax = prem50+1.96*se, fill = side),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) + geom_point(aes(size = n)) +
  scale_size_area(max_size = 3.5, guide = "none") +
  scale_color_manual(values = c(East = "#e31a1c", West = "#1f78b4")) +
  scale_fill_manual(values = c(East = "#e31a1c", West = "#1f78b4"), guide = "none") +
  theme_minimal() +
  labs(title = "50ft vs 33ft lot price premium, by side",
       subtitle = "ln(P) ~ ln(sqft) + I(50) | eff-year FE, within side x year. + = 50ft dearer.",
       y = "50ft premium (log points)", x = NULL, color = NULL)
ggsave(file.path(out_dir, "premium_50v33_byside.png"), pA, width = 9, height = 5, dpi = 150)

# ---- (b) West-East premium computed SEPARATELY within 33 and within 50 ----
we <- rbindlist(lapply(yrs, function(y) rbindlist(lapply(c("33","50"), function(w) {
  d <- hw[sale_year == y & width_bucket == w & is.finite(eff_year)]
  if (d[, uniqueN(side)] < 2 || nrow(d) < 40) return(NULL)
  m  <- feols(lnP ~ ln_lot + ln_sqft + i(side, ref = "East") | eff_year, data = d)
  ct <- coeftable(m); r <- grep("^side", rownames(ct))
  data.table(sale_year = y, width_bucket = w, we_prem = ct[r,1], se = ct[r,2], n = nrow(d))
}))), fill = TRUE)

pB <- ggplot(we, aes(sale_year, we_prem, color = width_bucket, group = width_bucket)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_ribbon(aes(ymin = we_prem-1.96*se, ymax = we_prem+1.96*se, fill = width_bucket),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) + geom_point(aes(size = n)) +
  scale_size_area(max_size = 3.5, guide = "none") +
  scale_color_manual(values = c("33" = "#33a02c", "50" = "#ff7f00")) +
  scale_fill_manual(values = c("33" = "#33a02c", "50" = "#ff7f00"), guide = "none") +
  theme_minimal() +
  labs(title = "West-East price premium, separately within 33ft and 50ft lots",
       subtitle = "ln(P) ~ ln(lot)+ln(sqft) | eff-year FE, within width x year.\nConjecture: the 50ft W-E premium falls faster than the 33ft one.",
       y = "West premium (log points)", x = NULL, color = "Lot width (ft)")
ggsave(file.path(out_dir, "premium_WE_byWidth.png"), pB, width = 9, height = 5, dpi = 150)

# ---- Print both series ----
cat("\n(a) 50-vs-33 premium by side x year:\n")
print(dcast(wp, sale_year ~ side, value.var = "prem50")[order(sale_year)])
cat("\n(b) West-East premium within each width x year:\n")
print(dcast(we, sale_year ~ width_bucket, value.var = "we_prem")[order(sale_year)])
