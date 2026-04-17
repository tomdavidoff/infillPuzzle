# analysisUSA.R
# Lot-level analysis of plex propensity
# Tom Davidoff
# Unified by Claude, April 2026

library(data.table)
library(fixest)
library(ggplot2)
library(marginaleffects)

# ==========================================
# CITY CONFIGURATIONS
# ==========================================
city_config <- list(
  portland = list(
    city_name     = "Portland",
    lots_file     = "~/DropboxExternal/dataProcessed/portland_lot_level.rds",
    slopes_file   = "~/DropboxExternal/dataProcessed/portland_slopes.rds",
    tract_start   = 7,
    tract_end     = 11,
    zone_field    = "zone",
    core_zones    = c("R2.5", "R5"),
    all_zones     = c("R2.5", "R5", "R7", "R10")
  ),
  minneapolis = list(
    city_name     = "Minneapolis",
    lots_file     = "~/DropboxExternal/dataProcessed/minneapolis_lot_level.rds",
    slopes_file   = "~/DropboxExternal/dataProcessed/minneapolis_slopes.rds",
    tract_start   = 6,
    tract_end     = 11,
    zone_field    = "land_use_c",
    core_zones    = c("UN1", "UN2"),
    all_zones     = c("UN1", "UN2", "UN3")
  )
)

CITIES <- c("portland", "minneapolis")

# ==========================================
# LOAD & MERGE
# ==========================================
city_results <- list()

for (CITY in CITIES) {
  cfg <- city_config[[CITY]]
  message("\n========== ", cfg$city_name, " ==========\n")

  # Load lot-level permits and tract-level slopes
  dtLots  <- readRDS(cfg$lots_file)
  slopes  <- readRDS(cfg$slopes_file)
  setDT(dtLots); setDT(slopes)

  # Merge on tract
  dtLots[, tractNumeric := as.numeric(substring(geo_id, cfg$tract_start, cfg$tract_end))]
  slopes[, tractNumeric := as.numeric(tract)]
  dt <- merge(dtLots, slopes, by = "tractNumeric", all.x = TRUE)

  # Standardize zone column name
  zone_col <- cfg$zone_field
  if (zone_col != "zone") {
    dt[, zone := get(zone_col)]
  }
  dt[, city := cfg$city_name]

  # Log lot size (0 and NA -> NA)
  dt[, AreaLotSF := as.numeric(AreaLotSF)]
  dt[AreaLotSF <= 0, AreaLotSF := NA_real_]
  dt[, logLotSF := log(AreaLotSF)]

  # Ensure isMandated exists (FALSE for cities without mandates)
  if (!"isMandated" %in% names(dt)) dt[, isMandated := FALSE]

  # Permit year as factor for FE
  dt[, permit_year := as.integer(permit_year)]
  dt[, yr := factor(permit_year)]

  message("Lots with valid lot size: ", sum(!is.na(dt$logLotSF)), " of ", nrow(dt))

  message("Total lots: ", nrow(dt))
  message("Lots with lot size: ", sum(!is.na(dt$AreaLotSF)))
  message("Lots with slopes: ", sum(!is.na(dt$slope)))

  # ------------------------------------------
  # SUMMARY BY ZONE
  # ------------------------------------------
  cat("\n--- Zone summaries ---\n")
  print(dt[, .(
    n_lots      = .N,
    n_plex      = sum(isPlexLot),
    plex_share  = mean(isPlexLot),
    n_mandated  = sum(isMandated),
    mean_lotSF  = mean(AreaLotSF, na.rm = TRUE),
    med_lotSF   = median(AreaLotSF, na.rm = TRUE),
    pct_has_lot = mean(!is.na(AreaLotSF)) * 100
  ), by = zone])

  # Mandate breakdown (if any mandated lots exist)
  if (any(dt$isMandated)) {
    cat("\n--- Mandate breakdown ---\n")
    print(dt[!is.na(AreaLotSF), .(
      n = .N,
      n_plex = sum(isPlexLot),
      plex_share = round(mean(isPlexLot), 3),
      mean_lotSF = round(mean(AreaLotSF))
    ), by = .(zone, isMandated)])

    # Mandate × year for R2.5 and R5
    cat("\n--- Plex share by zone × mandate × year ---\n")
    for (z in c("R2.5", "R5")) {
      cat(sprintf("\n  Zone: %s\n", z))
      print(dt[zone == z & !is.na(permit_year), .(
        n = .N,
        n_plex = sum(isPlexLot),
        plex_share = round(mean(isPlexLot), 3)
      ), by = .(permit_year, isMandated)][order(permit_year, isMandated)])
    }
  }

  # ------------------------------------------
  # CITY-LEVEL LOGIT REGRESSIONS
  # ------------------------------------------
  cat("\n--- City-level logit regressions ---\n")

  # Filter to zones of interest and non-missing covariates
  dt_reg <- dt[zone %in% cfg$all_zones]
  dt_reg_full <- dt_reg[!is.na(logLotSF) & !is.na(lppsf) & !is.na(slope)]

  cat(sprintf("  Regression sample: %d lots (of %d in target zones)\n",
              nrow(dt_reg_full), nrow(dt_reg)))
  cat("  Zone distribution in regression sample:\n")
  print(dt_reg_full[, .(n = .N, n_plex = sum(isPlexLot), plex_share = round(mean(isPlexLot), 3)), by = zone])

  # Drop zones with no variation in outcome or too few obs
  zone_ok <- dt_reg_full[, .(n = .N, plex_share = mean(isPlexLot)), by = zone]
  zone_ok <- zone_ok[n >= 10 & plex_share > 0 & plex_share < 1, zone]
  dt_reg_full <- dt_reg_full[zone %in% zone_ok]
  dt_reg_full[, zone := factor(zone)]

  if (length(zone_ok) < 2) {
    cat("  Only one usable zone, dropping zone from regressions\n")
    m2 <- glm(isPlexLot ~ logLotSF + yr, family = binomial, data = dt_reg_full)
    m5 <- glm(isPlexLot ~ logLotSF + slope + lppsf + yr, family = binomial, data = dt_reg_full)
  } else {
    m2 <- glm(isPlexLot ~ zone + logLotSF + yr, family = binomial, data = dt_reg_full)
    m5 <- glm(isPlexLot ~ zone + logLotSF + slope + lppsf + yr, family = binomial, data = dt_reg_full)
  }

  # With isMandated control (only meaningful for Portland)
  if (any(dt_reg_full$isMandated)) {
    cat("\n  --- With isMandated control ---\n")
    m2m <- glm(isPlexLot ~ zone + logLotSF + isMandated + yr, family = binomial, data = dt_reg_full)
    m5m <- glm(isPlexLot ~ zone + logLotSF + slope + lppsf + isMandated + yr, family = binomial, data = dt_reg_full)
    print(summary(m2m))
    print(summary(m5m))
  }

  if ("slopeGrossGross" %in% names(dt_reg_full) && length(zone_ok) >= 2) {
    m6 <- glm(isPlexLot ~ zone + logLotSF + slopeGrossGross + lppsf, family = binomial, data = dt_reg_full)
  }

  cat("\n  Model summaries:\n")
  print(summary(m2))
  print(summary(m5))
  if (exists("m6")) { print(summary(m6)); rm(m6) }

  # Average marginal effects (uncomment when ready)
  # cat("\n  Average marginal effects (M5):\n")
  # print(avg_slopes(m5))

  # ------------------------------------------
  # BY-ZONE LOGIT
  # ------------------------------------------
  cat("\n--- By-zone logit regressions ---\n")
  for (z in cfg$all_zones) {
    dtz <- dt_reg[zone == z & !is.na(logLotSF)]
    if (nrow(dtz) < 10) {
      cat(sprintf("\n  Zone %s: only %d obs with lot size, skipping\n", z, nrow(dtz)))
      next
    }
    cat(sprintf("\n  Zone: %s (n=%d, plex=%d)\n", z, nrow(dtz), sum(dtz$isPlexLot)))

    mz1 <- glm(isPlexLot ~ logLotSF + yr, family = binomial, data = dtz)
    mz2 <- glm(isPlexLot ~ logLotSF + lppsf + yr, family = binomial, data = dtz)
    mz3 <- glm(isPlexLot ~ logLotSF + slope + yr, family = binomial, data = dtz)

    print(summary(mz2))
    # cat("  AME:\n")
    # print(avg_slopes(mz2))
  }

  city_results[[CITY]] <- dt
}

# ==========================================
# POOLED CROSS-CITY ANALYSIS
# ==========================================
message("\n========== Pooled Cross-City ==========\n")

dtAll <- rbindlist(city_results, use.names = TRUE, fill = TRUE)

cat("Total lots:", nrow(dtAll), "\n")
cat("By city:\n")
print(dtAll[, .N, by = city])

# Core zones only
core_zones <- unlist(lapply(city_config, `[[`, "core_zones"))
dtCore <- dtAll[zone %in% core_zones]

cat("\nCore-zone lot counts:\n")
print(dtCore[, .(n = .N, n_plex = sum(isPlexLot), plex_share = mean(isPlexLot),
                  pct_has_lot = mean(!is.na(AreaLotSF)) * 100), by = .(city, zone)])

# ------------------------------------------
# POOLED LOGIT WITH CITY FE
# ------------------------------------------
cat("\n--- Pooled logit (core zones, city FE) ---\n")

# feglm for logit with fixed effects (including permit year)
pc1 <- feglm(isPlexLot ~ logLotSF | city + yr, family = binomial, data = dtCore)
pc2 <- feglm(isPlexLot ~ logLotSF | city + zone + yr, family = binomial, data = dtCore)
pc3 <- feglm(isPlexLot ~ logLotSF + lppsf | city + zone + yr, family = binomial, data = dtCore)
pc4 <- feglm(isPlexLot ~ logLotSF + slope | city + zone + yr, family = binomial, data = dtCore)
pc5 <- feglm(isPlexLot ~ logLotSF + slope + lppsf | city + zone + yr, family = binomial, data = dtCore)

# With isMandated control
pc2m <- feglm(isPlexLot ~ logLotSF + isMandated | city + zone + yr, family = binomial, data = dtCore)
pc5m <- feglm(isPlexLot ~ logLotSF + slope + lppsf + isMandated | city + zone + yr, family = binomial, data = dtCore)

cat("\n  Without mandate control:\n")
print(etable(pc1, pc2, pc3, pc4, pc5))
cat("\n  With mandate control:\n")
print(etable(pc2, pc2m, pc5, pc5m))

# AME for main spec
cat("\nAverage marginal effects (pooled M5):\n")
# TODO: avg_slopes doesn't work with feglm fixed effects
# print(avg_slopes(pc5))

# ------------------------------------------
# ALL ZONES POOLED
# ------------------------------------------
cat("\n--- Pooled logit (all zones, city FE) ---\n")
dtAllZones <- dtAll[zone %in% unlist(lapply(city_config, `[[`, "all_zones"))]

pa1 <- feglm(isPlexLot ~ logLotSF | city + zone + yr, family = binomial, data = dtAllZones)
pa2 <- feglm(isPlexLot ~ logLotSF + lppsf | city + zone + yr, family = binomial, data = dtAllZones)
pa3 <- feglm(isPlexLot ~ logLotSF + slope + lppsf | city + zone + yr, family = binomial, data = dtAllZones)
pa3m <- feglm(isPlexLot ~ logLotSF + slope + lppsf + isMandated | city + zone + yr, family = binomial, data = dtAllZones)

print(etable(pa1, pa2, pa3, pa3m))

# ==========================================
# PLOTS
# ==========================================

# Plex share by lot size bin
dtCore[, lotBin := cut(AreaLotSF, breaks = c(0, 3000, 5000, 7000, 10000, 15000, Inf),
                       labels = c("<3k", "3-5k", "5-7k", "7-10k", "10-15k", "15k+"))]

pLot <- ggplot(dtCore[!is.na(lotBin)],
               aes(x = lotBin, fill = isPlexLot)) +
  geom_bar(position = "fill") +
  facet_wrap(~ city) +
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "tomato"),
                    labels = c("Single-Family", "Plex"),
                    name = "") +
  labs(x = "Lot Size (sqft)", y = "Share of Permits",
       title = "Plex Share by Lot Size and City") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("text/plex_by_lotsize.png", pLot, width = 10, height = 5, dpi = 300)

# Plex share by price level
pPrice <- ggplot(dtCore[!is.na(lppsf)],
                 aes(x = lppsf, y = as.numeric(isPlexLot), color = city)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, linewidth = 0.8) +
  facet_wrap(~ city, scales = "free_x") +
  labs(x = "Log Price Per Square Foot",
       y = "Pr(Plex)",
       title = "Plex Probability and Price Level by City") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("text/plex_by_price.png", pPrice, width = 10, height = 5, dpi = 300)

# Plex share by slope
pSlope <- ggplot(dtCore[!is.na(slope)],
                 aes(x = slope, y = as.numeric(isPlexLot), color = city)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, linewidth = 0.8) +
  facet_wrap(~ city, scales = "free_x") +
  labs(x = "Price-Size Slope",
       y = "Pr(Plex)",
       title = "Plex Probability and Price-Size Gradient by City") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("text/plex_by_slope.png", pSlope, width = 10, height = 5, dpi = 300)

# ==========================================
# SAVE
# ==========================================
saveRDS(dtAll, "~/DropboxExternal/dataProcessed/analysis_lot_level_pooled.rds")
