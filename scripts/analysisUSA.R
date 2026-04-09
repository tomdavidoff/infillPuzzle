# analysisUnified.R
# Analyze ppsf slope vs. infill propensity, pooled across cities
# Tom Davidoff
# Unified by Claude, April 2026

library(data.table)
library(fixest)
library(ggplot2)

# ==========================================
# CITY CONFIGURATIONS
# ==========================================
city_config <- list(
  portland = list(
    city_name     = "Portland",
    permits_file  = "~/DropboxExternal/dataProcessed/portland_tract_propensity.rds",
    slopes_file   = "~/DropboxExternal/dataProcessed/portland_slopes.rds",
    tract_start   = 7,   # substring position for numeric tract ID
    tract_end     = 11,
    zone_field    = "zone",
    core_zones    = c("R2.5", "R5"),       # main zones for focused regressions
    all_zones     = c("R2.5", "R5", "R7", "R10", "R20")
  ),
  minneapolis = list(
    city_name     = "Minneapolis",
    permits_file  = "~/DropboxExternal/dataProcessed/minneapolis_tract_propensity.rds",
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
# MAIN LOOP
# ==========================================
city_results <- list()

for (CITY in CITIES) {
  cfg <- city_config[[CITY]]
  message("\n========== ", cfg$city_name, " ==========\n")

  # ------------------------------------------
  # 1. LOAD
  # ------------------------------------------
  permits <- readRDS(cfg$permits_file)
  slopes  <- readRDS(cfg$slopes_file)
  setDT(permits); setDT(slopes)

  # ------------------------------------------
  # 2. MERGE ON TRACT
  # ------------------------------------------
  permits[, tractNumeric := as.numeric(substring(geo_id, cfg$tract_start, cfg$tract_end))]
  slopes[,  tractNumeric := as.numeric(tract)]

  dt <- merge(permits, slopes, by = "tractNumeric")

  # Standardize zone column name
  if (cfg$zone_field != "zone") {
    dt[, zone := get(cfg$zone_field)]
  }

  dt[, city := cfg$city_name]

  message("Merged rows: ", nrow(dt))

  # ------------------------------------------
  # 3. SUMMARY BY ZONE
  # ------------------------------------------
  cat("\n--- Zone summaries ---\n")
  print(dt[, .(
    mean_propensity = mean(propensity, na.rm = TRUE),
    mean_lppsf      = mean(lppsf, na.rm = TRUE),
    mean_sqft       = if ("medianSqft" %in% names(dt)) mean(medianSqft, na.rm = TRUE) else NA_real_,
    n_tracts        = .N
  ), by = zone])

  # ------------------------------------------
  # 4. REGRESSIONS: POOLED ACROSS ZONES
  # ------------------------------------------
  cat("\n--- Pooled regressions ---\n")

  m1 <- lm(propensity ~ lppsf * zone, dt)
  m2 <- lm(propensity ~ slope + lppsf + zone, dt)

  # Print slope variants if available (Minneapolis has slopeGrossGross)
  if ("slopeGrossGross" %in% names(dt)) {
    m3 <- lm(propensity ~ slopeGrossGross + lppsf + zone, dt)
    print(summary(m3))
  }

  print(summary(m1))
  print(summary(m2))

  # ------------------------------------------
  # 5. REGRESSIONS: BY ZONE
  # ------------------------------------------
  cat("\n--- By-zone regressions ---\n")
  for (z in unique(dt[, zone])) {
    cat("\n  Zone:", z, "\n")
    dtz <- dt[zone == z]
    print(summary(lm(propensity ~ lppsf, dtz)))
    print(summary(lm(propensity ~ slope + lppsf, dtz)))
  }

  # ------------------------------------------
  # 6. STORE FOR POOLING
  # ------------------------------------------
  city_results[[CITY]] <- dt
}

# ==========================================
# POOLED CROSS-CITY ANALYSIS
# ==========================================
message("\n========== Pooled Cross-City ==========\n")

dtAll <- rbindlist(city_results, use.names = TRUE, fill = TRUE)

cat("Total tract-zone obs:", nrow(dtAll), "\n")
cat("By city:\n")
print(dtAll[, .N, by = city])

# Core zones only (R2.5/R5 for Portland, UN1/UN2 for Minneapolis)
core_zones <- unlist(lapply(city_config, `[[`, "core_zones"))
dtCore <- dtAll[zone %in% core_zones]

cat("\nCore-zone regressions (formerly single-family areas):\n")
mc1 <- feols(propensity ~ slope + lppsf | city, dtCore, vcov = "hc1")
mc2 <- feols(propensity ~ slope + lppsf | city + zone, dtCore, vcov = "hc1")
mc3 <- feols(propensity ~ lppsf | city + zone, dtCore, vcov = "hc1")

print(etable(mc1, mc2, mc3))

# ==========================================
# PLOT
# ==========================================
ggplot(dtCore, aes(x = lppsf, y = propensity, color = city)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  facet_wrap(~ city, scales = "free_x") +
  theme_minimal() +
  labs(x = "Log Price Per Square Foot",
       y = "Infill Propensity",
       title = "Price Level and Infill Propensity by City") +
  theme(legend.position = "none")

ggsave("text/propensity_cross_city.png", width = 10, height = 5, dpi = 300)

ggplot(dtCore, aes(x = slope, y = propensity, color = city)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  facet_wrap(~ city, scales = "free_x") +
  theme_minimal() +
  labs(x = "Price-Size Slope",
       y = "Infill Propensity",
       title = "Price-Size Gradient and Infill Propensity by City") +
  theme(legend.position = "none")

ggsave("text/slope_cross_city.png", width = 10, height = 5, dpi = 300)

# ==========================================
# SAVE
# ==========================================
saveRDS(dtAll, "~/DropboxExternal/dataProcessed/analysis_pooled.rds")
