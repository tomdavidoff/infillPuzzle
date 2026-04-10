# permitsUnified.R
# Unified permit analysis for Portland and Minneapolis
# Tom Davidoff
# Consolidation by Claude, April 2026

library(data.table)
library(sf)
library(ggplot2)
library(tigris)

options(timeout = 600)
options(tigris_use_cache = TRUE)

# ==========================================
# TOGGLES
# ==========================================
GEOGRAPHY <- "tract"
CITIES <- c("minneapolis", "portland")  # run both, or subset
# ==========================================

# ==========================================
# CITY CONFIGURATIONS
# ==========================================
city_config <- list(
  minneapolis = list(
    city_name    = "Minneapolis",
    crs          = 26915,
    # --- permit data ---
    permit_file  = "~/DropboxExternal/dataRaw/shp_econ_residential_building_permts/ResidentialPermits.shp",
    permit_type  = "shp",
    # --- permit filters ---
    filter_permits = function(dt) {
      dt <- dt[year >= 2020 & year <= 2025]
      qmin <- quantile(dt[res_permit=="NU", permit_val], 0.2, na.rm = TRUE) # more than .1 as would be single detached
      dt <- dt[res_permit == "NU" | permit_val >= qmin]
      dt <- dt[ctu_name == "Minneapolis"]
      dt <- dt[(co_code == "053") | (co_code == 53)]
      dt
    },
    # --- plex logic ---
    flag_plex = function(dt) {
      dt[, isPlex := units >= 2 |
           !housing_ty %in% c("SFD") |
           grepl("duplex|triplex|fourplex|twinhome|townhouse",
                 housing__1, ignore.case = TRUE)]
    },
    # --- zoning ---
    zoning_file  = "~/DropboxExternal/dataRaw/Planning_Primary_Zoning/Planning_Primary_Zoning.shp",
    zone_field   = "land_use_c",
    zone_filter  = "UN1|UN2|UN3",
    zone_plot_values = c("UN1", "UN2", "UN3"),
    zone_colors  = c("UN1" = "lightyellow", "UN2" = "lightblue", "UN3" = "lightgreen"),
    # --- parcel id ---
    parcel_id    = "address",
    # --- units field ---
    units_field  = "units",
    # --- geography ---
    tract_state  = "MN",
    tract_county = "Hennepin",
    zip_pattern  = "^55[34]"
  ),

  portland = list(
    city_name    = "Portland",
    crs          = 3857,
    # --- permit data ---
    permit_file  = "~/DropboxExternal/dataRaw/portlandPermit-Search-Results.csv",
    permit_type  = "csv",
    csv_coords   = c("x_web_mercator", "y_web_mercator"),
    # --- permit filters ---
    filter_permits = function(dt) {
      dt <- dt[as.IDate(issued) >= "2021-01-01"]
      qmin <- quantile(dt[work == "New Construction", total_sqft], 0.2, na.rm = TRUE)
      dt <- dt[work == "New Construction" | total_sqft >= qmin]
      dt <- dt[!is.na(x_web_mercator)]
      dt
    },
    # --- plex logic ---
    flag_plex = function(dt) {
      dt[, isPlex := new_units >= 2 |
           grepl("duplex|triplex|fourplex", description, ignore.case = TRUE) |
           grepl("Townhouse", type, ignore.case = TRUE)]
    },
    # --- zoning ---
    zoning_file  = "~/DropboxExternal/dataRaw/portlandZoning",
    zoning_is_dir = TRUE,  # Portland zoning is a directory; grab the .shp inside
    zone_field   = "zone",
    zone_filter  = "^R2.5|^R5|^R7|^R10|^R20",
    zone_plot_values = c("R2.5", "R5", "R7", "R10", "R20"),
    zone_colors  = c("R2.5" = "lightyellow", "R5" = "lightblue",
                      "R7" = "lightgreen", "R10" = "wheat", "R20" = "lavender"),
    # --- parcel id ---
    parcel_id    = "property_id",
    # --- units field (Portland uses total_sqft instead of units) ---
    units_field  = "new_units",
    # --- geography ---
    tract_state  = "OR",
    tract_county = "Multnomah",
    zip_pattern  = "^972"
  )
)

# ==========================================
# MAIN LOOP
# ==========================================
results <- list()

for (CITY in CITIES) {
  cfg <- city_config[[CITY]]
  message("\n========== Processing: ", cfg$city_name, " ==========\n")

  # ------------------------------------------
  # 1. READ PERMITS
  # ------------------------------------------
  if (cfg$permit_type == "shp") {
    sfPermit <- st_read(cfg$permit_file)
    names(sfPermit) <- tolower(names(sfPermit))
    sfPermit <- st_transform(sfPermit, cfg$crs)
    dtPermit <- as.data.table(sfPermit)
  } else {
    dtPermit <- fread(cfg$permit_file)
    setnames(dtPermit, tolower(names(dtPermit)))
  }
  print("HERE ARE THE PERMIT FIELDS")
  print(names(dtPermit))

  # ------------------------------------------
  # 2. FILTER PERMITS
  # ------------------------------------------
  dtPermit <- cfg$filter_permits(dtPermit)

  # ------------------------------------------
  # 3. FLAG PLEX
  # ------------------------------------------
  cfg$flag_plex(dtPermit)

  message("Permits after filtering: ", nrow(dtPermit))

  # ------------------------------------------
  # 4. CONVERT TO SF
  # ------------------------------------------
  if (cfg$permit_type == "csv") {
    sfPermit <- st_as_sf(dtPermit,
                         coords = cfg$csv_coords,
                         crs = cfg$crs)
  } else {
    sfPermit <- st_as_sf(dtPermit)
    sfPermit <- st_transform(sfPermit, cfg$crs)
  }

  # ------------------------------------------
  # 5. READ ZONING & SPATIAL JOIN
  # ------------------------------------------
  if (isTRUE(cfg$zoning_is_dir)) {
    shp_path <- list.files(cfg$zoning_file, pattern = "\\.shp$", full.names = TRUE)
    sfZoning <- st_read(shp_path)
  } else {
    sfZoning <- st_read(cfg$zoning_file)
  }
  names(sfZoning) <- tolower(names(sfZoning))
  sfZoning <- st_transform(sfZoning, cfg$crs)

  message("Zoning fields: ", paste(names(sfZoning), collapse = ", "))

  sfPermitZoned <- st_join(sfPermit, sfZoning)

  # ------------------------------------------
  # 6. GEOGRAPHY JOIN (TRACT or ZIP)
  # ------------------------------------------
  if (GEOGRAPHY == "tract") {
    message("Fetching census tracts...")
    sfTracts <- tracts(state = cfg$tract_state, county = cfg$tract_county,
                       year = 2021, cb = TRUE)
    sfTracts <- st_transform(sfTracts, cfg$crs)
    sfFinal <- st_join(sfPermitZoned, sfTracts[, c("GEOID", "NAMELSAD")])
    dtFinal <- as.data.table(sfFinal)
    setnames(dtFinal, c("GEOID", "NAMELSAD"), c("geo_id", "geo_name"),
             skip_absent = TRUE)
  } else {
    message("Fetching ZCTAs...")
    sfZips <- zctas(year = 2020, cb = TRUE)
    sfZips <- sfZips[grepl(cfg$zip_pattern, sfZips$ZCTA5CE20), ]
    sfZips <- st_transform(sfZips, cfg$crs)
    sfFinal <- st_join(sfPermitZoned, sfZips[, c("ZCTA5CE20")])
    dtFinal <- as.data.table(sfFinal)
    setnames(dtFinal, "ZCTA5CE20", "geo_id", skip_absent = TRUE)
    dtFinal[, geo_name := paste("ZIP", geo_id)]
  }

  # ------------------------------------------
  # 7. FILTER TO RESIDENTIAL ZONES
  # ------------------------------------------
  zone_col <- cfg$zone_field
  dtAnalysis <- dtFinal[grepl(cfg$zone_filter, get(zone_col), ignore.case = TRUE)]
  message("Permits in residential zones: ", nrow(dtAnalysis))

  # ------------------------------------------
  # 8. EXTRACT COORDINATES & COLLAPSE TO LOTS
  # ------------------------------------------
  dtAnalysis[, `:=`(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )]

  pid <- cfg$parcel_id
  dtLots <- dtAnalysis[!is.na(geo_id) & !is.na(get(pid)), .(
    isPlexLot    = any(isPlex, na.rm = TRUE),
    total_units  = sum(get(cfg$units_field), na.rm = TRUE),
    permit_count = .N,
    lon = lon[1],
    lat = lat[1]
  ), by = c(pid, "geo_id", zone_col, "geo_name")]

  sfLots <- st_as_sf(dtLots, coords = c("lon", "lat"), crs = cfg$crs)

  message("Total lots with permits: ", nrow(dtLots))
  message("Lots with plex permits: ", sum(dtLots$isPlexLot))

  # ------------------------------------------
  # 9. PLOT
  # ------------------------------------------
  sfZoningRes <- sfZoning[sfZoning[[zone_col]] %in% cfg$zone_plot_values, ]

  p <- ggplot() +
    geom_sf(data = sfZoningRes,
            aes(fill = .data[[zone_col]]),
            alpha = 0.5, color = "grey50", linewidth = 0.1) +
    geom_sf(data = sfLots,
            aes(color = isPlexLot, size = isPlexLot), shape = 16) +
    scale_fill_manual(values = cfg$zone_colors, name = "Zone") +
    scale_color_manual(
      values = c("FALSE" = "grey50", "TRUE" = "red"),
      labels = c("FALSE" = "Non-Plex", "TRUE" = "Plex"),
      name = "Permit Type"
    ) +
    scale_size_manual(
      values = c("FALSE" = 1, "TRUE" = 1.5),
      labels = c("FALSE" = "Non-Plex", "TRUE" = "Plex"),
      name = "Permit Type"
    ) +
    labs(title = sprintf("%s New Construction Permits by Zoning District",
                         cfg$city_name)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(order = 2, override.aes = list(size = 3)),
      size = "none"
    )

  plot_file <- sprintf("text/permitLocation%s.png", cfg$city_name)
  ggsave(plot_file, p, width = 10, height = 8, dpi = 300)
  message("Saved plot: ", plot_file)

  # ------------------------------------------
  # 10. GEOGRAPHY-LEVEL SUMMARY
  # ------------------------------------------
  dtGeo <- dtLots[, .(
    infill_lot_count  = sum(isPlexLot, na.rm = TRUE),
    total_lots_active = .N,
    total_units       = sum(total_units, na.rm = TRUE),
    propensity        = sum(isPlexLot, na.rm = TRUE) / .N
  ), by = c("geo_id", zone_col, "geo_name")]

  setorder(dtGeo, -total_lots_active)

  output_file <- sprintf("~/DropboxExternal/dataProcessed/%s_%s_propensity.rds",
                          CITY, GEOGRAPHY)
  saveRDS(dtGeo, output_file)

  cat(sprintf("\n=== %s LOT-LEVEL SUMMARY BY %s ===\n",
              toupper(cfg$city_name), toupper(GEOGRAPHY)))
  cat("Total Active Lots:", nrow(dtLots), "\n")
  cat("Total Infill Lots:", sum(dtLots$isPlexLot), "\n")
  cat("Overall Propensity:",
      round(sum(dtGeo$infill_lot_count) / sum(dtGeo$total_lots_active), 4), "\n")
  print(head(dtGeo, 15))

  # Store for cross-city comparison
  results[[CITY]] <- list(dtLots = dtLots, dtGeo = dtGeo,
                          sfLots = sfLots, sfZoning = sfZoning)
}
