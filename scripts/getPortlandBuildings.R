# getPortlandBuildings.R
# from Chat
# Tom Davidoff
# 12/28/25

suppressPackageStartupMessages({
  library(httr)
  library(sf)
  library(data.table)
  library(jsonlite)
  library(arrow)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# -------------------------
# Settings
# -------------------------
YEAR_MIN <- 2018L
FORCE_DOWNLOAD_LOTS <- FALSE   # set TRUE only if you want to re-download lots
FORCE_DOWNLOAD_BLDG <- FALSE

buildings_url <- "https://www.portlandmaps.com/od/rest/services/COP_OpenData_Property/MapServer/48/query"
taxlots_url   <- "https://www.portlandmaps.com/arcgis/rest/services/Public/CGIS_Layers/MapServer/0/query"

buildings_file <- sprintf("data/raw/portland_buildings_%dplus.geojson", YEAR_MIN)
taxlots_file   <- "data/raw/portland_taxlots_bbox_of_buildings.geojson"

out_bld_rds     <- sprintf("data/derived/pdx_buildings_%dplus_with_lots.rds", YEAR_MIN)
out_bld_parquet <- sprintf("data/derived/pdx_buildings_%dplus_with_lots.parquet", YEAR_MIN)
out_lot_rds     <- sprintf("data/derived/pdx_lots_%dplus_agg.rds", YEAR_MIN)
out_lot_parquet <- sprintf("data/derived/pdx_lots_%dplus_agg.parquet", YEAR_MIN)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)

# -------------------------
# Helper: paged ArcGIS query (GET or POST)
# -------------------------
arcgis_paged <- function(method = c("GET","POST"), url, query_or_body, page_size = 2000L) {
  method <- match.arg(method)
  offset <- 0L
  feats_all <- list()

  repeat {
    payload <- query_or_body
    payload$resultOffset <- offset
    payload$resultRecordCount <- page_size

    resp <- if (method == "GET") GET(url, query = payload) else POST(url, body = payload, encode = "form")
    txt  <- content(resp, "text", encoding = "UTF-8")

    if (status_code(resp) != 200 || grepl('"error"\\s*:', txt)) {
      stop("ArcGIS request failed. HTTP ", status_code(resp), "\n", substr(txt, 1, 1200))
    }

    j <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    feats <- j$features %||% list()
    n <- length(feats)
    message("  offset=", offset, " got=", n)

    if (n == 0) break
    feats_all <- c(feats_all, feats)
    offset <- offset + n
    if (n < page_size) break
  }

  list(type="FeatureCollection", features=feats_all)
}

# -------------------------
# 1) Buildings (cache)
# -------------------------
if (FORCE_DOWNLOAD_BLDG || !file.exists(buildings_file)) {
  message("Downloading buildings YEAR_BUILT >= ", YEAR_MIN)
  geo <- arcgis_paged(
    method = "GET",
    url = buildings_url,
    query_or_body = list(
      where = sprintf("YEAR_BUILT >= %d", YEAR_MIN),
      outFields = "*",
      returnGeometry = "true",
      outSR = "4326",
      f = "geojson"
    )
  )
  writeLines(toJSON(geo, auto_unbox = TRUE), buildings_file, useBytes = TRUE)
  message("Wrote: ", buildings_file)
} else {
  message("Using cached buildings: ", buildings_file)
}

sfB <- st_read(buildings_file, quiet = TRUE)
if (is.na(st_crs(sfB))) st_crs(sfB) <- 4326
sfB <- st_transform(sfB, 4326)
message("Buildings: ", nrow(sfB))

# -------------------------
# 2) Taxlots in buildings bbox (cache)
# -------------------------
if (FORCE_DOWNLOAD_LOTS || !file.exists(taxlots_file)) {
  message("Downloading taxlots in buildings bbox")
  bb <- st_bbox(sfB)
  bb_str <- paste(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"], sep=",")

  geo <- arcgis_paged(
    method = "POST",
    url = taxlots_url,
    query_or_body = list(
      geometry = bb_str,
      geometryType = "esriGeometryEnvelope",
      inSR = 4326,
      spatialRel = "esriSpatialRelIntersects",
      where = "1=1",
      outFields = "*",
      returnGeometry = "true",
      outSR = 4326,
      f = "geojson"
    )
  )
  writeLines(toJSON(geo, auto_unbox = TRUE), taxlots_file, useBytes = TRUE)
  message("Wrote: ", taxlots_file)
} else {
  message("Using cached taxlots: ", taxlots_file)
}

sfL <- st_read(taxlots_file, quiet = TRUE)
if (is.na(st_crs(sfL))) st_crs(sfL) <- 4326

# compute lot area (sqft) in UTM 10N
sfL_utm <- st_transform(sfL, 26910)
sfL_utm$lot_sqft <- as.numeric(st_area(sfL_utm)) * 10.7639
stopifnot("TLID" %in% names(sfL_utm))  # Portland taxlots should have TLID

lots_small <- sfL_utm[, c("TLID","lot_sqft")]

# -------------------------
# 3) Join: building centroid within taxlot
# -------------------------
sfB_utm <- st_transform(sfB, 26910)
footprint_sqft_vec <- as.numeric(st_area(sfB_utm)) * 10.7639

sfB_cent <- suppressWarnings(st_centroid(sfB_utm))
sfB_cent$.__brow__ <- seq_len(nrow(sfB_cent))   # building row id

# Primary: centroid within lot
joined <- st_join(sfB_cent, lots_small, join = st_within, left = TRUE)
dtB <- as.data.table(joined)
setorder(dtB, .__brow__)
dtB <- dtB[, .SD[1], by = .__brow__]  # one row per building, always

# Fallback: buffer centroid slightly and intersect (handles boundary cases)
miss <- is.na(joined$lot_sqft)
n_miss <- sum(miss)
if (n_miss > 0) {
  message("Centroid-within missing lot_sqft for ", n_miss, " buildings; trying 1m buffered centroid intersects...")

  # 1 meter buffer in UTM units
  buf <- st_buffer(sfB_cent[miss, ], dist = 1)

  # IMPORTANT: carry original building row index INTO the geometry before join
  buf$.__row__ <- which(miss)

  # Join: may return multiple rows per buffered centroid
  j2 <- st_join(buf, lots_small, join = st_intersects, left = TRUE)

  # Keep first match per original row (you could pick "best" later if needed)
  dt2 <- as.data.table(j2)
  setorder(dt2, .__row__)
  dt2 <- dt2[, .SD[1], by = .__row__]

  # Assign back into 'joined' using those original row indices
  joined$lot_sqft[dt2$.__row__] <- dt2$lot_sqft
  joined$TLID[dt2$.__row__]     <- dt2$TLID
}


# enforce 1 row per building using carried id
setorder(dtB, .__brow__)
dtB <- dtB[, .SD[1], by = .__brow__]

# safe footprint assignment
dtB[, footprint_sqft := footprint_sqft_vec[.__brow__]]
dtB[, footprint_sqft := footprint_sqft_vec[.__brow__]]
dtB[, lot_sqft := as.numeric(lot_sqft)]
if ("BLDG_SQFT" %in% names(dtB)) dtB[, BLDG_SQFT := as.numeric(BLDG_SQFT)]
if ("UNITS_RES" %in% names(dtB)) dtB[, UNITS_RES := as.numeric(UNITS_RES)]

dtB[, far_proxy := if ("BLDG_SQFT" %in% names(dtB)) BLDG_SQFT / lot_sqft else NA_real_]
dtB[, coverage  := footprint_sqft / lot_sqft]

message("Missing lot_sqft (centroid-within): ", sum(is.na(dtB$lot_sqft)))

# save buildings (drop geometry for parquet)
dtB_no_geom <- copy(dtB); if ("geometry" %in% names(dtB_no_geom)) dtB_no_geom[, geometry := NULL]
write_parquet(dtB_no_geom, out_bld_parquet)
saveRDS(dtB, out_bld_rds)
message("Wrote: ", out_bld_parquet)
message("Wrote: ", out_bld_rds)

# -------------------------
# 4) Lot-level aggregation
# -------------------------
if (!("BLDG_TYPE" %in% names(dtB))) dtB[, BLDG_TYPE := NA_character_]
dtB[, BLDG_TYPE := trimws(gsub("[[:space:]\u00A0]+", " ", as.character(BLDG_TYPE)))]

dtB[, has_adu := grepl("Accessory Dwelling Unit|^ADU$", BLDG_TYPE, ignore.case=TRUE)]
dtB[, year_built_num := suppressWarnings(as.numeric(YEAR_BUILT))]

dtLot <- dtB[!is.na(TLID), .(
  lot_sqft = first(lot_sqft),
  n_structures = .N,
  units_total = base::sum(fifelse(is.na(UNITS_RES), 0, UNITS_RES), na.rm=TRUE),
  bldg_sqft_total = base::sum(fifelse(is.na(BLDG_SQFT), 0, BLDG_SQFT), na.rm=TRUE),
  footprint_sqft_total = base::sum(fifelse(is.na(footprint_sqft), 0, footprint_sqft), na.rm=TRUE),
  has_adu = any(has_adu),
  year_built_max = suppressWarnings(max(year_built_num, na.rm=TRUE))
), by=TLID]

dtLot[is.infinite(year_built_max), year_built_max := NA_real_]
dtLot[, far := bldg_sqft_total / lot_sqft]
dtLot[, coverage := footprint_sqft_total / lot_sqft]
dtLot[, post_reform := year_built_max >= 2020]

write_parquet(copy(dtLot), out_lot_parquet)
saveRDS(dtLot, out_lot_rds)
message("Wrote: ", out_lot_parquet)
message("Wrote: ", out_lot_rds)

message("Lots: ", nrow(dtLot), " | mean structures/lot: ", mean(dtLot$n_structures))
message("Lots with ADU: ", dtLot[has_adu==TRUE, .N])
