# permitOriginalLotVancouver.R 
# R to map R-zone permits to original (2016) Vancouver lots via parcel-address fabric and BCA data.
# Tom Davidoff
# 01/07/26
# ------------------------------------------------------------
# Vancouver infill puzzle: Permits x Parcel-address fabric x BCA(2016)
#
# Goal: produce one row per ORIGINAL (2016) lot:
#   (a) outcome ∈ {sf_rebuild_or_upgrade, duplex, multiplex}
#   (b) lot_size_original = landWidth * landDepth  (ignore land_area)
#
# Notes / tips baked in:
# - Fixes Vancouver address quirks (unit numbers, VANCOUVER BC, postal codes)
# - Canonicalizes street types (STREET->ST, AVENUE/AVE->AV), ordinals (12TH->12),
#   direction placement (e.g. "... ST E" -> "E ... ST")
# - Uses parcel GeoJSON as address normalizer (since it has civic_number + streetname)
# - BCA join path: permit -> parcel addr_key -> BCA.address -> folio -> rollNumber
# - Correct BCA dims query: residentialInventory has roll_number; folioDescription has rollNumber
# - Classifier tightened (multiplex overcount guard): excludes obvious big projects and,
#   if units column exists, uses it.
#
# Dependencies: data.table, DBI, RSQLite, jsonlite, stringi
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(DBI)
  library(RSQLite)
  library(jsonlite)
  library(stringi)
})

# ---- paths
dir_raw <- "~/OneDrive - UBC/dataRaw"
dir_bca <- "~/OneDrive - UBC/Documents/data/bca"
f_perm  <- file.path(dir_raw, "vancouver_permits_full.csv")
f_parc  <- file.path(dir_raw, "property-parcel-polygons.geojson")
f_bca   <- file.path(dir_bca, "REVD16_and_inventory_extracts.sqlite3")
f_bca18 <- file.path(dir_bca, "REVD18_and_inventory_extracts_CSV_files/address.csv")

library(data.table)
library(stringi)

# ---------- helpers
norm <- function(x) {
  x <- fifelse(is.na(x), "", x)
  x <- toupper(trimws(x))
  x <- stri_replace_all_regex(x, "[^A-Z0-9 ]+", " ")
  x <- stri_replace_all_regex(x, "\\s+", " ")
  trimws(x)
}

# Extract civic number (first integer)
civic_num <- function(addr) as.integer(stri_extract_first_regex(norm(addr), "\\b\\d{1,6}\\b"))

# Extract postal (optional)
postal <- function(addr) stri_extract_first_regex(norm(addr), "\\b[ABCEGHJ-NPRSTVXY]\\d[ABCEGHJ-NPRSTVXY]\\s*\\d[ABCEGHJ-NPRSTVXY]\\d\\b")

# Canonical street string from permit free-form:
# remove civic num, unit, city/province/postal tail; normalize common street types + ordinals
canon_street <- function(addr) {
  a <- norm(addr)

  a <- stri_replace_all_regex(a, "#\\s*\\d+\\b", "")                  # unit like #205
  a <- stri_replace_first_regex(a, "^\\s*\\d{1,6}\\s+", "")           # drop civic num
  a <- stri_replace_all_regex(a, "\\bVANCOUVER\\b.*$", "")            # drop city onward
  a <- stri_replace_all_regex(a, "\\bBC\\b.*$", "")
  a <- stri_replace_all_regex(a, "\\b[ABCEGHJ-NPRSTVXY]\\d[ABCEGHJ-NPRSTVXY]\\s*\\d[ABCEGHJ-NPRSTVXY]\\d\\b.*$", "")

  a <- stri_replace_all_regex(a, "\\b(\\d+)(ST|ND|RD|TH)\\b", "$1")   # 1ST -> 1
  a <- stri_replace_all_regex(a, "\\bAVENUE\\b|\\bAVE\\b", "AV")
  a <- stri_replace_all_regex(a, "\\bSTREET\\b", "ST")
  a <- stri_replace_all_regex(a, "\\bROAD\\b", "RD")
  a <- stri_replace_all_regex(a, "\\bDRIVE\\b", "DR")
  a <- stri_replace_all_regex(a, "\\bLANE\\b", "LN")
  a <- stri_replace_all_regex(a, "\\bBOULEVARD\\b", "BLVD")
  a <- stri_replace_all_regex(a, "\\s+", " ")
  trimws(a)
}

# Build key: "NUM DIR NAME TYPE DIR2" (spaces normalized)
mk_key <- function(num, dir_pre, name, type, dir_suf) {
  norm(paste(num, dir_pre, name, type, dir_suf))
}

# ---------- load permit data (example: you already have perm)
# perm <- fread("vancouver_permits_full.csv", sep=";")
# expects column: address and permitnumber (or similar)
perm <- fread(f_perm, sep=";", na.strings=c("", "NA"))
perm <- copy(perm)  # if already in memory

perm[, civic := civic_num(address)]
perm[, street := canon_street(address)]
perm[, permit_key := norm(paste(civic, street))]
perm_use <- perm[!is.na(civic) & civic > 0 & nzchar(street), .(permitnumber, permit_key)]

# ---------- load BCA address CSV (2018)
# bca_addr <- fread(".../address_2018.csv")
bca_addr <- fread(f_bca18)

# primary only (as text "true"/"false" in your head)
bca_addr <- bca_addr[norm(primaryFlag) %in% c("TRUE", "T", "1")]

# Build BCA key using structured parts
bca_addr[, bca_key := mk_key(streetNumber, streetDirectionPrefix, streetName, streetType, streetDirectionSuffix)]
bca_use <- bca_addr[!is.na(streetNumber) & streetNumber != "", .(folioID, bca_key)]

setkey(perm_use, permit_key)
setkey(bca_use, bca_key)

m <- bca_use[perm_use, on=c(bca_key="permit_key"), nomatch=0L]

cat("Permits with usable address:", nrow(perm_use), "\n")
cat("Matched permits:", uniqueN(m$permitnumber), "\n")
cat("Match rate:", round(uniqueN(m$permitnumber) / nrow(perm_use), 3), "\n")
cat("Avg folios per matched permit:", nrow(m) / uniqueN(m$permitnumber), "\n")

# optional: save unmatched for inspection
unmatched <- perm_use[!permit_key %in% m$bca_key]
stop("FF")

# ---- output
f_out_lot   <- "lot_outcomes_lotsize_2016.csv"
f_out_match <- "permit_matches_debug.csv"

# ---------------------------
# Helpers
# ---------------------------
norm_str <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- toupper(trimws(x))
  x <- stri_replace_all_regex(x, "[^A-Z0-9 ]+", " ")
  x <- stri_replace_all_regex(x, "\\s+", " ")
  trimws(x)
}

numify <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))

extract_postal <- function(addr) {
  a <- norm_str(addr)
  stri_extract_first_regex(a, "\\bV[0-9][A-Z]\\s*[0-9][A-Z][0-9]\\b")
}

extract_civic_num <- function(addr) {
  a <- norm_str(addr)
  as.integer(stri_extract_first_regex(a, "\\b\\d{1,6}\\b"))
}

# Canonical street string to match parcel fabric style:
# - drop city/province/postal/unit tail
# - standardize street types: STREET->ST, AVENUE/AVE->AV, etc.
# - strip ordinals: 12TH->12
# - move trailing direction: "... ST E" -> "E ... ST"
canon_street <- function(addr_or_street) {
  a <- norm_str(addr_or_street)

  # If this is a full address, remove leading civic number (and ranges like 1234-1238)
  a <- stri_replace_first_regex(a, "^\\s*\\d{1,6}(\\s*\\-\\s*\\d{1,6})?\\s+", "")

  # Remove VANCOUVER / BC / postal tail (order matters)
  a <- stri_replace_all_regex(a, "\\bVANCOUVER\\b.*$", "")
  a <- stri_replace_all_regex(a, "\\bBC\\b.*$", "")
  a <- stri_replace_all_regex(a, "\\bV[0-9][A-Z]\\s*[0-9][A-Z][0-9]\\b.*$", "")

  # Remove trailing unit number if present: "... ST 109"
  a <- stri_replace_all_regex(a, "\\s+\\d{1,6}$", "")

  # Strip ordinals: 12TH -> 12
  a <- stri_replace_all_regex(a, "\\b(\\d+)(ST|ND|RD|TH)\\b", "$1")

  # Street-type normalization to align with parcel fabric (AV not AVE)
  a <- stri_replace_all_regex(a, "\\bSTREET\\b", "ST")
  a <- stri_replace_all_regex(a, "\\bAVENUE\\b", "AV")
  a <- stri_replace_all_regex(a, "\\bAVE\\b", "AV")
  a <- stri_replace_all_regex(a, "\\bBOULEVARD\\b", "BLVD")
  a <- stri_replace_all_regex(a, "\\bROAD\\b", "RD")
  a <- stri_replace_all_regex(a, "\\bDRIVE\\b", "DR")
  a <- stri_replace_all_regex(a, "\\bCRESCENT\\b", "CRES")
  a <- stri_replace_all_regex(a, "\\bLANE\\b", "LN")
  a <- stri_replace_all_regex(a, "\\bPLACE\\b", "PL")
  a <- stri_replace_all_regex(a, "\\bCOURT\\b", "CT")
  a <- stri_replace_all_regex(a, "\\bTERRACE\\b", "TER")

  # Move trailing direction token to front (if present)
  a <- stri_replace_all_regex(a, "^(.*)\\s+([NSEW])\\b$", "$2 $1")

  a <- stri_replace_all_regex(a, "\\s+", " ")
  trimws(a)
}

# Outcome classifier (conservative; multiplex overcount guard)
# - If units column exists, prefer it.
# - Exclude obvious big projects by text
classify_outcome <- function(propertyuse, usecat, desc, work, units = NA) {
  pu <- norm_str(propertyuse)
  uc <- norm_str(usecat)
  ds <- norm_str(desc)
  wk <- norm_str(work)

  units_n <- suppressWarnings(as.integer(units))

  # obvious big-project exclusion tokens (Vancouver permit language)
  big_project <- stri_detect_regex(ds, "(APARTMENT|TOWER|MID\\s*RISE|HIGH\\s*DENSITY|CONDO|MIXED\\s*USE|CD\\s*\\-\\s*1|REZONING)") |
                 stri_detect_regex(pu, "(COMMERCIAL|OFFICE|RETAIL|INDUSTRIAL)")

  # duplex
  is_duplex <- (!is.na(units_n) & units_n == 2) |
               stri_detect_regex(ds, "\\bDUPLEX\\b|TWO\\s*FAMILY") |
               stri_detect_regex(uc, "\\bDUPLEX\\b")

  # multiplex (infill scale) -- if units known, require 3..6; else require explicit triplex/fourplex/...
  is_multiplex <- (!is.na(units_n) & units_n >= 3 & units_n <= 6) |
                  (is.na(units_n) &
                    stri_detect_regex(ds, "(TRIPLEX|FOURPLEX|FIVEPLEX|SIXPLEX|MULTIPLEX)"))

  # sf rebuild/upgrade
  is_sf <- (!is.na(units_n) & units_n == 1) |
           (stri_detect_regex(pu, "(SINGLE\\s*FAMILY|ONE\\s*FAMILY|DWELLING)") &
            stri_detect_regex(wk, "(NEW|ADDITION|ALTERATION|RENOVATION|REPAIR|CONVERSION)"))

  out <- ifelse(is_multiplex & !big_project, "multiplex",
         ifelse(is_duplex   & !big_project, "duplex",
         ifelse(is_sf, "sf_rebuild_or_upgrade", NA_character_)))
  out
}

# ---------------------------
# 1) Load permits (CSV) + build clean addr_key
# ---------------------------
perm <- fread(f_perm, sep=";", na.strings=c("", "NA"))
stopifnot("address" %in% names(perm))
stopifnot(is.character(perm$address))   # critical: avoids list/function clobber
stop("CHECK PERM")

# Units column (optional): autodetect by name
unit_candidates <- grep("unit", names(perm), value = TRUE, ignore.case = TRUE)
units_col <- if (length(unit_candidates) > 0) unit_candidates[1] else NA_character_
message("Units column detected: ", ifelse(is.na(units_col), "<none>", units_col))

perm[, postal := extract_postal(address)]
perm[, civic_number := extract_civic_num(address)]
perm[, street_name := canon_street(address)]
perm[, addr_key1 := paste(civic_number, street_name)]

# classify
if (!is.na(units_col)) {
  perm[, outcome := classify_outcome(propertyuse, specificusecategory, projectdescription, typeofwork, get(units_col))]
} else {
  perm[, outcome := classify_outcome(propertyuse, specificusecategory, projectdescription, typeofwork)]
}

perm_evt <- perm[!is.na(outcome) & civic_number > 0 & nzchar(street_name),
                 .(permitnumber, issuedate, issueyear, geolocalarea,
                   address, civic_number, street_name, postal, addr_key1, outcome)]

message("Permit outcomes:")
print(perm_evt[, .N, by=outcome][order(-N)])

# ---------------------------
# 2) Load parcel fabric (GeoJSON) via jsonlite (no GDAL needed)
#    properties: civic_number, streetname, site_id, tax_coord, geo_point_2d
# ---------------------------
parc_json <- fromJSON(f_parc, simplifyDataFrame = TRUE)
parc <- as.data.table(parc_json$features$properties)

stopifnot(all(c("civic_number","streetname") %in% names(parc)))

parc[, civic_number := as.integer(civic_number)]
parc[, street_name := canon_street(streetname)]
parc[, addr_key1 := paste(civic_number, street_name)]

# de-dup by key
setkey(parc, addr_key1)
parc_uniq <- parc[!is.na(civic_number) & civic_number > 0 & nzchar(street_name),
                  .SD[1], by=addr_key1]

# quick overlap check
ov <- length(intersect(perm_evt$addr_key1, parc_uniq$addr_key1))
message("Key overlap permits vs parcel fabric: ", ov)

# ---------------------------
# 3) Join permits -> parcel fabric (address bridge)
# ---------------------------
setkey(perm_evt, addr_key1)
setkey(parc_uniq, addr_key1)
perm_parc <- parc_uniq[perm_evt, on="addr_key1", nomatch=0L]

message("perm_evt rows: ", nrow(perm_evt))
message("perm_parc matched rows: ", nrow(perm_parc))

# Write a debug file you can inspect if matching is low
fwrite(perm_parc, f_out_match)

# ---------------------------
# 4) Pull BCA (2016) from SQLite
#    Join path: BCA.address -> folio -> rollNumber
#    Lot size: width*depth from residentialInventory (fallback folioDescription)
# ---------------------------
con <- dbConnect(RSQLite::SQLite(), f_bca)
# --- Address table (one row per folioID per address record) ---
bca_addr <- as.data.table(dbGetQuery(con, "
  SELECT
    folioID,
    streetNumber,
    streetName,
    streetType,
    city,
    province,
    postalCode,
    primaryFlag
  FROM address
"))

# --- Folio table: stable parcel ID + roll number ---
bca_folio <- as.data.table(dbGetQuery(con, "
  SELECT
    folioID,
    rollNumber
  FROM folio
"))

# --- Dimensions: residentialInventory keyed by roll_number;
#     folioDescription keyed by folioID, so bridge via folio ---
bca_dims <- as.data.table(dbGetQuery(con, "
  SELECT
    ri.roll_number AS roll_number,
    f.folioID      AS folioID,

    ri.land_width  AS land_width_ri,
    ri.land_depth  AS land_depth_ri,

    fd.landWidth   AS land_width_fd,
    fd.landDepth   AS land_depth_fd
  FROM residentialInventory ri
  LEFT JOIN folio f
    ON f.rollNumber = ri.roll_number
  LEFT JOIN folioDescription fd
    ON fd.folioID = f.folioID
"))

# --- Optional stable identifier (PID) ---
bca_pid <- as.data.table(dbGetQuery(con, "
  SELECT
    folioID,
    PID
  FROM legalDescription
"))

dbDisconnect(con)


# Build BCA address keys matching our canonical form
bca_addr[, civic_number := as.integer(streetNumber)]
bca_addr[, street_name := canon_street(paste(streetName, streetType))]
bca_addr[, postal := norm_str(postalCode)]
bca_addr[, addr_key1 := paste(civic_number, street_name)]

# ---------------------------
# 5) Join matched permits (perm_parc) -> BCA by addr_key1, optionally confirm with postal if present
# ---------------------------
# (A) join by addr_key1
setkey(bca_addr, addr_key1)
setkey(perm_parc, addr_key1)
m <- bca_addr[perm_parc, on="addr_key1", nomatch=0L]

# (B) optional: if permit has postal, enforce it when BCA has it (reduces false matches)
# After x[i] join, overlapping columns from i are named i.<col>
m[, permit_postal := norm_str(i.postal)]
m[, bca_postal    := norm_str(postal)]

# keep match if permit postal is missing OR equals BCA postal (when BCA postal exists)
m <- m[
  permit_postal == "" | is.na(permit_postal) |
  (bca_postal != "" & !is.na(bca_postal) & permit_postal == bca_postal)
]

message("Rows after BCA address match: ", nrow(m))
if (nrow(m) == 0) stop("No BCA matches. Inspect permit_matches_debug.csv and BCA address canonicalization.")

# ---------------------------
# 6) Join rollNumber -> dims -> compute lot size (width*depth)
# ---------------------------
setkey(bca_folio, folioID)
setkey(m, folioID)
m <- bca_folio[m, on="folioID"]

m[, roll_number := rollNumber]
setkey(bca_dims, roll_number)
setkey(m, roll_number)
m <- bca_dims[m, on="roll_number"]

setkey(bca_pid, folioID)
m <- bca_pid[m, on="folioID"]

m[, land_width := fcoalesce(numify(land_width_ri), numify(land_width_fd))]
m[, land_depth := fcoalesce(numify(land_depth_ri), numify(land_depth_fd))]
m[, lot_size_original := land_width * land_depth]

m[, lot_size_source := fifelse(!is.na(land_width_ri) & !is.na(land_depth_ri),
                               "residentialInventory",
                        fifelse(!is.na(land_width_fd) & !is.na(land_depth_fd),
                               "folioDescription", NA_character_))]

# stable original lot id
m[, lot_id := fifelse(!is.na(PID) & nzchar(PID), PID, folioID)]

# ---------------------------
# 7) Extra guardrail: multiplex should be plausible lot size (optional)
#     (uncomment if needed; helps kill townhouse/apartment false positives)
# ---------------------------
# m[outcome=="multiplex" & !is.na(lot_size_original) & lot_size_original > 12000, outcome := NA_character_]
# m <- m[!is.na(outcome)]

# ---------------------------
# 8) Collapse to one row per lot_id (earliest permit event)
# ---------------------------
setorder(m, lot_id, issuedate)

lot_panel <- m[, .(
  first_issue_date  = issuedate[1],
  first_issue_year  = issueyear[1],
  outcome           = outcome[1],
  lot_size_original = lot_size_original[1],
  lot_size_source   = lot_size_source[1],
  land_width        = land_width[1],
  land_depth        = land_depth[1],
  geolocalarea      = geolocalarea[1],
  roll_number       = roll_number[1],
  folioID           = folioID[1],
  PID               = PID[1],
  site_id           = site_id[1],
  tax_coord         = tax_coord[1]
), by=lot_id]

# QA
lot_panel[, lot_size_missing := is.na(lot_size_original) | lot_size_original <= 0]
cat("Lots:", nrow(lot_panel),
    "| missing lot size:", lot_panel[lot_size_missing == TRUE, .N], "\n")

# data.table i-expression gotcha: use explicit boolean
print(lot_panel[lot_size_missing == FALSE,
  .(n=.N,
    p50_w=median(land_width, na.rm=TRUE),
    p50_d=median(land_depth, na.rm=TRUE),
    p50_area=median(lot_size_original, na.rm=TRUE),
    p95_area=quantile(lot_size_original, 0.95, na.rm=TRUE))])

# write
fwrite(lot_panel, f_out_lot)
cat("Wrote:", f_out_lot, "\n")

# Optional: see outcome counts on final lots
print(lot_panel[, .N, by=outcome][order(-N)])

