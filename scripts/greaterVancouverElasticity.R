# greaterVancouverElasticity.R
# Tract -> neighbourhood -> jurisdiction.
# Headline: cor(ppsf_fe, elasticity) UNWEIGHTED at each level.
#           The point is the cross-level pattern -- as cells get coarser,
#           per-cell precision rises and N falls, and we want to see the
#           cor move without imposing a weighting scheme on that tradeoff.
# Supplement: weightedCorDiagnostic() reports precision-weighted and
#           SE-trimmed versions at each level for robustness only.
# Tom Davidoff
# 05/16/26  (rev 05/18/26: per-cell elasticity SE + weighted-cor diagnostic)

library(sf)
library(data.table)
library(DBI)
library(RSQLite)
library(fixest)
library(ggplot2)

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------
trimByQuantile <- function(dt, cols, q) {
  for (c in cols) {
    lo <- quantile(dt[[c]], q,     na.rm = TRUE)
    hi <- quantile(dt[[c]], 1 - q, na.rm = TRUE)
    dt <- dt[get(c) %between% c(lo, hi)]
  }
  dt
}

# Supplementary diagnostic. Input: a per-cell results table with columns
#   ppsf_fe, elasticity, se_elasticity, n
# Reports four flavours of cor(ppsf_fe, elasticity):
#   (1) unweighted (headline -- recomputed here for the table)
#   (2) precision-weighted by 1/se^2
#   (3) unweighted on the precision-trimmed subset (drop top SE quartile)
#   (4) unweighted on cells with n >= nFloor
# Intent: see whether the level-by-level cor pattern is driven by noisy
# cells. If (2)-(4) move toward the coarser-level number, the noise story
# holds; if they don't, the coarsening effect is substantive.
weightedCorDiagnostic <- function(dt,
                                  nFloor = 30,
                                  seQuantile = 0.75,
                                  label = "") {
  d <- dt[!is.na(ppsf_fe) & !is.na(elasticity) & !is.na(se_elasticity)]
  if (nrow(d) < 5) {
    return(data.table(label = label, flavour = NA_character_,
                      cor = NA_real_, n = nrow(d)))
  }
  seCut <- quantile(d$se_elasticity, seQuantile, na.rm = TRUE)
  dTrim <- d[se_elasticity <= seCut]
  dNflr <- d[n >= nFloor]
  wCor <- function(x, y, w) {
    w <- w / sum(w)
    mx <- sum(w * x); my <- sum(w * y)
    cxy <- sum(w * (x - mx) * (y - my))
    vx  <- sum(w * (x - mx)^2)
    vy  <- sum(w * (y - my)^2)
    cxy / sqrt(vx * vy)
  }
  rbind(
    data.table(label = label, flavour = "unweighted",
               cor = cor(d$ppsf_fe, d$elasticity),
               n   = nrow(d)),
    data.table(label = label, flavour = "precision_wtd_1_over_se2",
               cor = wCor(d$ppsf_fe, d$elasticity, 1 / d$se_elasticity^2),
               n   = nrow(d)),
    data.table(label = label,
               flavour = sprintf("trim_se_below_q%.2f", seQuantile),
               cor = if (nrow(dTrim) >= 5)
                 cor(dTrim$ppsf_fe, dTrim$elasticity) else NA_real_,
               n   = nrow(dTrim)),
    data.table(label = label, flavour = sprintf("n_geq_%d", nFloor),
               cor = if (nrow(dNflr) >= 5)
                 cor(dNflr$ppsf_fe, dNflr$elasticity) else NA_real_,
               n   = nrow(dNflr))
  )
}

# Diagnostic scatter: elasticity vs ppsf_fe with point size proportional to
# 1/se^2. Visual companion to the weighted diagnostic.
plotElasticityVsFE <- function(dt, label, outPath) {
  d <- dt[!is.na(ppsf_fe) & !is.na(elasticity) & !is.na(se_elasticity) &
          se_elasticity > 0]
  if (nrow(d) < 5) return(invisible(NULL))
  d[, w := 1 / se_elasticity^2]
  p <- ggplot(d, aes(x = ppsf_fe, y = elasticity, size = w)) +
    geom_point(alpha = 0.45) +
    scale_size(trans = "sqrt", guide = "none") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(title = sprintf("Elasticity vs ppsf FE -- %s", label),
         subtitle = "point size proportional to 1/SE^2 of elasticity",
         x = "ppsf fixed effect (from log-age + year-FE regression)",
         y = "elasticity of log(ppsf) wrt log(sqft)") +
    theme_minimal()
  ggsave(outPath, plot = p, width = 6.5, height = 5, dpi = 130)
  invisible(p)
}

# ============================================================================
# Extract Greater Van residential parcels  (unchanged from prior version)
# ============================================================================

bcaGeoFolder <- "~/bigFiles/latestSpatialBCA/"
bcaFiles     <- list.files(bcaGeoFolder, pattern = "*.gpkg", full.names = FALSE)
geoInFile    <- paste0(bcaGeoFolder, bcaFiles[1])
geoOutFile   <- "~/DropboxExternal/dataProcessed/greaterVancouverSingle.rds"

if (!file.exists(geoOutFile)) {
  cat("Loading Greater Vancouver residential parcels from BCA\n")
  sBCA <- st_read(
    geoInFile,
    layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
    query = paste("SELECT ROLL_NUMBER, NEIGHBOURHOOD, JURISDICTION_CODE, geom",
                  "FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
                  "WHERE REGIONAL_DISTRICT_CODE='15'",
                  "AND ACTUAL_USE_DESCRIPTION IN",
                  "('Residential Dwelling with Suite','Single Family Dwelling')"),
    quiet = TRUE)
  dCT  <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
  dCT  <- st_transform(dCT, st_crs(sBCA))
  sBCA <- st_join(sBCA, dCT, join = st_within)
  dtBCA <- as.data.table(sBCA)
  dtBCA[, rollNumber   := trimws(as.character(ROLL_NUMBER))]
  dtBCA[, jurisdiction := trimws(as.character(JURISDICTION_CODE))]
  dtBCA <- dtBCA[!is.na(rollNumber) & rollNumber != "" &
                 !is.na(jurisdiction) & jurisdiction != ""]
  saveRDS(dtBCA, geoOutFile)
}
dtBCA <- as.data.table(readRDS(geoOutFile))

cfgVancouver <- list(
  sqliteFile     = "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3",
  trimDimsQ      = 0.05,
  trimYQ         = 0.01,
  salesYearRange = c(2014, 2018)
)
cfg <- cfgVancouver

# ============================================================================
# Appraisal + sales merge  (unchanged)
# ============================================================================
con <- dbConnect(SQLite(), cfg$sqliteFile)
valuation   <- as.data.table(dbGetQuery(con, "SELECT folioID, landValue FROM valuation"))
inventory   <- as.data.table(dbGetQuery(con,
                "SELECT roll_number, land_width, land_depth, zoning,
                        MB_effective_year, MB_total_finished_area, jurisdiction
                 FROM residentialInventory"))
folio       <- as.data.table(dbGetQuery(con,
                "SELECT folioID, rollNumber, jurisdictionCode FROM folio"))
description <- as.data.table(dbGetQuery(con,
                paste("SELECT folioID, actualUseDescription, landWidth, landDepth",
                      "FROM folioDescription",
                      "WHERE actualUseDescription IN",
                      "('Residential Dwelling with Suite','Single Family Dwelling')")))
dbDisconnect(con)

setkey(valuation, folioID); setkey(folio, folioID)
valuation <- valuation[folio]
setkey(description, folioID); setkey(valuation, folioID)
valuation <- valuation[description]
setnames(valuation, "jurisdictionCode", "jurisdiction")

valuation[, rollNumber   := trimws(as.character(rollNumber))]
inventory[, rollNumber   := trimws(as.character(roll_number))]
valuation[, jurisdiction := trimws(as.character(jurisdiction))]
inventory[, jurisdiction := trimws(as.character(jurisdiction))]
valuation <- valuation[!is.na(rollNumber) & rollNumber != "" &
                       !is.na(jurisdiction) & jurisdiction != ""]
inventory <- inventory[!is.na(rollNumber) & rollNumber != "" &
                       !is.na(jurisdiction) & jurisdiction != ""]
valuation <- unique(valuation, by = c("rollNumber", "jurisdiction"))
inventory <- unique(inventory, by = c("rollNumber", "jurisdiction"))

setkey(inventory, rollNumber, jurisdiction)
setkey(valuation, rollNumber, jurisdiction)
valuation <- valuation[inventory, nomatch = NULL]

numericCols <- c("landValue","land_width","land_depth","landWidth","landDepth")
valuation[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
valuation[is.na(landWidth) & !is.na(land_width), landWidth := land_width]
valuation[is.na(landDepth) & !is.na(land_depth), landDepth := land_depth]

setkey(valuation, rollNumber, jurisdiction)
setkey(dtBCA,     rollNumber, jurisdiction)
dt <- valuation[dtBCA, nomatch = 0]
dt[, landArea := landWidth * landDepth]
dt[, ppsf     := landValue / landArea]
dt <- dt[!is.na(ppsf) & !is.na(landArea) & landArea > 0 & ppsf > 0]
dt <- trimByQuantile(dt, c("landWidth","landDepth"), cfg$trimDimsQ)
dt <- trimByQuantile(dt, "ppsf", cfg$trimYQ)
dt[, logPPSF := log(ppsf)]
dt[, logArea := log(landArea)]
dtMerge <- dt; dt <- NULL

# ---- sales ------------------------------------------------------------------
con <- dbConnect(SQLite(), cfg$sqliteFile)
dt  <- as.data.table(dbGetQuery(con,
        paste("SELECT folioID, conveyanceDate, conveyancePrice,",
              "conveyanceTypeDescription FROM sales")))
dbDisconnect(con)

dt <- dt[conveyanceTypeDescription == "Improved Single Property Transaction",
         .(folioID, conveyanceDate, conveyancePrice)]
dt <- merge(dt, dtMerge, by = "folioID")
dt[, conveyancePrice := as.numeric(conveyancePrice)]
dt[, landWidth       := as.numeric(landWidth)]
dt[, landDepth       := as.numeric(landDepth)]
dt <- dt[!is.na(conveyancePrice) & !is.na(landWidth) & !is.na(landDepth) &
         conveyancePrice > 0 & landWidth > 0 & landDepth > 0]
dt[, year      := as.numeric(substring(conveyanceDate, 1, 4))]
dt[, landArea  := landWidth * landDepth]
dt[, ppsfSale  := conveyancePrice / landArea]
dt[, logPPSFSale := log(ppsfSale)]
dt[, age       := year - as.numeric(MB_effective_year)]
dt[, sqft      := as.numeric(MB_total_finished_area)]
dt[, FSR       := sqft / landArea]
dt <- trimByQuantile(dt, c("landWidth","landDepth"), cfg$trimDimsQ)
dt <- trimByQuantile(dt, "ppsfSale", cfg$trimYQ)
dt <- dt[year %between% cfg$salesYearRange]
dt <- dt[age >= 1 & !is.na(age) & !is.na(sqft) & sqft > 0]
dt[, ppsf := conveyancePrice / sqft]

# ============================================================================
# Per-level results: FE + per-cell elasticity (with SE)
# ============================================================================

MINREGOBS  <- 10
levelsToRun <- c("CTNAME","NEIGHBOURHOOD","jurisdiction")

# Per-cell elasticity regression. Returns coef and its SE so the
# diagnostic helper can build precision-weighted comparisons later.
fitCell <- function(sd) {
  if (nrow(sd) < MINREGOBS)
    return(list(elasticity = NA_real_, se_elasticity = NA_real_, n = nrow(sd)))
  reg <- tryCatch(
    feols(log(ppsf) ~ log(age) + i(year) + log(sqft), data = sd, warn = FALSE),
    error = function(e) NULL)
  if (is.null(reg))
    return(list(elasticity = NA_real_, se_elasticity = NA_real_, n = nrow(sd)))
  cm <- coeftable(reg)
  rn <- rownames(cm)
  idx <- match("log(sqft)", rn)
  if (is.na(idx))
    return(list(elasticity = NA_real_, se_elasticity = NA_real_, n = nrow(sd)))
  list(elasticity    = unname(cm[idx, 1]),
       se_elasticity = unname(cm[idx, 2]),
       n             = nrow(sd))
}

out      <- list()
corBoard <- list()
diagBoard <- list()

for (level in levelsToRun) {
  regFE <- feols(as.formula(paste0("ppsf ~ log(age) + i(year) | ", level)),
                 data = dt)
  fe_dt <- data.table(level_value = names(fixef(regFE)[[1]]),
                      ppsf_fe     = as.numeric(fixef(regFE)[[1]]))

  elas_dt <- dt[!is.na(get(level)) & !is.na(FSR) & FSR > 0,
                fitCell(.SD),
                by = .(level_value = get(level))]

  out[[level]] <- merge(fe_dt, elas_dt, by = "level_value", all.x = TRUE)

  unwCor <- out[[level]][!is.na(elasticity), cor(ppsf_fe, elasticity)]
  corBoard[[level]] <- data.table(level = level, cor_unweighted = unwCor,
                                  cells = sum(!is.na(out[[level]]$elasticity)))
  cat(sprintf("\n[%s] cor(ppsf_fe, elasticity) UNWEIGHTED = %.4f  (cells = %d)\n",
              level, unwCor,
              sum(!is.na(out[[level]]$elasticity))))

  diagBoard[[level]] <- weightedCorDiagnostic(out[[level]], label = level)
  plotElasticityVsFE(out[[level]], label = level,
                     outPath = sprintf("text/greaterVan_elasFE_%s.png", level))
}

cat("\n============== HEADLINE: unweighted cor by level ==============\n")
print(rbindlist(corBoard))

cat("\n============== SUPPLEMENT: weighted-cor diagnostics ==============\n")
print(rbindlist(diagBoard))

# Optional: save per-level result tables for the paper.
saveRDS(out, "~/DropboxExternal/dataProcessed/greaterVan_elasticityByLevel.rds")
