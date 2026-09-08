# duplexPrices.R
# Vancouver duplex/SFD price analysis, East/West (Ontario St), 2014-2019 (extract ends 2019)
#   (1) price-sqft elasticity, single/duplex x East/West
#   (2) duplex orientation (side-by-side vs front/back) x East/West
#   (3) SFD 33' vs 50' elasticity x East/West  (puzzle: WS 33s go duplex, 50s don't)
# Derived from vancouverMatch.R; reads prebuilt sales rds, no writes.
# Tom Davidoff
# 09/08/26

SALE_MINYEAR <- 2014
SALE_MAXYEAR <- 2025          # extract actually ends 2019; kept for label only
ONTARIO_LON  <- -123.1036     # lon < this = West Side

library(data.table)
library(fixest)

fnameSales <- "~/DropboxExternal/dataProcessed/bca19VancouverSalesDupSFD.rds"
dtSales <- readRDS(fnameSales)

sfdUse <- c("Single Family Dwelling", "Residential Dwelling with Suite")
dtSales[, price    := as.numeric(conveyancePrice)]
dtSales[, saleYear := as.numeric(substring(conveyanceDate, 1, 4))]
dtSales[, type := fifelse(grepl("Duplex", actualUseDescription), "duplex",
                  fifelse(actualUseDescription %in% sfdUse,      "SFD", NA_character_))]

# ============================================================================
# (1) ELASTICITY: log(price) ~ log(sqft), by type x side
# ============================================================================
dt <- dtSales[!is.na(type) & price > 0 & !is.na(longitude) &
              !is.na(MB_total_finished_area) & MB_total_finished_area > 0 &
              saleYear %between% c(SALE_MINYEAR, SALE_MAXYEAR)]
dt[, side := fifelse(longitude < ONTARIO_LON, "West", "East")]

runElastTS <- function(ty, sd) {
  d <- dt[type == ty & side == sd]
  cat(sprintf("\n==== %s / %s : log(price) ~ log(sqft) | MB_effective_year + saleYear  (n=%d) ====\n",
              ty, sd, nrow(d)))
  if (nrow(d) < 10) { cat("  too few obs\n"); return(invisible()) }
  print(summary(feols(log(price) ~ log(MB_total_finished_area) | MB_effective_year + saleYear,
                      data = d, vcov = "hetero")))
}
for (ty in c("SFD","duplex")) for (sd in c("East","West")) runElastTS(ty, sd)

# ============================================================================
# (2) DUPLEX ORIENTATION: side-by-side vs front/back, by side
# ============================================================================
dtD <- dt[type == "duplex"]
dtD[, orient := fifelse(grepl("Side by Side", actualUseDescription), "sideBySide",
                fifelse(grepl("Front / Back",  actualUseDescription), "frontBack", NA_character_))]

cat("\n=== duplex orientation counts x side (2014-2019) ===\n")
print(dtD[!is.na(orient), .N, by = .(side, orient)][order(side, orient)])

cat("\n=== duplex mean/median price by orientation x side ===\n")
print(dtD[!is.na(orient),
          .(n = .N, mean = mean(price), p50 = median(price)),
          by = .(side, orient)][order(side, orient)])

cat("\n=== duplex: log(price) ~ sideBySide | MB_effective_year + saleYear, by side ===\n")
for (sd in c("East","West")) {
  d <- dtD[!is.na(orient) & side == sd]
  d[, sbs := orient == "sideBySide"]
  cat(sprintf("\n---- %s (n=%d) ----\n", sd, nrow(d)))
  if (nrow(d) < 10 || uniqueN(d$sbs) < 2) { cat("  too few obs / no contrast\n"); next }
  print(summary(feols(log(price) ~ sbs | MB_effective_year + saleYear, data = d, vcov = "hetero")))
}

# ============================================================================
# (3) SFD 33' vs 50' ELASTICITY x side
#     Tests whether the size-elasticity gap driving duplex take-up also
#     separates 33' from 50' SFD lots.
# ============================================================================
# (3) SFD 33' vs 50' price distribution x side  (analogue of duplex orient table)
# ============================================================================
dtS <- dtSales[actualUseDescription %in% sfdUse & price > 0 & !is.na(longitude) &
               !is.na(landWidth) &
               saleYear %between% c(SALE_MINYEAR, SALE_MAXYEAR)]
dtS[, side       := fifelse(longitude < ONTARIO_LON, "West", "East")]
dtS[, widthClass := round(landWidth)]
dtS <- dtS[widthClass %in% c(33, 50)]

cat("\n=== SFD price by width x side (2014-2019) ===\n")
print(dtS[, .(n = .N, mean = mean(price), p50 = median(price)),
          by = .(side, widthClass)][order(side, widthClass)])# ============================================================================
