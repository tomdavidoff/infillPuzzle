# priceEventStudy.R
# Price analogue of permitEventStudy.R: did house-price convergence between high- and low-priced tracts jump in the
# 2023 "announcement period"? Universe: arm's-length single-family sales in the City of Vancouver (BCA 2026 sales layer,
# "Improved Single Property Transaction"; 2026 use Single Family Dwelling or Residential Dwelling with Suite), structures
# under 20 years old at sale by effective year (0 <= sale year - effective year < 20; effective year after the sale year
# dropped: the 2026 inventory describes a later structure). Tract by sale-parcel centroid; tract log ppsf (ppsf33) as in
# the permit regressions.
# ln P ~ i(sale year, ppsf, ref = 2022) + ln lot + ln sqft | tract + sale year + 5-year effective-year bin; SEs clustered
# by tract. Sale years 2019-2025 (the layer ends in 2025); 2023 is estimated, not excluded.
# Argument "condo" (11/22/26): strata condominium units (2026 use Strata-Lot Residence (Condominium)) under 1,000 sq ft
# finished, any age; ln P ~ i(year, ppsf) + ln sqft | tract + year + effective-year bin (no lot control).
# Writes text/eventStudyPrice.png (sfd, default) or text/eventStudyPriceCondo.png (condo)
# Tom Davidoff 11/22/26

library(data.table)
library(sf)
library(fixest)
library(ggplot2)

fGpkg  <- list.files("~/bigFiles/latestSpatialBCA/", pattern = "\\.gpkg$", full.names = TRUE); stopifnot(length(fGpkg) == 1)
fInv   <- "~/DropboxExternal/dataRaw/Residential_inventory_202601/20260101_A09_Residential_Inventory_Extract.txt"
fTract <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
fPpsfT <- "~/DropboxExternal/dataProcessed/ppsf33Tract.csv"
YEARS  <- 2019:2025
TYPE   <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "sfd"; stopifnot(TYPE %in% c("sfd", "condo"))
CONDO  <- TYPE == "condo"
MAXAGE <- 20                       # sfd only: excludes teardowns
MAXSQFT <- 1000                    # condo only
USES   <- if (CONDO) "Strata-Lot Residence (Condominium)" else c("Single Family Dwelling", "Residential Dwelling with Suite")
cat("type:", TYPE, "\n")

lost <- function(n0, n1, what) cat(sprintf("%-50s %7d -> %7d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))
rollKey <- function(x) suppressWarnings(as.numeric(gsub("[^0-9]", "", x)))
numify  <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", x)))

# ---- sales, CoV, arm's length, 2019-2025 ----
sales <- as.data.table(st_read(fGpkg, quiet = TRUE, query = sprintf(
  "SELECT ROLL_NUMBER, CONVEYANCE_PRICE, CONVEYANCE_DATE, CONVEYANCE_TYPE_DESCRIPTION, geom
   FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_SALES_SV
   WHERE JURISDICTION_CODE = '200' AND CONVEYANCE_PRICE > 100000
     AND CAST(substr(CONVEYANCE_DATE, 1, 4) AS INTEGER) BETWEEN %d AND %d", min(YEARS), max(YEARS))))
sales[, year := as.integer(substr(as.character(CONVEYANCE_DATE), 1, 4))]
print(sales[, .N, by = CONVEYANCE_TYPE_DESCRIPTION][order(-N)][1:6])
n0 <- nrow(sales); sales <- sales[CONVEYANCE_TYPE_DESCRIPTION == "Improved Single Property Transaction"]
lost(n0, nrow(sales), "arm's-length improved single property")
sales[, roll := rollKey(ROLL_NUMBER)]

# tract by parcel centroid
ct <- st_transform(st_make_valid(st_read(fTract, quiet = TRUE)[, "CTUID"]), 26910)
cen <- st_centroid(st_transform(st_make_valid(st_as_sf(sales)), 26910))
j <- st_intersects(cen, ct); stopifnot(all(lengths(j) <= 1))
sales[, CTUID := ct$CTUID[sapply(j, function(i) if (length(i)) i else NA_integer_)]]
sales[, geom := NULL]

# 2026 use (single family); lot size from the inventory (gpkg LAND_SIZE is blank for ~90% of CoV SFD folios)
desc <- as.data.table(st_read(fGpkg, quiet = TRUE, query =
  "SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'"))
desc[, roll := rollKey(ROLL_NUMBER)]; desc <- unique(desc, by = "roll")
n0 <- nrow(sales); h <- merge(sales, desc[ACTUAL_USE_DESCRIPTION %in% USES, .(roll)], by = "roll")
lost(n0, nrow(h), paste("2026 use", TYPE))

# finished area, effective year, lot area (2026 inventory; Land_Area_Total is acres: x 43560 matches 2018 width x depth)
inv <- fread(fInv, select = c("Jurisdiction", "Roll_Number", "MB_Effective_Year", "MB_Total_Finished_Area", "Land_Area_Total"), colClasses = "character")
inv <- inv[numify(Jurisdiction) == 200][, .(roll = rollKey(Roll_Number), eff = numify(MB_Effective_Year), sqft = numify(MB_Total_Finished_Area),
                                           lot = 43560 * numify(Land_Area_Total))]
inv <- unique(inv, by = "roll")
n0 <- nrow(h); h <- merge(h, inv, by = "roll"); lost(n0, nrow(h), "in 2026 inventory")
n0 <- nrow(h); h <- h[(CONDO | (is.finite(lot) & lot > 0)) & is.finite(sqft) & sqft > 0 & is.finite(eff)]; lost(n0, nrow(h), "lot (sfd), sqft, effective year")
n0 <- nrow(h); h <- h[eff <= year]; lost(n0, nrow(h), "effective year <= sale year")
if (CONDO) { n0 <- nrow(h); h <- h[sqft < MAXSQFT]; lost(n0, nrow(h), sprintf("sqft < %d", MAXSQFT))
} else { n0 <- nrow(h); h <- h[year - eff < MAXAGE]; lost(n0, nrow(h), sprintf("age < %d", MAXAGE)) }

pT <- fread(fPpsfT, colClasses = list(character = "id"))[, .(CTUID = id, ppsf = withEff)]
n0 <- nrow(h); h <- merge(h, pT, by = "CTUID"); lost(n0, nrow(h), "tract has ppsf33")
h[, `:=`(lnP = log(CONVEYANCE_PRICE), lnLot = log(lot), lnSqft = log(sqft), effBin = 5 * floor(eff / 5))]
print(h[, .(n = .N, medPrice = median(CONVEYANCE_PRICE), medAge = median(year - eff)), keyby = year])
cat(sprintf("tracts: %d\n", uniqueN(h$CTUID)))

fml <- if (CONDO) lnP ~ i(year, ppsf, ref = 2022) + lnSqft | CTUID + year + effBin else lnP ~ i(year, ppsf, ref = 2022) + lnLot + lnSqft | CTUID + year + effBin
m <- feols(fml, h, cluster = ~CTUID)
print(coeftable(m))
w <- wald(m, keep = "year::(2019|2020|2021):ppsf", print = FALSE)
cat(sprintf("joint 2019-2021 test F = %.2f, p = %.3f\n", w$stat, w$p))

es <- data.table(year = as.integer(sub("^year::([0-9]+):ppsf$", "\\1", grep("^year::", names(coef(m)), value = TRUE))))
es[, `:=`(b = coef(m)[sprintf("year::%d:ppsf", year)], se = se(m)[sprintf("year::%d:ppsf", year)])]
es <- rbind(es, data.table(year = 2022L, b = 0, se = 0))[order(year)]
es <- merge(es, h[, .N, keyby = year], by = "year")
es[, mid := as.IDate(sprintf("%d-07-01", year))]
print(es)
p <- ggplot(es, aes(mid, b)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = as.IDate(c("2023-01-01", "2023-10-17")), linetype = 2, colour = "grey40") +
  annotate("text", x = as.IDate(c("2023-01-01", "2023-10-17")), y = Inf, label = c("Jan 2023 ", " R1-1 in force"),
           hjust = c(1, 0), vjust = 1.5, size = 3, colour = "grey30") +
  geom_errorbar(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), width = 60) +
  geom_point(size = 2) +
  geom_text(aes(y = b - 1.96 * se, label = paste0("n = ", N)), vjust = 1.6, size = 2.7, colour = "grey30") +
  scale_x_date(breaks = as.Date(paste0(YEARS, "-07-01")), labels = YEARS, minor_breaks = NULL) +
  labs(x = "Sale year", y = sprintf("Coefficient on tract log ppsf, log %s price (reference: 2022)", if (CONDO) "condo" else "house")) +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(if (CONDO) "../text/eventStudyPriceCondo.png" else "../text/eventStudyPrice.png", p, width = 7.5, height = 4.8, dpi = 300)
