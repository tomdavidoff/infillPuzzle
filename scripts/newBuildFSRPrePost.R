# newBuildFSRPrePost.R
# Realized FSR (2026 BCA finished floor area / 2018 lot area) of new single family homes and duplexes, duplex era
# (2018-09-18 to 2023-10-16) vs R1-1 era, on the permitShareDiD.R sample (33-ft 2018 SFD lots now R1-1, latest permit per
# lot x era). Floor area: sum of MB_Total_Finished_Area over the 2026 folios on the lot's rollStart; single-folio duplexes
# with a duplex use record one unit, so x2 (rule and validation from vancouverR1FSRDate.R). A permit counts only if it
# is the lot's latest principal permit (any era) and the 2026 building is complete: max year built >= max(2019, issue year).
# Writes text/sqftPrePost.tex (macros); FSR summary statistics are in text/sumStats.tex (sumStats.R)
# Tom Davidoff 11/20/26

library(data.table)
library(RSQLite)

OUT     <- "../text/"
fGpkg26 <- list.files("~/bigFiles/latestSpatialBCA/", pattern = "\\.gpkg$", full.names = TRUE); stopifnot(length(fGpkg26) == 1)
fInv26  <- "~/DropboxExternal/dataRaw/Residential_inventory_202601/20260101_A09_Residential_Inventory_Extract.txt"
lost <- function(n0, n1, what) cat(sprintf("%-55s %6d -> %6d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))
rollKey <- function(x) as.numeric(gsub("[^0-9]", "", x))

S  <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")
pm <- S$pm[, .(rollStart, era, product, permitnumber, applied, issued, side)]
stopifnot(!anyDuplicated(pm[, .(rollStart, era)]), pm$product %in% c("single", "duplex"))

# latest principal permit on the lot across eras and products (incl. multiplex): the 2026 building belongs to it
lk <- readRDS("~/DropboxExternal/dataProcessed/permitLotLinks.rds")
lastP <- lk[newBuild == TRUE & !is.na(product)][order(rollStart, -as.numeric(applied))][, .SD[1], by = rollStart][, .(rollStart, lastPermit = permitnumber)]
n0 <- nrow(pm); pm <- merge(pm, lastP, by = "rollStart", all.x = TRUE); stopifnot(nrow(pm) == n0)

# 2018 lot area
dtS <- unique(readRDS("~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds")[, .(rollNumber, landWidth, landDepth)])
stopifnot(!anyDuplicated(dtS$rollNumber))
dtS[, rollStart := floor(as.numeric(rollNumber) / 1000)]
dtS <- dtS[landWidth > 0 & landDepth > 0][, if (.N == 1) .SD, by = rollStart][, .(rollStart, lotArea = landWidth * landDepth)]
n0 <- nrow(pm); pm <- merge(pm, dtS, by = "rollStart", all.x = TRUE); stopifnot(nrow(pm) == n0)

# 2026 folios on each rollStart: use (gpkg) + finished area, year built (inventory)
con <- dbConnect(SQLite(), fGpkg26)
d26 <- as.data.table(dbGetQuery(con, "SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV WHERE JURISDICTION_CODE = '200'"))
dbDisconnect(con)
d26[, roll := rollKey(ROLL_NUMBER)]; d26 <- unique(d26, by = "roll"); d26[, rollStart := floor(roll / 1000)]
d26 <- d26[rollStart %in% pm$rollStart]
inv <- fread(fInv26, select = c("Jurisdiction", "Roll_Number", "MB_Year_Built", "MB_Total_Finished_Area"))
inv <- inv[as.numeric(Jurisdiction) == 200][, roll := rollKey(Roll_Number)]
stopifnot(!anyDuplicated(inv$roll))
n0 <- nrow(d26); d26 <- merge(d26, inv[, .(roll, yb = as.numeric(MB_Year_Built), area = as.numeric(MB_Total_Finished_Area))], by = "roll", all.x = TRUE)
stopifnot(nrow(d26) == n0)
r26 <- d26[, .(nFolio = .N, nArea = sum(!is.na(area)), area26 = sum(area, na.rm = TRUE),
               yb26 = suppressWarnings(max(yb, na.rm = TRUE)), use26 = paste(sort(unique(ACTUAL_USE_DESCRIPTION)), collapse = "; ")), by = rollStart]
n0 <- nrow(pm); pm <- merge(pm, r26, by = "rollStart", all.x = TRUE); stopifnot(nrow(pm) == n0)

halfUse <- c("Duplex, Non-Strata Side by Side or Front / Back", "Duplex, Strata Front / Back", "Duplex, Strata Side by Side")
pm[, scale := fifelse(product == "duplex" & nFolio == 1 & use26 %in% halfUse, 2, 1)]
pm[, fsr := area26 * scale / lotArea]

# sample flow
cat("permits:", nrow(pm), "\n"); n <- nrow(pm)
f <- list(latest = quote(permitnumber == lastPermit), lot = quote(!is.na(lotArea)), folio = quote(!is.na(nFolio) & nArea == nFolio),
          built = quote(is.finite(yb26) & yb26 >= pmax(2019, year(issued))),
          units = quote(!(product == "duplex" & nFolio == 1 & scale == 1)))
keep <- rep(TRUE, n)
for (k in names(f)) { k0 <- sum(keep); keep <- keep & pm[, eval(f[[k]])] %in% TRUE; lost(k0, sum(keep), k) }
pm[, ok := keep]
print(pm[, .(permits = .N, inFSR = sum(ok), coverage = round(mean(ok), 2)), keyby = .(era, product)])
print(pm[, .(permits = .N, coverage = round(mean(ok), 2)), keyby = .(era, issueYear = year(issued))])
cat("2026 use of FSR-sample lots, by product:\n"); print(pm[ok == TRUE, .N, keyby = .(product, use26)][N >= 5])
cat("x2 check, duplex-era duplexes, median FSR: multi-folio", pm[ok & product == "duplex" & nFolio > 1 & era == "duplexEra", round(median(fsr), 3)],
    "| single-folio x2", pm[ok & product == "duplex" & scale == 2 & era == "duplexEra", round(median(fsr), 3)], "\n")

d <- pm[ok == TRUE]
saveRDS(d, "~/DropboxExternal/dataProcessed/newBuildFSR.rds")   # FSR sample, reused by sumStats.R
stopifnot(d[, all(fsr > 0.1 & fsr < 2)])
q <- function(x, p) unname(quantile(x, p))
st <- d[, .(N = .N, mean = mean(fsr), sd = sd(fsr), p10 = q(fsr, .1), p25 = q(fsr, .25), p50 = median(fsr), p75 = q(fsr, .75), p90 = q(fsr, .9),
            sqft = mean(area26 * scale), lot = mean(lotArea)), keyby = .(era, product)]
print(st, digits = 3)
print(d[, .(N = .N, mean = round(mean(fsr), 3), sd = round(sd(fsr), 3)), keyby = .(era, product, side)])
print(d[, .(N = .N, mean = round(mean(fsr), 3), p50 = round(median(fsr), 3)), keyby = .(product, applyYear = year(applied))])
tt <- function(e) t.test(d[era == e & product == "duplex", fsr], d[era == e & product == "single", fsr])
cat(sprintf("duplex - single mean FSR: duplex era %.3f (p %.3g), R1-1 era %.3f (p %.3g)\n",
            -diff(tt("duplexEra")$estimate), tt("duplexEra")$p.value, -diff(tt("r1Era")$estimate), tt("r1Era")$p.value))

# ---- outputs ----
g <- function(e, p, v) st[era == e & product == p][[v]]
f2 <- function(x) sprintf("%.2f", x); fI <- function(x) formatC(x, format = "d", big.mark = ",")

cv <- pm[, .(c = mean(ok)), keyby = .(era, product)]
mac <- c("% generated by scripts/newBuildFSRPrePost.R; do not edit",
  sprintf("\\newcommand{\\fsrSingleDupEra}{%s}", f2(g("duplexEra", "single", "mean"))), sprintf("\\newcommand{\\fsrDuplexDupEra}{%s}", f2(g("duplexEra", "duplex", "mean"))),
  sprintf("\\newcommand{\\fsrSingleRoneEra}{%s}", f2(g("r1Era", "single", "mean"))), sprintf("\\newcommand{\\fsrDuplexRoneEra}{%s}", f2(g("r1Era", "duplex", "mean"))),
  sprintf("\\newcommand{\\fsrNDupEra}{%s}", fI(d[era == "duplexEra", .N])), sprintf("\\newcommand{\\fsrNRoneEra}{%s}", fI(d[era == "r1Era", .N])),
  sprintf("\\newcommand{\\fsrCoverDupEra}{%.0f}", 100 * pm[era == "duplexEra", mean(ok)]), sprintf("\\newcommand{\\fsrCoverRoneEra}{%.0f}", 100 * pm[era == "r1Era", mean(ok)]),
  sprintf("\\newcommand{\\fsrRollYear}{2026}"))
writeLines(mac, paste0(OUT, "sqftPrePost.tex")); cat(mac, sep = "\n")
