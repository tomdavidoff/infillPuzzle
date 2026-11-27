library(data.table); library(sf); library(DBI)

# Redevelopment of 2018 Burnaby single-family lots by the 2022 and 2026 rolls, by 2018 effective age; break in age where P(redevelop) gets low.
g26 <- list.files("~/bigFiles/latestSpatialBCA", "\\.gpkg$", full.names = TRUE)[1]
g22 <- "~/bigFiles/bca_folios.gpkg"
sf1 <- c("Single Family Dwelling", "Residential Dwelling with Suite")
rebuiltRatio <- 3  # improvement value ratio flagging a rebuilt house
out <- "~/DropboxExternal/dataProcessed/burnabyEffYear.rds"

q  <- \(lyr, cols) sprintf("SELECT %s FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_%s_SV WHERE JURISDICTION_CODE = '301'", cols, lyr)
rd <- \(g, ...) as.data.table(st_drop_geometry(st_read(g, query = q(...), quiet = TRUE)))
val <- \(g) unique(rd(g, "GNRL_PROP_VALUES", "FOLIO_ID AS folio, GEN_PROPERTY_CLASS_CODE AS cls, GEN_GROSS_IMPROVEMENT_VALUE AS impr"),
                   by = c("folio", "cls"))[, .(impr = sum(impr)), folio]
use <- \(g) unique(rd(g, "DESCRIPTIONS", "FOLIO_ID AS folio, ACTUAL_USE_DESCRIPTION AS use"), by = "folio")

con <- dbConnect(RSQLite::SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")
b <- as.data.table(dbGetQuery(con, "
  SELECT f.folioID AS folio, d.actualUseDescription AS use19, i.MB_year_built AS yb, i.MB_effective_year AS ey, v.impr19
  FROM folio f JOIN folioDescription d USING (folioID)
  JOIN residentialInventory i ON i.roll_number = f.rollNumber AND trim(i.jurisdiction) = '301'
  LEFT JOIN (SELECT folioID, sum(improvementValue + 0) AS impr19 FROM valuation GROUP BY folioID) v USING (folioID)
  WHERE trim(f.jurisdictionCode) = '301'"))
dbDisconnect(con)
cat("2019 Burnaby inventory rows", nrow(b), "\n")
b <- b[use19 %in% sf1]; cat("single-family", nrow(b), "\n")
stopifnot(!anyDuplicated(b$folio))
b[, `:=`(yb = as.integer(yb), ey = as.integer(ey))]
n0 <- nrow(b); b <- b[ey %between% c(1880, 2019) & impr19 > 0]; cat("valid eff. year & impr: lost", round(1 - nrow(b) / n0, 4), "\n")

for (y in c("22", "26")) {
  g <- get(paste0("g", y))
  x <- merge(val(g), use(g), by = "folio", all = TRUE)
  setnames(x, c("impr", "use"), paste0(c("impr", "use"), y))
  n0 <- nrow(b); b <- x[b, on = "folio"]; stopifnot(nrow(b) == n0)
}
redev <- \(i, u) fcase(is.na(i) & is.na(u), "gone", !u %in% sf1, "use change", i / b$impr19 > rebuiltRatio, "rebuilt", default = "none")
b[, `:=`(r22 = redev(impr22, use22), r26 = redev(impr26, use26))]
b[r22 != "none" & r26 == "none", r26 := r22]
print(b[, .N, r22][, share := round(N / sum(N), 4)][])
print(b[, .N, r26][, share := round(N / sum(N), 4)][])
print(round(quantile(b$impr26 / b$impr19, c(.01, .1, .25, .5, .75, .9, .95, .97, .99), na.rm = TRUE), 2))
print(b[, .(impr19 = as.numeric(median(impr19)), ratio26 = median(impr26 / impr19, na.rm = TRUE)), .(rebuilt = r26 == "rebuilt")])

b[, `:=`(age = 2018L - ey, red = r26 != "none")]
cat("\nP(redeveloped 2018->2025) by 2018 effective age\n")
print(b[, .(n = .N, p = round(mean(red), 4)), keyby = .(age = pmin(5 * (age %/% 5), 80))])

# break: p = a + b * max(age - k, 0), k by least squares; bootstrap k
print(b[age <= 6, .(n = .N, p = round(mean(red), 4)), keyby = age])
cat("dropping effective age < 2 from the break fit (houses under construction in 2018 look 'rebuilt'):", b[age < 2, .N], "\n")
f <- b[age >= 2]
fitk <- \(d, ks = 2:60) { ssr <- sapply(ks, \(k) sum(lm.fit(cbind(1, pmax(d$age - k, 0)), d$red)$residuals^2)); ks[which.min(ssr)] }
kHat <- fitk(f)
set.seed(1); kBoot <- replicate(200, fitk(f[sample(.N, replace = TRUE)]))
cat("\nkink age", kHat, " bootstrap 90% CI", quantile(kBoot, c(.05, .95)), "\n")
print(f[, .(n = .N, p = round(mean(red), 4)), .(young = age < kHat)])
print(f[, .(n = .N, p = round(mean(red), 4)), .(under20 = age < 20)])
print(coef(lm(red ~ pmax(age - kHat, 0), f)))

saveRDS(list(lots = b[, .(folio, yb, ey, age, r22, r26)], kink = kHat, kBoot = kBoot), out)
