# permitShareDiD.R
# Eq. ShareDiD: duplex share of single+duplex new-build permits on 33-ft 2018 SFD lots now R1-1, by area (tract or
# BCA neighbourhood) x year, on post-2023 x local log ppsf (ppsf33), area FE
# and period FE. Weighted by permits per cell, so the point estimates equal a permit-level LPM with the same FE.
# SEs clustered by area; wild cluster restricted bootstrap p-values (Webb weights) because there are few clusters.
# East vs West 2x2 (era x side) difference in shares and ratio of odds ratios, no controls.
# Sample as vancouverPermitMap.R: links from multiplexLotWidth.R, latest permit per lot x era.
# Periods (11/22/26): calendar years of application. Pre 2019-2022 (per -4 ... -1, reference 2022), post 2024-2026
# (per 0, 1, 2; 2026 partial). Excluded, after keeping the latest permit per lot x era: Sept 18 - Dec 31 2018 (partial
# year) and all of 2023 (announcement period Jan-Sep and post-announcement Oct-Dec: anticipation from the staff recommendation, the pre-R1-1 filing rush, and the
# first weeks under R1-1; permitMonthly2023.R). Column 3/6 ("adjacent years") compares 2022 with 2024 only; it replaces the +-6 month window,
# which fell almost entirely inside the dropped period. The period-length comparison is removed (fixed years); its macros
# are kept as \fact{} flags so main.tex still compiles, and text/shareDiDPeriods.tex is a placeholder.
# Saves the permit sample (pm) and the undropped sample (pmAll, per = NA in the dropped window) for downstream scripts.
# Writes text/shareDiD.tex (macros), text/shareDiDTable.tex, text/shareDiDPeriods.tex (placeholder), text/permitPhaseCounts.tex
# Tom Davidoff 11/16/26, periods 11/22/26

library(data.table)
library(RSQLite)
library(sf)
library(fixest)
set.seed(20261116)

SFX      <- if (Sys.getenv("ZONESET") == "RT") "RT" else ""   # ZONESET=RT: lots now in RT zones; outputs suffixed, no macros
D_REFORM <- as.IDate("2018-09-18")   # council approves duplexes in RS
R1_START <- as.IDate("2023-10-17")   # R1-1 in force
YEARS_PRE  <- 2019:2022               # pre-period calendar years (2022 = reference)
YEARS_POST <- 2024:2026               # post-period calendar years (2026 partial)
ONT      <- -123.101                 # Ontario St
NBOOT    <- 9999
BOOT     <- TRUE
CUTOFFS  <- readRDS("~/DropboxExternal/dataProcessed/permitNewBuildCutoffs.rds")  # from multiplexLotWidth.R
fLinks   <- paste0("~/DropboxExternal/dataProcessed/permitLotLinks", SFX, ".rds")
fBCA19   <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
fTract   <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
fPpsfT   <- "~/DropboxExternal/dataProcessed/ppsf33Tract.csv"
fPpsfN   <- "~/DropboxExternal/dataProcessed/ppsf33Neighbourhood.csv"
OUT      <- "../text/"

lost <- function(n0, n1, what) cat(sprintf("%-45s %6d -> %6d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

# ---- wild cluster restricted bootstrap (H0: coefficient on `var` = 0), Webb six-point weights ----
webb <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
# FE partialled out by weighted demeaning (FWL); draws vectorized. The t-stats omit the CR1 constant, which cancels.
wcrBoot <- function(xs, fe, var, d, cl = ~area, B = NBOOT, boot = BOOT) {
  m  <- feols(as.formula(paste("y ~", paste(xs, collapse = " + "), "|", fe)), d, weights = ~w, cluster = cl)
  if (!boot) return(list(m = m, p = NA_real_))
  dm <- demean(as.matrix(d[, c("y", xs), with = FALSE]), d[, all.vars(as.formula(paste("~", fe))), with = FALSE], weights = d$w)
  sw <- sqrt(d$w); y <- dm[, 1] * sw; X <- dm[, xs, drop = FALSE] * sw
  g  <- as.integer(factor(d[[all.vars(cl)]])); k <- which(xs == var)
  tst <- function(Y) {
    XtXi <- solve(crossprod(X)); bet <- XtXi %*% crossprod(X, Y); E <- Y - X %*% bet
    S <- lapply(seq_len(max(g)), function(h) crossprod(X[g == h, , drop = FALSE], E[g == h, , drop = FALSE]))
    v <- Reduce(`+`, lapply(S, function(Sg) (XtXi[k, ] %*% Sg)^2))
    list(b = bet[k, ], t = bet[k, ] / sqrt(c(v)))
  }
  t0 <- tst(matrix(y))
  stopifnot(abs(t0$b - coef(m)[var]) < 1e-8)
  Xr <- X[, -k, drop = FALSE]
  u  <- if (ncol(Xr)) y - Xr %*% solve(crossprod(Xr), crossprod(Xr, y)) else y
  V  <- matrix(sample(webb, max(g) * B, replace = TRUE), max(g), B)
  tb <- tst(c(y - u) + V[g, , drop = FALSE] * c(u))$t
  list(m = m, p = mean(abs(tb) >= abs(t0$t)))
}

# ---- validate the bootstrap on simulated cell panels with known beta ----
simPanel <- function(beta, G = 20, Tn = 8) {
  a <- data.table(area = 1:G, x = rnorm(G, 0, .25), ae = rnorm(G, 0, .1))
  d <- CJ(area = 1:G, per = 1:Tn)[a, on = "area"]
  d[, `:=`(post = as.numeric(per > 5), w = rpois(.N, 4) + 1)]
  d[, y := .3 + ae + .03 * per + beta * post * x + rnorm(.N, 0, .45 / sqrt(w)) + rnorm(G, 0, .05)[area] * per / Tn]
  d[, postX := post * x][]
}
if (BOOT && SFX == "") {
sim <- rbindlist(lapply(1:2000, function(i) {
  b <- if (i <= 1500) 0 else 0.5
  r <- wcrBoot("postX", "area + per", "postX", simPanel(b), B = 999)
  data.table(beta = b, est = coef(r$m)["postX"], pBoot = r$p, pCl = pvalue(r$m)["postX"])
}))
print(sim[, .(reps = .N, meanEst = mean(est), rejBoot05 = mean(pBoot < .05), rejCluster05 = mean(pCl < .05)), by = beta])
stopifnot(sim[beta == 0, abs(mean(est)) < .05 & mean(pBoot < .05) < .1], sim[beta == .5, abs(mean(est) - .5) < .05])
}

# ---- permits: new-build single/duplex on 33-ft lots, latest per lot x era ----
lk0 <- readRDS(fLinks)
stopifnot(!anyDuplicated(lk0$permitnumber), all(lk0$newBuild))
buildSample <- function(cut, drop = TRUE) {
  lk <- lk0[(typeofwork == "New Building" | projectvalue >= cut) & round(landWidth) == 33 & product %in% c("single", "duplex", "multiplex")]
  lk <- lk[applied >= D_REFORM]
  lk[, era := fifelse(applied < R1_START, "duplexEra", "r1Era")]
  lk <- lk[order(rollStart, era, -issued)][, .SD[1], by = .(rollStart, era)]
  if (drop) lk <- lk[year(applied) %in% c(YEARS_PRE, YEARS_POST)]
  lk[product != "multiplex"]
}
pm <- buildSample(CUTOFFS["A"])
cat(sprintf("33-ft single+duplex new builds, latest per lot x era: %d (duplex era %d, R1-1 era %d)\n",
            nrow(pm), pm[era == "duplexEra", .N], pm[era == "r1Era", .N]))

# area keys for every linked lot: tract by lot centroid, BCA 2019 neighbourhood by rollStart
lots <- unique(lk0[, .(rollStart, lotLon, lotLat)])
stopifnot(!anyDuplicated(lots$rollStart))
ct <- st_transform(st_make_valid(st_read(fTract, quiet = TRUE)[, "CTUID"]), 26910)
j  <- st_intersects(st_transform(st_as_sf(lots, coords = c("lotLon", "lotLat"), crs = 4326), 26910), ct)
stopifnot(all(lengths(j) <= 1))
lots[, CTUID := ct$CTUID[sapply(j, function(i) if (length(i)) i else NA_integer_)]]
con <- dbConnect(SQLite(), path.expand(fBCA19))
nb <- as.data.table(dbGetQuery(con, "
  SELECT f.rollNumber, d.neighbourhoodDescription AS nbhd FROM folio f JOIN folioDescription d USING(folioID)
  WHERE trim(f.jurisdictionCode) = '200' AND d.neighbourhoodDescription != ''"))
dbDisconnect(con)
nb[, rollStart := floor(as.numeric(rollNumber) / 1000)]
nb <- nb[rollStart %in% lots$rollStart, .N, by = .(rollStart, nbhd)][order(rollStart, -N)]
cat("lots with >1 BCA neighbourhood:", nb[, uniqueN(nbhd), by = rollStart][V1 > 1, .N], "\n")
nb <- nb[, .SD[1], by = rollStart]
n0 <- nrow(lots); lots <- merge(lots, nb[, .(rollStart, nbhd)], by = "rollStart", all.x = TRUE); stopifnot(nrow(lots) == n0)
cat("lots without tract:", lots[is.na(CTUID), .N], " without neighbourhood:", lots[is.na(nbhd), .N], "\n")

pT <- fread(fPpsfT, colClasses = list(character = "id"))[, .(CTUID = id, ppsf = withEff)]
pN <- fread(fPpsfN)[, .(nbhd = id, ppsf = withEff)]
stopifnot(!anyDuplicated(pT$CTUID), !anyDuplicated(pN$nbhd), all(pT$CTUID %in% ct$CTUID))

# calendar years: pre 2019-2022 -> per -4..-1, post 2024-2026 -> per 0..2; 2018 and 2023 -> NA
perOf <- function(applied) { y <- year(applied)
  fcase(y %in% YEARS_PRE, y - 2023L, y %in% YEARS_POST, y - 2024L, default = NA_integer_) }
addAreas <- function(d) {
  n0 <- nrow(d); d <- merge(d, lots[, .(rollStart, CTUID, nbhd)], by = "rollStart"); stopifnot(nrow(d) == n0)
  d[, `:=`(per = perOf(applied), dup = as.numeric(product == "duplex"), side = fifelse(lotLon < ONT, "West", "East"))]
  d[, post := as.numeric(applied >= R1_START)][]
}
pm <- addAreas(pm)
stopifnot(!anyNA(pm$per))
pmAll <- addAreas(buildSample(CUTOFFS["A"], drop = FALSE))
cat("excluded from regressions (pmAll, per = NA):\n"); print(pmAll[is.na(per), .N, keyby = .(yr = year(applied))])
print(pm[, .(N = .N, dupShare = round(mean(dup), 3), from = min(applied), to = max(applied)), keyby = per])

# cell panel: area x period (or 2022 vs 2024 only). Trend in years: post periods sit one extra year later
# because of the dropped year (per + post).
cells <- function(d, area, pp, narrow = FALSE) {
  if (narrow) d <- d[per %in% c(-1, 0)]
  n0 <- nrow(d); d <- merge(d[!is.na(get(area))], pp, by = area)
  lost(n0, nrow(d), sprintf("%s: area has ppsf33 estimate%s", area, if (narrow) " (adjacent years)" else ""))
  setnames(d, area, "area")
  d[, .(y = mean(dup), w = .N, ppsf = ppsf[1]), by = .(area, per, post)][
    , `:=`(postX = post * ppsf, trendX = (per + post) * ppsf)][]
}
specs <- list(base = "postX", trend = c("postX", "trendX"), window = "postX")
runLevel <- function(d, area, pp) {
  lapply(names(specs), function(s) {
    dc <- cells(d, area, pp, narrow = s == "window")
    r  <- wcrBoot(specs[[s]], "area + per", "postX", dc)
    list(spec = s, m = r$m, p = r$p, nArea = r$m$fixef_sizes[["area"]], nPer = uniqueN(dc$per), nCell = nobs(r$m),
         nPermit = sum(dc$w[obs(r$m)]), ybar = dc[obs(r$m), weighted.mean(y, w)], sdPpsf = dc[, sd(ppsf[!duplicated(area)])])
  })
}
res <- c(runLevel(pm, "CTUID", pT), runLevel(pm, "nbhd", pN))
for (r in res) { cat("\n====", r$spec, "| areas", r$nArea, "| cells", r$nCell, "| permits", r$nPermit, "| WCR p", r$p, "\n"); print(coeftable(r$m)) }

# cell-size facts for the text: tract x month and tract x year cells
cellFacts <- function(d) d[, .(w = .N), by = .(CTUID, per)][, .(med = median(w), share5 = mean(w >= 5))]
cfY <- cellFacts(merge(pm, pT, by = "CTUID"))
cfM <- cellFacts(merge(pm, pT, by = "CTUID")[, per := format(applied, "%Y-%m")])
print(rbind(cbind(cells = "tract x month", cfM), cbind(cells = "tract x year", cfY)))

# unweighted cells, for the record
for (a in list(list("CTUID", pT), list("nbhd", pN)))
  print(coeftable(feols(y ~ postX | area + per, cells(pm, a[[1]], a[[2]]), cluster = ~area))["postX", ])

# West - East ppsf gap at each level, to scale the coefficient
ewGap <- function(area, pp) { d <- merge(unique(pm[!is.na(get(area)), .(a = get(area), side)], by = "a"), setnames(copy(pp), area, "a"), by = "a")
  d[, mean(ppsf[side == "West"]) - mean(ppsf[side == "East"])] }
gapT <- ewGap("CTUID", pT); gapN <- ewGap("nbhd", pN)
cat("West-East mean ppsf gap: tract", round(gapT, 3), " nbhd", round(gapN, 3), "\n")

# new-build cutoff sensitivity: base spec under cutoffs B and C
sens <- rbindlist(lapply(names(CUTOFFS), function(k) {
  d <- addAreas(buildSample(CUTOFFS[k]))
  rbindlist(lapply(list(list("tract", "CTUID", pT), list("nbhd", "nbhd", pN)), function(a) {
    m <- feols(y ~ postX | area + per, cells(d, a[[2]], a[[3]]), weights = ~w, cluster = ~area)
    data.table(cutoff = k, level = a[[1]], N = nrow(d), b = coef(m)["postX"], se = se(m)["postX"])
  }))
}))
print(sens)

# ---- East vs West, era x side, no controls ----
ew <- pm[, .(n = .N, d = sum(dup)), keyby = .(side, post)][, p := d / n]
print(ew)
g <- function(s, q, v) ew[side == s & post == q][[v]]
did   <- (g("West", 1, "p") - g("West", 0, "p")) - (g("East", 1, "p") - g("East", 0, "p"))
didSE <- sqrt(ew[, sum(p * (1 - p) / n)])
lor   <- function(s) log(g(s, 1, "d") / (g(s, 1, "n") - g(s, 1, "d"))) - log(g(s, 0, "d") / (g(s, 0, "n") - g(s, 0, "d")))
lorSE <- function(s) sqrt(sum(1 / c(g(s, 1, "d"), g(s, 1, "n") - g(s, 1, "d"), g(s, 0, "d"), g(s, 0, "n") - g(s, 0, "d"))))
rorL  <- lor("West") - lor("East"); rorSE <- sqrt(lorSE("West")^2 + lorSE("East")^2)
chiW  <- (rorL / rorSE)^2
mEW   <- feols(dup ~ i(side, post, "East") | side + post, pm, vcov = "hetero")
stopifnot(abs(coef(mEW)[1] - did) < 1e-8)
lgEW  <- feglm(dup ~ i(side, post, "East") | side + post, pm, family = binomial)
cat(sprintf("EW DiD %.3f (SE %.3f, z %.2f, p %.4f) | ratio of odds ratios %.2f, Woolf chi2(1) %.2f, p %.4f | logit coef %.3f\n",
            did, didSE, did / didSE, 2 * pnorm(-abs(did / didSE)), exp(rorL), chiW, pchisq(chiW, 1, lower.tail = FALSE), coef(lgEW)[1]))
PREYEAR <- "2022"   # reference year (per -1)
ewY <- pm[, .(n = .N, p = mean(dup)), keyby = .(side, per)]; print(ewY)
ewS <- rbindlist(lapply(names(CUTOFFS), function(k) {
  e <- addAreas(buildSample(CUTOFFS[k]))[, .(p = mean(dup), n = .N), keyby = .(side, post)]
  data.table(cutoff = k, did = e[, (p[4] - p[3]) - (p[2] - p[1])], se = e[, sqrt(sum(p * (1 - p) / n))])
}))
print(ewS)

# ---- counts by year x side x product (pmAll), 2019-2026; 2023 split into announcement period (Jan-Sep) and
# post-announcement (Oct-Dec). Rules separate pre years, 2023, post years. ----
pmAll[, phase := fcase(year(applied) == 2023 & month(applied) <= 9, "2023.1 Announcement period 2023 (Jan--Sep)",
                       year(applied) == 2023, "2023.2 Post-announcement 2023 (Oct--Dec)",
                       year(applied) == 2026, "2026 2026 (Jan--Sep)", default = paste(year(applied), year(applied)))]
ph <- pmAll[year(applied) >= 2019, .(n = .N), keyby = .(phase, side, product)]
pmAll[, phase := NULL]
ph <- dcast(ph, phase ~ side + product, value.var = "n", fill = 0); print(ph)
cat("2018 (Sept 18 - Dec 31), excluded:\n"); print(pmAll[year(applied) == 2018, .N, keyby = .(side, product)])
shr <- function(du, si) sprintf("%.0f", 100 * du / (du + si))
tabC <- c("% generated by scripts/permitShareDiD.R; do not edit",
  "{\\footnotesize", "\\begin{tabular}{lrrrrrr}", "\\hline",
  " & \\multicolumn{3}{c}{West Side} & \\multicolumn{3}{c}{East Side} \\\\",
  "Application period & Single & Duplex & Duplex \\% & Single & Duplex & Duplex \\% \\\\", "\\hline",
  {r <- ph[, sprintf("%s & %d & %d & %s & %d & %d & %s \\\\", sub("^[0-9.]+ ", "", phase), West_single, West_duplex, shr(West_duplex, West_single),
               East_single, East_duplex, shr(East_duplex, East_single))]
   k <- substr(ph$phase, 1, 4); c(r[k < "2023"], "\\hline", r[k == "2023"], "\\hline", r[k > "2023"])},
  "\\hline", "\\end{tabular}", "}")
writeLines(tabC, paste0(OUT, "permitPhaseCounts", SFX, ".tex")); cat(tabC, sep = "\n")

saveRDS(list(pm = pm, pmAll = pmAll, pT = pT, pN = pN, gapT = gapT, gapN = gapN), paste0("~/DropboxExternal/dataProcessed/shareDiDPermits", SFX, ".rds"))

# ---- outputs ----
f2 <- function(x) sprintf("%.2f", x); f3 <- function(x) sprintf("%.3f", x)
fmtInt <- function(x) formatC(x, format = "d", big.mark = ",")
pv <- function(p) if (is.na(p)) "--" else if (p < .001) "$<$0.001" else sprintf("%.3f", p)
pt <- function(p) if (is.na(p)) "\\fact{bootstrap off}" else if (p < .001) "$p<0.001$" else sprintf("$p=%.3f$", p)
stars <- function(p) if (is.na(p)) "" else if (p < .01) "^{***}" else if (p < .05) "^{**}" else if (p < .1) "^{*}" else ""
cellB <- function(r, v) if (v %in% names(coef(r$m))) sprintf("$%s%s$", f3(coef(r$m)[v]), stars(pvalue(r$m)[v])) else ""
cellS <- function(r, v) if (v %in% names(coef(r$m))) sprintf("(%s)", f3(se(r$m)[v])) else ""
row <- function(lab, x) paste(lab, "&", paste(x, collapse = " & "), "\\\\")
tab <- c("% generated by scripts/permitShareDiD.R; do not edit",
  "\\begin{tabular}{lcccccc}", "\\hline",
  " & \\multicolumn{3}{c}{Census tract} & \\multicolumn{3}{c}{Neighbourhood} \\\\",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\", "\\hline",
  row("Post 2023 $\\times$ log ppsf", sapply(res, cellB, "postX")),
  row("", sapply(res, cellS, "postX")),
  if (BOOT) row("\\quad wild bootstrap $p$", sapply(res, function(r) pv(r$p))),
  row("Period $\\times$ log ppsf", sapply(res, cellB, "trendX")),
  row("", sapply(res, cellS, "trendX")),
  "\\hline",
  row("Years", sapply(res, function(r) if (r$spec == "window") "2022, 2024" else "all")),
  row("Areas", sapply(res, function(r) r$nArea)),
  row("Periods", sapply(res, function(r) r$nPer)),
  row("Area $\\times$ period cells", sapply(res, function(r) r$nCell)),
  row("Permits", sapply(res, function(r) fmtInt(r$nPermit))),
  row("Mean duplex share", sapply(res, function(r) f2(r$ybar))),
  "\\hline", "\\end{tabular}")
writeLines(tab, paste0(OUT, "shareDiDTable", SFX, ".tex")); cat(tab, sep = "\n")

if (SFX != "") quit(save = "no")
tabP <- c("% generated by scripts/permitShareDiD.R; do not edit",
  "% period-length comparison removed 11/22/26: calendar years, 2018 and 2023 excluded",
  "\\begin{tabular}{l}", "\\fact{Period-length table removed: periods are now fixed years; see permitShareDiD.R}", "\\end{tabular}")
writeLines(tabP, paste0(OUT, "shareDiDPeriods.tex"))

b <- function(i) coef(res[[i]]$m)["postX"]; s <- function(i) se(res[[i]]$m)["postX"]
mac <- c("% generated by scripts/permitShareDiD.R; do not edit",
  sprintf("\\newcommand{\\sdTractB}{%s}", f3(b(1))), sprintf("\\newcommand{\\sdTractSE}{%s}", f3(s(1))),
  sprintf("\\newcommand{\\sdTractP}{%s}", pt(res[[1]]$p)),
  sprintf("\\newcommand{\\sdNbhdB}{%s}", f3(b(4))), sprintf("\\newcommand{\\sdNbhdSE}{%s}", f3(s(4))),
  sprintf("\\newcommand{\\sdNbhdP}{%s}", pt(res[[4]]$p)),
  sprintf("\\newcommand{\\sdTractTrendB}{%s}", f3(b(2))), sprintf("\\newcommand{\\sdTractTrendP}{%s}", pt(res[[2]]$p)),
  sprintf("\\newcommand{\\sdNbhdTrendB}{%s}", f3(b(5))), sprintf("\\newcommand{\\sdNbhdTrendP}{%s}", pt(res[[5]]$p)),
  sprintf("\\newcommand{\\sdTractWinB}{%s}", f3(b(3))), sprintf("\\newcommand{\\sdTractWinP}{%s}", pt(res[[3]]$p)),
  sprintf("\\newcommand{\\sdNbhdWinB}{%s}", f3(b(6))), sprintf("\\newcommand{\\sdNbhdWinP}{%s}", pt(res[[6]]$p)),
  sprintf("\\newcommand{\\sdWinPermits}{%s}", fmtInt(res[[3]]$nPermit)),
  sprintf("\\newcommand{\\sdNTract}{%d}", res[[1]]$nArea), sprintf("\\newcommand{\\sdNNbhd}{%d}", res[[4]]$nArea),
  sprintf("\\newcommand{\\sdNPermits}{%s}", fmtInt(res[[1]]$nPermit)),
  sprintf("\\newcommand{\\sdGapTract}{%s}", f2(gapT)), sprintf("\\newcommand{\\sdGapNbhd}{%s}", f2(gapN)),
  sprintf("\\newcommand{\\sdEffectGapTract}{%.0f}", 100 * b(1) * gapT),
  sprintf("\\newcommand{\\sdEffectGapNbhd}{%.0f}", 100 * b(4) * gapN),
  sprintf("\\newcommand{\\ewDiD}{%.0f}", 100 * did), sprintf("\\newcommand{\\ewDiDSE}{%.1f}", 100 * didSE),
  sprintf("\\newcommand{\\ewDiDP}{%s}", pt(2 * pnorm(-abs(did / didSE)))),
  sprintf("\\newcommand{\\ewOddsRatio}{%.2f}", exp(rorL)), sprintf("\\newcommand{\\ewChiSq}{%.2f}", chiW),
  sprintf("\\newcommand{\\ewChiSqP}{%s}", pt(pchisq(chiW, 1, lower.tail = FALSE))),
  sprintf("\\newcommand{\\ewPreYear}{%s}", PREYEAR),
  sprintf("\\newcommand{\\ewDupEastPreYear}{%.0f}", 100 * ewY[side == "East" & per == -1, p]), sprintf("\\newcommand{\\ewDupWestPreYear}{%.0f}", 100 * ewY[side == "West" & per == -1, p]),
  sprintf("\\newcommand{\\ewDupEastPost}{%.0f}", 100 * g("East", 1, "p")), sprintf("\\newcommand{\\ewDupWestPost}{%.0f}", 100 * g("West", 1, "p")),
  sprintf("\\newcommand{\\ewN}{%s}", fmtInt(nrow(pm))), sprintf("\\newcommand{\\sdNBoot}{%s}", if (BOOT) fmtInt(NBOOT) else "\\fact{bootstrap off}"),
  sprintf("\\newcommand{\\sdMonthMedPermits}{%g}", cfM$med),
  sprintf("\\newcommand{\\sdMonthShareFive}{%.0f}", 100 * cfM$share5),
  sprintf("\\newcommand{\\sdYearMedPermits}{%g}", cfY$med),
  "\\newcommand{\\sdSeRangeTract}{\\fact{period-length comparison removed}}",
  "\\newcommand{\\sdSeRangeNbhd}{\\fact{period-length comparison removed}}",
  "\\newcommand{\\sdBRangeTract}{\\fact{period-length comparison removed}}",
  "\\newcommand{\\sdBRangeNbhd}{\\fact{period-length comparison removed}}",
  "\\newcommand{\\sdUnwRangeTract}{\\fact{period-length comparison removed}}")
writeLines(mac, paste0(OUT, "shareDiD.tex")); cat(mac, sep = "\n")
