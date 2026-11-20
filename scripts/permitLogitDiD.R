# permitLogitDiD.R
# Lot-level analogue of eq. ShareDiD: duplex (vs single) on post-2023 x local log ppsf, 12-month period FE, and either
# area FE (tract or BCA neighbourhood) or the area's log ppsf as a control. Estimators: LPM (wild cluster restricted
# bootstrap p), logit with area dummies (unconditional ML, incidental-parameter bias), conditional logit on area
# (Chamberlain), logit with ppsf control. Monte Carlo on the actual design for small-sample bias of each logit.
# Conley (1999) spatial SEs, Bartlett kernel in lot-to-lot distance, for the LPM and logit (fixest's conley is uniform-kernel
# and not PSD at long cutoffs here); validated by reproducing fixest's cluster SE and by a Monte Carlo on the actual coordinates.
# Permit sample from permitShareDiD.R (latest new-build permit per 33-ft lot x era).
# Writes text/logitDiD.tex (macros), text/logitDiDTable.tex
# Tom Davidoff 11/16/26

library(data.table)
library(fixest)
library(survival)
library(sf)
set.seed(20261117)
setFixest_notes(FALSE)

NBOOT <- 9999
BOOT  <- TRUE
CUTS  <- c(1, 2, 3, 5)   # Conley cutoffs, km
CUT0  <- 2               # cutoff shown in the table
NSIM  <- 1000
OUT   <- "../text/"
webb  <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))

S <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")
lvls <- list(tract = list(area = "CTUID", pp = S$pT, gap = S$gapT), nbhd = list(area = "nbhd", pp = S$pN, gap = S$gapN))

# wild cluster restricted bootstrap for OLS, permit level (unit weights); FE partialled out by demeaning
wcr <- function(d, xs, fe, var, B = NBOOT) {
  dm <- demean(as.matrix(d[, c("dup", xs), with = FALSE]), d[, fe, with = FALSE])
  y <- dm[, 1]; X <- dm[, xs, drop = FALSE]; g <- as.integer(factor(d$area)); k <- which(xs == var)
  XtXi <- solve(crossprod(X)); G <- max(g)
  agg <- function(M) rowsum(M, g)                                  # cluster sums
  tst <- function(Y) {
    bet <- XtXi %*% crossprod(X, Y); E <- Y - X %*% bet
    sc <- Reduce(`+`, lapply(seq_len(ncol(X)), function(c) XtXi[k, c] * agg(X[, c] * E)))  # G x B
    list(b = bet[k, ], t = bet[k, ] / sqrt(colSums(sc^2)))
  }
  t0 <- tst(matrix(y))
  Xr <- X[, -k, drop = FALSE]
  u  <- if (ncol(Xr)) y - Xr %*% solve(crossprod(Xr), crossprod(Xr, y)) else y
  tb <- unlist(lapply(1:9, function(chunk) {
    V <- matrix(sample(webb, G * ceiling(B / 9), replace = TRUE), G)
    tst(c(y - u) + V[g, , drop = FALSE] * c(u))$t
  }))
  list(b = t0$b, p = mean(abs(tb) >= abs(t0$t)))
}

# Conley SE of postX: bread x sum_ij K(d_ij) s_i s_j' x bread, Bartlett K = max(1 - d/k, 0); K = same-area indicator gives CR0
conleySE <- function(m, D, K = NULL, k = NULL) {
  i <- obs(m); s <- as.matrix(m$scores); B <- m$cov.iid / if (m$method == "feols") m$sigma2 else 1
  if (is.null(K)) K <- pmax(1 - D[i, i] / k, 0) else K <- K[i, i]
  sqrt(diag(B %*% crossprod(s, K %*% s) %*% B))[["postX"]]
}
distKm <- function(d) as.matrix(dist(st_coordinates(st_transform(st_as_sf(d, coords = c("lotLon", "lotLat"), crs = 4326), 26910)))) / 1000

# Monte Carlo of the Conley estimator on the actual tract design: LPM rejection rates at 5%, errors iid or exponential spatial field
conleyMC <- function(d, D, R = 400, ranges = c(0, 0.3, 1)) {
  n <- nrow(d); Ks <- lapply(CUTS, function(k) pmax(1 - D / k, 0))
  rbindlist(lapply(ranges, function(rho) {
    L <- if (rho > 0) t(chol(exp(-D / rho) + diag(1e-8, n))) else diag(n)
    r <- rbindlist(lapply(seq_len(R), function(i) {
      d$y <- 0.4 * d$postX + ifelse(d$post == 1, L %*% rnorm(n), L %*% rnorm(n)) * .4 + rnorm(n, sd = .3)
      m <- feols(y ~ postX | area + per, d); z <- coef(m)[["postX"]] - .4
      as.data.table(as.list(c(range = rho, iid = z / fixest::se(m, vcov = "iid")[["postX"]], cluster = z / fixest::se(m, cluster = ~area)[["postX"]],
        setNames(sapply(Ks, function(K) z / conleySE(m, K = K)), paste0("conley", CUTS)))))
    }))
    r[, lapply(.SD, function(t) mean(abs(t) > qnorm(.975))), by = range]
  }))
}

est <- function(d) {
  ols   <- feols(dup ~ postX | area + per, d, cluster = ~area)
  olsC  <- feols(dup ~ ppsf + postX | per, d, cluster = ~area)
  lgt   <- feglm(dup ~ postX | area + per, d, family = binomial, cluster = ~area)
  lgtC  <- feglm(dup ~ ppsf + postX | per, d, family = binomial, cluster = ~area)
  cl    <- clogit(dup ~ postX + factor(per) + strata(area), d, method = "exact")
  list(ols = ols, olsC = olsC, lgt = lgt, lgtC = lgtC, cl = cl)
}
b  <- function(m) if (inherits(m, "clogit")) coef(m)[["postX"]] else coef(m)[["postX"]]
se <- function(m) if (inherits(m, "clogit")) sqrt(vcov(m)["postX", "postX"]) else fixest::se(m)[["postX"]]

res <- lapply(names(lvls), function(l) {
  L <- lvls[[l]]
  n0 <- nrow(S$pm); d <- merge(S$pm[!is.na(get(L$area))], L$pp, by = L$area)
  cat(sprintf("%s: %d of %d permits have an area ppsf\n", l, nrow(d), n0))
  setnames(d, L$area, "area"); d[, postX := post * ppsf]
  m <- est(d); D <- distKm(d)
  G <- outer(d$area, d$area, "==") * 1
  for (k in c("ols", "olsC", "lgt", "lgtC"))
    stopifnot(abs(conleySE(m[[k]], D, K = G) - fixest::se(m[[k]], cluster = ~area, ssc = ssc(adj = FALSE, cluster.adj = FALSE))[["postX"]]) < 1e-8)
  cy <- sapply(c("ols", "olsC", "lgt", "lgtC"), function(k) c(cluster = fixest::se(m[[k]])[["postX"]], setNames(sapply(CUTS, function(cc) conleySE(m[[k]], D, k = cc)), paste0(CUTS, "km"))))
  cat("SE of postX: clustered vs Conley (Bartlett) by cutoff\n"); print(round(cy, 3))
  if (l == "tract") { mc <- conleyMC(d, D); cat("Conley MC: LPM rejection rate at 5% (beta = true), by error-field range (km)\n"); print(mc, digits = 3) } else mc <- NULL
  print(etable(m[c("ols", "olsC", "lgt", "lgtC")], keep = "postX|ppsf")); print(summary(m$cl)$coefficients["postX", ])
  cat("logit: areas dropped (all single or all duplex):", length(m$lgt$fixef_removed$area), "| permits used", nobs(m$lgt), "\n")
  if (BOOT) {
    pF <- wcr(d, "postX", c("area", "per"), "postX"); pC <- wcr(d, c("ppsf", "postX"), "per", "postX")
    stopifnot(abs(pF$b - b(m$ols)) < 1e-8, abs(pC$b - b(m$olsC)) < 1e-8)
  } else pF <- pC <- list(p = NA_real_)

  # Monte Carlo: true logit with the estimated area and period effects and beta = conditional-logit estimate
  bT <- b(m$cl)
  fe <- fixef(m$lgt); dd <- d[!area %in% m$lgt$fixef_removed$area]
  xb <- fe$area[dd$area] + fe$per[as.character(dd$per)] + bT * dd$postX
  sim <- rbindlist(lapply(seq_len(NSIM), function(i) {
    dd$dup <- rbinom(nrow(dd), 1, plogis(xb))
    r <- tryCatch(c(fe = b(feglm(dup ~ postX | area + per, dd, family = binomial)),
                    cl = b(clogit(dup ~ postX + factor(per) + strata(area), dd, method = "exact")),
                    ctl = b(feglm(dup ~ ppsf + postX | per, dd, family = binomial))), error = function(e) NULL)
    if (!is.null(r)) as.data.table(as.list(r))
  }))
  sm <- sim[, lapply(.SD, function(x) c(mean(x), median(x), sd(x)))]
  cat(sprintf("MC (%d reps, true %.3f): mean fe %.3f cl %.3f ctl %.3f | median fe %.3f cl %.3f ctl %.3f | sd fe %.3f cl %.3f ctl %.3f\n",
              nrow(sim), bT, sm$fe[1], sm$cl[1], sm$ctl[1], sm$fe[2], sm$cl[2], sm$ctl[2], sm$fe[3], sm$cl[3], sm$ctl[3]))
  list(level = l, m = m, cy = cy, mc = mc, pF = pF$p, pC = pC$p, gap = L$gap, n = nrow(d), nArea = uniqueN(d$area),
       nAreaLgt = m$lgt$fixef_sizes[["area"]], nLgt = nobs(m$lgt), bT = bT, sim = sm, nSim = nrow(sim))
})
names(res) <- names(lvls)

# ---- outputs ----
f3 <- function(x) sprintf("%.3f", x); f2 <- function(x) sprintf("%.2f", x)
fmtInt <- function(x) formatC(x, format = "d", big.mark = ",")
pv <- function(p) if (is.na(p)) "--" else if (p < .001) "$<$0.001" else sprintf("%.3f", p)
pt <- function(p) if (is.na(p)) "\\fact{bootstrap off}" else if (p < .001) "$p<0.001$" else sprintf("$p=%.3f$", p)
pz <- function(m) 2 * pnorm(-abs(b(m) / se(m)))
stars <- function(p) if (p < .01) "^{***}" else if (p < .05) "^{**}" else if (p < .1) "^{*}" else ""
cB <- function(m) if (is.null(m)) "" else sprintf("$%s%s$", f3(b(m)), stars(pz(m)))
cS <- function(m) if (is.null(m)) "" else sprintf("(%s)", f3(se(m)))
row <- function(lab, x) paste(lab, "&", paste(x, collapse = " & "), "\\\\")
cK <- paste0(CUT0, "km")
cyRow <- function(k) sapply(cols, function(cc) sprintf("[%s]", f3(res[[cc[1]]]$cy[cK, paste0(k, if (cc[2] == "C") "C" else "")])))
cols <- list(c("tract", "F"), c("tract", "C"), c("nbhd", "F"), c("nbhd", "C"))
pick <- function(k) lapply(cols, function(cc) { r <- res[[cc[1]]]; key <- c(ols = "ols", lgt = "lgt", cl = "cl")[k]
  if (cc[2] == "C") { if (k == "cl") NULL else r$m[[paste0(key, "C")]] } else r$m[[key]] })
tab <- c("% generated by scripts/permitLogitDiD.R; do not edit",
  "\\begin{tabular}{lcccc}", "\\hline",
  " & \\multicolumn{2}{c}{Census tract} & \\multicolumn{2}{c}{Neighbourhood} \\\\",
  " & Fixed effects & ppsf control & Fixed effects & ppsf control \\\\",
  " & (1) & (2) & (3) & (4) \\\\", "\\hline",
  "\\multicolumn{5}{l}{\\textit{A. Linear probability}} \\\\",
  row("Post 2023 $\\times$ log ppsf", sapply(pick("ols"), cB)), row("", sapply(pick("ols"), cS)), row("", cyRow("ols")),
  if (BOOT) row("\\quad wild bootstrap $p$", c(pv(res$tract$pF), pv(res$tract$pC), pv(res$nbhd$pF), pv(res$nbhd$pC))),
  "\\multicolumn{5}{l}{\\textit{B. Logit}} \\\\",
  row("Post 2023 $\\times$ log ppsf", sapply(pick("lgt"), cB)), row("", sapply(pick("lgt"), cS)), row("", cyRow("lgt")),
  row("\\quad odds ratio at West$-$East gap", sapply(seq_along(cols), function(i) f2(exp(b(pick("lgt")[[i]]) * res[[cols[[i]][1]]]$gap)))),
  "\\multicolumn{5}{l}{\\textit{C. Conditional logit}} \\\\",
  row("Post 2023 $\\times$ log ppsf", sapply(pick("cl"), cB)), row("", sapply(pick("cl"), cS)),
  "\\hline",
  row("Areas", c(res$tract$nArea, res$tract$nArea, res$nbhd$nArea, res$nbhd$nArea)),
  row("Permits", fmtInt(c(res$tract$n, res$tract$n, res$nbhd$n, res$nbhd$n))),
  "\\hline", "\\end{tabular}")
writeLines(tab, paste0(OUT, "logitDiDTable.tex")); cat(tab, sep = "\n")

t <- res$tract; n <- res$nbhd
mac <- c("% generated by scripts/permitLogitDiD.R; do not edit",
  sprintf("\\newcommand{\\lgTractFE}{%s}", f3(b(t$m$lgt))), sprintf("\\newcommand{\\lgTractFESE}{%s}", f3(se(t$m$lgt))),
  sprintf("\\newcommand{\\lgNbhdFE}{%s}", f3(b(n$m$lgt))), sprintf("\\newcommand{\\lgNbhdFESE}{%s}", f3(se(n$m$lgt))),
  sprintf("\\newcommand{\\lgTractCL}{%s}", f3(b(t$m$cl))), sprintf("\\newcommand{\\lgNbhdCL}{%s}", f3(b(n$m$cl))),
  sprintf("\\newcommand{\\lgTractCtl}{%s}", f3(b(t$m$lgtC))), sprintf("\\newcommand{\\lgNbhdCtl}{%s}", f3(b(n$m$lgtC))),
  sprintf("\\newcommand{\\lgTractCtlP}{%s}", pt(pz(t$m$lgtC))), sprintf("\\newcommand{\\lgNbhdCtlP}{%s}", pt(pz(n$m$lgtC))),
  sprintf("\\newcommand{\\lpTractCtl}{%s}", f3(b(t$m$olsC))), sprintf("\\newcommand{\\lpNbhdCtl}{%s}", f3(b(n$m$olsC))),
  sprintf("\\newcommand{\\lpTractCtlP}{%s}", pt(t$pC)), sprintf("\\newcommand{\\lpNbhdCtlP}{%s}", pt(n$pC)),
  sprintf("\\newcommand{\\lgTractOR}{%s}", f2(exp(b(t$m$lgt) * t$gap))), sprintf("\\newcommand{\\lgNbhdOR}{%s}", f2(exp(b(n$m$lgt) * n$gap))),
  sprintf("\\newcommand{\\lgTractAreasDropped}{%d}", t$nArea - t$nAreaLgt), sprintf("\\newcommand{\\lgNbhdAreasDropped}{%d}", n$nArea - n$nAreaLgt),
  sprintf("\\newcommand{\\lgNSim}{%s}", fmtInt(NSIM)),
  sprintf("\\newcommand{\\lgSimTrueTract}{%s}", f3(t$bT)), sprintf("\\newcommand{\\lgSimTrueNbhd}{%s}", f3(n$bT)),
  sprintf("\\newcommand{\\lgSimFETract}{%s}", f3(t$sim$fe[1])), sprintf("\\newcommand{\\lgSimFENbhd}{%s}", f3(n$sim$fe[1])),
  sprintf("\\newcommand{\\lgSimCLTract}{%s}", f3(t$sim$cl[1])), sprintf("\\newcommand{\\lgSimCLNbhd}{%s}", f3(n$sim$cl[1])),
  sprintf("\\newcommand{\\lgSimCtlTract}{%s}", f3(t$sim$ctl[1])), sprintf("\\newcommand{\\lgSimCtlNbhd}{%s}", f3(n$sim$ctl[1])),
  sprintf("\\newcommand{\\lgSimBiasFETract}{%.0f}", 100 * (t$sim$fe[1] / t$bT - 1)),
  sprintf("\\newcommand{\\lgSimBiasFENbhd}{%.0f}", 100 * (n$sim$fe[1] / n$bT - 1)),
  sprintf("\\newcommand{\\lgSimSdFETract}{%s}", f3(t$sim$fe[3])), sprintf("\\newcommand{\\lgSimSdFENbhd}{%s}", f3(n$sim$fe[3])),
  sprintf("\\newcommand{\\lgConleyKm}{%d}", CUT0), sprintf("\\newcommand{\\lgConleyKmMin}{%d}", min(CUTS)), sprintf("\\newcommand{\\lgConleyKmMax}{%d}", max(CUTS)),
  sprintf("\\newcommand{\\lpTractFEConley}{%s}", f3(t$cy[cK, "ols"])), sprintf("\\newcommand{\\lpNbhdFEConley}{%s}", f3(n$cy[cK, "ols"])),
  sprintf("\\newcommand{\\lgTractFEConley}{%s}", f3(t$cy[cK, "lgt"])), sprintf("\\newcommand{\\lgNbhdFEConley}{%s}", f3(n$cy[cK, "lgt"])),
  sprintf("\\newcommand{\\lgTractFEConleyRange}{%s--%s}", f3(min(t$cy[-1, "lgt"])), f3(max(t$cy[-1, "lgt"]))),
  sprintf("\\newcommand{\\lgNbhdFEConleyRange}{%s--%s}", f3(min(n$cy[-1, "lgt"])), f3(max(n$cy[-1, "lgt"]))),
  sprintf("\\newcommand{\\lgConleyMCRejCl}{%.0f}", 100 * t$mc[range == 0.3, cluster]),
  sprintf("\\newcommand{\\lgConleyMCRejConley}{%.0f}", 100 * t$mc[range == 0.3, get(paste0("conley", CUT0))]),
  sprintf("\\newcommand{\\lgAreasNbhdLogit}{%d}", n$nAreaLgt),
  sprintf("\\newcommand{\\lgPermitsPerTract}{%.0f}", t$n / t$nArea), sprintf("\\newcommand{\\lgPermitsPerNbhd}{%.0f}", n$n / n$nArea))
writeLines(mac, paste0(OUT, "logitDiD.tex")); cat(mac, sep = "\n")
