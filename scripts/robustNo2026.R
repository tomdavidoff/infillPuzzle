# robustNo2026.R
# Robustness: main permit estimates without 2026 applications. The permit data contain issued permits only (extract
# mid-Nov 2026), so 2026 applications (to Sept 8) are incomplete and skewed toward fast-issued permits.
# Same specifications as permitShareDiD.R col. 1/4 (permit-level LPM, equal to the permit-weighted cell estimates),
# permitEventStudy.R, and permitHonestDiD.R, on S$pm without per == 2 (2026). Prints only; no paper outputs.
# Tom Davidoff 11/22/26

library(data.table)
library(fixest)
library(HonestDiD)

S <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")   # from permitShareDiD.R
pm <- S$pm[per != 2]; cat(sprintf("permits without 2026: %d of %d\n", nrow(pm), nrow(S$pm)))
dT <- merge(pm, S$pT, by = "CTUID"); dN <- merge(pm, S$pN, by = "nbhd")
for (a in list(list("tract", dT, ~CTUID), list("nbhd", dN, ~nbhd))) {
  m <- feols(as.formula(paste("dup ~ post:ppsf |", all.vars(a[[3]]), "+ per")), a[[2]], cluster = a[[3]])
  cat(sprintf("pooled DiD, %s: %.3f (%.3f), p = %.4f, N = %d\n", a[[1]], coef(m), se(m), pvalue(m), nobs(m)))
}
m <- feols(dup ~ i(per, ppsf, ref = -1) | CTUID + per, dT, cluster = ~CTUID)
print(coeftable(m))
w <- wald(m, keep = "per::-[2-9]:ppsf", print = FALSE); cat(sprintf("joint pre-period test p = %.3f\n", w$p))
b <- coef(m); V <- vcov(m); pp <- as.numeric(sub("^per::(-?[0-9]+):ppsf$", "\\1", names(b)))
NPRE <- sum(pp < -1); NPOST <- sum(pp > -1)
cat(sprintf("largest pre-period change = %.3f\n", max(abs(diff(c(b[pp < -1], 0))))))
MBAR <- c(0, 0.1, 0.2, 0.3, 0.5, 1); M <- c(0, 0.05)
for (tg in c("First post year", "Average of post years")) {
  l  <- if (tg == "First post year") c(1, rep(0, NPOST - 1)) else rep(1 / NPOST, NPOST)
  o  <- constructOriginalCS(b, V, NPRE, NPOST, l_vec = l)
  rm <- as.data.table(suppressWarnings(createSensitivityResults_relativeMagnitudes(b, V, NPRE, NPOST, Mbarvec = MBAR, l_vec = l)))
  sd <- as.data.table(suppressWarnings(createSensitivityResults(b, V, NPRE, NPOST, method = "C-LF", Mvec = M, l_vec = l)))
  cat(sprintf("\n%s: estimate %.3f, original 95%% CI [%.3f, %.3f]\n", tg, sum(l * b[pp > -1]), o$lb, o$ub))
  print(rm[, .(Mbar, lb = round(lb, 3), ub = round(ub, 3))]); print(sd[, .(M, lb = round(lb, 3), ub = round(ub, 3))])
  cat(sprintf("%s: breakdown Mbar = %s; breakdown M = %s\n", tg, rm[lb > 0, if (.N) max(Mbar) else "none"], sd[lb > 0, if (.N) max(M) else "none"]))
}
