# SUPERSEDED 11/22/26 by permitEventStudy.R / permitHonestDiD.R (Sept/Oct years, Sept 18 2022 - Oct 16 2023 dropped).
# Results recorded in honestDiDSummary.md. Note shareDiDPermits.rds now has the anticipation window dropped from pm,
# so rerunning this script will NOT reproduce those results (use S$pmAll and the old 12-month per to do so).
# permitHonestDiDJan23.R
# HonestDiD (Rambachan-Roth 2023) for the event study of permitEventStudyDonut.R: permit-level LPM of duplex on
# period x tract log ppsf, tract and period FE, SEs clustered by tract, applications Jan 1-Oct 16 2023 excluded,
# pre periods Sep 2018-Dec 2019, 2020, 2021, 2022 (reference); post = 12-month periods from 2023-10-17.
# Targets: first post period and average of post periods. Relative magnitudes on Mbar = 0, .1, .2, .3, .5, 1 (breakdown bracketed) and
# smoothness. Writes ~/DropboxExternal/dataProcessed/honestDiDJan23.csv and text/honestDiDJan23.png
# Tom Davidoff 11/22/26

library(data.table)
library(fixest)
library(HonestDiD)
library(ggplot2)

S  <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")   # from permitShareDiD.R
R1 <- as.IDate("2023-10-17"); D0 <- as.IDate("2023-01-01")
d  <- merge(S$pm, S$pT, by = "CTUID")[!(applied >= D0 & applied < R1)]
d[, ev := fifelse(applied < R1, pmax(year(applied), 2019L) - 2023L, as.integer((as.numeric(applied - R1)) %/% 365.25))]
es <- fread("~/DropboxExternal/dataProcessed/eventStudyPpsfDonut.csv")[ev != -1][order(ev)]
m  <- feols(dup ~ i(ev, ppsf, ref = -1) | CTUID + ev, d, cluster = ~CTUID)
stopifnot(max(abs(coef(m) - es$b)) < 1e-8)
b <- coef(m); V <- vcov(m); pp <- as.numeric(sub("^ev::(-?[0-9]+):ppsf$", "\\1", names(b)))
NPRE <- sum(pp < -1); NPOST <- sum(pp > -1)
cat(sprintf("largest pre-period change = %.3f\n", max(abs(diff(c(b[pp < -1], 0))))))
W <- character()
quiet <- function(expr) withCallingHandlers(expr, warning = function(w) { W <<- c(W, conditionMessage(w)); invokeRestart("muffleWarning") })
MBAR <- c(0, 0.1, 0.2, 0.3, 0.5, 1); M <- c(0, 0.05)
targets <- list(`First post period` = c(1, rep(0, NPOST - 1)), `Average of post periods` = rep(1 / NPOST, NPOST))
res <- rbindlist(lapply(names(targets), function(tg) {
  l  <- targets[[tg]]
  o  <- constructOriginalCS(b, V, NPRE, NPOST, l_vec = l)
  rm <- as.data.table(quiet(createSensitivityResults_relativeMagnitudes(b, V, NPRE, NPOST, Mbarvec = MBAR, l_vec = l)))
  sd <- as.data.table(quiet(createSensitivityResults(b, V, NPRE, NPOST, method = "C-LF", Mvec = M, l_vec = l)))
  cat(sprintf("\n%s: estimate %.3f, original 95%% CI [%.3f, %.3f]\n", tg, sum(l * b[pp > -1]), o$lb, o$ub))
  print(rm[, .(Mbar, lb = round(lb, 3), ub = round(ub, 3))]); print(sd[, .(M, lb = round(lb, 3), ub = round(ub, 3))])
  cat(sprintf("%s: breakdown Mbar = %s; breakdown M = %s\n", tg, rm[lb > 0, if (.N) max(Mbar) else "none"], sd[lb > 0, if (.N) max(M) else "none"]))
  rbind(data.table(target = tg, restr = "Relative magnitudes (Mbar)", x = c(NA, rm$Mbar), lb = c(o$lb, rm$lb), ub = c(o$ub, rm$ub)),
        data.table(target = tg, restr = "Smoothness (M)", x = c(NA, sd$M), lb = c(o$lb, sd$lb), ub = c(o$ub, sd$ub)))
}))
cat("\nwarnings from HonestDiD:\n"); print(table(W))
fwrite(res, "~/DropboxExternal/dataProcessed/honestDiDJan23.csv")
pd <- res[!is.na(x)]
og <- res[is.na(x), .(lb = lb[1], ub = ub[1]), by = .(target, restr)]
p <- ggplot(pd, aes(x, ymin = lb, ymax = ub)) +
  geom_hline(yintercept = 0, colour = "grey60") + geom_errorbar(width = 0) +
  geom_hline(data = og, aes(yintercept = lb), linetype = 3) + geom_hline(data = og, aes(yintercept = ub), linetype = 3) +
  facet_grid(target ~ restr, scales = "free_x") + labs(x = NULL, y = "Robust 95% CI, coefficient on log ppsf") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave("../text/honestDiDJan23.png", p, width = 7.5, height = 5.5, dpi = 300)
