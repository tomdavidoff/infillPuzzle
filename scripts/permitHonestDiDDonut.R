# SUPERSEDED 11/22/26 by permitEventStudy.R / permitHonestDiD.R (Sept/Oct years, Sept 18 2022 - Oct 16 2023 dropped).
# Results recorded in honestDiDSummary.md. Note shareDiDPermits.rds now has the anticipation window dropped from pm,
# so rerunning this script will NOT reproduce those results (use S$pmAll and the old 12-month per to do so).
# permitHonestDiDDonut.R
# HonestDiD (Rambachan-Roth 2023) for the permit event study of permitHonestDiD.R (permit-level LPM of duplex on
# 12-month period x tract log ppsf, tract and period FE, SEs clustered by tract, reference period -1), on two samples:
#   (1) all permits; (2) donut dropping applications from DONUT_START to 2023-10-16 (the pre-R1-1 filing rush of
#   singles, East Side; see permitMonthly2023Side.R). DONUT_START is the first command-line argument, default 2023-04-17
#   (6-month donut); 2023-07-01 = from the staff recommendation. Output files carry the suffix _<DONUT_START as yyyymmdd>.
# Targets: first post period and average of post periods. Restrictions: relative magnitudes (Mbar) and smoothness (M).
# Also prints the joint pre-period Wald test and the pooled post x ppsf DiD for each sample.
# Writes ~/DropboxExternal/dataProcessed/honestDiDDonut.csv and text/honestDiDDonut.png
# Tom Davidoff 11/22/26

library(data.table)
library(fixest)
library(HonestDiD)
library(ggplot2)

S  <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")   # from permitShareDiD.R
n0 <- nrow(S$pm); dAll <- merge(S$pm, S$pT, by = "CTUID"); cat(sprintf("permits %d -> %d with tract ppsf\n", n0, nrow(dAll)))
args <- commandArgs(trailingOnly = TRUE)
DONUT_START <- as.IDate(if (length(args)) args[1] else "2023-04-17"); SUF <- format(DONUT_START, "_%Y%m%d")
dDon <- dAll[!(applied >= DONUT_START & applied <= as.IDate("2023-10-16"))]
cat(sprintf("donut drops %d permits -> %d\n", nrow(dAll) - nrow(dDon), nrow(dDon)))
print(dcast(rbind(dAll[, .(s = "all", per)], dDon[, .(s = "donut", per)]), per ~ s, fun.aggregate = length))

W <- character()
quiet <- function(expr) withCallingHandlers(expr, warning = function(w) { W <<- c(W, conditionMessage(w)); invokeRestart("muffleWarning") })
MBAR <- seq(0, 1, by = 0.05)
M    <- seq(0, 0.3, by = 0.03)

run <- function(d, spec) {
  m  <- feols(dup ~ i(per, ppsf, ref = -1) | CTUID + per, d, cluster = ~CTUID)
  m0 <- feols(dup ~ post:ppsf | CTUID + per, d, cluster = ~CTUID)
  cat(sprintf("\n==== %s ====\n", spec)); print(coeftable(m))
  w <- wald(m, keep = "per::-[2-5]:ppsf", print = FALSE)
  cat(sprintf("[%s] joint pre-period test F = %.2f, p = %.3f; pooled post x ppsf = %.3f (%.3f)\n", spec, w$stat, w$p, coef(m0), se(m0)))
  b <- coef(m); V <- vcov(m)
  pp <- as.numeric(sub("^per::(-?[0-9]+):ppsf$", "\\1", names(b)))
  NPRE <- sum(pp < -1); NPOST <- sum(pp > -1); stopifnot(NPRE + NPOST == length(b), all(diff(pp) > 0))
  cat(sprintf("[%s] largest pre-period change = %.3f\n", spec, max(abs(diff(c(b[pp < -1], 0))))))
  targets <- list(`First post period` = c(1, rep(0, NPOST - 1)), `Average of post periods` = rep(1 / NPOST, NPOST))
  rbindlist(lapply(names(targets), function(tg) {
    l  <- targets[[tg]]
    o  <- constructOriginalCS(b, V, NPRE, NPOST, l_vec = l)
    rm <- as.data.table(quiet(createSensitivityResults_relativeMagnitudes(b, V, NPRE, NPOST, Mbarvec = MBAR, l_vec = l)))
    sd <- as.data.table(quiet(createSensitivityResults(b, V, NPRE, NPOST, method = "C-LF", Mvec = M, l_vec = l)))
    cat(sprintf("\n[%s] %s: estimate %.3f, original 95%% CI [%.3f, %.3f]\n", spec, tg, sum(l * b[pp > -1]), o$lb, o$ub))
    print(rm[, .(Mbar, lb = round(lb, 3), ub = round(ub, 3))]); print(sd[, .(M, lb = round(lb, 3), ub = round(ub, 3))])
    cat(sprintf("[%s] %s: largest Mbar with CI above 0 = %s; largest M with CI above 0 = %s\n", spec, tg,
                rm[lb > 0, if (.N) max(Mbar) else "none"], sd[lb > 0, if (.N) max(M) else "none"]))
    rbind(data.table(spec, target = tg, restr = "Relative magnitudes (Mbar)", x = c(NA, rm$Mbar), lb = c(o$lb, rm$lb), ub = c(o$ub, rm$ub)),
          data.table(spec, target = tg, restr = "Smoothness (M)", x = c(NA, sd$M), lb = c(o$lb, sd$lb), ub = c(o$ub, sd$ub)))
  }))
}
res <- rbind(run(dAll, "All permits"), run(dDon, sprintf("Donut: drop %s-16 Oct 2023", format(DONUT_START, "%d %b"))))
cat("\nwarnings from HonestDiD:\n"); print(table(W))
res[, orig := is.na(x)]
fwrite(res, sprintf("~/DropboxExternal/dataProcessed/honestDiDDonut%s.csv", SUF))

pd <- res[!(orig)]
og <- res[(orig), .(lb = lb[1], ub = ub[1]), by = .(spec, target, restr)]
p <- ggplot(pd, aes(x, ymin = lb, ymax = ub, colour = spec)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.02)) +
  geom_hline(data = og, aes(yintercept = lb, colour = spec), linetype = 3) + geom_hline(data = og, aes(yintercept = ub, colour = spec), linetype = 3) +
  facet_grid(target ~ restr, scales = "free_x") + scale_colour_manual(values = c("black", "grey55"), name = NULL) +
  labs(x = NULL, y = "Robust 95% CI, coefficient on log ppsf") + theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
ggsave(sprintf("../text/honestDiDDonut%s.png", SUF), p, width = 7.5, height = 5.5, dpi = 300)
