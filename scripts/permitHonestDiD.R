# permitHonestDiD.R
# Rambachan-Roth (2023) HonestDiD sensitivity for the event study of permitEventStudy.R, at the permit level: LPM of
# duplex (vs single) on period x tract log ppsf (ppsf33), tract and 12-month period FE, SEs clustered by tract (56).
# Same point estimates as the permit-weighted tract-cell event study. Reference period -1 (year before R1-1).
# Targets: first post period, and the average of the three post periods. Restrictions: relative magnitudes
# (Delta^RM(Mbar): post violations at most Mbar x the largest pre-period period-to-period change) and smoothness
# (Delta^SD(M): change in slope of the violation at most M per period).
# Also ref -2 (anticipation of the May 2023 proposal). Writes text/honestDiDPpsf.png
# Tom Davidoff 11/21/26

library(data.table)
library(fixest)
library(HonestDiD)
library(ggplot2)

S  <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")   # from permitShareDiD.R
n0 <- nrow(S$pm); d <- merge(S$pm, S$pT, by = "CTUID"); cat(sprintf("permits %d -> %d with tract ppsf\n", n0, nrow(d)))
W <- character()
quiet <- function(expr) withCallingHandlers(expr, warning = function(w) { W <<- c(W, conditionMessage(w)); invokeRestart("muffleWarning") })
es <- fread("~/DropboxExternal/dataProcessed/eventStudyPpsf.csv")[grepl("^Census", level) & per != -1][order(per)]

# ref = -1: reform as dated (R1-1 in force 2023-10-17). ref = -2: anticipation from the May 2023 RS simplification
# proposal, so the Nov 2022 - Oct 2023 period counts as post (Rambachan-Roth's recommended handling of anticipation).
run <- function(ref, spec) {
  m <- feols(as.formula(sprintf("dup ~ i(per, ppsf, ref = %d) | CTUID + per", ref)), d, cluster = ~CTUID)
  if (ref == -1) stopifnot(max(abs(coef(m) - es$b)) < 1e-8)   # = permit-weighted tract cells
  print(coeftable(m))
  b <- coef(m); V <- vcov(m)
  pp <- as.numeric(sub("^per::(-?[0-9]+):ppsf$", "\\1", names(b)))
  pre <- which(pp < ref); post <- which(pp > ref); stopifnot(length(pre) + length(post) == length(b), all(diff(pp) > 0))
  NPRE <- length(pre); NPOST <- length(post)
  targets <- list(`First post period` = c(1, rep(0, NPOST - 1)), `Average of post periods` = rep(1 / NPOST, NPOST))
  rbindlist(lapply(names(targets), function(tg) {
    l <- targets[[tg]]
    o  <- constructOriginalCS(b, V, NPRE, NPOST, l_vec = l)
    rm <- as.data.table(quiet(createSensitivityResults_relativeMagnitudes(b, V, NPRE, NPOST, Mbarvec = MBAR, l_vec = l)))
    sd <- as.data.table(quiet(createSensitivityResults(b, V, NPRE, NPOST, method = "C-LF", Mvec = M, l_vec = l)))
    cat(sprintf("\n[%s] %s: estimate %.3f, original 95%% CI [%.3f, %.3f]\n", spec, tg, sum(l * b[post]), o$lb, o$ub))
    print(rm[, .(Mbar, lb = round(lb, 3), ub = round(ub, 3))]); print(sd[, .(M, lb = round(lb, 3), ub = round(ub, 3))])
    cat(sprintf("[%s] %s: largest Mbar with CI above 0 = %s; largest M with CI above 0 = %s\n", spec, tg,
                rm[lb > 0, if (.N) max(Mbar) else "none"], sd[lb > 0, if (.N) max(M) else "none"]))
    rbind(data.table(spec, target = tg, restr = "Relative magnitudes (Mbar)", x = c(NA, rm$Mbar), lb = c(o$lb, rm$lb), ub = c(o$ub, rm$ub)),
          data.table(spec, target = tg, restr = "Smoothness (M)", x = c(NA, sd$M), lb = c(o$lb, sd$lb), ub = c(o$ub, sd$ub)))
  }))
}
MBAR <- seq(0, 1, by = 0.1)
M    <- seq(0, 0.3, by = 0.03)
res <- rbind(run(-1, "Reference Nov 2022-Oct 2023"), run(-2, "Reference Oct 2021-Oct 2022 (anticipation)"))
cat("\nwarnings from HonestDiD:\n"); print(table(W))
res[, orig := is.na(x)]
fwrite(res, "~/DropboxExternal/dataProcessed/honestDiDPpsf.csv")

pd <- res[!(orig)]
og <- res[(orig), .(lb = lb[1], ub = ub[1]), by = .(spec, target, restr)]
p <- ggplot(pd, aes(x, ymin = lb, ymax = ub, colour = spec)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.02)) +
  geom_hline(data = og, aes(yintercept = lb, colour = spec), linetype = 3) + geom_hline(data = og, aes(yintercept = ub, colour = spec), linetype = 3) +
  facet_grid(target ~ restr, scales = "free_x") + scale_colour_manual(values = c("black", "grey55"), name = NULL) +
  labs(x = NULL, y = "Robust 95% CI, coefficient on log ppsf") + theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
ggsave("../text/honestDiDPpsf.png", p, width = 7.5, height = 5.5, dpi = 300)
