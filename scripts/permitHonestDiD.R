# permitHonestDiD.R
# Rambachan-Roth (2023) HonestDiD sensitivity for the event study of permitEventStudy.R (permit-level LPM of duplex on
# period x tract log ppsf, tract and period FE, SEs clustered by tract; calendar years, reference 2022; 2018 and 2023
# excluded). Targets: first post year and the average of the post years. Restrictions: relative magnitudes
# (Delta^RM(Mbar): post violations at most Mbar x the largest pre-period period-to-period change), Mbar in
# {0, .1, .2, .3, .5, 1}, so breakdowns are bracketed; smoothness (Delta^SD(M)), M in {0, .05}.
# Writes text/honestDiDPpsf.png, ~/DropboxExternal/dataProcessed/honestDiDPpsf.csv
# Tom Davidoff 11/21/26, calendar years and coarse grid 11/22/26

library(data.table)
library(fixest)
library(HonestDiD)
library(ggplot2)

SFX <- if (Sys.getenv("ZONESET") == "RT") "RT" else ""   # ZONESET=RT: RT-zone run, outputs suffixed
S  <- readRDS(paste0("~/DropboxExternal/dataProcessed/shareDiDPermits", SFX, ".rds"))   # from permitShareDiD.R
n0 <- nrow(S$pm); d <- merge(S$pm, S$pT, by = "CTUID"); cat(sprintf("permits %d -> %d with tract ppsf\n", n0, nrow(d)))
es <- fread(paste0("~/DropboxExternal/dataProcessed/eventStudyPpsf", SFX, ".csv"))[per != -1][order(per)]
m  <- feols(dup ~ i(per, ppsf, ref = -1) | CTUID + per, d, cluster = ~CTUID)
stopifnot(max(abs(coef(m) - es$b)) < 1e-8)   # = permitEventStudy.R
b <- coef(m); V <- vcov(m); pp <- as.numeric(sub("^per::(-?[0-9]+):ppsf$", "\\1", names(b)))
NPRE <- sum(pp < -1); NPOST <- sum(pp > -1); stopifnot(NPRE + NPOST == length(b), all(diff(pp) > 0))
cat(sprintf("largest pre-period change = %.3f\n", max(abs(diff(c(b[pp < -1], 0))))))
W <- character()
quiet <- function(expr) withCallingHandlers(expr, warning = function(w) { W <<- c(W, conditionMessage(w)); invokeRestart("muffleWarning") })
MBAR <- c(0, 0.1, 0.2, 0.3, 0.5, 1); M <- c(0, 0.05)
targets <- list(`First post year` = c(1, rep(0, NPOST - 1)), `Average of post years` = rep(1 / NPOST, NPOST))
res <- rbindlist(lapply(names(targets), function(tg) {
  l  <- targets[[tg]]
  o  <- constructOriginalCS(b, V, NPRE, NPOST, l_vec = l)
  rm <- as.data.table(quiet(createSensitivityResults_relativeMagnitudes(b, V, NPRE, NPOST, Mbarvec = MBAR, l_vec = l)))
  sd <- as.data.table(quiet(createSensitivityResults(b, V, NPRE, NPOST, method = "C-LF", Mvec = M, l_vec = l)))
  cat(sprintf("\n%s: estimate %.3f, original 95%% CI [%.3f, %.3f]\n", tg, sum(l * b[pp > -1]), o$lb, o$ub))
  print(rm[, .(Mbar, lb = round(lb, 3), ub = round(ub, 3))]); print(sd[, .(M, lb = round(lb, 3), ub = round(ub, 3))])
  cat(sprintf("%s: largest Mbar with CI above 0 = %s; largest M with CI above 0 = %s\n", tg,
              rm[lb > 0, if (.N) max(Mbar) else "none"], sd[lb > 0, if (.N) max(M) else "none"]))
  rbind(data.table(target = tg, restr = "Relative magnitudes (Mbar)", x = c(NA, rm$Mbar), lb = c(o$lb, rm$lb), ub = c(o$ub, rm$ub)),
        data.table(target = tg, restr = "Smoothness (M)", x = c(NA, sd$M), lb = c(o$lb, sd$lb), ub = c(o$ub, sd$ub)))
}))
cat("\nwarnings from HonestDiD:\n"); print(table(W))
fwrite(res, paste0("~/DropboxExternal/dataProcessed/honestDiDPpsf", SFX, ".csv"))
pd <- res[!is.na(x)]
og <- res[is.na(x), .(lb = lb[1], ub = ub[1]), by = .(target, restr)]
p <- ggplot(pd, aes(x, ymin = lb, ymax = ub)) +
  geom_hline(yintercept = 0, colour = "grey60") + geom_errorbar(width = 0) +
  geom_hline(data = og, aes(yintercept = lb), linetype = 3) + geom_hline(data = og, aes(yintercept = ub), linetype = 3) +
  facet_grid(target ~ restr, scales = "free_x") + labs(x = NULL, y = "Robust 95% CI, coefficient on log ppsf") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(paste0("../text/honestDiDPpsf", SFX, ".png"), p, width = 7.5, height = 5.5, dpi = 300)
