# SUPERSEDED 11/22/26 by permitEventStudy.R / permitHonestDiD.R (Sept/Oct years, Sept 18 2022 - Oct 16 2023 dropped).
# Results recorded in honestDiDSummary.md. Note shareDiDPermits.rds now has the anticipation window dropped from pm,
# so rerunning this script will NOT reproduce those results (use S$pmAll and the old 12-month per to do so).
# permitEventStudyDonut.R
# Permit-level event study of duplex (vs single) on period x tract log ppsf (ppsf33), tract and period FE, SEs clustered
# by tract (no aggregation to cells). Excludes applications 2023-01-01 to 2023-10-16: by Jan 2023 the anticipated
# reversal of the duplex-price gradient was in place (permitMonthly2023.R), so the window up to R1-1 (2023-10-17) is
# anticipation. Pre periods: Sep 2018-Dec 2019 (2018 is a partial year), 2020, 2021, 2022 (reference).
# Post periods: 12-month periods from 2023-10-17.
# Writes text/eventStudyPpsfDonut.png and ~/DropboxExternal/dataProcessed/eventStudyPpsfDonut.csv
# Tom Davidoff 11/22/26

library(data.table)
library(fixest)
library(ggplot2)

S  <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")   # from permitShareDiD.R
R1 <- as.IDate("2023-10-17"); D0 <- as.IDate("2023-01-01")
n0 <- nrow(S$pm); d <- merge(S$pm, S$pT, by = "CTUID"); cat(sprintf("permits %d -> %d with tract ppsf\n", n0, nrow(d)))
n1 <- nrow(d); d <- d[!(applied >= D0 & applied < R1)]; cat(sprintf("drop Jan 1 - Oct 16 2023: %d -> %d\n", n1, nrow(d)))
d[, ev := fifelse(applied < R1, pmax(year(applied), 2019L) - 2023L,          # -4 = Sep 2018-Dec 2019, ..., -1 = 2022
                  as.integer((as.numeric(applied - R1)) %/% 365.25))]       # 0, 1, 2 = 12-month periods from R1-1
lab <- d[, .(n = .N, lab = paste0(format(min(applied), "%b %Y"), "-\n", format(max(applied), "%b %Y"))), keyby = ev]
print(lab)
m  <- feols(dup ~ i(ev, ppsf, ref = -1) | CTUID + ev, d, cluster = ~CTUID)
m0 <- feols(dup ~ I(ev >= 0):ppsf | CTUID + ev, d, cluster = ~CTUID)
print(coeftable(m))
w <- wald(m, keep = "ev::-[2-4]:ppsf", print = FALSE)
cat(sprintf("joint pre-period test F = %.2f, p = %.3f (%d restrictions); pooled post x ppsf = %.3f (%.3f)\n",
            w$stat, w$p, w$df1, coef(m0), se(m0)))
es <- data.table(ev = as.numeric(sub("^ev::(-?[0-9]+):ppsf$", "\\1", names(coef(m)))), b = coef(m), se = se(m))
es <- merge(rbind(es, data.table(ev = -1, b = 0, se = 0)), lab, by = "ev")
fwrite(es[, !"lab"], "~/DropboxExternal/dataProcessed/eventStudyPpsfDonut.csv")
p <- ggplot(es, aes(factor(ev), b)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 4.5, linetype = 2, colour = "grey40") +
  geom_errorbar(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), width = 0.15) +
  geom_point(size = 2) +
  scale_x_discrete(labels = setNames(paste0(lab$lab, "\n(n = ", lab$n, ")"), lab$ev)) +
  labs(x = "Permit application period (Jan 1-Oct 16, 2023 excluded)", y = "Coefficient on tract log ppsf (reference: 2022)") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave("../text/eventStudyPpsfDonut.png", p, width = 7.5, height = 4.8, dpi = 300)
