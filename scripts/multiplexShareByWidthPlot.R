# multiplexShareByWidthPlot.R
# Multiplex share of new-build permits issued 2024+ on 2018 R1-1 SFD lots, by lot width (round(width): <33, 33, 34-40,
# 41-49, 50+). Bar widths = bin's share of redeveloped lots. Sample as multiplexLotWidth.R product-mix table: new build,
# latest permit per lot, otherMultiple dropped. Reads permitLotLinks.rds (run multiplexLotWidth.R first).
# Tom Davidoff 11/23/26

library(data.table)
library(ggplot2)

YEAR_MIN <- 2024
lost <- function(n0, n1, what) cat(sprintf("%-40s %6d -> %6d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

L <- readRDS("~/DropboxExternal/dataProcessed/permitLotLinks.rds")
n0 <- nrow(L); dt <- L[newBuild == TRUE & year(issued) >= YEAR_MIN]; lost(n0, nrow(dt), "new build, issued 2024+")
n0 <- nrow(dt); dt <- dt[order(rollStart, -issued)][, .SD[1], by = rollStart]; lost(n0, nrow(dt), "latest permit per lot")
n0 <- nrow(dt); dt <- dt[product != "otherMultiple"]; lost(n0, nrow(dt), "drop otherMultiple")
stopifnot(!anyDuplicated(dt$rollStart))
cat("latest issue date:", format(max(dt$issued)), "\n")

dt[, w := round(landWidth)]
dt[, bin := factor(fcase(w < 33, "<33", w == 33, "33", w <= 40, "34-40", w < 50, "41-49", default = "50+"),
                   c("<33", "33", "34-40", "41-49", "50+"))]
b <- dt[, .(N = .N, mx = sum(product == "multiplex")), keyby = bin][, `:=`(share = mx / N, lotShare = N / sum(N))]
stopifnot(sum(b$N) == nrow(dt), nrow(b) == 5)
b[, `:=`(xmax = cumsum(lotShare), xmin = cumsum(lotShare) - lotShare)]
print(b[, .(bin, N, mx, share = round(share, 3), lotShare = round(lotShare, 3))])
cat("overall multiplex share:", round(dt[, mean(product == "multiplex")], 3), " N =", nrow(dt), "\n")

gap <- 0.003
p <- ggplot(b) +
  geom_rect(aes(xmin = xmin + gap, xmax = xmax - gap, ymin = 0, ymax = share), fill = "#2a78d6") +
  geom_text(aes(x = (xmin + xmax) / 2, y = share, label = sprintf("%.0f%%", 100 * share)), vjust = -0.5, size = 3.5) +
  scale_x_continuous(breaks = (b$xmin + b$xmax) / 2, labels = sprintf("%s\n%.0f%% of lots", b$bin, 100 * b$lotShare),
                     expand = c(0, 0.005)) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%"), expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Lot width (ft); bar width = share of redeveloped lots", y = "Multiplex share of new builds") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
ggsave("../text/multiplexShareByWidth.png", p, width = 7, height = 4, dpi = 300)
