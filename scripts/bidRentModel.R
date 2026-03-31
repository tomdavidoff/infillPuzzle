# bidRentModel.R
# from Claude
# Tom Davidoff 
# 03/31/26

library(ggplot2)
library(data.table)

# Bottom panel: income sorting — rich outbids poor for BOTH unit types near CBD
# AND poor-small outbids rich-large
d <- CJ(x = seq(0, 20, by = 0.1),
         income = c("High", "Low"),
         size = c("Large", "Small"))

d[income == "High" & size == "Large", y := 9 - 0.9 * x]
d[income == "High" & size == "Small", y := 10 - 1 * x]
d[income == "Low"  & size == "Large", y :=  5 - 0.5 * x]
d[income == "Low"  & size == "Small", y := 8 - 0.6 * x]
d[, panel := "Largest gains in worse, low-income locations"]

# Top panel: income mixing — rich outbids poor for large, poor outbids rich for small
d2 <- CJ(x = seq(0, 20, by = 0.1),
          income = c("High", "Low"),
          size = c("Large", "Small"))

d2[income == "High" & size == "Large", y := 9 - 0.5 * x]
d2[income == "High" & size == "Small", y :=  11 - 1 * x]
d2[income == "Low"  & size == "Large", y :=  5 - 0.2 * x]
d2[income == "Low"  & size == "Small", y := 9 - 0.7 * x]
d2[, panel := "Largest gains in better, high-income locations"]

dd <- rbind(d, d2)

dd[, panel := factor(panel, levels = c("Largest gains in worse, low-income locations",
                                        "Largest gains in better, high-income locations"))]

# in graph below, no tick marks on axis -- unit free
ggplot(dd, aes(x = x, y = y, colour = income, linetype = size)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ panel, ncol = 1) +
  scale_colour_manual(values = c("High" = "black", "Low" = "grey50")) +
  scale_linetype_manual(values = c("Large" = "solid", "Small" = "dashed")) +
  labs(x = "Distance from CBD",
       y = "Bid rent",
       colour = "Income",
       linetype = "Unit size") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
	axis.text = element_blank(),
	axis.ticks = element_blank(),
        panel.grid.minor = element_blank()
  )
 
 ggsave("text/bidRentModel.png", width = 8, height = 6, dpi = 300)
