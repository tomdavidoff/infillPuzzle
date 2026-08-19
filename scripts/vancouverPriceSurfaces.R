# vancouverPriceSurfaces.R
# get sqft-price plots for every neighbourhood
# Tom Davidoff 
# 08/19/26

library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)
library(RSQLite)
library(fixest)
library(units)



# Merge with BCA 2019 Vancouver single family R1 -zoned parcels
bca19 <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
con <- dbConnect(RSQLite::SQLite(), bca19)
# (1) one join across folio (jurisdiction) + residentialInventory (zoning/area) + folioDescription
# (use description) -- replaces the three separate pulls and the two R merges, and filters
# in SQL before pulling into R. INNER JOINs reproduce the old merge()-without-all=TRUE
# behavior (rows without a match in every table are dropped, same as before).
dtBCA19 <- data.table(dbGetQuery(con, "
  SELECT d.folioID, f.rollNumber, i.zoning, i.MB_effective_year, i.MB_total_finished_area, i.land_width, i.land_depth,
         d.actualUseDescription, d.neighbourhoodDescription, d.landWidth, d.landDepth, s.conveyanceDate, s.conveyancePrice
  FROM folio f
  JOIN residentialInventory i ON i.roll_number = f.rollNumber
  JOIN folioDescription d      ON d.folioID     = f.folioID
  JOIN sales s ON s.folioID = f.folioID
  WHERE f.jurisdictionCode = '200' AND s.conveyanceTypeDescription = 'Improved Single Property Transaction'
"))
print(head(dtBCA19))
print(table(dtBCA19[,actualUseDescription])[order(-table(dtBCA19[,actualUseDescription]))])
print(nrow(dtBCA19))
dtBCA19[,saleYear:=substring(conveyanceDate, 1, 4)]
dtBCA19 <- dtBCA19[saleYear==2017]
for (v in c("MB_total_finished_area", "land_width","land_depth","conveyancePrice")) dtBCA19[, (v) := as.numeric(get(v))]
pdf("text/vancouverPriceSurfaces.pdf", width=8, height=6)
for (n in sort(unique(dtBCA19[nchar(neighbourhoodDescription) > 0, neighbourhoodDescription]))) {
  d <- dtBCA19[neighbourhoodDescription==n & MB_total_finished_area>0 & conveyancePrice>0]
  if (nrow(d) < 2) next   # geom_smooth(lm) needs at least 2 points
  p <- ggplot(d, aes(x=MB_total_finished_area, y=conveyancePrice)) +
    geom_point(alpha=0.5) +
    geom_smooth(method="lm", formula=y~x, color="blue") +
    scale_y_continuous(labels=scales::dollar_format()) +
    labs(title=n, x="Finished Area (sqft)", y="Conveyance Price ($)") +
    theme_minimal()
  print(p)   # <-- this is what writes a page to the PDF
}
dev.off()   # <-- must close the device or the file stays empty/corrupt
