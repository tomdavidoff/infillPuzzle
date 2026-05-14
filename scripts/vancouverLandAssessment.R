# vancouverLandAssessment.R
# R to explore spatial elements of land valuation by location
# Want spatial heatmap of elasticity of land value with respect to lot size
# Tom Davidoff
# 05/13/26

library(data.table)
library(ggplot2)
library(RSQLite)
library(sf)

# 2019 assessment data
filename <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
con <- dbConnect(SQLite(), filename)
# Get the data
queryValuation <- "SELECT folioID, landValue FROM valuation"
queryInventory <- "SELECT roll_number,land_width, land_depth, zoning FROM residentialInventory WHERE jurisdiction==200 and zoning LIKE 'RS%'"
queryFolio <- "SELECT folioID, rollNumber from folio WHERE jurisdictionCode == 200"
queryDescription <- "SELECT folioID, actualUseDescription, landWidth, landDepth FROM folioDescription "

# load queries
valuation <- data.table(dbGetQuery(con, queryValuation))
inventory <- data.table(dbGetQuery(con, queryInventory))
folio <- data.table(dbGetQuery(con, queryFolio))
description <- data.table(dbGetQuery(con, queryDescription))
# Merge the data
setkey(valuation, folioID)
setkey(folio, folioID)
valuation <- valuation[folio]
setkey(inventory, roll_number)
setkey(valuation, rollNumber)
valuation <- valuation[inventory]
setkey(description, folioID)
setkey(valuation, folioID)
valuation <- valuation[description]

print(head(valuation))
numericCols <- c("landValue", "land_width", "land_depth", "landWidth", "landDepth")
dtValuation <- as.data.table(valuation)
# use SD operation to make chars numeric
dtValuation[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
print(summary(dtValuation[!is.na(land_width) & !is.na(land_depth),land_width==landWidth]))
dtValuation[is.na(landWidth) & !is.na(land_width), landWidth := land_width]
dtValuation[is.na(landDepth) & !is.na(land_depth), landDepth := land_depth]

sfBCA <- st_as_sf(readRDS("~/DropboxExternal/dataProcessed/bca_vancouver_residential.rds"))
sfBCA <- st_transform(sfBCA, 4326)

cent <- st_centroid(st_geometry(sfBCA))
sfBCA$lon <- st_coordinates(cent)[, 1]
sfBCA$lat <- st_coordinates(cent)[, 2]

dtBCA <- as.data.table(st_drop_geometry(sfBCA))[, .(lat, lon, rollStart)]
print(head(dtBCA))

dtValuation[,rollStart:=floor(as.numeric(rollNumber)/1000)]
print(head(dtValuation))
print(nrow(dtValuation))
setkey(dtValuation, rollStart)
setkey(dtBCA, rollStart)
dtValuation <- dtValuation[dtBCA]
print(head(dtValuation))
print(nrow(dtValuation))
print(summary(dtValuation))
critQuant <- .2

# Tidy up
dtValuation[,landArea := landWidth * landDepth]
dtValuation[,ppsf := landValue/landArea]
dtValuation <- dtValuation[!is.na(lat) & !is.na(lon) & !is.na(ppsf) & !is.na(landArea)]
dtValuation <- dtValuation[landWidth %between% quantile(landWidth, probs = c(critQuant, 1-critQuant))]
dtValuation <- dtValuation[landDepth %between% quantile(landDepth, probs = c(critQuant, 1-critQuant))]
print(summary(dtValuation))

critQuant <- .01
print(dtValuation[,.(lat,lon)])
dtValuation <- dtValuation[ppsf %between% quantile(ppsf, probs = c(critQuant, 1-critQuant))]
print(summary(dtValuation))
# plot ppsf heatmap by location, make a nice map using st tools
ggplot(dtValuation, aes(x=lon, y=lat, color=ppsf)) + geom_point() + scale_color_viridis_c() + theme_void() + labs(color="Price per sqft")
ggsave("text/landPPSFHeatMapVancouver.png")

# show a lat lon of kernel reg of ppsf on lot size



q("no")

