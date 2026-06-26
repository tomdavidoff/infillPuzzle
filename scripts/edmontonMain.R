# edmontonMain.R
# price/elasticity surface from pre-2023 RS stock, evaluated at post-2023
# RS permit locations, with maps.
# data loading inline; GWR sourced (it's the only thing reused per-point).
# Tom Davidoff
# 06/25/26

library(data.table)
library(sf)
library(fixest)
library(ggplot2)
library(ggspatial)
library(parallel)

source("scripts/edmontonGWR.R")

RAWDIR <- "~/DropboxExternal/dataRaw/edmonton/"
PERMITFILE <- paste0(RAWDIR,"edmontonBuildingPermits.csv")
CTFILE <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
INCOMEFILE <- "~/DropboxExternal/dataRaw/9810005801_databaseLoadingData.csv"
OUTDIR <- "~/text/"
UTMCRS <- 26912 # edmonton UTM zone 12N, metric distances

# ---------------------------------------------------------------------
# plotPPSF: cartolight street map, points colored by colVar (a string).
# only plot style we use, so it lives here.
# ---------------------------------------------------------------------
plotPPSF <- function(sfData, colVar, title, legendLab, outFile,
                     width=10, height=8, dpi=300) {
  if (st_crs(sfData)$epsg != 4326) sfData <- st_transform(sfData, 4326)
  p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 12, progress = "none") +
    geom_sf(data = sfData, aes(color = .data[[colVar]]), size = 1.5, alpha = 0.8) +
    coord_sf(xlim = st_bbox(sfData)[c(1, 3)],
             ylim = st_bbox(sfData)[c(2, 4)],
             crs = 4326) +
    scale_color_viridis_c(labels = scales::comma) +
    theme_minimal() +
    labs(title = title, color = legendLab) +
    theme(legend.position = "bottom", panel.grid = element_blank())
  ggsave(outFile, p, width = width, height = height, dpi = dpi)
  print(paste("wrote",outFile))
  invisible(p)
}

# ---- load assessment: pre-2023 RS parcels, price characteristics ----
dtA  <- fread(paste0(RAWDIR,"Property_Assessment_Data_(Historical)_20260611.csv"),
              select=c("Account Number","Assessed Value","Assessment Year","Latitude","Longitude"))
dtA <- dtA[`Assessment Year`==2023]
print(head(dtA))

dtI <- fread(paste0(RAWDIR,"Property_Information_(Current_Calendar_Year)_20260612.csv"),
             select=c("Account Number","zoning","lot_size","Total Gross Area","year_built"))
dtI <- dtI[zoning=="RS"]
dtI <- dtI[year_built<2023]
print(head(dtI))

dtPrice <- merge(dtA,dtI,by="Account Number")
print(head(dtPrice))

# convert value to number $386,000.00
dtPrice[,assessedValue:=as.numeric(gsub("\\$|,","",`Assessed Value`))]
dtPrice[,ppsf:=assessedValue/`Total Gross Area`]
dtPrice <- dtPrice[ppsf>quantile(ppsf,0.01,na.rm=TRUE) & ppsf<quantile(ppsf,0.99,na.rm=TRUE)]
dtPrice[,lot_size:=as.numeric(lot_size)]
dtPrice <- dtPrice[!is.na(lot_size) & lot_size>0]
dtPrice[,ppsfLot:=assessedValue/lot_size]
dtPrice <- dtPrice[ppsfLot>quantile(ppsfLot,0.01,na.rm=TRUE) & ppsfLot<quantile(ppsfLot,0.99,na.rm=TRUE)]
dtPrice <- dtPrice[!is.na(Longitude) & !is.na(Latitude) & `Total Gross Area`>0]

# logs for the GWR formula
dtPrice[,logAssessedValue:=log(assessedValue)]
dtPrice[,logGrossArea:=log(`Total Gross Area`)]
dtPrice[,logLotSize:=log(lot_size)]
dtPrice[,logAge:=log(2024-year_built)]
dtPrice <- dtPrice[is.finite(logAge)]
print(summary(dtPrice$ppsf))

# ---- load permits: post-2023 RS, census tract + income --------------
if (!file.exists(PERMITFILE)) {
  dp <- fread("https://data.edmonton.ca/api/views/24uj-dj8v/rows.csv?accessType=DOWNLOAD")
  fwrite(dp, PERMITFILE)
} else dp <- fread(PERMITFILE)

print(table(dp[,ZONING]))
newZone <- c("RS")
formerZones  <- c("RF1","RF2","RF3","RF4","RF4t","RF4T")
dp <- dp[ZONING %in% formerZones | ZONING %in% newZone]
print(table(dp[,ZONING]))

dCT <- st_read(CTFILE)

print(summary(is.na(dp[,LATITUDE]))) # small minority
dps <- st_as_sf(dp[!is.na(LATITUDE)], coords=c("LONGITUDE","LATITUDE"), crs=4326, remove=FALSE)
dps <- st_transform(dps, st_crs(dCT))
print(nrow(dps))
dps <- st_join(dps, dCT[,c("CTUID","CTNAME")], join=st_within)
print(nrow(dps))

dtIncome <- fread(INCOMEFILE)
dtIncome <- dtIncome[`Household income statistics (6)` == "Median household total income (2020) (2020 constant dollars)"]
dtIncome[,CTUID:=gsub("2021S0507","",DGUID)] # DGUID 2021S0507XXXXXXX -> CTUID

print(nrow(dps))
dps <- merge(dps, dtIncome[,c("CTUID","VALUE")], by="CTUID")
print(nrow(dps))
dtPermit <- data.table(dps)[YEAR>2023]
print(summary(dtPermit))

# ---- pooled OLS analog before going local ---------------------------
logReg <- feols(logAssessedValue ~ logGrossArea + logLotSize + logAge, data=dtPrice)
print(summary(logReg))

# ---- make both samples spatial, metric CRS for distances ------------
sfPrice <- st_as_sf(dtPrice, coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)
sfPrice <- st_transform(sfPrice, UTMCRS)
sfPermit <- st_as_sf(dtPermit[!is.na(LATITUDE)], coords=c("LONGITUDE","LATITUDE"),
                     crs=4326, remove=FALSE)
sfPermit <- st_transform(sfPermit, UTMCRS)

# ---- GWR: local elasticity + price level at each permit -------------
gwr <- gwrAtPoints(sfPermit, sfPrice, k=200)
sfPermit$elasticity <- gwr$elasticity
sfPermit$priceLevel <- gwr$priceLevel
sfPermit$bw <- gwr$bw

# permit-level rowhouse indicator (0/1) - same grepl as edmontonPermit.R
sfPermit$isRow <- as.integer(grepl("Row House", sfPermit$BUILDING_TYPE))
sfPermit$isSingle <- as.integer(grepl("Single Detached House", sfPermit$BUILDING_TYPE))

dtLook <- as.data.table(st_drop_geometry(sfPermit))
print(cor(dtLook[,.(elasticity,priceLevel,VALUE,isRow)],use="complete.obs"))

# ---- maps -----------------------------------------------------------
# single family vs row house: just color by the 0/1 isRow, viridis handles it
plotPPSF(st_as_sf(sfPermit), "isRow",
         "Edmonton RS Permits: Row House (1) vs Single Family (0), Post-2023",
         "Row house", paste0(OUTDIR,"edmontonMapBuildType.png"))

plotPPSF(sfPrice, "ppsf",
         "Assessed Value per Square Foot for Edmonton Buildings Built Before 2023",
         "Assessed Value per Sq Ft", paste0(OUTDIR,"edmontonMapPPSF.png"))

plotPPSF(sfPrice, "ppsfLot",
         "Assessed Value per Lot Area for Edmonton Buildings Built Before 2023",
         "Assessed Value per Lot Area", paste0(OUTDIR,"edmontonMapPPSFLot.png"))

plotPPSF(sfPermit, "priceLevel",
         "Local Predicted Log Assessed Value at Permit Locations",
         "Local price level (log $)", paste0(OUTDIR,"edmontonMapPriceLevel.png"))

plotPPSF(sfPermit, "elasticity",
         "Local Elasticity of Assessed Value wrt Gross Area at Permit Locations",
         "Local elasticity", paste0(OUTDIR,"edmontonMapElasticity.png"))

print("done")
