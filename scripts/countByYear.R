# countByYear.R
# John Ries says problem with count by year
# Tom Davidoff 
# 06/02/26

library(data.table)
library(sf)

# latest transaction data 
ds <- read_sf("~/bigFiles/latestSpatialBCA/2026-04-08_bca_folios.gpkg",layer="WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_SALES_SV")
dd <- read_sf("~/bigFiles/latestSpatialBCA/2026-04-08_bca_folios.gpkg",layer="WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV")
ds <- data.table(ds)
dd <- data.table(dd)
dd <- dd[JURISDICTION==200,.(FOLIO_ID,PROPERTY_CLASS,ACTUAL_USE_DESCRIPTION)]
ds <- ds[,.(FOLIO_ID,CONVEYANCE_DATE,CONVEYANCE_TYPE_DESCRIPTION)]


