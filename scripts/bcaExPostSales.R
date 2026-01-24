# bcaExPostSales.R
# R to look at duplex premium by census tract and relate to tract-level mean vs slope
# Tom Davidoff
# 01/23/26

library(data.table)
library(ggplot2)
library(sf)
library(fixest)

# Prep data analysis

fClean <- "~/OneDrive - UBC/dataProcessed/bca25Sales_cleaned.rds"
if (!file.exists(fClean)) {
  # tract level coefficients from bcaSales.R

# get property geometry
# from bcaSales.R
# get sales and other data
  library(sf)
  library(data.table)

  BCA25 <- "/Volumes/T7Office/bigFiles/bca_folios_spatial_file_20251103/bca_folios.gpkg.gpkg"

  library(sf)


  bca_subset <- st_read(
    BCA25,
    query = "
      SELECT 
        d.FOLIO_ID,
        d.ROLL_NUMBER,
        d.ACTUAL_USE_DESCRIPTION,
        d.JURISDICTION_CODE,
        d.NEIGHBOURHOOD,
        s.CONVEYANCE_PRICE,
        s.CONVEYANCE_DATE,
        d.geom
      FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV d
      LEFT JOIN WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_SALES_SV s
        ON d.FOLIO_ID = s.FOLIO_ID
      WHERE d.JURISDICTION_CODE = '200'
    "
  )

# get year built from inventory
  fInventory <- "~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt"
  dtInventory <- fread(fInventory,select=c("Roll_Number","MB_Effective_Year","MB_Total_Finished_Area","MB_Year_Built","Other_Building_Flag"),colClasses=list(character=c("Roll_Number")))
  print(names(dtInventory))
  print(head(dtInventory))
  print(head(bca_subset))


  ct_file <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
  sCT <- st_read(ct_file, quiet = TRUE)
  sCT <- st_make_valid(sCT)
  stBCA <- st_crs(bca_subset)
  sCT <- st_transform(sCT, stBCA)

  print(nrow(bca_subset))
  bca_subset <- st_join(bca_subset, sCT[, c("CTUID")], join = st_within, left = TRUE)
  print(nrow(bca_subset))
  dtBCA <- merge(as.data.table(bca_subset), dtInventory, by.x="ROLL_NUMBER", by.y="Roll_Number", all.x=TRUE)
  print(nrow(dtBCA))

  saveRDS(dtBCA, fClean)
}

dtBCA <- readRDS(fClean)
print(head(dtBCA))

dtCoefs <- fread("tables/bca19_mean_ppsf_slope_by_tract.csv",colClasses=list(character=c("CTUID")))
CRITN <- quantile(dtCoefs[,nobs],.1)
dtCoefs  <- dtCoefs[nobs >= CRITN]
print(head(dtCoefs))
print(cor(dtCoefs[,.(meanPPSF,slope,elasticity)]))

dtBCA <- merge(dtBCA, dtCoefs, by="CTUID")
print(head(dtBCA))

print(table(dtBCA[,ACTUAL_USE_DESCRIPTION]))
dtBCA[, duplex := fifelse(grepl("uplex", ACTUAL_USE_DESCRIPTION), 1, 0)]
dtBCA <- dtBCA[duplex==1 | ACTUAL_USE_DESCRIPTION %in% c("Residential Dwelling with Suite","Single Family Dwelling")]
dtBCA[,year:=year(CONVEYANCE_DATE)]
dtBCA <- dtBCA[year >= 2020 ]
dtBCA <- dtBCA[MB_Effective_Year>=2018]
# drop size and price outliers
CUT <- .05
dtBCA <- dtBCA[MB_Total_Finished_Area >= quantile(MB_Total_Finished_Area,CUT,na.rm=TRUE) &
                 MB_Total_Finished_Area <= quantile(MB_Total_Finished_Area,1-CUT,na.rm=TRUE)]
dtBCA[,ppsf := CONVEYANCE_PRICE / MB_Total_Finished_Area]

print(cor(dtBCA[,.(meanPPSF,slope,elasticity)]))
## weird positive correlation??
print(summary(dtBCA))

print(table(dtBCA[,duplex]))
r1 <- feols(ppsf ~ duplex*meanPPSF |CTUID+year, data=dtBCA)
r2 <- feols(ppsf ~ duplex*slope | CTUID+year, data=dtBCA)
r3 <- feols(ppsf ~ duplex*meanPPSF + duplex*slope | CTUID+year, data=dtBCA)
r4 <- feols(ppsf ~ duplex*meanPPSF + duplex*elasticity | CTUID+year, data=dtBCA)
# print table of regression results akin to outreg
print(etable(r1,r2,r3,r4,tex=TRUE))

pDuplex <- dtBCA[duplex==1,.(meanPDuplex=mean(ppsf,na.rm=TRUE),meanPPSF=mean(meanPPSF,na.rm=TRUE),elasticity=mean(elasticity,na.rm=TRUE),nDuplex=.N),by=.(CTUID)]
pSingle <- dtBCA[duplex==0,.(meanPSingle=mean(ppsf,na.rm=TRUE),nSingle=.N),by=.(CTUID)]
pMerge <- merge(pDuplex,pSingle,by="CTUID")
pMerge[,duplexPremium:=meanPDuplex - meanPSingle]
pMerge[,shareDuplex:=nDuplex / (nDuplex + nSingle)]
print(cor(pMerge[,.(duplexPremium,meanPDuplex,shareDuplex,meanPPSF,elasticity)]))
rt1 <- feols(shareDuplex ~ meanPPSF , data=pMerge)
rt2 <- feols(shareDuplex ~ meanPPSF + elasticity, data=pMerge)
rt3 <- feols(shareDuplex ~  elasticity, data=pMerge)
print(etable(rt1,rt2,rt3,tex=TRUE))
