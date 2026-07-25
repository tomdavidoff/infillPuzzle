# edmontonFSR
# was Edmonton FSR limited?
# Tom Davidoff
# 07/24/26

library(data.table)
library(ggplot2)
library(sf)
library(ggspatial)

df <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv",select=c("zoning","lot_size","Total Gross Area","Account Number","year_built","Latitude","Longitude"))
print(names(df))
print(head(df))


# list zones by numeric count
df[,.(count=.N),by=zoning][order(-count)]
df <- df[zoning=="RS"]
df[,lot_size:=as.numeric(gsub(",","",lot_size))]

print(df[year_built %between% c(2010,2026),median(lot_size),by=year_built][order(year_built)])

df <- df[year_built %between% c(2010,2023)]
df[,FSR:=`Total Gross Area`/lot_size]



dA <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv",select=c("Assessment Year","Account Number","Assessed Value"))
print(head(dA))
dA <- dA[`Assessment Year`==max(`Assessment Year`)]
dA[,value:=as.numeric(gsub("\\$","",gsub(",","",`Assessed Value`)))]
setkey(dA,"Account Number")
setkey(df,"Account Number")

dM <- merge(df,dA,by="Account Number")
print(head(dM))
dM <- dM[`Total Gross Area`>0]
dM <- dM[!is.na(FSR) & !is.na(value)]
dM[,ppsf:=value/`Total Gross Area`]
dM[,pparea:=value/lot_size]
print(summary(dM))

print(cor(dM[lot_size %between% c(300,400),.(FSR,ppsf,pparea)],use="complete.obs"))
print(summary(dM))

# annotation_map_tile colors with FSR by lat/lon


dM <- st_as_sf(dM[FSR %between% c(.25,.75) & lot_size %between% c(300,400)],coords=c("Longitude","Latitude"),crs=4326)
p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 12, progress = "none") +
    geom_sf(data = dM, aes(color = FSR), size = 1.5, alpha = 0.8) +
    coord_sf(xlim = st_bbox(dM)[c(1, 3)],
	     ylim = st_bbox(dM)[c(2, 4)],
	     crs = 4326) +
    scale_color_viridis_c(labels = scales::comma) +
    theme_minimal() +
    labs(title="Edmonton FSR", color="FSR")

ggsave(p,filename="text/edmontonFSR.png",width=8,height=6)

