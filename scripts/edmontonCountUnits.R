# edmontonCountUnits.R
# do fancy neighbourhoods (maybe by assembly) just build more units?
# Tom Davidoff
# 07/24/26

library(data.table)
library(ggplot2)
library(ggspatial)

df <- fread("~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv")
print(head(df))
df[,zone2:=substring(ZONING,1,2)]
df <- df[grepl("New", WORK_TYPE)==TRUE]
df <- df[zone2 %in% c("RF","RS")]
print(df[order(YEAR),.N,by="YEAR"])
print(df[order(YEAR),sum(UNITS_ADDED,na.rm=TRUE),by="YEAR"])
dsf <- df[YEAR %between% c(2020,2022),.(sfPre=median(FLOOR_AREA,na.rm=TRUE)),by=NEIGHBOURHOOD]
df[,unitsPre:=sum(UNITS_ADDED*(YEAR %between% c(2020,2023)),na.rm=TRUE),by=NEIGHBOURHOOD]
df[,unitsPost:=sum(UNITS_ADDED*(YEAR %between% c(2024,2026)),na.rm=TRUE),by=NEIGHBOURHOOD]
df[,growth:=unitsPost/unitsPre]
df <- df[unitsPre>0 & unitsPost>0]
dN <- df[,.(lat=median(LATITUDE,na.rm=TRUE),lon=mean(LONGITUDE,na.rm=TRUE),growth=mean(growth)),by=NEIGHBOURHOOD]
dN <- merge(dN,dsf,by="NEIGHBOURHOOD")
print(dN)
print(summary(dN))

library(sf)

# turn into street map
dN <- st_as_sf(dN,coords=c("lon","lat"),crs=4326)
p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 12, progress = "none") +
    geom_sf(data = dN, aes(color = pmin(growth,4)), size = 1.5, alpha = 0.8) +
    coord_sf(xlim = st_bbox(dN)[c(1, 3)],
	     ylim = st_bbox(dN)[c(2, 4)],
	     crs = 4326) +
    scale_color_viridis_c(labels = scales::comma) +
    theme_minimal() +
    labs(title="Edmonton Neighbourhood Growth (2024-2026 vs 2020-2023)",
	 color="Growth (capped at 4x)")

ggsave("text/edmontonNeighbourhoodGrowth.png",p,width=8,height=6)

p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 12, progress = "none") +
    geom_sf(data = dN, aes(color = sfPre), size = 1.5, alpha = 0.8) +
    coord_sf(xlim = st_bbox(dN)[c(1, 3)],
	     ylim = st_bbox(dN)[c(2, 4)],
	     crs = 4326) +
    scale_color_viridis_c(labels = scales::comma) +
    theme_minimal() +
    labs(title="Edmonton SF PRE (2020-2023)",
	 color="SF PRE")

ggsave("text/edmontonNeighbourhoodSFPre.png",p,width=8,height=6)

