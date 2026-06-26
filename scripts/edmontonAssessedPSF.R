# edmontonAssessedPSF.R
# get 2023 assessed value per square foot for 2026 buildings built before 2023
# And plot lat/lon
# Tom Davidoff 
# 06/23/26

library(data.table)
library(ggspatial)
library(fixest)
# inventory 2026 for building info
dtA  <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv",select=c("Account Number","Assessed Value","Assessment Year","Latitude","Longitude"))
print(head(dtA))
dtA <- dtA[`Assessment Year`==2023]

dtI <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv",select=c("Account Number","zoning","lot_size","Total Gross Area","year_built"))
dtI <- dtI[zoning=="RS"]
print(head(dtI))

dtI <- dtI[year_built<2023]

dtM <- merge(dtA,dtI,by="Account Number")
print(head(dtM))

# convert value to number $386,000.00 
dtM[,assessedValue:=as.numeric(gsub("\\$|,","",`Assessed Value`))]
dtM[,ppsf:=assessedValue/`Total Gross Area`]
dtM <- dtM[ppsf>quantile(ppsf,0.01) & ppsf<quantile(ppsf,0.99)]
dtM[,lot_size:=as.numeric(lot_size)]
dtM <- dtM[!is.na(lot_size) & lot_size>0]
dtM[,ppsfLot:=assessedValue/lot_size]
dtM <- dtM[ppsfLot>quantile(ppsfLot,0.01) & ppsfLot<quantile(ppsfLot,0.99)]
print(summary(dtM$ppsf))

reg <- feols(assessedValue ~ `Total Gross Area` + I(`Total Gross Area`^2) + I(`Total Gross Area`^3) + year_built + I(year_built^2) + I(year_built^3) + lot_size+I(lot_size^2)+I(lot_size^3), data=dtM)
print(summary(reg))
dtM[,residualValue:=residuals(reg)]
print(cor(dtM[,.(residualValue,ppsf,ppsfLot)],use="complete.obs"))
logReg <- feols(log(assessedValue) ~ log(lot_size) + log(`Total Gross Area`) + log(2024-year_built), data=dtM)
print(summary(logReg))
dtM[,logResidualValue:=residuals(logReg)]
print(cor(dtM[,.(logResidualValue,ppsf,ppsfLot,residualValue)],use="complete.obs"))

# make spatial and make pretty map of value/gross area
library(sf)
library(ggplot2)
dtM <- st_as_sf(dtM[!is.na(Longitude)],coords=c("Longitude","Latitude"),crs=4326)
# make a map with streets and stuff
ggplot() + 
  # Automatically fetches and overlays a clean, light street grid background
  annotation_map_tile(type = "cartolight", zoom = 12, progress = "none") +
  
  # Your initial data layer
  geom_sf(data = dtM, aes(color = ppsf), size = 1.5, alpha = 0.8) +
  
  # Crucial step: crop the map tightly to your data's bounding box
  coord_sf(xlim = st_bbox(dtM)[c(1, 3)], 
           ylim = st_bbox(dtM)[c(2, 4)],
           crs = 4326) +
  
  # Your original styling
  scale_color_viridis_c(labels = scales::comma) + 
  theme_minimal() + 
  labs(title = "Assessed Value per Square Foot for Edmonton Buildings Built Before 2023",
       color = "Assessed Value per Sq Ft") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank() # Removes the harsh lat/long grid lines over the streets
  )

# Save the high-res result
ggsave("~/text/edmontonMapPPSF.png", width = 10, height = 8, dpi = 300)

ggplot() + 
  # Automatically fetches and overlays a clean, light street grid background
  annotation_map_tile(type = "cartolight", zoom = 12, progress = "none") +
  
  # Your initial data layer
  geom_sf(data = dtM, aes(color = ppsfLot), size = 1.5, alpha = 0.8) +
  
  # Crucial step: crop the map tightly to your data's bounding box
  coord_sf(xlim = st_bbox(dtM)[c(1, 3)], 
           ylim = st_bbox(dtM)[c(2, 4)],
           crs = 4326) +
  
  # Your original styling
  scale_color_viridis_c(labels = scales::comma) + 
  theme_minimal() + 
  labs(title = "Assessed Value per  lot area for Edmonton Buildings Built Before 2023",
       color = "Assessed Value per lot area") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank() # Removes the harsh lat/long grid lines over the streets
  )
ggsave("~/text/edmontonMapPPSFLot.png", width = 10, height = 8, dpi = 300)

# plot residuals - otherwise same as PPSF stuff
ggplot() + 
  # Automatically fetches and overlays a clean, light street grid background
  annotation_map_tile(type = "cartolight", zoom = 12, progress = "none") +
  
  # Your initial data layer
  geom_sf(data = dtM, aes(color = residualValue), size = 1.5, alpha = 0.8) +
  
  # Crucial step: crop the map tightly to your data's bounding box
  coord_sf(xlim = st_bbox(dtM)[c(1, 3)], 
	   ylim = st_bbox(dtM)[c(2, 4)],
	   crs = 4326) +
  
  # Your original styling
  scale_color_viridis_c(labels = scales::comma) + 
  theme_minimal() + 
  labs(title = "Residual Assessed Value for Edmonton Buildings Built Before 2023",
       color = "Residual Assessed Value") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank() # Removes the harsh lat/long grid lines over the streets
  )
  ggsave("~/text/edmontonMapResiduals.png", width = 10, height = 8, dpi = 300)
