# vancouverPermitCost.R
# cost per square foot for new builds by geography
# Tom Davidoff
# 06/10/26

library(ggplot2)
library(data.table)
library(sf)

# permit data
dp <- fread("~/DropboxExternal/dataRaw/issued-building-permits.csv",select=c("PermitNumber","geo_point_2d","IssueDate","ProjectValue","TypeOfWork","PropertyUse","SpecificUseCategory"))
# merge with oldest available lat-lon
dp <- dp[PropertyUse=="Dwelling Uses"]
print(table(dp[,SpecificUseCategory]))
dp <- dp[SpecificUseCategory %in% c("Duplex","Duplex w/Secondary Suite","Single Detached House","Single Detached House w/Sec Suite")]
print(table(dp[,SpecificUseCategory]))
print(table(dp[,TypeOfWork]))
dp <- dp[TypeOfWork=="New Building"]

# merge with single family file
ssf <- readRDS("~/DropboxExternal/dataProcessed/bca_vancouver_singleFamily.rds")

# convert dp to spatial with centroid as geo_point_2d. then merge with ssf geom as point within
dp[, lat := as.numeric(sub(",.*", "", geo_point_2d))]
dp[, lon := as.numeric(sub(".*,", "", geo_point_2d))]
dp <- dp[!is.na(lat)]
print(nrow(dp))
sdp <- st_as_sf(dp,coords=c("lon","lat"),crs=4326)
print(nrow(sdp))
ssf <- st_transform(ssf,crs=4326)
print(head(ssf))
dp_sf <- st_join(sdp,ssf,join=st_within)
print(head(dp_sf))
print(nrow(dp_sf))
print(table(dp_sf$zoning2025))
dt <- as.data.table(dp_sf)
dt[,landArea:=landWidth*landDepth]
print(table(dt[,SpecificUseCategory]))
print(dt[,summary(round(landWidth)),by=SpecificUseCategory])
print(table(dt[,round(landWidth)==33,by=SpecificUseCategory]))
dt <- dt[round(landWidth)==33]
print(table(dt[,SpecificUseCategory]))
print(nrow(dt))
FSR <- 0.7 # appx
dt[,costPerSqFt:=ProjectValue/(landArea*FSR)]
dt <- dt[!is.na(costPerSqFt)]
print(table(dt[,SpecificUseCategory]))

print(table(dt[,SpecificUseCategory]))
print(dt[,summary(costPerSqFt),by=SpecificUseCategory])
# might be a plug number
print(head(dt))
dt[,issueMonth:=substring(IssueDate,1,7)]
print(dt[SpecificUseCategory=="Duplex",summary(costPerSqFt),by=c("SpecificUseCategory","issueMonth")])
dt[,monthCategory:=paste0(issueMonth,SpecificUseCategory)]
dt[,nMonthCategory:=.N,by=monthCategory]
print(summary(dt[nMonthCategory>5,var(costPerSqFt),by=monthCategory]))
q5 <- quantile(dt$costPerSqFt,probs=0.05)
q95 <- quantile(dt$costPerSqFt,probs=0.95)
dt[,meanCostPerSqFt:=mean(costPerSqFt),by=monthCategory]
dt[,devCostPerSqFt:=costPerSqFt-meanCostPerSqFt]



# plot costPerSqFt by lon,lat

library(ggspatial)

dplot <- st_as_sf(dt[costPerSqFt %between% c(q5, q95) & SpecificUseCategory == "Duplex"])  # geometry column survived as.data.table
ggplot(dplot) +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(aes(color = devCostPerSqFt), size = 0.8) +
  scale_color_viridis_c(name = "$/sqft dev.") +
  theme_void()
ggsave("text/vancouverCostMap.png", width = 6, height = 6, dpi = 300)
