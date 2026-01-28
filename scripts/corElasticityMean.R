# corElasticityMean.R
# how correlated are different elasticity measures
# Tom Davidoff
# 01/27/26

library(data.table)

dtE1 <- fread("tables/bca19_mean_ppsf_slope_All_by_neighbourhood.csv")
dtE2 <- fread("tables/bca19_mean_ppsf_slope_by_neighbourhood.csv")
print(head(dtE1))
print(head(dtE2))
setnames(dtE1, c("slope", "elasticity","meanPPSF","nobs"), c("slope_all", "elasticity_all","meanPPSF_all","nobs_all"))
dtMerged <- merge(dtE1, dtE2, by = "neighbourhoodDescription", suffixes = c("_all", "_grouped"))
print(head(dtMerged))
print(cor(dtMerged[,.(meanPPSF, meanPPSF_all,elasticity, elasticity_all,slope,slope_all)], use="pairwise.complete.obs"))
print(cor(dtMerged[nobs>quantile(nobs,.1),.(meanPPSF, meanPPSF_all,elasticity, elasticity_all)], use="pairwise.complete.obs"))
