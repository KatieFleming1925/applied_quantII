## MAIN MODEL: Difference-in-Differences

# Loading libraries
library(fixest)
library(dplyr)
library(tidyr)
library(modelsummary)
library(ggplot2)

# Loading the data I will need
fdata=read.csv("finalproject/data/output/final_data.csv")

## DESCRIPTIVE STATISTICS

# Summary
summary(fdata[,c("sec_share", "repression_pc", "pct_left", "pct_left", "pct_centleft", "pct_right")])

# Municipal distribution of repression
repression_muni=fdata[!duplicated(fdata$muni_code),]
summary(repression_muni$repression_pc)
hist(repression_muni$repression_pc, main="Distribution of Repression (per 1000)", xlab="Victims per 1000")

# How many municipalities have zero repression
table(repression_muni$repression_pc==0)
# 102 municipalities experienced at least some repression during the Civil War, which is more than half the total sample of municipalities I am working with.

# Average secessionist vote share by year
aggregate(sec_share~year, data=fdata, FUN=mean)

# Average secessionist vote share in each province
fdata$province<-ifelse(fdata$muni_code<28000, "Lugo", "Ourense")
aggregate(sec_share~province, data=fdata, FUN=mean)

# Correlation
cor(fdata$repression_pc, fdata$sec_share, use="complete.obs")
# This shows an initial low correlation between the independent and dependent variable.

## MODEL 1: Basic DiD
model1=lm(sec_share~repression_pc+pct_left+pct_centleft+pct_right+factor(year), data=fdata)
summary(model1)

## MODEL 2: Fixed Effects (province)
fdata$province=ifelse(fdata$muni_code<28000, "Lugo", "Ourense")

model2=feols(sec_share~repression_pc+pct_left+pct_centleft+pct_right|province, data=fdata, cluster=~muni_code)
summary(model2)

## MODEL 3: TWFE (province + year)
model3=feols(sec_share~repression_pc+pct_left+pct_centleft+pct_right|province+year, data=fdata, cluster=~muni_code)
summary(model3)

## TABLE
modelsummary(list("Basic DiD" = model1, "Province FE" = model2, "Two-way FE" = model3), stars = TRUE, coef_rename = c("repression_pc" = "Repression (per 1000)", "pct_left" = "Left vote 1936", "pct_centleft" = "Centre-left vote 1936", "pct_right" = "Right vote 1936"), output = "finalproject/analyses/output/main_models.html")

## ROBUSTNESS CHECKS

## Robustness Check 1: Binary Repression Measurement
# Binary indicator of repression: 1 if present, 0 if otherwise
fdata$repression_binary=ifelse(fdata$repression_pc>0,1,0)

# Model 4: Basic DiD with binary repression
model4=lm(sec_share~repression_binary+pct_left+pct_centleft+pct_right+factor(year), data=fdata)
summary(model4)

# Model 5: Province FE with binary repression
model5=feols(sec_share~repression_binary+pct_left+pct_centleft+pct_right|province, data=fdata, cluster=~muni_code)
summary(model5)

# Model 6: TWFE with binary repression indicator
model6=feols(sec_share~repression_binary+pct_left+pct_centleft+pct_right|province+year, data=fdata, cluster=~muni_code)
summary(model6)

## Table of all robustness models
modelsummary(list("Basic DiD" = model4, "Province FE" = model5, "Two-way FE" = model6), stars = TRUE, coef_rename = c("repression_binary" = "Repression (binary)", "pct_left" = "Left vote 1936", "pct_centleft" = "Centre-left vote 1936", "pct_right" = "Right vote 1936"), output = "finalproject/analyses/output/robustness_binary.html")

## SPATIAL ANALYSIS
library(sf)
library(spdep)

# Download IGN municipality shapefile
url_shp = "https://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios/PYSLayout"

library(geodata)
library(giscoR)
library(sf)
library(spdep)

munis_spain <- gisco_get_lau(country = "ES", year = "2019")

# Filter to Lugo and Ourense
munis_lo <- munis_spain[substr(munis_spain$LAU_ID, 1, 2) %in% c("27", "32"), ]
nrow(munis_lo)

# Create neighbors list based on queen contiguity 
nb <- poly2nb(munis_lo, queen = TRUE)

# Convert to spatial weights
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Check
summary(nb)

# Need to merge spatial data with final_data for a single cross-section
# Use one year (2001) to avoid duplicates
fdata_2001 <- fdata[fdata$year == 2001, ]

# Checking that the muni codes match the package codes
munis_lo$muni_code <- as.integer(munis_lo$LAU_ID)

# Merge
fdata_spatial <- merge(munis_lo, fdata_2001, by = "muni_code")

# Remove rows with NAs used in the model
fdata_spatial_clean <- fdata_spatial[!is.na(fdata_spatial$sec_share) & !is.na(fdata_spatial$repression_pc) & !is.na(fdata_spatial$pct_left) & !is.na(fdata_spatial$pct_centleft) & !is.na(fdata_spatial$pct_right), ]

# Building clean weights
nb_clean <- poly2nb(fdata_spatial_clean, queen = TRUE)
listw_clean <- nb2listw(nb_clean, style = "W", zero.policy = TRUE)

# Cross-sectional OLS
ols_spatial <- lm(sec_share ~ repression_pc + pct_left + pct_centleft + pct_right, data = fdata_spatial_clean)

# Moran's I test on the residuals
moran.test(residuals(ols_spatial), listw = listw_clean, zero.policy = TRUE)

# There is significant autocorrelation between the municipalities so I will run an SEM and SLM.

library(spatialreg)
# Spatial Error Model (SEM)

lm.LMtests(ols_spatial, listw_clean, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))

sem_fit <- errorsarlm(sec_share ~ repression_pc + pct_left + pct_centleft + pct_right, data = fdata_spatial_clean, listw = listw_clean, zero.policy = TRUE)
summary(sem_fit)

# Spatial Lag Model (SLM)
slm_fit <- lagsarlm(sec_share ~ repression_pc + pct_left + pct_centleft + pct_right, data = fdata_spatial_clean, listw = listw_clean, zero.policy = TRUE)
summary(slm_fit)

# Table of spatial analyses
modelsummary(list("OLS" = ols_spatial, "SEM" = sem_fit, "SLM" = slm_fit), stars = TRUE, coef_rename = c("repression_pc" = "Repression (per 1000)", "pct_left" = "Left vote 1936", "pct_centleft" = "Centre-left vote 1936", "pct_right" = "Right vote 1936"), output = "finalproject/analyses/output/spatial_models.html")

## Making maps

# Repression distribution map
repression_map <- fdata[!duplicated(fdata$muni_code), c("muni_code", "repression_pc")]

# Average secessionist vote share across all years
secession_map <- aggregate(sec_share ~ muni_code, data = fdata, FUN = mean)

# Merge with spatial data
munis_lo$muni_code <- as.integer(munis_lo$LAU_ID)
map_data <- merge(munis_lo, repression_map, by = "muni_code")
map_data <- merge(map_data, secession_map, by = "muni_code")

# Map 1: Repression level
ggplot(map_data) + 
  geom_sf(aes(fill = repression_pc), color = "black", linewidth = 0.1) + 
  scale_fill_gradient(low = "white", high = "red", name = "Victims per 1000 inhabitants") + 
  labs(title = "Wartime Repression by Municipality", subtitle = "Lugo and Ourense, 1936-1942") + 
  theme_void()
ggsave("finalproject/analyses/output/map_repression.pdf")

# Map 2: Average secessionist vote share
ggplot(map_data) +
geom_sf(aes(fill = sec_share), color = "black", linewidth = 0.1) +
  scale_fill_gradient(low = "white", high = "darkblue", name = "Vote share (%)") +
  labs(title = "Average Secessionist Vote Share", subtitle = "Lugo and Ourense, 2001-2024") +
  theme_void()
ggsave("finalproject/analyses/output/map_secession.pdf")