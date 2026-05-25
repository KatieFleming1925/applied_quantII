library(dplyr)
library(ggplot2)
library(broom)
library(modelsummary)

# Loading the dataset for first regression
raw=haven::read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/other/corruption.dta")


## Running first regression model WITHOUT controls
m1=lm(ti_cpi~undp_gdp, raw)

## Running second regression model WITH controls.


## Saving the regression table 
modelsummary(list("GDP on Corruption"=m1), output="assignment10/analyses/output/m1table.tex")

## 
