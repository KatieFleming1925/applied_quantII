# IN-CLASS PORTION
# PROBLEM 1.1 SETUP AND DATA EXPLORATION
# 1.1.a: 
raw=haven::read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/other/corruption.dta")
summary(raw)
# 1.1.b: Drop observations with missing values on ti_cpi or undp_gdp. How many observations remain?
library(tidyverse)
df=raw%>%drop_na(ti_cpi, undp_gdp)
# 1.1.c: Compute summary statistics for ti cpi and undp gdp. In a comment, note the range and standard deviation of each variable. Is GDP per capita right-skewed?
summary(df$ti_cpi)
summary(df$undp_gdp)

# PROBLEM 1.2 EXPLORATORY VISUALIZATION
## 1.2.a: Create a scatter plot of ti cpi (y-axis) against undp gdp (x-axis) using geom point(). Add a smooth line with geom smooth(method = "lm").