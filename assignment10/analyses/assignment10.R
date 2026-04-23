library(dplyr)
library(ggplot2)
library(broom)

# Loading the dataset
raw=haven::read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/other/corruption.dta")

# Running one regression model

