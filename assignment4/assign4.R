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
sd(df$undp_gdp)


library(ggplot2)
# PROBLEM 1.2 EXPLORATORY VISUALIZATION
## 1.2.a: Create a scatter plot of ti cpi (y-axis) against undp gdp (x-axis) using geom point(). Add a smooth line with geom smooth(method = "lm").
plot1=ggplot(df, aes(x = undp_gdp, y = ti_cpi)) +
  geom_smooth(method="lm") +
  geom_point() +
  labs(x = "GDP", y = "Corruption Perceptions Index", title = "GDP and CPI")
plot1
## 1.2.b:  In a comment, describe the pattern. Does the relationship look linear?

## 1.2.c: Now create a second scatter plot with log(undp gdp) on the x-axis. Does the log transformation improve the linearity of the relationship?
plot2=ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_smooth(method="lm") +
  geom_point() +
  labs(x = "GDP", y = "Corruption Perceptions Index", title = ")logGDP and CPI")
plot2

# PROBLEM 1.3 BIVARIATE REGRESSION
## 1.3.a: Estimate a bivariate regression of corruption on GDP per capita:
m1 = lm(ti_cpi ~ undp_gdp, data = df)
## 1.3.b: Print the results using summary() or broom::tidy(). In a comment, interpret the coefficient on undp gdp. What is the predicted change in the corruption index for a $10,000 increase in GDP per capita?
summary(m1)
coef(m1)["undp_gdp"]*10000
### ANSWER:
## 1.3.c: Compute the 25th and 75th percentiles of GDP per capita using quantile(). Then use predictions() to get predicted corruption scores at these values.
q25=quantile(df$undp_gdp, 0.25)
q75=quantile(df$undp_gdp, 0.75)
c(q25, q75)
predictions(m1, newdata = datagrid(undp_gdp = c(q25, q75)))
#### datagrid function is creating another dataframe just with observations i tell you, and then they give averages of the other variables for each observation.
## In a comment, report the predicted values and their 95% confidence intervals. What is the difference in predicted corruption between a country at the 25th percentile vs. the 75th percentile of GDP?

# PROBLEM 1.4 NON-LINEAR SPECIFICATIONS
## 1.4.a: Estimate a model using the log of GDP per capita:
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)

## 1.4.b
plot_predictions(m2, condition="undp_gdp")

##1.4.c
m3=lm(ti_cpi~undp_gdp+I(undp_gdp^2), data=df)
broom::tidy(m3)
plot_predictions(m3, condition="undp_gdp")

#how to check which model is better? we don't really care about the fit but about how the value of the variable changes. 

##1.4.d: Compare the R2 of all three models. Which specification fits the data best? In a comment, explain why a non-linear specification might be appropriate for this relationship.
r2=c(
  "Level-Level" = summary(m1)$r.squared,
  "Level-Log" = summary(m2)$r.squared,
  "Quadratic" = summary(m3)$r.squared)
r2

# PROBLEM 1.5 MARGINAL EFFECTS
## 1.5.a: For the log model (m2), compute the average marginal effect of GDP using:
avg_slopes(m2, variables = "undp_gdp")

## 1.5.b: In a comment, explain why the AME differs from the raw coefficient on log(undp gdp). What does the AME tell you in substantive terms?

## 1.5.c

