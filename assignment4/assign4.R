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
### ANSWER: The relationship does not look linear but it does look positive in that the richer a country is the lower its corruption level. It is strongly skewed to the left. 

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
### ANSWER/COMMENT: The predicted change in the corruption index is expected to be 1.729782. 
## 1.3.c: Compute the 25th and 75th percentiles of GDP per capita using quantile(). Then use predictions() to get predicted corruption scores at these values.
q25=quantile(df$undp_gdp, 0.25)
q75=quantile(df$undp_gdp, 0.75)
c(q25, q75)
marginaleffects::predictions(m1, newdata = marginaleffects::datagrid(undp_gdp = c(q25, q75)))
#### note about datagrid function: it is creating another dataframe just with observations i tell you, and then they give averages of the other variables.
## In a comment, report the predicted values and their 95% confidence intervals. What is the difference in predicted corruption between a country at the 25th percentile vs. the 75th percentile of GDP?
### ANSWER: Countries at the 25% percentile ($1,974.00) of GDP will have an estimated corruption perception index of 2.84 with a 95% confidence interval of [2.62, 3.07], and countries at the 75th percentile ($10,862 per capita) are predicted to have an estimated corruption perception index of 4.38 with a 95% CI of [4.20, 4.57]. So the difference between these two is 1.54.

# PROBLEM 1.4 NON-LINEAR SPECIFICATIONS
## 1.4.a: Estimate a model using the log of GDP per capita:
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)

## 1.4.b: Interpret the coefficient on log(undp gdp). You can infer this from the mathematical properties of level-log models, but the most efficient way is to create a prediction plot.
marginaleffects::plot_predictions(m2, condition="undp_gdp")
broom::tidy(m2)
### COMMENT/ANSWER: In a level log model, a 1% increase in GDP per capita yields a predicted increase of Beta/100 in the corruption perception index. If we double the GDP (log(2) approx 0.693)
coef(m2)["log(undp_gdp)"]*log(2)

##1.4.c: Estimate a model with a quadratic GDP term.
m3=lm(ti_cpi~undp_gdp+I(undp_gdp^2), data=df)
broom::tidy(m3)
marginaleffects::plot_predictions(m3, condition="undp_gdp")

#how to check which model is better? we don't really care about the fit but about how the value of the variable changes. 

## 1.4.d: Compare the R2 of all three models. Which specification fits the data best? In a comment, explain why a non-linear specification might be appropriate for this relationship.
r2=c(
  "Level-Level" = summary(m1)$r.squared,
  "Level-Log" = summary(m2)$r.squared,
  "Quadratic" = summary(m3)$r.squared)
r2
### ANSWER/COMMENT: The log specification fits the data best. In this case, the marginal return to additional GDP decreases at higher GDP levels, e.g., a move from $1,000 to $5,000 GDP per capita makes a bigger difference on governance quality than a move from $25,000 to $29,000. In cases like this, non-linear specifications are appropriate.


# PROBLEM 1.5 MARGINAL EFFECTS
## 1.5.a: For the log model (m2), compute the average marginal effect of GDP using:
marginaleffects::avg_slopes(m2, variables = "undp_gdp")

## 1.5.b: In a comment, explain why the AME differs from the raw coefficient on log(undp gdp). What does the AME tell you in substantive terms?
### ANSWER: The AME differs from the raw coefficient of the logged GDP because the marginal effect of GDP in a log model depends on the level of GDP. The AME is the average of all the marginal effects (i.e., the marginal effect on governance quality per observation given an observation's respective GDP level)

## 1.5.c: For the quadratic model (m3), compute marginal effects at specific GDP values using
marginaleffects::slopes(m3, variables = "undp_gdp", newdata = marginaleffects::datagrid(undp_gdp = c(2000, 10000, 30000)))
### In a comment, describe how the marginal effect of GDP changes as countries become richer. Does the effect diminish?
###A ANSWER/COMMENT: Yes, the effect diminishes as countries become richer. When a country reaches a certain GDP per capita threshold, the effect of increases in GDP does not change the governance quality as much as we see when there are increases for very poor countries. 

## 1.6 PREDICT PLOTS
### 1.6.a: Create a prediction plot for the log model:
p1=marginaleffects::plot_predictions(m2, condition = "undp_gdp")
p1
### Save the plot.
ggsave("assignment4/pred_plot_m2.png")

### 1.6.b: Create a prediction plot for the quadratic model (m3) on the same variable. Save this plot too.
p2=marginaleffects::plot_predictions(m3, condition = "undp_gdp")
p2
ggsave("assignment4/pred_plot_m3.png")
### 1.6.c: In a comment, compare the two plots. Do the models tell a similar story about the corruption–wealth relationship? Where do they diverge?
### ANSWER/COMMENT: Both models convey the same relationship between GDP and corruption perceptions, i.e., that as countries become richer, the corruption level will decrease. The log model produces a smoother curve whereas the quadratic model curves back up at more extreme GDP values (a feature that does not convey much meaning).

# 1.7 PROBLEM RESIDUAL DIAGNOSTICS
## 1.7.a: Use broom augment m1 to to get residuals and fitted values from the level-level model. Create a scatter plot of residuals (.resid) vs. fitted values (.fitted). 
## Does the plot suggest non-linearity or heteroskedasticity?
broom::augment(m1)
m1_aug=broom::augment(m1)
augmentplotm1=ggplot(m1_aug, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept = 0, linetype="dashed")+
  labs(x="Fitted values", y="Residuals", title="Residuals vs Fitted: Level-Level (m1")
augmentplotm1
### ANSWER/COMMENT: This plot shows a curved pattern, which indicates that the linear specification does not capture the non-linear relationship between the variables. The residuals' spread also suggests heterskedasticity because the spread increases with fitted values.
 
## 1.7.b: Now do the same for the log model (m2). Does the log transformation improve the residual pattern?
broom::augment(m2)
m2_aug=broom::augment(m2)
augmentplotm2=ggplot(m2_aug, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed")+
  labs(x="Fitted values", y="Residuals", title="Residuals vs Fitted: Level-Log (m2)")
augmentplotm2
## Does the log transformation improve the residual pattern?
### ANSWER/COMMENT: Yes, the log transformation improves the residual pattern by reducing the curvature of the model. 

## 1.7.c: Identify influential observations using Cook’s distance. Use plot(m2, which = 4) or compute Cook’s distance manually with cooks.distance(m2).
plot(m2, which=4)
cooks.distance(m2)
## Which countries (if any) have Cook’s distance above 4/n? Look up their names.
n=nrow(df)
threshold=4/n
cooks_d=cooks.distance(m2)
influential=which(cooks_d>threshold)
df$cname[influential]
plot(m2, which=4)
## 1.7.c: In a comment, discuss: should these influential observations be removed? What would you recommend as a robustness check?
### ANSWER/COMMENT: Influential observations should NOT be removed because they may not represent data errors but rather genuine cases of, for example, a very wealthy or a very corrupt country. For a robustness check, we can re-estimate the model EXCLUDING these influential observations and compare the coefficients. If the coefficients between the two models are similar, then we can say that the results are robust.

# PROBLEM 1.8 PUBLICATION-QUALITY TABLE
## 1.8.a: Create a regression table comparing all three models using:
library(modelsummary)
modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))
## 1.8.b: In a comment, summarize: which model would you choose for a final presentation, and why?
### ANSWER/COMMENT: m2 (the level-log) is the preferred specification because it has the highest R-squared and the best residual diagnostics, and its functional form has a clear substantive interpretation showing that wealth and corruption has diminishing returns. The log transformation also avoids the problem seen in the quadratic model where there is a sign reversal at extreme values.  

# PART 2: HOME PORTION (WEALTH & INFANT MORTALITY)
## PROBLEM 2.1 DATA SETUP AND EXPLORATION
### 2.1.a: Load the dataset and print summary statistics for all variables. 
infantdata=haven::read_dta("data/infantmortality.dta")
summary(infantdata)
### How many countries are in the data?
### ANSWER: There are 101 countries in the data. 
## 2.1.b: Create a histogram of infant and a histogram of income. 
hist(infantdata$infant)
hist(infantdata$income)
## Are either of them rightskewed?
### ANSWER: Both are rightskewed.
## 2.1.c:  Create a scatter plot of infant (y-axis) against income (x-axis), coloring points by region. Describe the relationship in a comment.
plotii=ggplot(infantdata, aes(x = infant, y = income, col = region)) +
  geom_point() +
  labs(x = "Infant Mortality (per 1,000)", y = "Income Per Capita ($)", title = "Infant Mortality & Income")
plotii
## 2.1.d: Create the same scatter plot but using log(income) on the x-axis and log(infant) on the y-axis. 
plotiilog=ggplot(infantdata, aes(x = log(income), y = log(infant), col = region)) +
  geom_point() +
  labs(x = "Income Per Capita ($)", y = "Infant Mortality (per 1,000)", title = "Infant Mortality & Income")
plotiilog
## Does the log-log relationship look more linear?
### ANSWER: Yes, the log-log relationship looks more linear. There is a negative trend; as income per capita increases, infant mortality decreases.

## PROBLEM 2.2 COMPARING SPECIFICATIONS
## 2.2.a: a) Estimate a level-level model:
m1_2 = lm(infant ~ income, data = infantdata)
## 2.2.b: Estimate a log-log model:
m2_2 = lm(log(infant) ~ log(income), data = infantdata)
## 2.2.c: Interpret the coefficient on income in each model - 
## In m1: what is the predicted change in infant mortality for a $1,000 increase in income? 
m1_2
### ANSWER: The coefficient on income here is -0.02091, which we multiply by 1000 to get 20.9. In short: A $1,000 increase in income per capita is associated with a DECREASE of about 20 deaths per 1,000 births.
## In m2: recall that the log-log coefficient is an elasticity. What does it mean here? (e.g., “A 10% increase in income is associated with a % change in infant mortality.”)
m2_2
### ANSWER: The log coefficient here is -0.5118. We multiply this by 10 here to get 5.118. A 10% increase in income is associated with a 5.118% DECREASE in infant mortality.

## 2.2.d: Create a residuals vs. fitted values plot for both models. Which specification has a better residual pattern? Discuss in a comment.
broom::augment(m1_2)
broom::augment(m2_2)

plot(m1_2, which=1)
plot(m2_2, which=1)

## ANSWER: The log-log model has a better residual pattern. Here, the residuals are more or less varying to the same extent and spread evenly  around 0, suggesting homoskedasticity. In the level-level model, there is more curvature and residuals are clustered toward one end, suggesting heteroskedasticity. 

# PROBLEM 2.3: MULTIPLE REGRESSION WITH CONTROLS
## 2.3.a: Estimate a log-log model with controls for region and oil-exporting status:
m3_2 = lm(log(infant) ~ log(income) + region + oil, data = infantdata)

## 2.3.b: Print the results. 
m3_2
## In a comment, interpret the coefficient on log(income): does controlling for region and oil status change the income effect?
### ANSWER: This is a log-log so the coefficient is an elasticity; here, the results show that a 10% increase in income is associated with a 3.4% decrease in infant mortality. Controlling for region and oil status did, therefore, change the income effect, but not by a substantive amount.

## 2.3.c: Interpret the coefficient on the Africa region indicator (relative to the reference category). What does it tell you about infant mortality in Africa, controlling for income?
### ANSWER: The coefficient on the Africa region indicator would be 0 here. Here, the coefficients on the other regions are negative compared to Africa, meaning that Africa has higher infant mortality rates even after controlling for income.

## 2.3.d: Compute average marginal effects using avg slopes(m3). Focus on the AME of income and report it in a comment.
marginaleffects::avg_slopes(m3_2)
### ANSWER: The AME of income here is -0.00159, so a one-unit increase in income is associated with a 0.00159 decrease in infant mortality. In other words, a $1,000 increase in income is associated with a 1.59 decrease in infant mortality on average.

# PROBLEM 2.4 INTERACTION: OIL STATUS AND INCOME
## 2.4.a: Estimate a model with an interaction between oil status and log income:
m4 = lm(log(infant) ~ log(income) * oil + region, data = infantdata)
## 2.4.b: Use avg slopes(m4, variables = "income", by = "oil") to compute the marginal effect of income separately for oil-exporting and non-oil countries.
marginaleffects::avg_slopes(m4, variables = "income", by="oil")
## 2.4.c: In a comment, discuss: does the relationship between income and infant mortality differ for oil-exporting countries? What might explain this?
### ANSWER: Yes, the relationship between income and infant mortality does change for oil-exporting and non-oil-exporting countries. In countries that do not export oil, higher income has a much stronger association with decreased infant mortality; we do not see such a strong effect from income in oil-exporting countries. This may be due to the possibility that in oil-exporting countries, the oil-derived wealth may be strongly concentrated. 

## 2.4.d: Plot how the marginal effect of income varies by oil status and save the plot.
m4_plot=marginaleffects::plot_slopes(m4, 
  variables = "income", 
  condition = "oil"
)
m4_plot
ggsave("assignment4/m4_plot.png")

# PROBLEM 2.5 PREDICTED VALUES FOR SPECIFIC SCENARIOS
## 2.5.a: Using model m3 (without interaction), compute predicted infant mortality rates for: 
## a) A non-oil African country with income = $1,000. 
## b) A non-oil European country with income = $20,000. 
## c) An oil-exporting country in the Americas with income = $10,000.
newdf=marginaleffects::predictions(m3_2, newdata=marginaleffects::datagrid(income=c(1000, 20000, 10000), region=c("Africa", "Europe", "Americas"), oil=c("no", "no", "yes")))
print(newdf)
## Note: since the outcome is log(infant), you need to exponentiate the predictions toget infant mortality in the original scale. Use exp() on the estimate column.
predvalues=marginaleffects::predictions(model=m3_2, newdata=newdf)
predvalues$infant_pred=exp(predvalues$estimate)
pred_table=cbind(newdf, infant_pred=predvalues$infant_pred)
pred_table

## 2.5.b: In a comment, discuss the predicted values. Are they plausible? How large is the gap between the African and European scenarios?
### ANSWER/COMMENT: Yes, the predicted values here are plausible given that they capture the high infant mortality rates in low-income countries in Africa. There is a big gap between the mortality rates in African countries and European countries.


# 2.6 PUBLICATION-QUALITY VISUALIZATION
## 2.6.a: Create a prediction plot showing predicted infant mortality across income levels, separately by region. Customize the plot to make it suitable for a general audience: add informative axis labels, a title, and use theme minimal() or similar.
plot4=marginaleffects::plot_predictions(m3_2, condition = c("income", "region")) +
  labs(x="Income", y="Mortality", title="Predicting Infant Mortality with Income")+
  theme_minimal()
## Save the plot.
ggsave("assignment4/plot4.png", plot = plot4, width = 6, height = 4, dpi = 300)
plot4

## 2.6.b: In a comment (5–10 sentences), discuss: 
## - what does this plot tell a general audience about the relationship between wealth and infant mortality? 
## - What role does geography play? 
## - What are the main limitations of this analysis (e.g., omitted variables, reverse causality, ecological fallacy)?
### ANSWER: This plot tells a general audience that there is a negative relationship between income and infant mortality. As income increases, infant mortality rates decrease. This plot also conveys the differential impact that increases in wealth has on infant mortality among differing levels of prior income. In other words, we can see that at lower levels of income, an increase will lead to a steeper decrease in infant mortality. We can also compare the different regions against one another in this graph; this conveys the regional variation in that, while the relationship between income and mortality is always negative, the levels are different across regions. Some limitations of this analysis could be reverse causality (i.e., higher infant mortality leads to lower income per capita via the costs associated with healthcare etc.); an omitted variable bias (here, for instance, we cannot control for health infrastructure quality, etc.); and the ecological fallacy that we can only see the overall picture and not the impact of a higher income on an individual child's survival rate.

# PROBLEM 2.7 DIAGNOSTICS AND ROBUST INFERENCE
## 2.7.a: Create a residuals vs. fitted values plot for m3. 
plot(m3_2, which=1)
## Does the plot suggest heteroskedasticity?
### ANSWER: Yes, the plot suggests heteroskedasticity. The values all move in a similar pattern around 0.

## 2.7.b: Create a regression table comparing all four models with robust standard errors:
modelsummary(
  list("Level" = m1_2, "Log-Log" = m2_2,
  "Controls" = m3_2, "Interaction" = m4),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

## 2.7.c: Compare the robust and default standard errors for m3. Run modelsummary() with and without vcov = "robust". Do the conclusions change? Why use robust SEs?
modelsummary(
  list("Level" = m1_2, "Log-Log" = m2_2,
  "Controls" = m3_2, "Interaction" = m4),
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))
## Do the conclusions change? Why use robust SEs?
### ANSWER: The conclusions do not change across the models; we see the same negative relationship between income per capita and infant mortality. We use robust SEs to account for heteroskedasticity; it increases our confidence in the results.