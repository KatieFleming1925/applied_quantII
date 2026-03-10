# PROBLEM 1.1 SETUP AND DATA EXPLORATION
## 1.1.a: Load the dataset.
raw=read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")
## How many unique states and years are in the data? Use length(unique()) or n distinct() to check. Is the panel balanced (i.e., does every state appear the same number of times)?
length(unique(raw$State)) ## this shows how many unique states in the data
length(unique(raw$Year)) ## this shows how many unique years reflected in the data
dplyr::n_distinct(raw$State) ## another way to find unique states
dplyr::n_distinct((raw$Year)) ## another way to find unique years
## ANSWER: There are 50 unique states in the data and 32 unique years in the data. 
## Is the panel balanced? (i.e., does every state appear the same number of times)
table(table(raw$State))
## ANSWER: No, the panel is NOT balanced. The table we generate shows that 5 states appear 6 times, 3 states appear 8 times, etc. In a balanced panel dataset, each state would appear an equal number of times/have an equal number of observations attached to it.

## 1.1.b: Compute summary statistics for PresApprov variable and UnemPct using summary or modelsummary.
summary(raw$PresApprov)
summary(raw$UnemPct)
## Then plot PresApprov over Year for a few selected states (e.g., California, TX, NY) 
library(dplyr)
library(ggplot2)
df_sub = raw %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x= Year, y=PresApprov, color=State))+
  geom_line()+
  theme_minimal()+
  labs(x="Year", y="Presidential approval (%)", color="State")
## In a comment, describe the trends. Do states move together over time?
### ANSWER: This graph shows that CA, TX, and NY do move together over time and have the same large swings in approval. The parallel movement suggests that common national factors (e.g., the incumbent president's party, economic cycles, foreign policy events) are the dominant driver of approval, while state-level differences are relatively stable.

## 1.1.c: Create a scatterplot of PresApproval on the y-axis and UnemPct on the x-axis across all state-year observations. Add a regression line.
ggplot(raw, aes(x=UnemPct, y=PresApprov))+
  geom_point(alpha = 0.4)+
  geom_smooth(method="lm")+
  theme_minimal()+
  labs(x="Unemployment rate (%)", y="Presidential approval (%)")
## In a comment, describe the cross-sectional relationship: does higher unemployment seem to be associated with lower or higher approval ratings?
### ANSWER: Across the observations (state-year), higher unemployment rates are associated with lower presidential approval. However, the cross-sectional pattern pools observations across states and years. This means that it reflects within-state variation over time AND PERMANENT between-state differences in unemployment levels and approval. This makes it difficult to draw causal conclusions. 

# PROBLEM 1.2 POOLED OLS
## 1.2.a:
## Estimate a pooled OLS model regressing presidential approval on unemployment:
m_pooled=lm(PresApprov~UnemPct, data=raw)
## Report the results using summary.
summary(m_pooled)
## In a comment, interpret the coefficient on UnemPCT: what does it say about the relationship between unemployment and approval?
### ANSWER: The coefficient on UnemPct is NEGATIVE, so a one-percentage-point increase in the unemployment rate is associated with a one-percentage-point decrease in the presidential approval rating. The relationship is statistically significant but since it is pooled OLS results, this conflates cross-state variation with within-state variation over time.

## 1.2.b:
## Add South as a control variable:  
m_pooled2=lm(PresApprov~UnemPct+South, data=raw)
summary(m_pooled2)
## Does controlling for whether a state is located in the South change the coefficient on UnemPct? In a comment, explain why or why not.
### ANSWER: Yes, the coefficient changes slightly when we control for Southern states. This suggests that the North-South distinction did NOT strongly confound the bivariate OLS estimate. The coefficient change shows that southern states differ systematically from Northern states in their approval levels, but this difference is not strongly correlated with the unemployment-approval association in this pooled specification.

## 1.2.c:
## In a comment, reflect on the limitations of pooled OLS for this type of data (i.e., panel). What kinds of unobserved, time-invariant differences across states might bias the estimate of the unemployment effect? Give two or three concrete examples.
### ANSWER: When working with panel data, pooled OLS is problematic because it ignores unobserved, time-invariant differences across states that may be correlated with unemployment. For example: (1) states with historically weaker economies may have structurally higher unemployment and different political cultures that shape the baseline presidential approval; (2) states in particular regions may have persistent partisan leanings that affect how residents evaluate the president independently of economic conditions; (3) states with large unionized labor forces may have both higher unemployment sensitivity and different approval baselines. All of these would produce omitted variable bias in the pooled OLS estimate.

modelsummary::modelsummary(list(m_pooled, m_pooled2), stars=TRUE)

# PROBLEM 1.3: Entity Fixed Effects
## 1.3.a: Estimate a model with state fixed effects:
install.packages("dreamerr", type = "binary")
install.packages("fixest", dependencies = TRUE, type = "binary")
library(fixest)
m_fe=feols(PresApprov~UnemPct|State, data=raw)
## Report the results alongside the pooled OLS model in a single modelsummary table.
install.packages("modelsummary")
library(modelsummary)
modelsummary(
  list("Pooled OLS"=m_pooled, "State FE" = m_fe),
  vcov=~State,
  stars = TRUE,
  gof_map=c("r.squared", "nobs"),
  output="markdown"
)
## How does the coefficient on UnemPct change compared to pooled OLS?
### ANSWER: The coefficient on UnemPct changes relative to pooled OLS. The state fixed effects model compares approval within the same state across different years, removing the influence of any time-invariant state characterisitcs.

## 1.3.b: In a comment, explain what the state fixed effects are absorbing. Note that the South variable drops out of the model - why can't it be estimated when state fixed effects are included? What does this imply about any variable that does not vary within a state over time?
### ANSWER: State fixed effects absorb all the states' time-invariant differences between one another, e.g., geography, political culture, long-run economic structure, and regional identity. This is why South drops from the model: it does not vary within a state over time, so its effect is indistinguishable from the state-specific intercept (fixed effect). Any time-invariant variable is collinear with the set of state dummies and cannot be estimated separately.

## 1.3.c: What does the coefficient on UnemPct now identify? In a comment, explain the intuition: the state FE estimator compares approval ratings WITHIN the same state across different years, rather than across different states. How does this differ from the pooled OLS interpretation. 
### ANSWER: The coefficient on UnemPct in the state FE model identifies a within-state effect: it measures how approval changes in a given state when its unemployment rate rises or falls, compared to that state's own average. This is fundamentally different from pooled OLS, which compares states with different unemployment levels to one another. The FE estimator controls for all stable state-level confounders (observed or not) but cannot account for time-varying omitted variables.

# PROBLEM 1.4: TWO-WAY FIXED EFFECTS
## 1.4.a: Add year fixed effects to absorb common time shocks (e.g., national economic conditions, wars, presidential scandals) that affect all states simultaneously:
m_twfe=feols(PresApprov~UnemPct|State+Year, data=raw)
## 1.4.b: Compare all three models in a single modelsummary table with standard errors clustered by state.
library(modelsummary)
modelsummary(
  list("Pooled OLS"=m_pooled, "State FE"=m_fe, "Two-Way FE"=m_twfe),
  vcov=~State, stars=TRUE, gof_map = c("r.squared", "nobs"),
  output="markdown"
)
## 1.4.c: In a comment, discuss what the year fixed effects are controlling for. Does adding them change the coefficient on UnemPct? If so, what does that suggest about the role of common time trends in driving the relationship between unemployment and approval?
### ANSWER: Year fixed effects absorb common time shocks such as national economic cycles, presidential scandals, wars, or any other event that affects approval in all states simultaneously in a given year. If national unemployment rises during a recession, both the unemployment rate and presidential approval will move together in all states at once, not because of a state-level effect but because of the shared macro environment. Adding year dummies removes this source ofn confounding and identifies the effect of a state's unemployment relative to the national average in each year. If the coefficient on UnemPct changes noticeably after adding year fixed effects, then this would suggest that common time trends/shocks were partly driving the relationship estimated with state FEs alone.


