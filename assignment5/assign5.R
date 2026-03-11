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

# PART 2: TAKE-HOME (TEACHING EVALS)
## PROBLEM 2.1: DATA EXPLORATION
evaldf=haven::read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/teaching_evals/teaching_evals.dta")
## 2.1.a: How many unique instructors and courses are in the data? Us n_distinct df$InstrID and n_distinct df$CourseID
n_distinct(evaldf$InstrID)
n_distinct(evaldf$CourseID)
### ANSWER: There are 48 unique instructors and 254 unique courses in the dataset.
## What is the average number of observations (course-year pairs) per instructor? 
mean(table(evaldf$InstrID))
## In a comment, note whether this looks like a long or short panel.
### ANSWER: This looks like it is a short panel dataset. With short panel data, there is a higher total number of individuals than there are observations (i.e., years) attached to each individual. Here we have 48 instructors and the average number of observations attached to each professor/individual is 17.5, so there are not as many time observations. 
## 2.1.b: Create a scatter plot of Eval on the y-axis against Apct on the x-axis. Add a regression line.
ggplot(evaldf, aes(x=Apct, y=Eval))+
  geom_point(alpha = 0.4)+
  geom_smooth(method="lm")+
  theme_minimal()+
  labs(x="% Students Receiving at least A-", y="Avg Course Eval")
## In a comment, describe the cross-sectional relationship between grading generosity and evaluations: Is it positive or negative? Does this pattern surprise you?
### ANSWER: The scatterplot shows that there is a positive relationship between grading generosity and course evaluations, but I do not think it is a very strong relationship. There do not seem to be many low course evaluations to begin with. The fact that the relationship is not stronger or more starkly positive is what surprises me the most.

## PROBLEM 2: POOLED OLS BASELINE
## 2.2.a: Estimate a pooled OLS model with all three regressors.
m1.2=lm(Eval~Apct+Enrollment+Required, data=evaldf)
## Report the results using summary or modelsummary.
modelsummary(m1.2)
## In a comment, intrepret the coefficient on Apct: a one-percentage point increase is associated with how much of a change in evaluation scores?
### ANSWER: A one-percentage point increase is associated with a 0.0036 increase in evaluation scores.

## 2.2.b: In a comment, explain why the OLS estimate of Apct might be biased. What unobserved characteristics of instructors could simultaneously drive both grading generosity and evaluation scores? Give at least two concrete examples. Is the expected bias upward or downward?
## ANSWER: Pooled OLS is problematic because it does not take into account unobserved heterogeneity among individuals or over time, so in this case, characteristics unique to a professor are not taken into account by this coefficient. These unobserved characteristics can be driven by an instructor's personality. For example (1), more laidback professors might give out higher grades and receive better evaluations. Another example (2) can be a professor's ability to communicate: Better communicators can teach the material better; therefore, students will perform better and might provide higher evals because of the professor's communication/teaching quality. The expected bias is upward because these characteristics correlate with both variables.


## PROBLEM 2.3: FIXED EFFECTS MODELS
## 2.3.a: Estimate a model with instructor fixed effects and a two-way model adding year fixed effects
library(fixest)
m_instr=feols(Eval~Apct+Enrollment+Required|InstrID, data=evaldf)
m_twfe2=feols(Eval~Apct+Enrollment+Required|InstrID+Year, data=evaldf)
## 2.3.b: Compare all three models in a single table with standard errors clustered by instructor.
modelsummary(
  list("Pooled OLS"=m1.2, "Instructor FE"=m_instr, "Two-Way FE"=m_twfe2),
  vcov=~InstrID,
  stars=TRUE,
  gof_map=c("r.squared", "nobs")
)

## 2.3.c: Interpret the coefficient on Apct in the instructor-FE model. In a comment, explain what the instructor fixed effect is controlling for. Is the FE coefficient on Apct larger or smaller than in the pooled OLS? What does this tell us about the direction of omitted variable bias in the pooled OLS estimate? Are more lenient graders systematically better or worse evaluators in terms of their unobserved characteristics?
### ANSWER: The instructor fixed effect controls for all time-invariant characteristics of an instructor, e.g., an instructor's personality traits. The FE coefficient is smaller than the pooled OLS coefficient, which tells us that these unobserved characteristics were behind the coefficient in the pooled OLS estimate. Lenient graders are systematically better evaluators when it comes to their unobserved characteristics.

# PROBLEM 2.4: RANDOM EFFECTS AND THE HAUSMAN TEST
## 2.4.a: Estimate a random effects model using plm. The random effects model assumes that the unobserved instructor-level heterogeneity is uncorrelated with the regressors.
library(plm)
pdata=pdata.frame(evaldf, index=c("InstrID", "CourseID"))
m_re=plm(Eval~Apct+Enrollment+Required, data=pdata, model="random")

## 2.4.b: Run the Hausman test to assess whether fixed or random effects is more appropriate. The Hausman test checks whether the random effects assumption (no correlation between unobservables and regressors) holds.
me_fe_plm=plm(Eval~Apct+Enrollment+Required, data=pdata, model="within")
phtest(me_fe_plm, m_re)

## In a comment, interpret the Hausman test result. What is the null hypothesis? Is it rejected? Based on the rest and on the substantive reasoning from the previous subsection, should you prefer the fixed effects or the random effects estimator for the dataset? Explain in 3-5 sentences.
### ANSWER: With the Hausman test, the null hypothesis is that random effects is more appropriate. Based on the results here, we can reject that hypothesis. The p-value is very high at 0.178, which means that we cannot say that the unobserved instructor-level heterogeneity is uncorrelated with the regressors. Fixed effects here is therefore more appropriate.