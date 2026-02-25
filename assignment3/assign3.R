# PART 1: IN-CLASS
## Problem 1.1: Setup and data prep
## 1.1.a: Loading dataset and definining key variables.
anes=read.csv("data/anes_timeseries_2020.csv")
anes1 = anes %>%
  transmute(
    voted=ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(V201600 == 2 ~ 1, V201600 == 1 ~ 0, TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
      income = ifelse(V201617x < 0, NA, V201617x),
      party_id = ifelse(V201231x < 0, NA, V201231x)
    )
## 1.1.b: Drop observations with missing values on any of these variables. How many observations remain?
df = na.omit(anes1)
nrow(df)
### ANSWER: 6733 observations remain after we drop the NA values.
## 1.1.c: Compute the overall turnout rate (proportion of voted == 1) and print summary statistics for all variables.
mean(df$voted)
### overall turnout rate is 0.8609832
summary(df)

# PROBLEM 1.2: EXPLORATORY VISUALIZATION
## 1.2.a: Create a bar chart showing the turnout rate by education level (hint: compute the mean of voted for each value of education, then use geom col()).
turnout_by_edu=df%>%
  group_by(education)%>%
  summarize(turnout=mean(voted))
ggplot(turnout_by_edu, aes(x=factor(education), y=turnout))+geom_col()+labs(x="Years of Ed", y="Turnout rate")

## 1.2.b: In a comment, describe the pattern. Does turnout increase with education?
### ANSWER: Voter turnout does increase with level of education.

# PROBLEM 1.3: LINEAR PROBABILITY MODEL
## 1.3.a: Estimate an LPM with voted as the outcome and age, education, income, and female as predictors.
lpm = lm(voted ~ age + education + income + female, data = df)
## 1.3.b: Print the results using broom tidy.
tidy(lpm)
## 1.3.c: Interpret the coefficient on education in a comment. What does it mean in terms of probability?
### ANSWER: The coefficient on education (0.0193) represents the estimated change in the probability of voting with each extra year of education as long as we hold the other variables constant.
## 1.3.d: Check the predicted probabilities: how many are below 0 or above 1? Report minimum and maximum predicted values.
preds_lpm=predict(lpm)
sum(preds_lpm < 0)
### This shows that there are 0 probabilities below 0.
sum(preds_lpm > 1)
### This shows that there are 802 probabilities above 1.
range(preds_lpm)
### This shows that the minimum is 0.5150876. The maximum 1.1708206. 

# PROBLEM 1.4: LOGISTIC REGRESSION
## 1.4.a: Estimate a logit model with the same predictors.
logit = glm(voted ~ age + education + income + female, family = binomial, data = df)
## 1.4.b: Print the results using tidy.
tidy(logit)
## 1.4.c: Compute the odds ratios using exp(coef(logit)). Interpret the odds ratio for education in a comment.
exp(coef(logit))
### ANSWER: The odds ratio for education (1.24898963) shows how many times the odds of voting are multiplied with each increment of education year. 
## 1.4.d: Verify that all predicted probabilities are between 0 and 1.
preds_logit = predict(logit, type = "response")
range(preds_logit)
### ANSWER: The minimum here is 0.2511085 and the maximum is 0.9945010, so all predicted probabilities are bounded (i.e., between 0 and 1).

# PROBLEM 1.5: COMPARING LPM AND LOGIT
## 1.5.a: Compute average marginal effects for the logit model using.
avg_slopes(logit)
## 1.5.b: Compare the AMEs to the LPM coefficients. How similar are they? Discuss in a comment.
### COMMENT: Both the AMEs in the logit model and the LPM coefficients are similar to one another. Both models capture the same relationship between education level and voter turnout.
## 1.5.c: Create a table with modelsummary() showing the LPM and logit side by side. Use robust standard errors for the LPM:
modelsummary(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL), output = "markdown")

# PROBLEM 1.6: PREDICTED PROBABILITIES
## 1.6.a: Use plot predictions(logit, condition = "education") to plot the predicted probability of voting across education levels. Save the plot.
p1 = plot_predictions(logit, condition = "education")
p1
## 1.6.b: Create a second plot showing predicted probabilities across age for men and women separately: plot predictions(logit, condition = c("age", "female")).
p2 = plot_predictions(logit, condition = c("age", "female"))
p2
ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)
## 1.6.c: In a comment, describe the patterns. How does the effect differ from the effect of education?
### ANSWER: There is a clear positive relationship between education and turnout, but there is also a positive relationship between age and turnout. Gender does not seem to change the relationship as both men and women still exhibit this positive relationship when we control for gender.

# PROBLEM 1.7: PRESENTING RESULTS
## 1.7.a: Create a coefficient plot comparing the LPM and logit models using modelplot(). 
p3 = modelplot(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL))
## 1.7.b: Save the plot.
p3
## 1.7.c: In a comment: for this dataset, do the LPM and logit lead to different substantive conclusions? When might the differences matter?
### ANSWER: Here, the LPM and logit both lead to similar substantive conclusions in that both models show a positive relationship between voter turnout and age, education level, and income. The differences would matter more when predicted probabilities are close to boundaries. 


# PART 2: TAKE-HOME EXERCISES - STAR - High School Graduation
## PROBLEM 2.1: Data preparation
library(tidyr)
library(dplyr)
## 2.1.a: Load star csv and create the same factor variables as in Assignment 2: classtype with labels "Small", "Regular", "Regular+Aide", and race with labels "White", "Black", etc.
star=read.csv("data/star.csv")
star$classtype<-factor(star$classtype, levels=c(1,2,3), labels = c("Small", "Regular", "Regular+Aide"))
star$race<-factor(star$race, levels=c(1,2,3,4,5,6), labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
## 2.1.b: Create a binary variable small equal to 1 if the student was in a small class and 0 if otherwise.
star$small<-ifelse(star$classtype=="Small", 1,0)
## 2.1.c: Drop observations with missing values on hsgrad. How many observations remain?
star1 <- star %>%
  drop_na(hsgrad) %>%
  mutate(
    race = factor(race, levels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other")), small = as.numeric(small))
### This shows that 3047 observations remain after omitting those with an NA value in hsgrad.
## 2.1.d: Compute the high school graduation rate overall and by class type. 
mean(star1$hsgrad)
star1%>%
  group_by(classtype)%>%
  summarize(classtyperate=mean(hsgrad))

# PROBLEM 2.2 LPM AND LOGIT
## 2.2.a: Estimate an LPM predicting hsgrad from small:
lpm1=lm(hsgrad~small, data=star1)
summary(lpm1)
## 2.2.b: Estimate a logit model with the same predictor:
logit1=glm(hsgrad~small, family=binomial, data=star1)
summary(logit1)
## 2.2.c: Interpret the LPM coeffciient on small: what is the estimated difference in graduation probability between small and non-small classes?
### ANSWER: The LPM coefficient on small is -0.075556 which means that the graduation rate for students in smaller classes is 7.6 percentage points lower than students in non-small classes. 

install.packages("rlang")
library(rlang)

## 2.2.d: Compute the AME from the logit using avg_slopes(logit1). How does it compare to the LPM coefficient?
marginaleffects::avg_slopes(logit1)

# PROBLEM 2.3 ADDING CONTROLS
## 2.3.a: Estimate both LPM and logit with controls
star1$race<-factor(star1$race, levels=c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
lpm2=lm(hsgrad~small+race+yearssmall, data=star1)
summary(lpm2)
logit2=glm(hsgrad~small+race+yearssmall,family=binomial, data=star1)
summary(logit2)
## 2.3.b: Compare the coefficient on small between the bivariate and controlled models. Does it change much? What does this tell you about the randomization?
### ANSWER: There is no substantive change between the respective coefficients of the models, which means that randomization was successful in that other characteristics (e.g., race, years spent in a small class) did not influence the randomization.
## 2.3.c: Interpret the coefficient on yearssmall from the logit model. Use avg slopes to convert to marginal effect.
avg_slopes(logit2, variables = "yearssmall")
### ANSWER: The coefficient on yearssmall is 0.0283, which means that each additional year a student spends in a small class will increase the probability of their high school graduation by 2.83 percent. 

# PROBLEM 2.4 PREDICTED PROBABILITIES
## 2.4.a: Using the controlled logit model, compute predicted graduation probabilities for a White student in a small class with 3 years in a small class and a Black student in a regular class with 0 years in a small class. 
library(marginaleffects)
### creating new df for just these two students
newdata<-data.frame(
  classtype = c("Small", "Regular"),                  
  small = c(1, 0),
  yearssmall = c(3, 0),
  race = c("White", "Black"))
### need to make sure race is factor variable
newdata$race<-factor(newdata$race, levels=levels(star1$race))
### now i use predictions
pred<-predict(logit2, newdata=newdata, type="response", se.fit=TRUE)
fit_pred<-pred$fit
se_pred<-pred$se.fit
lower_int<-fit_pred-1.96*se_pred
upper_int<-fit_pred+1.96*se_pred
results<-data.frame(
  scenario = c("White, Small, 3", "Black, Regular, 0"),
  predicted_prob = fit_pred,
  lower_CI = lower_int,
  upper_CI = upper_int
)
### need to see my results
results
### now constructing the plot 
logit4_p2=glm(hsgrad~small+yearssmall, family=binomial, data=star1)
install.packages("ggplot2", type = "binary", lib="~/R/library")
library(ggplot2)
plot1_2<-plot_predictions(logit4_p2, condition=c("yearssmall", "small"))
plot1_2

# PROBLEM 2.5 INTERACTIONS
## 2.5.a: Does the small class effect on graduation differ by race? Estimate:
logit3=glm(hsgrad~small*race+yearssmall, family=binomial, data=star1)
## 2.5.b: Use avg slopes to compute the marginal effect of small separately for each racial group.
marginaleffects::avg_slopes(logit3, variables="small", by="race")
## 2.5.c: In a comment, discuss: Is the small class effect larger for some groups than others?
### ANSWER: Yes, the effect of a small class is strongest among Black students. Race seems to play a role in educational success, especially when coupled with class size.

# PROBLEM 2.6 PRESENTING RESULTS AND DISCUSSION
## 2.6.a: Create a table with modelsummary() comparing all four models (LPM bivariate, LPM controlled, logit bivariate, logit controlled). Use robust standard errors.
install.packages("modelsummary", type = "binary")
library(modelsummary)
modelsummary(
  list(
    "LPM Bivariate"=lpm1, 
    "Logit Bivariate"=logit1, 
    "LPM with controls" = lpm2, 
    "Logit with controls"=logit2
    ),
    vcov=list("robust", NULL, "robust", NULL),
    output="assignment3/modeltable.html"
  )
## Problem 2.6.b: Create a coefficient plot with modelplot
library(modelsummary)
library(ggplot2)
modelplot(
  list(
    "LPM Bivariate" = lpm1,
    "Logit Bivariate" = logit1,
    "LPM with controls" = lpm2,
    "Logit with controls" = logit2
  )
)
## Problem 2.6.c: In a comment, discuss: What does the STAR data suggest about the effect of small class sizes on high school graduation? How do the LPM and logit results compare? Do they tell a similar or different story? Why is this experimental evidence more credible than an observational study?
### ANSWER: The STAR data suggest that class size generally has a positive effect on high school graduation rates. This effect, however, is influenced by other variables. Before we add controls to the bivariate models, the effect of small class size is small and statistically insignificant. When we added controls for race and prior time spent in a small class, the effect in the linear probability model changed significantly. The logit model also showed a higher probability of graduating when we added controls here. Both models therefore tell a similar success story but in different ways. The experimental design of this study works better because the randomization of treatment reduces selection bias and other confounders.  