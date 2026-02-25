#PART 2: TAKE-HOME EXERCISES - STAR - High School Graduation
#2.1: Data preparation
library(tidyr)
#2.1.a: Load star csv and create the same factor variables as in Assignment 2: classtype with labels "Small", "Regular", "Regular+Aide", and race with labels "White", "Black", etc.
star=read.csv("data/star.csv")
star$classtype<-factor(star$classtype, levels=c(1,2,3), labels = c("Small", "Regular", "Regular+Aide"))
star$race<-factor(star$race, levels=c(1,2,3,4,5,6), labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
#2.1.b: Create a binary variable small equal to 1 if the student was in a small class and 0 if otherwise.
star$small<-ifelse(star$classtype=="Small", 1,0)
#2.1.c: Drop observations with missing values on hsgrad. How many observations remain?
star1 <- star %>%
  drop_na(hsgrad) %>%
  mutate(
    race = factor(race, levels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other")), small = as.numeric(small))
#This shows that 3047 observations remain after omitting those with an NA value in hsgrad.
#2.1.d: Compute the high school graduation rate overall and by class type. 
mean(star1$hsgrad)
star1%>%
  group_by(classtype)%>%
  summarize(classtyperate=mean(hsgrad))

#2.2 LPM AND LOGIT
#2.2.a: Estimate an LPM predicting hsgrad from small:
lpm1=lm(hsgrad~small, data=star1)
print(lpm1)
#2.2.b: Estimate a logit model with the same predictor:
logit1=glm(hsgrad~small, family=binomial, data=star1)
print(logit1)
#2.2.c: Interpret the LPM coeffciient on small: what is the estimated difference in graduation probability between small and non-small classes?


install.packages("rlang")
library(rlang)

#2.2.d: Compute the AME from the logit using avg_slopes(logit1). How does it compare to the LPM coefficient?
avg_slopes(logit1)

#PROBLEM 2.3 ADDING CONTROLS
#2.3.a: Estimate both LPM and logit with controls
star1$race<-factor(star1$race, levels=c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
lpm2=lm(hsgrad~small+race+yearssmall, data=star1)
summary(lpm2)
logit2=glm(hsgrad~small+race+yearssmall,family=binomial, data=star1)
summary(logit2)
#2.3.b: Compare the coefficient on small between the bivariate and controlled models. Does it change much? What does this tell you about the randomization?

#2.3.c:

#PROBLEM 2.4 PREDICTED PROBABILITIES
#2.4.a: Using the controlled logit model, compute predicted graduation probabilities for a White student in a small class with 3 years in a small class and a Black student in a regular class with 0 years in a small class. 
library(marginaleffects)
#creating new df for just these two students
newdata<-data.frame(
  classtype = c("Small", "Regular"),                  
  small = c(1, 0),
  yearssmall = c(3, 0),
  race = c("White", "Black"))

newdata$race<-factor(newdata$race, levels=levels(star1$race))
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

results

logit4_p2=glm(hsgrad~small+yearssmall, family=binomial, data=star1)
install.packages("ggplot2", type = "binary", lib="~/R/library")
library(ggplot2, lib.loc="~/R/library")
plot1_2<-plot_predictions(logit4_p2, condition=c("yearssmall", "small"))
