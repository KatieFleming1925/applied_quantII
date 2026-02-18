# IN-CLASS PORTION
## Problem 1.1: Setup
### Loading packages & cleaning
library(dplyr)
library(ggplot2)
library(broom)
### I tried to load modelsummary but had to install the package first
install.packages("modelsummary")
library(modelsummary)
### loading and cleaning the data
data<-read.csv("data/qog_std_cs_jan26.csv")
### renaming the columns i will use
df = data%>%
  select(country = cname, epi = epi_epi, women_parl = wdi_wip, gov_eff = wbgi_gee, green_seats = cpds_lg)
### to drop observations with missing values, i can use na.omit but it will remove too many observations. i'll save the data with omitted NA as separate dataset. 
df_nona<-na.omit(df)
### only 36 observations remain so i will use a cleaner alternative way with dataset: summmary statistics
nrow(df)
### shows 194 observations/countries in dataset
summary(df)
### shows that there are 158 observations with NA, aligns with number of observations in the df_nona dataset i created with na.omit
print(summary(df))

## Problem 1.2: Exploratory Visualization
### creating a scatterplot with women_parl on x-axis bc it is our independent/explanatory variable to find outcome variable, add linear regression fit
ggplot(df, aes(x=women_parl, y=epi))+
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="Women in Parliament (percentage)", y = "EPI Score")
## What is the relationship between women_parl and epi score?
### Answer: There is a positive trend between the percentage of parliamentary seats held by women and EPI scores (environmental performance). This implies that both variables are associated with development and governance quality. 

## Problem 1.3: Bivariate Regression
## run a bivariate regression between women in parliament and epi scores
m1 = lm(epi~women_parl, data=df)
## now extract the results using broom::tidy
broom::tidy(m1)
## The coefficient on women_parl is 0.308, which means that the EPI score will increase by 0.308 with each additional percentage of women in parliament. 
## to find predicted difference between countries in the 25th and 75th percentiles, we need to generate the quantile numbers. 
p25 = quantile(df$women_parl, 0.25, na.rm = TRUE)
print(p25)
p75=quantile(df$women_parl, 0.75, na.rm=TRUE)
print(p75)
### i wanted to print the results for easy reference as i proceed.
### there are two different methods to generate the predicted EPI score based on the quantile numbers we generated.
### First option: multiply the coefficient by the IQR or Interquartile Range, which is just the distance between the third quartile and the first quartile (both of which we generated).
coef(m1)["women_parl"]*(p75-p25)
### this gives 5.638584.
### Second option: use the predict function
pred = predict(m1, newdata=data.frame(women_parl = c(p25, p75)))
pred[2]-pred[1]
### So this means that the EPI score of a country in the 75th percentile of women_parl (i.e., higher percentage of women in parliament) will be 5.638584 percentage points higher than a country in 25th percentile of women parliamentary shares.

# Problem 1.4: Multiple Regression
## running the bivariate regression between women_parl and epi scores but control for 
lm(epi~women_parl + gov_eff, data=df)
### need to save the model
m2<-lm(epi~women_parl + gov_eff, data=df)
### now extract the results using broom::tidy
broom::tidy(m2)
### How does the coefficient on women_parl change between m1 and m2? 
### ANSWER: When we control for government efficiency, the women_parl coefficient decreases a lot. This means that government efficiency is correlated with a parliament's percentage of women members and that parliament's environmental score, which we did not see in the bivariate regression. This is an example of omitted variable bias - a variable that can predict both the independent and dependent variables.

# Problem 1.5: Demonstrating Omitted Variable Bias
### The OMV says that the bivariate coefficient is generated from the multiple regression coefficients and the coefficient from regressing the omitted variable on the included variable.
### to extract the coefficients we need:
beta1_biva = tidy(m1) %>% filter(term=="women_parl") %>% pull(estimate)
### this is 0.3078078....
beta1_multi = tidy(m2) %>% filter(term == "women_parl") %>% pull(estimate)
### this is 0.0978773....
beta2_multi = tidy(m2) %>% filter(term == "gov_eff") %>% pull(estimate)
### this is 8.710135.....
### Now need to find the auxiliary regression.
aux = lm(gov_eff~women_parl, data=df)
### extract the delta
delta = tidy(aux) %>% filter (term == "women_parl") %>% pull(estimate)
### this is 0.02672798267...
### now verify the OVB formula with rounding
round(beta1_multi + beta2_multi*delta, 4)
### this is 0.3307.
### compare to the left-hand side/beta1_biva, rounding
round(beta1_biva, 4)
### this is 0.3078. both values match, confirming that the OVB formula is correct.
### Why does the coefficient on women_parl change when we add gov_eff?
###ANSWER: Gov_eff is positively correlated with both women_parl and with epi. This inflated the bivariate estimate. It was a positive bias.

# Problem 1.6: Robust Standard Errors
## Use the modelsummary command to print the multi regression results with regular/default/classic standard errors
modelsummary(m2, output = "markdown")
print(modelsummary(m2, output = "markdown"))
## Now do the same model with ROBUST standard errors (used to address homoscedasticity).
modelsummary (m2, vcov="robust", output="markdown")
print(modelsummary (m2, vcov="robust", output="markdown"))
## Compare the SEs between the models. Do the SEs differ substantially? Does it change the conclusions?
### ANSWER: the SEs are different but the conclusions do not change. 

# Problem 1.7: Presenting Results
## Create a table comparing the bivariate and multiple regression models side-by-side, using robust SEs for both models.
modelsummary(list(m1,m2), vcov="robust")
### these tables are saying HC3 for Std Errors but that just means Robust
### Now need to create a coefficient plot using modelsummary::modelplot that compares both models
coef_plot<-modelplot(list("Bivariate" = m1, "Multiple" = m2), vcov = "robust")
### use ggsave to save the plot
ggsave("model_plot.png", plot = coef_plot, width = 6, height = 4, dpi = 300)
### this saved it in my folder for the class
###EXTRA: PROBLEM 1.7 EFFECT SIZE
#How do we know whether the EFFECT of women_parl is big or small?

# HOME PORTION: STAR DATASET
## 2.1: Data Preparation
star<-read.csv("data/star.csv")
summary(star) #i did this to view the columns before i clean
## 2.1.a: Creating factor variable of classtype
star$classtype<-factor(star$classtype, levels=c(1,2,3), labels = c("Small", "Regular", "Regular+Aide"))
## 2.1.b: Creating factor variable of race:
star$race<-factor(star$race, levels=c(1,2,3,4,5,6), labels=c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
## 2.1.c: Creating binary variable
star$small<-ifelse(star$classtype=="Small", 1,0)
summary(star)
## 2.1.d: Reporting the number of observations (missing & non-missing) in g4reading & g4math
summary(star)
### ANSWER: There are 6325 observations total. In g4reading there are 3972 missing (NAs) observations so there are 2353 non-missing observations for this variable. In g4math there are 3930 missing (NAs) observations so there are 2395 non-missing observations for this variable.

# Problem 2.2: Comparing Groups
## 2.2.a: Calculate the mean 4th grade reading score by class type. Which group scores highest?
mathmeans<-star %>%
  group_by(classtype) %>%
  summarise(mean_g4math = mean(g4math, na.rm = TRUE))
print(mathmeans)
### ANSWER: Regular-sized classes score the highest in math. The mean score for this is approximately 710. 
## 2.2.b: Run a bivariate regression on g4reading on small. Interpret the coefficient.
smallreading<-lm(g4reading~small, data=star)
print(smallreading)
### ANSWER: The intercept is 720.3, meaning this is the average score for students in regular and regular+aide classes. The coefficient on small is 3.1. This means that students in small class sizes have average reading scores that are higher by 3.1 points.
## 2.2.c: Verify that the regression coefficient equals the difference in means between small and regular+aide classes. (Hint: compare with the grouped means from part a.)
readingmeans<-star %>%
  group_by(small) %>%
  summarise(mean_g4reading = mean(g4reading, na.rm = TRUE))
print(readingmeans)
### ANSWER: Here, the mean for all non-small classes is 720.29 (same as intercept in regression) and the mean for small-sized classes is 723.39. 720.29+3.1 (coefficient of small in regression) = 723.39. This corroborates the regression.
## 2.2.d: Repeat the bivariate regression for g4math. Is the pattern similar?
smallmath<-lm(g4math~small, data=star)
print(smallmath)
### ANSWER: Here, the intercept (i.e., average math score for non-small classes) is 708.5940. The coefficient on small is 0.5912, meaning that students in small classes will score 0.5912 points higher on math than the non-small students. This does not seem to be a substantial increase compared the the difference that class size makes on reading scores.
### I wanted to double-check with the groupby function on this one, too.
mathmeans2<-star %>%
  group_by(small) %>%
  summarise(mean_g4math = mean(g4math, na.rm = TRUE))
print(mathmeans2)
### This shows that the non-small classes score an average of 708.59 and the average small class score on math is 709.19. 708.59 matches the intercept I yielded in the regression. 708.59+0.5912=709.1812, which matches the mean math score for small classes here. So this was correctly done.

# Problem 2.3: Adding controls
## 2.3.a: Run a multiple regression of g4reading on small, race, and yearssmall.
readingmulti<-lm(g4reading~small + race + yearssmall, data=star)
print(readingmulti)
## 2.3.b: Compare the coefficient on small with the bivariate model. Does it change much? What does this tell you about the quality of the randomization?
### ANSWER: Here in the multivariate regression, the coefficient is -4.00, which is a 7-point difference from the coefficient in the bivariate regression on this. If it were a perfectly randomized study, then adding the controls of race and years (spent in a small class) would not change the coefficient substantially. When we added the controls here, though, the change suggests that randomization was impacted by a student's race and the years that student had spent in a small class prior to the administration of treatment.
## 2.3.c: Interpret the coefficient on yearssmall. What does it capture?
### ANSWER: The coefficient on yearssmall is 2.170 here, which means that with each additional year that a student spent in a small class, they improved their score by 2.170 points. 

# Problem 2.4: Interactions
## 2.4.a: Does the effect of being in a small class differ by race? Fit the following model: lm(g4reading ~ small * race + yearssmall, data = df).
racemodel<-lm(g4reading ~ small * race + yearssmall, data = star)
print(racemodel)
### ANSWER: Yes, the effect of being in a small class size differs substantially by race. White students score 5 points lower when they are in small classes; Black students scored about 1.6 points higher (coefficient on small/White plus coefficient on small:raceBlack); Asian students in small classes score about 52 points lower in small classes; and other races score about 49 points higher.
## 2.4.b: Print the results using broom::tidy().
broom::tidy(racemodel)
## 2.4.c: What is the estimated effect of a small class for White students? For Black students? (Use the coefficients to calculate.)
### ANSWER: The estimated effect of a small class for White students is -5.318; they scored -5.3 points lower in a small class size. Black students, however, scored 1.66 points higher (coefficient on small plus coefficient on small:raceBlack).
## 2.4.d: In a comment, discuss whether the interaction is substantively meaningful.
### ANSWER/COMMENT: In regular-sized/other sized classes, Black students scored 36 points lower than White students. When moved to small classes, however, Black students scored 1.66 points higher than Black students in regular classes and 6.97 points higher than White students in small classes. There seems to be a mildly meaningful substantive effect of class size on Black students' performance.

# Problem 2.5: Presenting Results
## Problem 2.5.a: Create a table with modelsummary() comparing all your reading score models (bivariate, multiple, interaction), using robust standard errors.
modelsummary(list(smallreading, readingmulti, racemodel), vcov="robust")
## Problem 2.5.b: Create a coefficient plot with modelplot() for the three models.
star_plot<-modelplot(list("Bivariate"=smallreading, "Multiple"=readingmulti, "Interaction"=racemodel), vcov = "robust")
print(star_plot)
ggsave("assignment2/star_plot.png", plot = coef_plot, width = 6, height = 4, dpi = 300)
modelsummary(list(smallreading, readingmulti, racemodel), vcov="robust", output="assignment2/star_table.html")

# Problem 2.6: Brief Discussion
## In a comment (5–10 sentences), discuss: a) What does the STAR data suggest about the effect of small class sizes on student achievement? b) Why is this evidence more credible than a typical observational study of class size? c) Are there any limitations or caveats based on what you observed in the data?
### ANSWER: The STAR data suggest that moving students to smaller class sizes will improve their performance on scores. I would think this is because teachers will have more opportunities to give them individualized attention, or the students will have more opportunities to ask questions and receive answers. This was an experiment/randomized controlled trial (RCT), which means that everyone in the treatment and control groups had the exact same chances of being selected to receive treatment. In other words, the treatment of small class size was administered randomly to students. There do seem to be caveats when it comes to race, with differences in test scores in small class sizes different based on a student's race and the years that a student had spent in a small class prior to the administration of treatment.