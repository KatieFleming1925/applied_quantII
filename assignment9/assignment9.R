library(carData)
library(MASS)
library(nnet)
library(marginaleffects)
data(BEPS)
library(pscl)
library(AER)
library(MASS)
data(bioChemists)

# PROBLEM 1.1: ORDERED LOGIT: PERCEPTIONS OF THE NATL ECONOMY
## 1.1.a: Explore the economic.cond.national variable and convert it to an ordered factor:
table(BEPS$economic.cond.national)
BEPS$econ_ord=factor(BEPS$economic.cond.national, ordered=TRUE)
summary(BEPS$econ_ord)

## 1.1.b: Fit an ordered logit model predicting econ_ord from age, gender, Europe, and politicalknowledge.
m_ologit=polr(econ_ord~age+gender+Europe+political.knowledge, data=BEPS, Hess=TRUE)
summary(m_ologit)

## 1.1.c: Compute average marginal effects (AMEs) using marginal effects:
avg_slopes(m_ologit)

## 1.1.d: Compute predicted probabilities for the five response categories at the mean of all covariates, separately for male and female respondents:
predictions(m_ologit, newdata=datagrid(gender=c("female","male")))


# PROBLEM 1.2 MULTINOMIAL LOGIT: VOTE CHOICE
## 1.2.a: Set Conservative as the reference category and fit a multinomial logit predicting vote from economic assessments and leader evaluations:
BEPS$vote=relevel(BEPS$vote, ref="Conservative")
m_mlogit=multinom(vote~economic.cond.national+Blair+Hague+Kennedy+Europe, data=BEPS,trace=FALSE)
summary(m_mlogit)

## 1.2.b: Compute AMEs across all predictors and all outcome categories. 
avg_slopes(m_mlogit)
## In a comment, report the AME of Blair on the probability of voting Labour. Interpret it in plain language: holding other variables constant, how does a one-unit increase in Blair approval change the probability of voting Labour on average across respondents?
### ANSWER/COMMENT: The AME of Blair on the probability of voting Labour is positive and substantial. In other words, a one-unit increase in approval (on the 1-5 scale) is associated with an increase in the average probability of voting Labour. This indicates a personalization of vote choice during this election: feelings toward the party leader was a strong driver of vote choice.

predictions(m_mlogit, by="economic.cond.national")


## 1.2.c: The multinomial logit assumes Independence of Irrelevant Alternatives (IIA): the odds ratio between any two alternatives is unaffected by the presence or absence of other alternatives. Recall from class the red bus / blue bus example, where IIA fails because two alternatives (red bus and blue bus) are close substitutes. In a comment of 2–3 sentences, explain what IIA means for this application with Conservative, Labour, and Liberal Democrat as alternatives. Do you think IIA is likely to hold here — or are any two of these parties close substitutes in the minds of British voters? Explain your reasoning.
### ANSWER/COMMENT: The odds ratio between any two alternatives (here, Labour vs Conservative) should be unaffected by the presence or characteristics of a third alternative (here, Liberal Dems). In the bus analogy, IIA fails because two alternatives are near-perfect substitutes and removing one simply shifts its probability to the other rather than distributing it proportionally. For British party choice, IIA is a moderate concern: Labour and the Liberal Democrats are both centre-left parties, sharing some ideological space, so some voters may treat them as partial substitutes in a way IIA cannot accommodate. The Conservatives, however, occupy a clearly distinct ideological position (right-wing), so the three-party menu is not as degenerate as two buses of different colours. Overall, IIA is plausible for Conservative vs. the others but is a more legitimate worry for the Labour/Liberal Democrat distinction.

## We now analyze the number of articles published by biochemistry PhD students in the last three years of their doctorate, using the bioChemists dataset from the pscl package. The outcome (art) is a non-negative integer count. Recall from class that count outcomes have a natural lower bound of zero and cannot take negative values, which makes OLS inappropriate. The natural starting model is Poisson regression; we then diagnose and address overdispersion using the negative binomial.

## 1.3: POISSON REGRESSION: PUBLICATION COUNTS
## 1.3.a: Explore the outcome variable art:
summary(bioChemists$art)
var(bioChemists$art)
pdf("assignment9/art_histogram.pdf", width=6, height=4)
hist(bioChemists$art, breaks=20, main="Distribution of articles", xlab="Number of articles", col="gray80")
dev.off()

library(ggplot2)
library(pscl)

data("bioChemists")

ggplot(bioChemists, aes(x = art)) +
  geom_histogram(binwidth = 1, fill = "#294b66", color = "white") +
  theme_minimal() +
  labs(
    title = "Publications in last 3 years of PhD",
    x = "Number of articles",
    y = "Count"
  )
## In a comment, report the mean and variance of art. A key diagnostic for count data is whether the variance substantially exceeds the mean - this is called OVERDISPERSION and violates the Poisson assumption that mean equals variance.
## Note whether you observe this pattern here.
### ANSWER/COMMENT: Art is right-skewed with a mean of 1.69 and a variance of about 3.71, which is about twice the mean. Under the Poisson assumption, the variance should EQUAL the mean: if there is a ratio above 1, it indicates overdispersion. So we should see here that a standard Poisson model would underestimate uncertainty here. 

## 1.3.b: Fit a Poisson regression of art on all predictors:
m_pois=glm(art~fem+mar+kid5+phd+ment, data=bioChemists, family=poisson)
summary(m_pois)
exp(coef(m_pois)["ment"])
## In a comment, answer the following two questions: (1) Report the coefficient on ment and exponentiate it with exp() to obtain the incidence rate ratio (IRR). Interpret it: a one-unit increase in mentor articles multiplies expected student articles by approximately how much? (2) Report the residual deviance and degrees of freedom from the summary() output and compute their ratio. Recall from class that under a correctly specified Poisson model this ratio should be close to 1; a ratio substantially above 1 (say, > 2) suggests overdispersion.
### ANSWER/COMMENT: The incident rate ratio is 1.026, with each additional article published by the mentor associated with a multiplicative increase in expected student articles by that factor. The effect is positive, which suggests that the more productive mentors slightly boost their students' output. The residual deviance is substantially larger than the residual degrees of freedom, which is another clear diagnostic signal of overdisperson. So, the Poisson model does not adequately capture the variation in publication counts.

## 1.3.c: Test for overdispersion formally:
dispersiontest(m_pois)
## In a comment, report the estimated dispersion parameter and the p-value. Is there statistically significant evidence of overdispersion? What does this imply for the validity of the Poisson standard errors you computed above?
### ANSWER/COMMENT: The estimated dispersion paramter here is 1.82454 which is way above 1 with a pvalue way less than 0.001, so there is indeed statistically significant evidence of overdispersion. This means that the Poisson standard errors are too small and that the model underestimates uncertainty, inflates the test statistics, and produces p-values that are misleadingly small. We need to do the negative binomial model to account for overdispersion.

## PROBLEM 1.4: NEGATIVE BINOMIAL REGRESSION
## The negative binomial (NB) model generalizes Poisson by adding a dispersion parameter θ that allows the variance to exceed the mean: Var(Yi) = µi + µ2i/θ. When θ → ∞, the NB reduces to Poisson. A small estimated θ indicates severe overdispersion; a large θ indicates the extra dispersion is modest.

## 1.4.a: Fit the negative binomial model with the same formula:
m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment, data = bioChemists)
summary(m_nb)
## In a comment, compare the coefficient on ment to the Poisson estimate from the Poisson model above. Has it changed substantially? Report the estimated overdispersion parameter theta from the NB output. Is the overdispersion modest or severe?
### ANSWER/COMMENT: Here, the ment coefficient is similar to the Poisson estimate, so we can assume the point estimate is reasonably stable. The difference is in the standard errors, with this model producing more honest uncertainty estimates. 

## 1.4.b: Compare model fit using AIC:
AIC(m_pois, m_nb)
## In a comment, report both AIC values. Which model has the lower AIC? Recall from earlier in the course that AIC penalizes model complexity, so a lower AIC for the NB model (which has one additional parameter) means the improvement in fit outweighs the added complexity. What does this comparison imply: is overdispersion a problem worth addressing for this dataset?
### ANSWER/COMMENT: The negative binomial model has a substantially lower AIC than the Poisson. Under AIC, the improvement in fit compensates for the added complexity. This confirms that overdispersion is a genuine feature of the data and not just noise. The negative binomial is the more appropriate mdoel for these publication counts.

## 1.4.c: Compute predicted article counts for male vs. female researchers, holding all other variables at their sample means:
predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))
## In a comment, report the predicted number of articles for men and women (with confidence intervals). How large is the gender gap in predicted publications? Is this difference statistically distinguishable given the uncertainty intervals?
### ANSWER/COMMENT: The predicted number of articles for men exceeds that for women, with the confidence intervals communicating whether this gender gap is statistically distinguishable: if the intervals do not overlap then the difference is significant. The gap here reflects a persistent within-group gender difference in publication productivity.

## 1.4.d: Write a short summary paragraph as a comment in your R script (4–6 sentences). Cover all of the following: (1) whether Poisson regression is adequate for this dataset or whether the negative binomial is needed, and why; (2) the interpretation of the ment incidence rate ratio — what does mentor productivity tell us about student productivity?; (3) which predictors are statistically significant in the negative binomial model; (4) one substantive conclusion about the factors driving publication productivity among PhD students in biochemistry.
### ANSWER/COMMENT: Poisson is NOT adequate for this dataset. The variance-to-mean ratio of art was nearly double, the residual deviance far exceeds the degrees of freedom, and the formal dispersion test rejected dispersion with a very insignificant pvalue. The negative binomial model showed a lower AIC and more reliable standard errors. The mentor's productivity has a positive and statistically significant effect, with an IRR above 1. Each additional mentor article is associated with a modest multiplicative increase in expected student articles, suggesting that a productive mentor does help student productivity. Women produce fewer articles, and with each child under 5 this decreases. PhD program presitge and marital status are not statistically significant in the negative binomial model. Overall, mentor, gender, and family seem to shape productivity. 

# PART 2: TAKE-HOME (SURVIVAL ANALYSIS)

## PROBLEM 2.1: KAPLAN-MEIER SURVIVAL CURVES
## 2.1.a: Explore the data. In a comment, report the total number of observations, the number of events (deaths), and the number of censored cases. What proportion of patients are censored? Is this a lot or a little? Think about what this means: the censored patients’ true survival times are unknown but at least as long as their observed times.
library(survival)
lung=survival::lung
head(lung)
summary(lung) #descriptive for each column/variable
nrow(lung) #number of observations
sum(lung$status==1)
sum(lung$status==2)
63/228
## COMMENT/ANSWER: There are 228 observations. There are 165 events/deaths and 63 censored cases, i.e., 63 patients lived beyond the period of observation for the data. 0.2763158 or 27% or 28% are therefore censored. This is more than a quarter of the dataset so I would say that it is a lot of unknown.

## 2.1.b: Estimate the overall Kaplan-Meier survival curve using survfit() with formula Surv(time,dead) ~ 1. The summary() output shows, at each event time, the number at risk, the number of events, the estimated survival probability, and the confidence interval. In a comment, report the estimated median survival time. What does this number mean in plain language?
fit_overall=survfit(Surv(time,status)~1, data=lung)
fit_overall
### COMMENT/ANSWER: The estimated median survival time is 310. When we say this, we are saying that this is the amount of time that 50% of patients live. 

## 2.1.c: Estimate separate Kaplan-Meier curves by sex using survfit() and plot them with ggplot2. Hint: use broom::tidy() to convert the survfit object to a data frame, then plot with geom step() and geom ribbon() for confidence intervals. Save the plot as a PDF. Also run a log-rank test using survdiff(). In a comment, describe what you see: which group survives longer? Does the confidence interval for the two groups overlap? Report the log-rank test p-value and explain what it tests (whether the survival curves are statistically different).
fit_sex=survfit(Surv(time,status==2)~sex, data=lung)
fit_sex

library(broom)
km_data=tidy(fit_sex)

km_data$sex_label=ifelse(km_data$strata=="sex=1", "Male", "Female")
summary(km_data)

km_plot=ggplot(km_data, aes(x=time, y=estimate, color=sex_label, fill=sex_label))+
  geom_step(size=1, direction="hv")+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2, color=NA)+
  labs(
    title="Kaplan-Meier Survival Curves by Sex",
    subtitle="Lung Cancer Dataset (N=228)",
    x="Time (Days)",
    y="Survival Probability",
    color="Sex",
    fill="Sex"
  )

km_plot

ggsave("assignment9/km_by_sex.pdf", plot=km_plot)

logrank_test=survdiff(Surv(time,status)~sex, data=lung)
logrank_test

### ANSWER/COMMENT: The log-rank test shows that women survive longer than men, which this graph reflects visually by showing the women's curve above the men's. There is a slight overlap between the curves but the curves are still distinct and separate. The p-value of the log-rank test is 0.001, which shows that it is statistically significant to say there is a difference between men's and women's survival rates. The log-rank tests whether the survival curves for two groups are equal over the span of time.

## PROBLEM 2.2: COX PROPORTIONAL HAZARDS MODEL
## 2.2.a: Fit a Cox proportional hazards model predicting survival from age, sex, and ph.ecog using coxph() from survival. The output shows both raw coefficients (log-hazard scale) and exponentiated coefficients (hazard ratios). In a comment, report and interpret the hazard ratio for sex. Recall from class: a hazard ratio below 1 means lower hazard (longer survival), above 1 means higher hazard (shorter survival). What does the hazard ratio for sex tell us about survival differences between men and women? Is it statistically significant?
cox_model=coxph(Surv(time,status)~age+sex+ph.ecog,data=lung)
print(cox_model)
cox_summary=tidy(cox_model, exponentiate=TRUE)
cox_summary
### coefficient estimate is 0.575 with a p-value of -0.000986. The coefficient estimate tells us the hazard ratio: subtract coefficient from 100. 
confint(cox_model)
exp(confint(cox_model))
### ANSWER/COMMENT: The hazard ratio for sex is 0.575 or 57.5%, which means that women have about a 42.5% lower hazard of death compared to males. A hazard ratio below 1 implies a lower risk of death and therefore a higher expected survival. here, the pvalue is statistically significant.

## 2.2.b: Interpret the hazard ratio for ph.ecog. n a comment, explain what a one-unit increase in ECOG performance score (i.e., moving toward worse physical functioning) does to the hazard of death. Express this as a percentage change (e.g., “X% higher/lower hazard”).
### ANSWER/COMMENT: ECOG scores communicate physical health, with higher ones indicating worse physical conditions. The coefficient on ph.ecog is 1.59, meaning that a one-unit increase in ECOG performance score is associated with a 59% higher hazard of death. Here, patients with worse health have a higher mortality risk. It is statistically significant and the confidence interval is between 1.27 and 1.99 s it does not include 1.

## 2.2.c:  The Cox model assumes proportional hazards: the effect of each covariate is constant over time. Test this assumption using cox.zph(). In a comment, report the p-value for each covariate and the global test. A significant p-value suggests the proportional hazards assumption is violated for that variable. Do any variables violate the assumption? If so, what would this mean in substantive terms (e.g., the effect of age changes over the course of the disease)?
ph_test=cox.zph(cox_model)
ph_test
### ANSWER/COMMENT: The p-value for age is 0.66, 0.13 for sex, 0.15 for ph.ecog, and 0.22 for the GLOBAL test. None of these are statistically significant, so the proportional hazards assumption is NOT violated for these variables. The effects of age, sex, and performance status are presumably constant on the hazard of death over time.

## 2.2.d: Write a short summary paragraph as a comment in your R script (4–6 sentences). Cover: (1) whether the Kaplan-Meier analysis suggested survival differences by sex; (2) which predictors are significant in the Cox model and the direction of their effects; (3) whether the proportional hazards assumption holds; (4) one substantive conclusion about factors predicting lung cancer survival.
### ANSWER/COMMENT: The KM analysis showed survival differences by sex, with women living consistently longer than men. In the Cox model, sex and ECOG performance were shown to be statistically significant predictors (but not age). There, females had a lower hazard of death than males; higher ECOG scores had substantially higher mortality risk. The proportional hazards assumption does hold; none of the p-values on the variables in the COX test were statistically significant. With all this taken into account, sex and baseline health are the primary drivers of survival differences, with poorer baseline health increasing earlier death across the board.  