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

predictions(m_mlogit, by="economic.cond.national")


## 1.2.c: The multinomial logit assumes Independence of Irrelevant Alternatives (IIA): the odds ratio between any two alternatives is unaffected by the presence or absence of other alternatives. Recall from class the red bus / blue bus example, where IIA fails because two alternatives (red bus and blue bus) are close substitutes. In a comment of 2–3 sentences, explain what IIA means for this application with Conservative, Labour, and Liberal Democrat as alternatives. Do you think IIA is likely to hold here — or are any two of these parties close substitutes in the minds of British voters? Explain your reasoning.
### ANSWER/COMMENT:

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

## 1.3.b: Fit a Poisson regression of art on all predictors:
m_pois=glm(art~fem+mar+kid5+phd+ment, data=bioChemists, family=poisson)
summary(m_pois)
exp(coef(m_pois)["ment"])
## In a comment, answer the following two questions: (1) Report the coefficient on ment and exponentiate it with exp() to obtain the incidence rate ratio (IRR). Interpret it: a one-unit increase in mentor articles multiplies expected student articles by approximately how much? (2) Report the residual deviance and degrees of freedom from the summary() output and compute their ratio. Recall from class that under a correctly specified Poisson model this ratio should be close to 1; a ratio substantially above 1 (say, > 2) suggests overdispersion.
### ANSWER/COMMENT:

## 1.3.c: Test for overdispersion formally:
dispersiontest(m_pois)
## In a comment, report the estimated dispersion parameter and the p-value. Is there statistically significant evidence of overdispersion? What does this imply for the validity of the Poisson standard errors you computed above?

## PROBLEM 1.4: NEGATIVE BINOMIAL REGRESSION
## The negative binomial (NB) model generalizes Poisson by adding a dispersion parameter θ that allows the variance to exceed the mean: Var(Yi) = µi + µ2i/θ. When θ → ∞, the NB reduces to Poisson. A small estimated θ indicates severe overdispersion; a large θ indicates the extra dispersion is modest.

## 1.4.a: Fit the negative binomial model with the same formula:
m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment, data = bioChemists)
summary(m_nb)
## In a comment, compare the coefficient on ment to the Poisson estimate from the Poisson model above. Has it changed substantially? Report the estimated overdispersion parameter theta from the NB output. Is the overdispersion modest or severe?
### ANSWER/COMMENT:

## 1.4.b: Compare model fit using AIC:
AIC(m_pois, m_nb)
## In a comment, report both AIC values. Which model has the lower AIC? Recall from earlier in the course that AIC penalizes model complexity, so a lower AIC for the NB model (which has one additional parameter) means the improvement in fit outweighs the added complexity. What does this comparison imply: is overdispersion a problem worth addressing for this dataset?
### ANSWER/COMMENT:

## 1.4.c: Compute predicted article counts for male vs. female researchers, holding all other variables at their sample means:
predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))
## In a comment, report the predicted number of articles for men and women (with confidence intervals). How large is the gender gap in predicted publications? Is this difference statistically distinguishable given the uncertainty intervals?
### ANSWER/COMMENT:

## 1.4.d: Write a short summary paragraph as a comment in your R script (4–6 sentences). Cover all of the following: (1) whether Poisson regression is adequate for this dataset or whether the negative binomial is needed, and why; (2) the interpretation of the ment incidence rate ratio — what does mentor productivity tell us about student productivity?; (3) which predictors are statistically significant in the negative binomial model; (4) one substantive conclusion about the factors driving publication productivity among PhD students in biochemistry.
### ANSWER/COMMENT:

# PART 2: TAKE-HOME (SURVIVAL ANALYSIS)

## PROBLEM 2.1: KAPLAN-MEIER SURVIVAL CURVES
## 2.1.a: Explore the data. In a comment, report the total number of observations, the number of events (deaths), and the number of censored cases. What proportion of patients are censored? Is this a lot or a little? Think about what this means: the censored patients’ true survival times are unknown but at least as long as their observed times.
library(survival)
lung=survival::lung
nrow(lung)
## COMMENT/ANSWER: There are 228 observations. 
