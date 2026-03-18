# PART 1: IN-CLASS (CARD-KRUEGER MINIMUM WAGE)
## 1.1 DATA SETUP AND EXPLORATION
## loading the data
df=read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
install.packages("modelsummary", dependencies=TRUE, type="binary")
library(modelsummary)
install.packages(c(
  "BMisc",
  "DRDID",
  "fastglm",
  "ggplot2",
  "data.table",
  "pbapply",
  "sandwich",
  "future",
  "future.apply"), dependencies = TRUE, type = "binary")
install.packages("did", dependencies=TRUE, type="binary")
library(did)
## 1.1.a: Create a NJ dummy variable that equals 1 if location is not "PA" or otherwise. 
df$NJdummy<-ifelse(df$location=="PA", 0,1) # I did this bc so many diff variations of NJ by region
## Report the number of restaurants in NJ vs PA using table.
table(df$NJdummy)
### ANSWER: each observation corresponds to a restaurant and its location so this shows that there are 67 PA restaurants and 291 NJ restaurants.
## Now compute the average wageBefore and wageAfter separately for NJ and PA restaurants using group_by() and summarise().
df%>%
  group_by(NJdummy)%>%
  summarise(
    mean_wage_before=mean(wageBefore, na.rm=TRUE),
    mean_wage_after=mean(wageAfter, na.rm=TRUE))

## In a comment, note whether wages in NJ increased relative to PA after the policy change.
### ANSWER/COMMENT: Prior to the policy implementation, both states shared nearly the same mean minimum wage. Afterward, though, wages increased by more than half a dollar in NJ. This confirms that the policy change raised wages in the treated state. 

## 1.1.b: Compute the simple DiD estimate manually using the following steps:
means=df%>%
  group_by(NJdummy)%>%
  summarise(
    before=mean(fullBefore, na.rm=TRUE),
    after=mean(fullAfter, na.rm=TRUE),
    change= after - before
  )
means
nj_change=means$change[means$NJdummy==1]
pa_change=means$change[means$NJdummy==0]
did_est=nj_change-pa_change
cat("DiD estimate:", round(did_est, 3), "\n")
## In a comment, interpret the result in words. What does this number say about the effect of the minimum wage in crease on employment?
### ANSWER/COMMENT: The DiD estimate is the difference in within-group changes. A positive value means full-time employment grew more (or fell less) in NJ than in PA after the minimum wage increase, which contradicts the standard prediction that higher minimum wages reduce employment. 

## 1.1.c: To run regressions, reshape the data to long format (one row per restaurant-period) using the following code. 
library(dplyr)
library(tidyr)

df_long=df%>%
  mutate(id=row_number())%>%
  pivot_longer(
    cols=c(fullBefore, fullAfter),
    names_to="period",
    values_to="full_emp")%>%
  mutate(
    post=ifelse(period=="fullAfter", 1,0),
    NJdummy=ifelse(location!="PA", 1,0))
## Check that the resulting dataset has the correct structure: dflong should equal twice nrow. 
nrow(df_long)
nrow(df)
## In a comment, explain why the long format is needed for the DiD regression.
### ANSWER/COMMENT: There are twice as many rows/observations in the long dataset as there are in the original. The DiD regression requires long format because the interaction post*NJ is the DID estimator, which captures how the within-NJ change in employment (post to pre) differs from the same change in PA. 

## PROBLEM 1.2: DiD REGRESSION
## 1.2.a: Estimate the DiD regression using fixest:
library(fixest)
m_did=feols(full_emp~post*NJdummy, data=df_long, cluster=~id)
## Report the results using modelsummary(). 
modelsummary(m_did)
## Identify and interpret the coefficient on the interaction term post*NJ - this is the DiD estimator. Compare it to your manual calculation from question 1.1b - they should match.
### ANSWER: They do match, both are 2.927. The post coefficient reflects the pre-post change in PA (the counterfactual), the NJ coefficient is the baseline difference between NJ and PA, and the interaction captures the additional change in NJ relative to this baseline trend. 

## 1.2.b: Add chain fixed effects to absorb time-invariant differences across fast food chains:
m_did_fe=feols(full_emp~post*NJdummy|chain, data=df_long, cluster=~id)
## Compare the two models in a single modelsummary table.
modelsummary(
  list("DiD"=m_did, "DiD+Chain FE"=m_did_fe),
  stars=TRUE, gof_map=c("nobs", "r.squared"),
  output="markdown"
)
## Does controlling for chain type change the DiD estimate noticeably? In a comment, explain what the chain fixed effects are absorbing and why controlling for them may or may not matter here.
### ANSWER/COMMENT: Chain fixed effects do not noticeably change the DiD estimate. Chain fixed effects absorb the baseline differences in staffing levels across food chains (e.g., Wendy's might have structurally different employment level than KFC). However, chain types are rougly balanced across both states, so when we control for it there is little impact on the DiD coefficient.

## 1.2.c: In a comment, state the parallel trends assumption for this specific example. What would we need to observe about NJ and PA employment trends in the pre-period to be confident in the DiD estimate? Give one concrete example of something that could violate this assumption (i.e., something that would affect NJ but not PA employment independently of the minimum wage change).
### ANSWER/COMMENT: The parallel trend assumption here is that absent treatment of a minimum wage increase, NJ would have followed the same trajectory as PA because both states share a similar economic pattern and the two surveys were administered close in time, which limits the opportunities for divering trends. An example of a concrete violation that might occur is if NJ were to experience an independent economic shock during this period, e.g., a major employer opens or shuts plants in NJ in between the two survey waves. This would change NJ employment for reasons unrelated to the minimum wage and the DiD estimate would be biased. 

## PROBLEM 1.3 WAGES AS A VALIDATION CHECK
## 1.3.a: Repeat the DiD analysis using wages as the outcome instead of employment. Reshape the data for wages and estimate the model:
df_long_wage=df%>%
  mutate(id=row_number())%>%
  pivot_longer(
    cols=c(wageBefore, wageAfter),
    names_to="period",
    values_to="wage")%>%
  mutate(
    post=ifelse(period=="wageAfter", 1,0),
    NJ=ifelse(location!="PA",1,0))
m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)
## Report the results. Did the minimum wage increase actually raise wages in NJ relative to PA? Is the sign and magnitude of the DiD coefficient what you would expect?
modelsummary(m_wage)
### ANSWER/COMMENT: The coefficient on the interaction here is positive and statistically significant at the p < 0.001 level. I.e., NJ wages rose substantially compared to PA wages after the policy change. The magnitude is consistent with the $0.80 minimum wage increase ($5.05 from $4.25). This is precisely the sign and magnitude one would expect if the law was actually binding.

## 1.3.b: In a comment, explain why the wage result is important for interpreting the employment DiD. If wages had NOT risen in nJ after the law change, what would that imply about the employment result? Why is it reassuring (or not surprising) that wages did rise in NJ?
### ANSWER/COMMENT: The wage DiD serves as a "first stage" or manipulation check. If NJ wages had not risen after the minimum increase, it would be unclear whether this study is truly estimating the effect of a minimum wage change. The law might not have been binding or firms might have already been paying about the new minimum. The fact that wages did rise in NJ gives us confidence that the treatment actually occurred as intended, so the employment DiD can be credibly interpreted as a causal response to the minimum wage increase rather than a spurious or null comparison.

# PART 2: TAKE-HOME: STAGGERED D-I-D
install.packages("did", dependencies = TRUE, type = "binary")
library(did)
data(mpdta)

## 2.1 DATA STRUCTURE AND VISUALIZATION
## 2.1.a: How many countries are in the data?
head(mpdta)
length(unique(mpdta$countyreal))
## How many unique treatment cohorts (distinct values of first.treat) are there? 
length(unique(mpdta$first.treat))
## Use table(mpdta$first.treat) to see how many counties adopted treatment in each year.
table(mpdta$first.treat)
## In a comment, explain what "staggered treatment adoption" means in this context: Why is it a problem to simply compare treated vs untreated counties?
### ANSWER/COMMENT: Staggered treatment adoption refers to a phenomenon where the units of analysis are assigned treatment at different times from one another. It can be problematic because in this case, they differ in the timing, so cannot be said to be exactly comparable to one another.

## 2.1.b: Plot average log teen employment over years, separately for each cohort.
library(dplyr)
library(ggplot2)

mpdta_avg=mpdta%>%
  mutate(cohort=factor(first.treat,
  levels=c(0,2004,2006,2007),
  labels=c("Never treated", "Adopted 2004", "Adopted 2006", "Adopted 2007")))%>%
  group_by(year,cohort)%>%
  summarise(mean_lemp=mean(lemp, na.rm=TRUE))

plot2.1=ggplot(mpdta_avg, aes(x=year, y=mean_lemp, color=cohort))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x="Year", y="Log teen employment", color="Treatment cohort")

## Save the plot with ggsave(). 
ggsave("assignment6/plot2.1.png")

## In a comment, describe the patterns: do the cohorts appear to follow similar trends before their respective treatment years? What happens after treatment? Are there any cohorts whose pre-trends look problematic?

## PROBLEM 2.2 NAIVE TWFE vs CALLAWAY-SANTANNA ESTIMATOR
## 2.2.a: Estimate a naive TWFE model treating all treated counties as a single group. (Check the data, maybe you need to create a time-varying treatment indicator first, call it treated_post. It should indicate treatment units after treatment comes into effect.)
mpdta = mpdta %>%
  mutate(treated_post = ifelse(first.treat > 0 & year >= first.treat, 1, 0))

naive_twfe = feols(lemp ~ treated_post|countyreal+year, data = mpdta)
## Report and interpret the coefficient on treated_post. In a comment, note that this model pools all treatment cohorts together -- what implicit assumption is it making about the treatment effect 
naive_twfe
### ANSWER/COMMENT:

## 2.2.b: Now use the Callaway-Santanna (2021) estimator, which estimates group-time average treatment effects separately for each cohort and time period, using never-treated counties as the control group.
out <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  est_method = "reg",
  control_group = "nevertreated"
)
out

## Report the overall ATT estimate. Is it similar to or different from the naive TWFE estimate?
group_effects = aggte(out, type = "group")
print(group_effects)

## 2.2.c: Examine the event-study version of the Callaway-Santanna results.
es <- aggte(out, type = "dynamic")
summary(es)

## PROBLEM 2.3 PRE-TESTING THE PARALLEL TRENDS ASSUMPTION
## The group-time ATT estimates from att gt() include pre-treatment periods — specifically, ATT(g, t) for t < g — which can be used to construct a formal joint test of the parallel trends assumption.
## 2.3.a: Re-run the CS estimator with bootstrapped standard errors to obtain valid uniform confidence bands and a joint pre-test.
library(did)
cs_btstrp = att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  est_method = "reg",
  control_group = "nevertreated",
  bstrap = TRUE,
  cband = TRUE)

## The summary outpout includes a p-value for the pre-test of the parallel trends assumption. Report this p-value.
summary(cs_btstrp)
### ANSWER/COMMENT: The p-value is 0.16812.
## In a comment, explain what the test is doing: what is the null hypothesis, and what does a large p-value tell us?
### ANSWER/COMMENT:

## 2.3.b: Visualize all group-time ATT estimates - both pre-and post-treatment.
ggdid(cs_btstrp)
## Save the plot. Each panel corresponds to a treatment cohort; negative event-time values are pre-treatment periods.
ggsave("assignment6/plot2.2.png")
## In a comment, describe what you see: are the pre-treatment ATT estimates close to 0 and statistically indistinguishable from zero across all cohorts?
### ANSWER/COMMENT:

## 2.3.c: In a comment (2-3 sentences), reflect on the limitations of pre-testing. Even if we cannot reject parallel trends in the pre-period, can we be certain the assumption holds during the post-treatment period? What is the pre-test actually telling us, and what is it NOT telling us?

## PROBLEM 2.4 COMPARING CONTROL GROUP SPECIFICATIONS
## By default, the CS estimator uses never-treated units as the control group. An alternative is not-yet-treated units - counties that will eventually receive treatment but have not been treated at time t. This expands the control group (more observations, potentially more precision) but introduces a different assumption: that outcomes for not-yet-treated units are unaffected by anticipation of their own future treatment.
## 2.4.a: Re-restimate the CS model using not-yet-treated counties as the control group:
cs_out_nyt=att_gt(
  yname="lemp",
  gname="first.treat",
  idname = "countyreal",
  tname="year",
  xformla= ~ lpop,
  data=mpdta,
  control_group="notyettreated",
  bstrap=TRUE,
  cband=TRUE
)
## Report the overall ATT.
groupeffects2=aggte(cs_out_nyt, type="simple")
print(groupeffects2)
## Compare it to the never-treated estimate from Section 2.2.b. Are they similar or different in sign and magnitude?
### ANSWER/COMMENT:

## 2.4.b: Produce and save an event-study plot for this specification:
cs_dyn_nyt=aggte(cs_out_nyt, type="dynamic")
ggdid(cs_dyn_nyt)
ggsave("assignment6/mpdta_event_study_nyt.pdf", width=7, height=4)
## In a comment, compare the pre-trends and post-treatment patterns to the never-treated event study from Section 2.2.c. Does using the broader control group change the conclusions?
### ANSWER/COMMENT: 

## 2.4.c: In a comment (2-3 sentences), discuss the trade-off between the two control group choices. Under what conditions would you prefer never-treated as the control group? When might not-yet-treated be preferable despite the additional assumption it requires?
### ANSWER/COMMENT:

## PROBLEM 2.5: DISCUSSION: WHY DOES TWFE FAIL IN STAGGERED SETTINGS?
## 2.5.a: In a comment (3-5 sentences), explain intuitively why the TWFE estimator can produce misleading results in staggered DiD settings. What is the "forbidden comparison" problem? Which units get used as the control group in a way that is problematic, and why is that a problem if treatment effects are heterogeneous across cohorts or over time?
### ANSWER/COMMENT:

## 2.5.b: Compare the TWFE estimate from question 2.2a to the Callaway-Sant'anna estimates from question 2.2.b. Are they similar or different? In a comment, based on the event-study pre-trends from question 2.2.c, which estimate do you find more credible and why?