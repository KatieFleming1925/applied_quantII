library(sf)
library(spData)
install.packages("spdep")
library(spdep)
install.packages("spatialreg")
library(spatialreg)
data(world)

# IN-CLASS
## PROBLEM 1: SETUP AND OLS BASELINE
## 1.1.a: Load the world dataset. Remove rows with missing gdpPercap or lifeExp, and remove Antarctica. 
world=world[!is.na(world$gdpPercap)&!is.na(world$lifeExp),]
world=world[world$continent!="Antarctica",]
## How many observations remain?
nrow(world)
## ANSWERS/COMMENT: There are 160 observations remaining.
## Log-transform gdpPercap and store it as a new column.
world$log_gdp=log(world$gdpPercap)
## In a comment, report the number of remaining observations and explain why we log-transform GDP per capita. 
## Recall the discussion of skewed distributions and log transformations from earlier in the course.
### ANSWER/COMMENT:

## 1.1.b: Fit an OLS regression of life expectancy (lifeExp) on log GDP per capita. 
ols_fit=lm(lifeExp~log_gdp, data=world)
summary(ols_fit)
## In a comment, report the estimated coefficient on log_gdp. What does it mean substantively?
### ANSWER/COMMENT: The estimated coefficient is 5.5403. 
## Is it statistically significant? 
## ANSWER/COMMENT: Yes, the p-value is 2.67e-10 with 3 stars - it is statistically significantly at the 0.001 level.
## What is the R2?
### ANSWER/COMMENT: The R2 is 0.6472.

##1.1.c: Save the OLS residuals and map them.
world$ols_resid=residuals(ols_fit)

library(ggplot2)
ggplot(world)+
  geom_sf(aes(fill=ols_resid), color="white", linewidth=0.2)+
  scale_fill_gradient2(low="#2166ac", mid="white", high="#d6604d", midpoint=0, name="OLS residual")+
  theme_void()+
  labs(title="OLS residuals: life expectancy~log GDP per capita")

## trying to predict life expectancy based on income per capita. residuals are prediction error. there are a negative prediction error: underpredicting africa, people in africa live longer than we predict. ppeople in africa are living less than they should based on their income. 

## 1.2: SPATIAL WEIGHTS MATRIX
## 1.2.a: Create a queen contiguity neighborhood and row-standardized spatial weights. Recall from class and previous assignment that queen contiguity defines neighbors as any polygon sharing at least one point:
nb=poly2nb(world,queen=TRUE)
listw=nb2listw(nb, style="W", zero.policy=TRUE)
summary(nb)
## In a comment, report how many countries have 0 neighbors.
### ANSWER/COMMENT: 16 countries have 0 neighbors.
## Explain why some countries have no neighbors in a contiguity-based weights matrix.

## 1.2.b: MORAN TEST
moran.test(world$ols_resid, listw=listw, zero.policy=TRUE)
## In a comment, report the Moran's I statistic and p-value. 
### ANSWER/COMMENT: The I statistic is 0.4374 (positive - there is clustering) and the p-value is 8.054e-12.
## Is there statistically significant spatial autocorrelation in the residuals?
### ANSWER/COMMENT: Yes, there is a statistically significant spatial autocorrelation in the residuals.
## What does this imply for OLS - specifically what assumption of OLS is being violated?
### ANSWER/COMMENT: OLS assumes IID, that observations are independently and identically distributed. 

## PROBLEM 1.3: LAGRANGE MULTIPLIER TESTS
### Recall from class that when Moran's I on residuals is significant, we face a choice: use the Spatial Error Model (SEM) or the Spatial Lag Model (SLM)? The Lagrange Multiplier (LM) tests help guide this decision. 
### Run all four tests at once:
lm_tests=lm.RStests(ols_fit, listw=listw, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"), zero.policy=TRUE)

## 1.3.a: Report the test statistics and p-values for LMerr and LMlag. Are both significant?
lm_tests
### ANSWER: The test statistic for LMerr is 52.17 with a p-value of 5.089e-13. The test statistic for LMlag is 0.061576 with a p-value of 0.804. The only one that is statistically significant is therefore LMerr.
## In a comment, recall from class what each of these tests is checking: LMerr tests for spatial dependence in the error term, while LMlag tests for a spatially lagged dependent variable.
### ANSWER/COMMENT: LMerr tests whether there is spatial dependence in the error term, while LMlag tests whether a spatially lagged dependent variable belongs in the model. When both tests are significant, this means that both types of spatial dependence appear to be present in some form when tested individually. In this case, we turn to robust versions to discriminate.
## 1.3.b: Report the robust versions RLMerr and RMlag. These tests control for the presence of the other type of dependence. 
### ANSWER/COMMENT: The RLMerr test statistic is 54.306 with a p-value of 1.716e-13. The RMlag test statistic is 2.1973 with a p-value of 0.1383. 
## Which is more significant?
### ANSWER/COMMENT: In class our decision was to choose the model whose robustness test is more significant. Here, the RLMerr has a more significant p-value, so we will choose this one, meaning that we opt for the SEM model. 

## PROBLEM 1.4: SPATIAL ERROR MODEL (SEM)
## Based on the diagnostics above, fit the SEM using errorsarlm from sdedp
sem_fit=errorsarlm(lifeExp~log_gdp, data=world, listw=listw, zero.policy = TRUE)
summary(sem_fit)
## 1.4.a.: Report the estimated coefficient on log_gdp from the SEM and compare it to the OLS estimate.
### ANSWER/COMMENT: The estimated coefficient on log_gdp is 3.95785.
## Has the coefficient changed much? 
### ANSWER/COMMENT: Yes, the coefficient on log_gdp has gone down by about 1.5 points. This is because with SEM, the error-structure correction absorbs spatial confounding.
## Report the lambda parameter and its p-value. 
### ANSWER/COMMENT: The lambda paramter is 0.76254 with a p-value of less than 2.22e-16. 
## Is it statistically significant?
### ANSWER/COMMENT: Yes, this is statistically significant at the p<0.001 level.

## 1.4.b: In a comment (2-3 sentences), explain what lambda represents in the SEM. Recall from class: the SEM says u=lambdaWu+Epsilon, meaning the error at each unit is partly a function of the neighbors' errors. If lambda>0 and is significant, what does this tell us about the structure of the unmeasured factors driving life expectancy?
### ANSWER/COMMENT: In the SEM, lambda represents the spatial autoregressive process in the disturbances (u=lambdaWu+Epsilon). A positive and significant lambda means that the unmeasured factors driving life expectancy (in this case) are spatially correlated. That is, ommitted variables such as regional disease environments, cultural practices around healtjcare, or cross-border health infrastructure are themselves geographically clustered. The SEM filters this spatial correlation out of the residuals without positing that life expectancy itself directly diffuses across borders.

## 1.4.c: Check whether the SEM has removed the spatial autocorrelation from the residuals. Save the SEM residuals and run Moran's test again:
world$sem_resid=residuals(sem_fit)



moran.test(world$sem_resid, listw=listw, zero.policy=TRUE)
## In a comment, compare this result to the Moran's I on the OLS residuals from question 1.2.b. Has the spatial autocorrelation been removed or substantially reduced?
### ANSWER/COMMENT: The I statistic here reduced substantially from the OLS residuals in question 1.2.b. This means the spatial autocorrelation has been substantially reduced, making the test statistic much closer to zero and the p-value insiginficant. This indicates that the spatial error correction has absorbed most of the geographic clustering that OLS left behind in its residuals. 

## PROBLEM 1.5: DISTANCE-BASED WEIGHTS: AN ALTERNATIVE NEIGHBORHOOD
## So far we have used QUEEN CONTIGUITY to define neighbors: two countries are neighbors if their polygons share at lease one point. But this misses island nations entirely and treats all shared-border pairs as equally connected regardless of distance. An alternative is to define neighbors based on geographic proximity: two countries are neighbors if the distance between their centroids is below a threshold.
## 1.5.a: Compute the centroids of every country and build a distance-based neighborhood in which two countries are neighbors if their centroids are within 300km of each other. Use the following code:
coords=st_centroid(st_geometry(world))   
nb_dist=dnearneigh(coords, d1=0, d2=300)
summary(nb_dist)
## NOTE ON DISTANCES AND PROJECTIONS: Earlier in the course we said that computing distances requires projecting to a planar CRS (e.g. UTM). That advice applies applies when you work within a limited area where a single projection is accurate. Here we have a global dataset: no single planar projection preserves distances everywhere on Earth. The function dnearneigh handles this automatically: when it receives an sf object with a geographic CRS (WGS84), it computes great-circles distances on the ellipsoid, which are accurate worldwide. The 300 km threshold is therefore interpreted in km without needing to project. 
## In a comment, compare this neighborhood to the queen contiguity one from question 1.2.a.
## How many countries now have zero neighbors? 
### ANSWER/COMMENT:
## Is this number higher or lower than before?
### ANSWER/COMMENT:
## Why might that be?
### ANSWER/COMMENT:

## 1.5.b: Create row-standardized weights from the distance-based neighborhood and fit a SEM using the same formula (lifeExp ~ log_gdp):
listw_dist=nb2listw(nb_dist, style="W", zero.policy = TRUE)
sem_dist=errorsarlm(lifeExp~log_gdp, data=world, listw=listw_dist, zero.policy=TRUE)
summary(sem_dist)
## In a comment, report predicted lambda and its p-value. Compare the log_gdp coefficient and predicted lambda from this model to the contiguity-based SEM in question 1.4.a. Are the results substantially different? What does this tell you about the sensitivity of spatial models to the definition of the neighborhood?

## 1.4.c.: Run Moran's I on the residuals of this distance-based SEM (using listwdist). In a comment, does this model also succeed in removing spatial autocorrelation from the residuals? Compare to your answer in 1.4.c.
### ANSWER/COMMENT:

# PART 2: TAKE-HOME (SPATIAL LAG MODEL AND MODEL COMPARISON)
## 2.1 SPATIAL LAG MODEL
## The Spatial Lag Model (also called the Spatial Autoregressive model, or SAR) posits that the outcome itself diffuses across space: y=pWy+XBeta+Epsilon. Fit it using lagsarlm from spatialreg with the same formula, data, and weights as the SEM.
slm_fit=lagsarlm(lifeExp~log_gdp, data=world, listw=listw, zero.policy=TRUE)
## 2.1.a: Report the estimated rho parameter and its p-value and report the coefficient on log_gdp. Is p statistically significant?
summary(slm_fit)
### ANSWER/COMMENT: The estimated rho parameter is -0.0042561, the p-value is 0.805. The coefficient on log_gdp is 5.54820. The p is NOT statistically significant at the reported level. 

## 2.1.b: In a comment (2-3 sentences), interpet predicted p. Recall from class that the SLM captures genuine spatial diffusion: the outcome of unit i is partly determined by its neighbors' outcomes. If predicted p>0, what does this mean about the relationship between a country's life expectancy and its neighbors' life expectancy? 
### ANSWER/COMMENT: Rho signifies the spatial spillover effect, i.e., how much the life expectancy of one country is influenced by the life expectancy of another neighboring country. When the rho is low and the p-value statistically insignificant, there is no significant spatial lag effect. Here, the p-value is not significant. 

## 2.1.c: In a comment, explain why the coefficient on log_gdp in the SLM output is NOT the marginal effect of GDP on life expectancy. Recall from class: solving y=pWy+XB+E for y gives y=(I-pW)^-1(XB+E). What does this equilibrium matrix (I-pW)^-1 imply for how a change in xi propagates through the network?
### ANSWER/COMMENT: In the SLM, the presence of a spatial lag means that the y appears on both sides of the equation. The coefficient on log_gdp is the "pre-spatial" or direct internal impact, which does NOT account for spatial spillovers. Y is determined simultaneously across the map, a change in X causes a chain reaction through the spatial weights matrix. The (I-rhoW)^-1 matrix represents a spatial multiplicities effect in which a change in one country's GDP propagates through tr weights matrix W, affecting neighbors, neighbors-of-neighbors and so on. Additional steps need to be taken in order to account for marginal effects.

## PROBLEM 2.2 DIRECT AND INDIRECT EFFECTS:
## 2.2.a: Compute the equilibrium direct and indirect effects using the impacts function, passing the SLM fit and the spatial weights. Use R = 500 for simulation-based standard errors (and set a seed for reproducibility). 
set.seed(123)
imp=impacts(slm_fit, listw=listw, R=500)
print(imp$res)
## In a comment, report the direct effect, the indirect effect, and the total effect of log_gdp. How does the direct effect compare to the raw log_gdp coefficient from the SLM output and to the OLS coefficient?
### ANSWER/COMMENT: The direct effect is 5.548223. The indirect effect is -0.02353893. The total effect is 5.524685. The direct effect is almost the same as the raw SLM and OLS coefficients.

## 2.2.b: In a comment (2-3 sentences), explain the substantive meaning of the indirect effect. Recall from class: the indirect effect captures the spillover from unit i's x to all other units' y, after the spatial feedback loop reaches equilibrium. If log_GDP per capita in Country A increases by 1 unit, what does the indirect effect say about life expectancy in neighboring countries?
### ANSWER/COMMENT: The indirect effect reflects the spatial spillover; here, it suggests that an increase in log GDP per capita of country A by 1 unit will indirectly decrease the life expectancy of a neighboring country by 0.02 units. However, because this pvalue is so insignificant, the countries' wealth does not have a manginful spillover impact on other countries in this dataset. 

## 2.2.c: The total effect is larger than the direct effect. In a comment, explain whether this is an expected feature of the SLM. Under what conditions would the indirect effect be larger or smaller? (Hint: think about what happens to the spillover term as p approaches 0 versus as p grows larger.)
### ANSWER/COMMENT: Typically in an SLM, the total effect is larger than the direct effect because it includes both the direct effect and the indirect spillover effects through the spatial network. Here, though, the negative INDIRECT effect results in the opposite. When rho approaches 0, the indirect effect vanishes. When it approaches 1, the indirect effect grows exponentially because of the spatial multiplicities. 

## PROBLEM 2.3: MODEL COMPARISON
## 2.3.a: Compare OLS, SEM, and SLM using AIC. Lower AIC indicates better fit, penalized for model complexity. In a comment, report the three AIC values. Which model has the lowest AIC? Does this agree with your LM-test-based model choice in question 1.3b?
AIC(ols_fit, sem_fit, slm_fit)
### ANSWER/COMMENT: The AIC for the OLS_fit is 965.9880, for the sem_fit is 894.7021, and for the slm_fit 967.9270. The sem_fit therefore has the lowest AIC model. Looking at the LM-test-based model choice in question 1.3.b, this does align with our choice of the SEM model. 

## 2.3.b: Write a short summary paragraph as a comment in your R script. 
### ANSWER/COMMENT: The highly significant Moran's I test shows that in the OLS residuals there was spatial autocorrelation. This indicates that countries with similar life expectancy are clustered geographically. Based on the LM tests, the best spatial model is the SEM because the robust LM error statistic was significant. The Robust LM-Lag was insignificant. This suggests that spatial dependence lies in the omitted variables rather than a direct lag of the dependent variable. As the coefficient for logGDP stayed consistent across the three models, we can suggest that wealth is a robust predictor of life expectancy regardless of spatial specification. The SLM results showed an insignificant rho and a near-zero indirect effect, which implies that there is little evidence of life expectancy spillover across borders after a country's own GDP is accounted for. Queen contiguity (which weights for country level data)'s major limitation is that it ignores geographic closeness between countries that are separated by small bodies of water. It treats these countries as if they have no neighbors.  

## 2.4 EXTENSION: SPATIAL DURBIN MODEL (OPTIONAL/BONUS)
## The Spatial Durbin Model (SDM) nests both SEM and SLM by including a spatially lagged dependent variable AND spatially lagged covariates. It is estimated with lagsarlm by adding the Durbin = TRUE argument.
## 2.4.a.: Fit the SDM. 
sdm_fit=lagsarlm(lifeExp~log_gdp, data=world, listw=listw, Durbin=TRUE, zero.policy=TRUE)
## Inspect the output: you will see a coefficient for log gdp and a separate coefficient for lag.log gdp (the spatially lagged version, W×log gdp).
summary(sdm_fit)
## Is lag.log_gdp statistically significant? In a comment, explain what a significant lag.log_gdp coefficient would mean substantively: does it suggest that a neighbor's GDP predicts a country's life expectancy, beyond what the country's own GDP already explains?
### ANSWER/COMMENT: The coefficient on lag.log_gdp is -3.82746 and it is indeed statistically significant at the p<0.001 level, indicating that the GDP per capita of neighboring countries has an additional effect on a country's life expectancy beyond its own GDP.

## 2.4.b: Compare the AIC of the SDM to those of the SEM and SLM from question 2.3.a. In a comment, is the added complexity of the SDM (one extra paramter) justified by the improvement in fit? Use the AIC values to support your answer.
AIC(sem_fit, slm_fit, sdm_fit)
### ANSWER/COMMENT: The AIC for the sem is 894.7021, 967.9270 for the slm, and 939.8333 for sdm, so the sem has the lowest AIC. The difference between the SEM and the SDM is so substantial that it does not justify the added complexity of the SDM. The SEM is still the best model.    
