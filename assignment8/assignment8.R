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
