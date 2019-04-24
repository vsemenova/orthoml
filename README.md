# orthoml
Code associated with paper: "Orthogonal Machine Learning for Demand Estimation: High-Dimensional Causal Inference in Dynamic Panels"
Semenova, Goldman, Chernozhukov, Taddy (2017) 

# Introduction
## Data
AggData*.csv for * in {Drinks, DairyPart1, DairyPart2, NonEdible, Snacks} contains anonymized grocery sales data from a food distributor. The grocery items are sold at 8 different sites, via 2 different channels (Collection, Delivery), in the years 2012-2017. Using catalog descriptions, we organize the products into a tree (see example below)
![Figure_1 copy](https://user-images.githubusercontent.com/21160786/56327155-1b4b4200-6147-11e9-8837-694417ae332b.png)

To preserve the distributor's anonymity, we replaced the names of the nodes at Level 2 and below by numbers (for Drinks Level 3 and below) by numbers. We also added lags of sales and prices.  The resulting data set takes the form:

![Screenshot 2019-04-24 18 05 08](https://user-images.githubusercontent.com/21160786/56697127-e9445d80-66bb-11e9-95b6-4fb137841df2.png)



## Code

To replicate the results:

1. Download the github repo 
2. Open Rstudio (or R) and run
`install.packages(c("tictoc","cowplot","xtable","parallel","expm", "foreach", 'gamlr", "glmnet","ggplot2", "dplyr","plyr","reshape2","tidyr","iterators","assertthat","tidyverse","rmutil"))`

### Figure 3: Own price elasticities for categories as estimated by Orthogonal Least Squares (Double Machine Learning)
3. Open /orthoml-master/src/Figure3.R and set directoryname to the location of downloaded file. From the shell/or in R, `Figure3.R`. 

The code produces the estimate and 95% confidence interval for the average price elasticity for each category in {Drinks, Dairy, NonEdible, Snacks}. A plot example for Drinks is given below

![BoxDrinksLevel1](https://user-images.githubusercontent.com/21160786/56698438-ee0b1080-66bf-11e9-99de-47732256c928.png)

### Figure 4: Own price elasticities by the months of a calendar year as estimated by  Orthogonal Least Squares (Double Machine Learning)

4. Open /orthoml-master/src/Figure3.R and set directoryname to the location of downloaded file. From the shell/or in R, `Figure4.R`. 

The code produces the estimate and 95% confidence interval for the average price elasticity by calendar month for each category in {Dairy, NonEdible, Snacks, Sodas, Water}. A plot example is given below

### Figure 5: Distribution of Own price elasticities as estimated by Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares 

4. Open /orthoml-master/src/Figure3.R and to the location of downloaded file. From the shell/or in R, `Figure5.R`. 

The code produces a histogram of estimates for the average price elasticity for categories, aggregated at Level2, Level 3, Level4, grouped by color at Level1. A plot example is given below


We see that Lasso estimates are most concenrated (shrinked towards homogenous specification), Orthogonal Least Squares  is most dispersed (and least precise), and  Double Orthogonal Ridge is in the middle. 

# References:

"Double/Debiased Machine Learning for Treatment and Causal Parameters" (Victor Chernozhukov, Denis Chetverikov, Mert Demirer, Esther Duflo, Christian Hansen, Whitney Newey, James Robins), 2017, https://arxiv.org/abs/1608.00060

"Orthogonal Machine Learning for Demand Estimation: High-Dimensional Causal Inference in Dynamic Panels"
Vira Semenova, Matt Goldman, Victor Chernozhukov, Matt Taddy, 2017, https://economics.mit.edu/files/15984 

