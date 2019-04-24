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
`install.packages(c("tictoc","cowplot","xtable","parallel","expm", "foreach", 'gamlr", "glmnet","ggplot2", "dplyr", "plyr","reshape2","tidyr","iterators","assertthat","tidyverse","rmutil")'`

### Figure 3: Own price elasticities as estimated by Double Machine Learning


### Figure 4: Own price elasticities by the months of a calendar year as estimated by Orthogonal Least Squares

To replicate the results, run:

The estimation methods are contained in 

### Figure 5: Distribution of Own price elasticities as estimated by Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares


