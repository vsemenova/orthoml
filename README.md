# orthoml
Code associated with paper: "Orthogonal Machine Learning for Demand Estimation: High-Dimensional Causal Inference in Dynamic Panels"
Semenova, Goldman, Chernozhukov, Taddy (2017) 

# Introduction
## Data
PEAggData.csv contains anonymized grocery sales data from a food distributor. The grocery items are sold at 8 different sites, via 2 different channels (Collection, Delivery), in the years 2012-2017. Using catalog descriptions, we organize the products into a tree (see Figure 1 below). 


To preserve the distributor's anonymity, we replaced the names of the nodes by numbers. The resulting data set takes the form:

![Screenshot 2019-04-17 18 22 28](https://user-images.githubusercontent.com/21160786/56324769-d5d64700-613d-11e9-8db5-612dc62871b9.png)

where logmove stands for log weekly sales, logprice is log average week price, and the rest of the fields are self-explanatory.


## Code
### Figure 2: Own price elasticities by level 1 category as estimated by orthogonal least squares

### Figure 3: Own price elasticities across the months of a calendar year as estimated by orthogonal least squares

### Figure 4: Own price elasticities (using categorical dummies up through level 4) for selected Protein Products. Estimators are
###  Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares

### Figure 5: Own price elasticities (using categorical dummies up through level 4) for selected Non-Edible Products. Estimators are
###  Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares

### Figure 6: Own price elasticities (using categorical dummies up through level 4) for selected Snacks. Estimators are
###  Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares

### Table 2: Cross Price Elasticity for Drinks
