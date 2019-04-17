# orthoml
Code associated with paper: "Orthogonal Machine Learning for Demand Estimation: High-Dimensional Causal Inference in Dynamic Panels"
Semenova, Goldman, Chernozhukov, Taddy (2017) 

# Introduction
## Data
PEAggData.csv contains anonymized grocery sales data from a food distributor. The grocery items are sold at 8 different sites, via 2 different channels (Collection, Delivery), in the years 2012-2017. Using catalog descriptions, we organize the products into a tree (see example below). 
![Figure_1 copy](https://user-images.githubusercontent.com/21160786/56326955-218cee80-6146-11e9-8f4b-2e9aa128ac3d.png)

To preserve the distributor's anonymity, we replaced the names of the nodes by numbers. The resulting data set takes the form:

![Screenshot 2019-04-17 18 22 28](https://user-images.githubusercontent.com/21160786/56324769-d5d64700-613d-11e9-8db5-612dc62871b9.png)

where logmove stands for log weekly sales, logprice is log average week price, and the rest of the fields are self-explanatory.


## Code
### Figure 2: Own price elasticities by level 1 category as estimated by Orthogonal Least Squares

### Figure 3: Own price elasticities by the months of a calendar year as estimated by Orthogonal Least Squares

### Figure 4: Distribution of Own price elasticities for selected Protein Products as estimated by Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares

### Figure 5: Distribution of Own price elasticities for selected Non-Edible Products as estimated by Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares

### Distribution of Own price elasticities for selected Snacks Products as estimated by Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares

### Table 2: Estimated Cross Price Elasticity for Drinks
