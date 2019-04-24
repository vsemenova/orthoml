# orthoml
Code associated with paper: "Orthogonal Machine Learning for Demand Estimation: High-Dimensional Causal Inference in Dynamic Panels"
Semenova, Goldman, Chernozhukov, Taddy (2017) 

# Introduction
## Data
PEAggData.csv contains anonymized grocery sales data from a food distributor. The grocery items are sold at 8 different sites, via 2 different channels (Collection, Delivery), in the years 2012-2017. Using catalog descriptions, we organize the products into a tree (see example below). To preserve the distributor's anonymity, we replaced the names of the nodes by numbers. 


![Figure_1 copy](https://user-images.githubusercontent.com/21160786/56327155-1b4b4200-6147-11e9-8837-694417ae332b.png)

The resulting data set takes the form:

![Screenshot 2019-04-24 18 05 08](https://user-images.githubusercontent.com/21160786/56697127-e9445d80-66bb-11e9-95b6-4fb137841df2.png)



## Code
### Figure 2: Own price elasticities by level 1 category as estimated by Orthogonal Least Squares
To replicate the results, run:

The estimation methods are contained in 

### Figure 3: Own price elasticities by the months of a calendar year as estimated by Orthogonal Least Squares

To replicate the results, run:

The estimation methods are contained in 

### Figure 4-6: Distribution of Own price elasticities as estimated by Orthogonal Lasso, Double Orthogonal Ridge, and Orthogonal Least Squares
To replicate the results, run:

The estimation methods are contained in 

### Figure 4: Protein Products
where k=4
### Figure 5: Non-Edibles
where k=5
### Figure 6: Snacks
where k=6

### Table 2: Estimated Cross Price Elasticity for Drinks
To replicate the results, run:

The estimation methods are contained in 
