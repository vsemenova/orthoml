# orthoml
Code associated with paper: Orthogonal Machine Learning for Demand Estimation: High-Dimensional Causal Inference in Dynamic Panels,
Semenova, Goldman, Chernozhukov, Taddy (2017)

Data: PEAggData.csv contains anonymized grocery sales data from a food distributor. The grocery items are sold at 8 different sites, via 2 different channels (Collection, Delivery), in the years 2012-2017. Using catalog descriptions, we organize the products into a tree (see, e.g. page 2 of the paper). To preserve distributor's anonymity, we replace the names of the nodes by numbers. The resulting data sets takes the form:

