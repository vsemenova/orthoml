# orthoml
Code associated with paper: "Estimation and Inference on Heterogeneous Treatment Effects in High-Dimensional Dynamic Panels under Weak Dependence" Semenova, Goldman, Chernozhukov, Taddy (2022), Quantitative Economics, forthcoming

# Introduction

## Code
To replicate Figure 1, please run Figure1Code.R in the main folder. 

To replicate Figure 2,  one should execute the two files below in the following order

1. estimate_first_stage_Snacks.R

2. estimate_second_stage_Snacks.R 

We see that Lasso estimates are most concenrated (shrinked towards homogenous specification), Orthogonal Least Squares  is most dispersed (and least precise), and  Double Orthogonal Ridge is in the middle. 

# References:

"Double/Debiased Machine Learning for Treatment and Causal Parameters" (Victor Chernozhukov, Denis Chetverikov, Mert Demirer, Esther Duflo, Christian Hansen, Whitney Newey, James Robins), 2017, https://arxiv.org/abs/1608.00060

"Estimation and Inference about Heterogeneous Treatment Effects in High-Dimensional Dynamic Panels"
Vira Semenova, Matt Goldman, Victor Chernozhukov, Matt Taddy, 2017, https://economics.mit.edu/files/15984 

"Pricing Engine: Estimating Causal Impacts in Real World Business Settings" Matt Goldman, Brian Quistorff, 2018, https://arxiv.org/abs/1806.03285 
