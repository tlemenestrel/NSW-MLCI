# Repository Name

## Overview
This repository contains R scripts for preparing data, estimating propensity scores, and computing average causal effects (ACE) and area under the curve (AUC) for the National Supported Work dataset. The scripts are part of a workflow for causal inference analysis in observational studies.

## Files and Directories

- `data_preparation.R`: Script for data cleaning and preprocessing, including handling missing values and transforming variables.
- `propensity_scores.R`: Script for estimating propensity scores using various statistical models such as logistic regression, random forests, and neural networks.
- `compute_ace.R`: Script for calculating the average causal effect (ACE) using the estimated propensity scores.
- `compute_auc.R`: Script for evaluating the performance of propensity score models by computing the area under the receiver operating characteristic curve (AUC).

## Getting Started

To get started with this repository:

1. Clone the repository to your local machine using `git clone`.
2. Ensure you have R installed on your computer.
3. Install the necessary R packages listed under Dependencies.

## Dependencies

The following R packages are required to run the scripts:

```R
install.packages("dplyr")
install.packages("ggplot2")
install.packages("haven")
install.packages("MatchIt")
install.packages("Matching")
install.packages("optmatch")
install.packages("perm")
install.packages("pROC")
install.packages("RItools")
install.packages("randomForest")
install.packages("nnet")

To use the scripts, follow these steps:

1. Prepare your dataset according to the specifications in `data_preparation.R`.
2. Run `propensity_scores.R` to estimate propensity scores with your dataset.
3. Use `compute_ace.R` to calculate the ACE based on the estimated propensity scores.
4. Evaluate your propensity score models by executing `compute_auc.R` to get the AUC.
