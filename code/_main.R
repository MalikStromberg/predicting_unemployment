# This is the master script aiming to reproduce all data, figures and models in
# the R-project
# However, if you are interested in specific results such as empirical risks,
# you should run each skript for itself. Only then the results will show up
# in the Console

rm(list = ls())
options(warn = -1)

## 1. Data Preparation
source('data_cleaning.R')

## 2. Analyses and Models
source('lr.R')
source('svm.R')

## 3. Visualization
source('visualization.R')





