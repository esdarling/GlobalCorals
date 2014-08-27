rm(list=ls())
library(mgcv)
library(plyr)
source('R/functions-analyses.R')
source('R/functions-figures.R')
source('R/structure.R')
load('output/data/results.RData')

#save all to a single pdf file
to.pdf(plotAllModels(results, tab, plotGAMM), 'output/figures/allGAMM_fitted.pdf', width=7, height=7)
to.pdf(plotAllModels(results, tab, plotResidualGAMM), 'output/figures/allGAMM_residuals.pdf', width=7, height=7)

