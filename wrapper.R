# Wrapper of functions and pcks
rm(list = ls())
# Look if necessary packages are installed, if not, install it
list.of.packages <- c('tidyverse', 'data.table','cluster',
                      'ggthemes','minpack.lm','philentropy',
                      'gridExtra','grid','beanplot','magrittr',
                      'ggridges','e1071','lemon','latex2exp')

new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
# load packages

if(length(new.packages)){
  install.packages(new.packages)}

# Load packages
sapply(list.of.packages, library, character.only=T)

# Read and load data
source("~/Dropbox/mult_IF_aco_exp/v3/last_n_ses_v1.R") 
source("~/Dropbox/mult_IF_aco_exp/v3/bind_all_Ss.R")
source("~/Dropbox/mult_IF_aco_exp/v3/read_sep_by_lever.R")
source("~/Dropbox/mult_IF_aco_exp/v3/plt_metrics_dot_err.R")
# Response rate function
source("~/Dropbox/mult_IF_aco_exp/v3/special_functions/resp_rate_df.R")
## Plot data
source("~/Dropbox/mult_IF_aco_exp/v3/pub_theme.R")
source("~/Dropbox/mult_IF_aco_exp/v3/axis_ticks.R")
source("~/Dropbox/mult_IF_aco_exp/v3/sem_ul_lwl.R")
source("~/Dropbox/mult_IF_aco_exp/v3/special_functions/GeomStepHist.R")
source("~/Dropbox/mult_IF_aco_exp/v3/resp_rate_df.R")
source("~/Dropbox/mult_IF_aco_exp/v3/resp_norm_plot.R")
# kld
source("~/Dropbox/mult_IF_aco_exp/v3/kld_d.R")
#### break point analysis ####

source("~/Dropbox/mult_IF_aco_exp/v3/get_bins.R")
source("~/Dropbox/mult_IF_aco_exp/v3/f_table.R")
# These functions are to perform individual trial analyses by means of 
# piecewise linear regression and kmedoids
# source("~/Dropbox/mult_IF_aco_exp/v3/bp_pwise.R")
# source("~/Dropbox/mult_IF_aco_exp/v3/p_wise.R")
source("~/Dropbox/mult_IF_aco_exp/v3/pooled_ind_ana_km_df.R")
source("~/Dropbox/mult_IF_aco_exp/v3/bp_km.R")
source("~/Dropbox/mult_IF_aco_exp/v3/bp_km_wrap.R")
source("~/Dropbox/mult_IF_aco_exp/v3/pooled_ind_analysis_df.R")
source("~/Dropbox/mult_IF_aco_exp/v3/special_functions/read_sep_by_lever_phase.R")
source("~/Dropbox/mult_IF_aco_exp/v3/special_functions/r.rate_iri_df.R")
source("~/Dropbox/mult_IF_aco_exp/v3/fwhm.R") # 2 functions
source("~/Dropbox/mult_IF_aco_exp/v3/resp_times.R")
source("~/Dropbox/mult_IF_aco_exp/v3/qq_points_xy.R")
source("~/Dropbox/mult_IF_aco_exp/v3/rR_corr.R")
# low_high_low with Church's algorithm
source("~/Dropbox/mult_IF_aco_exp/v3/break_point.R")
source("~/Dropbox/mult_IF_aco_exp/v3/low_high_low.R")


# functions to fit piecewise reg

# the groups are [1, b1], (b1, b2], (b2, Inf)
# 
# fit <- function(b1, b2, x, y) {
#   grp <- factor((x > b1) + (x >= b2))
#   lm(y ~ grp)
# }
# 
# dv <- function(...) deviance(fit(...))

rm(list.of.packages,new.packages)
