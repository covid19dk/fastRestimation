library(rlang)
library(shiny)
library(data.table)
library(DT)
library(stringi)
require(reshape2)
library(ggcorrplot)
library(shinydashboardPlus)
library(shinyFiles)
library(rvest)
library(data.table)


rm(list=ls())
library(rvest)
library(data.table)
library(plotly)

#setwd("../.")

#update with latest data
source("./scripts/join_zip_raw.R") #deletes workspace
#source functions
source("./scripts/source_func_download_SSI_Data.R")
source("./scripts/source_model.R")

#read processed raw figures
admissions = fread("./data/SSI_daily_hosp_processed/fulltable_joined.csv")
X = admission2trainX(admissions,6)

#setwd("./shiny")

#standard linear regression models for each lag
models_reg_ss = list(
  m1 = lm(x6~x1 ,data=X),
  m2 = lm(x6~x2 ,data=X),
  m3 = lm(x6~x3 ,data=X),
  m4 = lm(x6~x4 ,data=X),
  m5 = lm(x6~x5 ,data=X)
) %>% 
  lapply(add_class,newclass="simple_univariate_lm") %>% #tag each model with non-formula interface
  add_class("model_list")  #interface for pair wise prediction of model_list and input data list

#OLR (ordinary least roots. OLS with a mean average deviation loss)
models_lin_mad = list(
  m1 = OLR(X$x1,X$x6),
  m2 = OLR(X$x2,X$x6),
  m3 = OLR(X$x3,X$x6),
  m4 = OLR(X$x4,X$x6),
  m5 = OLR(X$x5,X$x6)
) %>% add_class("model_list")



  reg_models = (do.call(rbind,lapply(models_reg_ss,coef)))
  coef.OLR_model = function(x) c("(intercept)"=0,b = x$minimum)
  mad_models=do.call(rbind,lapply(models_lin_mad,coef))
