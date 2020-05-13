rm(list=ls())
library(rvest)
library(data.table)
library(plotly)

#update with latest data
source("./scripts/join_zip_raw.R")

source("./scripts/source_func_download_SSI_Data.R")
source("./scripts/source_model.R")


#read processed raw figures
admissions = fread("./data/SSI_daily_hosp_processed/fulltable_joined.csv")
X = admission2trainX(admissions,6)

#show time series
make_ts_plot(admissions)

#refold data into training table, x0 is same day, x1 is the day after


View(X)
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


#showcase interface
#predicting with model 1, 2 and 3 with 3 datasets
predict(models_lin_mad[1:3],list(1:5,1:5,1:5))
predict(models_reg_ss[c("m2","m4","m5")],list(1:5,1:5,1:5))




class(admissions)
head(admissions,-5)[,plot(as.Date(Date),r200510,type="l",lwd=2)]

#points(X$Date,predict(models$m1,X$x1)*1.3,type="l",col="#FF0000")
olr_pred = predict(models_lin_mad,X[,.(x1,x2,x3,x4,x5)])
ssr_pred = predict(models_reg_ss ,X[,.(x1,x2,x3,x4,x5)])

x_pred = olr_pred
cols = colorRampPalette(c("blue","red"),bias=.3)(5)%>% rev
Data = admissions[-(nrow(admissions)-(0:5))]
names(Data)[ncol(Data)] = "latest" 
p = plot_ly(data = Data,x=~Date,y=~latest ,type="scatter",mode="lines+markers",name="True admissions",
            marker = list(color="black"),line=list(color="black"))
for(i in seq_along(x_pred) %>% rev) {
  p %<>% add_trace(x=X$Date,y=x_pred[[i]],name=paste0("m",i),
                   marker = list(color=cols[i]),line=list(color=cols[i]))  
}
p
View(admissions)


latest_vec = admissions[[ncol(admissions)]]
names(latest_vec) = admissions$Date

current_day = tail(admissions$Date,1)
class(current_day)

p = plot_ly(x=admissions$Date,y=latest_vec ,type="scatter",mode="lines+markers",name="True admissions",
            marker = list(color="black"),line=list(color="black"))
days_to_predict = latest_vec[as.character(as.Date(current_day)-(2:5))]
days_pred = predict(models_lin_mad[2:5],as.list(days_to_predict)) %>% unlist

  p = add_trace(p,x=names(days_pred),y=days_pred,name="combined models",
                   marker = list(color="red"),line=list(color="red")  )
p


View(admissions)



p = plot_ly(data = X,x=~Date,y=~x6,name="x6",type="scatter",mode="lines+markers")
for(i in 5:1) {
  this_name = paste0("x",i)
  this_model = paste0("m",i)
  these_preds = predict(models_lin_mad[[this_model]], X[[this_name]])
  p %<>% add_trace(y= these_preds ,name=this_name)
}
p


