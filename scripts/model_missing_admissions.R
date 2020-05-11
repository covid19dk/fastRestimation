rm(list=ls())
library(rvest)
library(data.table)
library(plotly)

make_ts_plot = function(admissions) {
  p = plot_ly(x=admissions$Date,y=admissions[[2]], type = "scatter", mode="lines+markers",name=names(admissions)[2])
  for(i in 3:ncol(admissions)) p %<>% add_trace(y=admissions[[i]],name=names(admissions)[i])
  p %<>% layout(
    xaxis=list(title="days since 1st march 2020"),
    yaxis=list(title="daily new hospitalizations",type="")
  )
  p
}


source("./scripts/source_func_download_SSI_Data.R")

#read processed raw figures
admissions = fread("./data/SSI_daily_hosp_processed/fulltable_joined.csv")
setkey(admissions,"Date")
class(admissions$Date)

make_ts_plot(admissions)


#make table of report release day(column) vs observed numberf or day (row)
col_dates = as.Date(colnames(admissions)[-1],format="r%y%m%d")
names(col_dates) = as.character(col_dates,format="r%y%m%d")
n_lag = 6
use_these_dates = head(col_dates,-n_lag)
X = do.call(rbind,lapply(use_these_dates, function(i) {
  this_date = i
  these_dates  = this_date+(0:n_lag) #days after report released
  this_cdate = as.character(this_date)
  these_reports = names(col_dates)[match(these_dates,col_dates)]
  x = admissions[this_cdate,..these_reports]
  names(x) = paste0("x",0:n_lag)
  x
}))
X = data.table(Date = use_these_dates,X)

View(X)

add_class = function(x,newclass,append=TRUE) {
  class(x) = c(newclass,if(append) class(x)) 
  return(x)
} 
 
#ols regression, gaussian loss (SS og sqaures)
models_reg_ss = list(
  m1 = lm(x6~x1 ,data=X),
  m2 = lm(x6~x2 ,data=X),
  m3 = lm(x6~x3 ,data=X),
  m4 = lm(x6~x4 ,data=X),
  m5 = lm(x6~x5 ,data=X)
) %>% lapply(add_class,newclass="ols_ss") %>% add_class("model_list")

#add simple predict interface
predict.ols_ss = function(x,newdata) {sapply(newdata,function(y) sum(coef(x)[1] + coef(x)[-1] *y ))}

#make a trainer and predicter to fit OLS_mad 
OLS_mad = function(x,y) {
  f = function(k) abs(mean(x*k)-mean(y))
  result = optimize(f,c(0,100))
  class(result) = "OLS_mad_model"
  return(result)
}
predict.OLS_mad_model = function(model,newx) {
  newx*model$minimum
}

models_ols_mad = list(
    m1 = OLS_mad(X$x1,X$x6),
    m2 = OLS_mad(X$x2,X$x6),
    m3 = OLS_mad(X$x3,X$x6),
    m4 = OLS_mad(X$x4,X$x6),
    m5 = OLS_mad(X$x5,X$x6)
) %>% add_class("model_list")

subset_keep_attr = function(x,val) {
  atrs = attributes(x)
  x = unclass(x)
  x = x[val]
  if(!is.null(atrs$names)) atrs$names = atrs$names[val]
  attributes(x) = atrs
  return(x)
}
'[.model_list' = subset_keep_attr

predict.model_list = function(modellist, newdata) {
  if(length(model_list)!=length(newdata)) stop("input data must be same length as model list")
  results = lapply(seq_along(model_list), function(i) predict(model_list[[i]],newdata[[i]]))
  return(results)
}




# class(models) = c("laggedlm","classlm")
# predict.laggedlm = function(model,x) {
#   lmod =length(model)
#   attrx = attributes(x)
#   x = head(x,-1) #drop last measurement, as always zero ang carrries no information
# 
#   #each day by corrosponding linear model
#   input = tail(x,lmod) #extract last n_days from timeseries
#   coefDF =t(sapply(models,coef)) %>% as.data.frame()
#   coefDF = coefDF[nrow(coefDF):1,]
#   results = input  * coefDF[["x1"]] + coefDF[["(Intercept)"]]
# 
#   #insert predictions into
#   lx = length(x)
#   x[(lx-lmod+1):lx] = results
# 
#   x = c(x,NA) #add last measurement as missing
#   attributes(x) = attrx #restore any attributes
#   return(x)
# }
# i=1
# 
# i=1
# x = admissions[[i<<-i+1]]
# names(x) =  admissions$Date
# x=na.omit(x)
# plot(
# x = names(x) %>% as.Date,  
# y = x,
# type="l"
# )
# points(
#   x = admissions$Date %>% as.Date,  
#   y = admissions$r200510,
#   type="b",col="green"
# )
# points(
#   x =names(x) %>% as.Date,  
#   y = predict(models,x),
#   type="b",col="red"
# )
# 


class(admissions)
head(admissions,-5)[,plot(as.Date(Date),r200510,type="l",lwd=2)]
plot(X$Date,X$x6,type="l",ylim=c(0,50),lwd=3)
#points(X$Date,predict(models$m1,X$x1)*1.3,type="l",col="#FF0000")
points(X$Date,predict(models_ols_mad$m2,X$x2),type="l",col="#DD0000")
points(X$Date,predict(models_ols_mad$m3,X$x3),type="l",col="#AA0000")
points(X$Date,predict(models_ols_mad$m4,X$x4),type="l",col="#990000")
points(X$Date,predict(models_ols_mad$m5,X$x5),type="l",col="#770000")
points(X$Date,predict(models_reg_ss$m2,X$x2),type="l",col="#00DD00")
points(X$Date,predict(models_reg_ss$m3,X$x3),type="l",col="#00AA00")
points(X$Date,predict(models_reg_ss$m4,X$x4),type="l",col="#009900")
points(X$Date,predict(models_reg_ss$m5,X$x5),type="l",col="#007700")



