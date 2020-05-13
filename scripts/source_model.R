library(plotly)

#' plot shothand
#'
#' @param admissions 
#'
#' @return
#' @export
#'
#' @examples
make_ts_plot = function(admissions) {
  p = plot_ly(x=admissions$Date,y=admissions[[2]], type = "scatter", mode="lines+markers",name=names(admissions)[2])
  for(i in 3:ncol(admissions)) p %<>% add_trace(y=admissions[[i]],name=names(admissions)[i])
  p %<>% layout(
    xaxis=list(title=""),
    yaxis=list(title="daily new hospitalizations",type="")
  )
  p
}


#'  add S3 subclass suitable for piping
#'
#' @param x 
#' @param newclass 
#' @param append 
#'
#' @return
#' @export
#'
#' @examples
add_class = function(x,newclass,append=TRUE) {
  class(x) = c(newclass,if(append) class(x)) 
  return(x)
} 


#' add simple predict interface
#'
#' @param x univariate model with or without intercept created from lm or similar
#' @param newdata vector of data
#'
#' @return
#' @export
#'
#' @examples
predict.simple_univariate_lm = function(x,newdata) {
  if(!is.numeric(newdata)) stop("input must be numeric")
  cx = coef(x)
  b0 = cx["(Intercept)"]
  bx = cx[setdiff(names(cx),"(Intercept)")]
  f = function(newval) sum(b0,bx*newval)
  result = vapply(newdata,f,0)
  return(result)
}


#'train function for OLR 
#'
#' @param x single feature
#' @param y single target
#'
#' @return
#' @export
#'
#' @examples
OLR = function(x,y) {
  f = function(k) abs(mean(x*k)-mean(y))
  result = optimize(f,c(0,100))
  class(result) = "OLR_model"
  return(result)
}


#' predictor for OLR_mad
#'
#' @param model model of S3 class "OLS_mad_model" 
#' @param newx 
#'
#' @return
#' @export
#'
#' @examples
predict.OLR_model = function(model,newx) {
  newx*model$minimum
}



#' generic function for subsetting
#'
#' @param x 
#' @param val 
#'
#' @return
#' @details subsests class and if any attribute names, leaves any other attributes as is
#'
#' @examples 
subset_keep_attr = function(x,val) {
  atrs = attributes(x)
  x = unclass(x)
  x = x[val]
  if(!is.null(atrs$names)) atrs$names = atrs$names[val]
  attributes(x) = atrs
  return(x)
}


#' subset behavior for model_list defined
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples model_list[1:2]
'[.model_list' = function(x,val) {
  subset_keep_attr(x,val)
} 



#' function to predict a model list
#'
#' @param modellist 
#' @param newdata 
#'
#' @return
#' @export
#'
#' @examples
predict.model_list = function(model_list, newdata) {
  if(!inherits(model_list,"model_list")) stop("this is not a model_list")
  if(length(model_list)!=length(newdata)) stop("input data must be same length as model list")
  results = lapply(seq_along(model_list), function(i) predict(model_list[[i]],newdata[[i]]))
  names(results) = names(newdata)
  return(results)
}

#' make train table
#'
#' @param admissions admission table as defined by "join_zip_raw.R"
#' @param n_lag number of days lagged
#'
#' @return table where each row is a specific day and column as the observed number for same or later days
#' @export
#'
#' @examples
admission2trainX = function(admissions,n_lag) {
  setkey(admissions,"Date")
  col_dates = as.Date(colnames(admissions)[-1],format="r%y%m%d")
  names(col_dates) = as.character(col_dates,format="r%y%m%d")
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
  return(X)
}