library(data.table)
library(plotly)
library(magrittr)
library(htmlwidgets)

fdir="./data/SSI_daily_hosp_raw/"
foutdir="./data/SSI_daily_hosp_processed//"
fext = ".csv"


fpaths = list.files(fdir,fext,full.names = T)
names(fpaths) = list.files(fdir,fext) %>% (function(x) gsub(fext,"",x))
fpaths %<>% sort
dt.list = lapply(fpaths,fread,col.names=c("diffday","dailyHosp"))

firstDay = as.Date.character("200301",format="%y%m%d")
dt.list.dates = as.Date.character(substr(names(fpaths),2,7) ,format="%y%m%d")
names(dt.list.dates) = names(dt.list)
lastDay = tail(dt.list.dates,1)
ALLDays = seq(firstDay,lastDay,by = 1)

#correct single offset in raw data
dt.list$r200505$diffday %<>% (function(x) x/64*63)

#round numbers and insert missing zeros

dt.list %<>% lapply(round)
dt.list[] = lapply(names(dt.list), function(dt_name)  {
  x = dt.list[[dt_name]]
  x[,Date := firstDay+diffday]
  theseDays = seq(firstDay,dt.list.dates[dt_name],by = 1)
  x = x[match(theseDays,x$Date),]
  x$Date = theseDays
  x$dailyHosp[1] = 1 
  x$dailyHosp[is.na(x$dailyHosp)] = 0
  #x$diffday[is.na(x$diffday)] = 0
  x$Cdate = as.character(x$Date)
  setkey(x,"Cdate")
  x
})

df =do.call(data.frame,lapply(dt.list, function(x) x[as.character(ALLDays),dailyHosp]))
row.names(df) = ALLDays
View(df)
write.csv(df,file = "./data/SSI_daily_hosp_processed/fulltable.csv")



View(dft)
X = do.call(rbind,lapply( head(seq_along(dt.list.dates),-5), function(i) {
  this_date = dt.list.dates[i] 
  these_dates  = this_date+(0:5)
  this_cdate = as.character(this_date)
  these_reports = names(dt.list.dates)[match(these_dates,dt.list.dates)]
  x = df[this_cdate,these_reports]
  names(x) = paste0("x",1:6)
  x
}))

X$wday = wday(row.names(X))
X$wd4 = abs(X$wday-4)
X$wd67 = X$wday%in%6:7

plot(X$wday,X$x3/X$x6)
text(X$wday,X$x3/X$x6,row.names(X),cex=.6)
plot(X$wday,X$wd4)

m2 = lm(x6 ~ x2,X)
plot(
  predict(m2,X),
  X$x6
)

m3 = lm(x6 ~ x3,X)
plot(
  predict(m3,X),
  X$x6
)

m4 = lm(x6 ~ x4,X)
plot(
  predict(m4,X),
  X$x6
)

m5 = lm(x6 ~ x5,X)
plot(
  predict(m5,X),
  X$x5
)




#simple plot of all timelines
p = plot_ly(data=dt.list[[1]],x=~Date,y=~dailyHosp, type = "scatter", mode="lines+markers",name=names(dt.list)[1])
j=1; for(i in dt.list[-1]) p %<>% add_trace(data=i,x=~Date,y=~dailyHosp,name=names(dt.list)[(j<<-j+1)])
p %<>% layout(
  xaxis=list(title="days since 1st march 2020"),
  yaxis=list(title="daily new hospitalizations",type="")
) 
p
#saveWidget(p,"viewReportedHospitalizations.html")

tail(dt.list,1)

X$x2

