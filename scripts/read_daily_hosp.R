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
lastDay = tail(dt.list.dates,1)
ALLDays = seq(firstDay,lastDay,by = 1)

#correct single offset in raw data
dt.list$r200505$diffday %<>% (function(x) x/64*63)

#round numbers and insert missing zeros

dt.list %<>% lapply(round)
j=0; dt.list = lapply(dt.list, function(x,J)  {
  x[,Date := firstDay+diffday]
  theseDays = seq(firstDay,dt.list.dates[j<<-j+1],by = 1)
  x = x[match(theseDays,x$Date),]
  x$Date = theseDays
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



#simple plot of all timelines
p = plot_ly(data=dt.list[[1]],x=~Date,y=~dailyHosp, type = "scatter", mode="lines+markers",name=names(dt.list)[1])
j=1; for(i in dt.list[-1]) p %<>% add_trace(data=i,x=~Date,y=~dailyHosp,name=names(dt.list)[(j<<-j+1)])
p %<>% layout(
  xaxis=list(title="days since 1st march 2020"),
  yaxis=list(title="daily new hospitalizations",type="")
) 
p
#saveWidget(p,"viewReportedHospitalizations.html")
