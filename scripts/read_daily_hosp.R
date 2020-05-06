library(data.table)
library(plotly)
library(magrittr)
library(htmlwidgets)

fdir="./data/SSI_daily_hosp/"
fext = ".csv"

fpaths = list.files(fdir,fext,full.names = T)
names(fpaths) = list.files(fdir,fext) %>% (function(x) gsub(fext,"",x))
dt.list = lapply(fpaths %>% sort,fread)

#correct offset in raw data
dt.list$r200505$V1 %<>% (function(x) x/64*63)

#simple plot of all timelines
p = plot_ly(data=dt,x=~V1,y=~V2, type = "scatter", mode="lines+markers",name=names(dt.list)[1])
j=1; for(i in dt.list[-1]) p %<>% add_trace(data=i,x=~V1,y=~V2,name=names(dt.list)[(j<<-j+1)])
p %<>% layout(xaxis=list(title="days since 1st march 2020"),yaxis=list(title="daily new hospitalizations")) 

saveWidget(p,"viewReportedHospitalizations.html")

