rm(list=ls())
library(rvest)
library(data.table)

source("./scripts/source_func_download_SSI_Data.R")

#read processed raw figures
ssi_proc = fread("./data/SSI_daily_hosp_processed/fulltable.csv")
names(ssi_proc)[1] = "Date"
setkey(ssi_proc,"Date")

#download latest zip data data source
ssi_url = get_download_url() #get url for latest zip file
ssi_date = tail(strsplit(ssi_url,"-")[[1]],2)[1]
new_files = download_unzip(ssi_url,paste0("./data/SSI_csv/date",ssi_date))

ssi.zip.paths = 
  list.files("./data/SSI_csv/",recursive = TRUE) %>% 
  (function(x) x[grep("Newly_admitted_over_time",x)]) %>%  #fetch admission
  (function(x) { #name
    attr(x,"rname") = paste0("r",gsub("-","",substr(x,5,12) %>% as.Date(format="%d%m%Y") %>% as.character) %>% substr(3,99))
    names(x) = substr(x,5,12) %>% as.Date(format="%d%m%Y") %>% as.character
    return(x)
  })

##append new data to table
for(i in seq_along(ssi.zip.paths)) {
  rname = attr(ssi.zip.paths,"rname")[i]
  x = fread(paste0("./data/SSI_csv/",ssi.zip.paths[i]))
  x = x[-seq_len(which(Dato=="2020-03-01")-1)]
  setkey(x,"Dato")
  x = rbind(x,x[as.character(max(as.Date(x$Dato))+1)]) #add same days report to stay concistant with tables
  x$Antal_nyindlagte[nrow(x)] = 0
  setkey(ssi_proc,"Date")
  ssi_proc = ssi_proc[x$Dato,]
  ssi_proc[[rname]] = x$Antal_nyindlagte
}

fwrite(ssi_proc,"./data/SSI_daily_hosp_processed/fulltable_joined.csv")
