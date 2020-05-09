library(rvest)
library(data.table)

source("./scripts/source_func_download_SSI_Data.R")

ssi_url = get_download_url() #get url for latest zip file
ssi_date = tail(strsplit(ssi_url,"-")[[1]],2)[1]
new_files = download_unzip(ssi_url,paste0("./data/SSI_csv/date",ssi_date))
new_csv_files = new_files[tools::file_ext(new_files)=="csv"]
ssi.list = lapply(new_csv_files,fread)


#access data like this, e.g.
print(ssi.list$Newly_admitted_over_time.csv)
