library(magrittr)
library(xml2)
library(rvest)

#' Get latest Data-Epidemiologiske-Rapport zipfile-url
#'
#' @param url target site to look for zipfile url
#' @param link_tag tag to choose(grep) between all links on site
#'
#' @return string of url to zipfile
#' @export
#'
#' @examples url_to_zipfile = get_download_url()
get_download_url = function(
  url = "https://www.ssi.dk/aktuelt/sygdomsudbrud/coronavirus/covid-19-i-danmark-epidemiologisk-overvaagningsrapport",
  link_tag = "Data-epidemiologisk-rapport"
) {
  nodes = read_html(url)
  all_links = xml2::xml_find_all(nodes,'//*[@id="top"]//a/@href') 
  the_link = all_links[grep(link_tag,all_links)] %>% as.character
  if(length(the_link)!=1) warning(paste("unexpected found not 1 link:",paste(the_link,collapse = " and also ")))
  if(!length(the_link)) stop("no link found")
  the_dl_url = the_link %>% trimws %>% gsub(pat="href=",repl="") %>% gsub(pat='\\"',repl='')
  return(the_dl_url)
}

#' Download, unzip and copy to path
#'
#' @param url target url of zipfile
#' @param path directory to copy extracted files to
#' @param filename filename(s) of files inside zip to extract, NULL is all
#'
#' @return
#' @export
#'
#' @examples new_filenames = download_unzip(get_download_url())
download_unzip = function(url,path=".",filename=NULL) {
  tfile = tempfile()
  on.exit(unlink(tfile),add = TRUE)
  tpath = paste0(tempdir(),"/",paste0(sample(letters,10),collapse = ""))
  dir.create(tpath)
  on.exit(unlink(tpath,recursive = TRUE,force = TRUE),add = TRUE)
  download.file(url,tfile)
  utils::unzip(tfile,filename,exdir= tpath)
  extracted_filenames = list.files(tpath)
  extracted_filenames_full = list.files(tpath,full.names = TRUE)
  new_paths = paste0(path,"/",extracted_filenames)
  if(!dir.exists(path)) dir.create(path)
  success_idx = file.copy(extracted_filenames_full,new_paths,overwrite = TRUE,copy.mode = FALSE)
  new_paths = new_paths[success_idx]
  names(new_paths) = extracted_filenames[success_idx]
  return(new_paths)
}

#' precodedure to read 
#'
#' @param fdir 
#' @param fext 
#'
#' @return
#' @export
#'
#' @examples
read_SSI_raw = function(
  fdir="./data/SSI_daily_hosp_raw/",
  fext = ".csv"
){
  
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
  
  return(dt.list)
}



