library(magrittr)
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
  link_tag = "Data-Epidemiologiske-Rapport"
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
