#' Read a script of an episode from seinology.com
#'
#' A function that read a script from seinology.com by episode's number (total number)
#' @param ep_num a number. The number of an episode starting from the beginning of the show (from 1 to 180)
#' @return a string. A long string with all the html code of the page.
get_raw_episode <- function(ep_num) {
  url <- paste0("http://www.seinology.com/scripts/script-",ep_num,".shtml")

  # evaluate input and convert to text
  txt <- htmlToText(url)
  txt
}

get_raw_data <- function(nums) {
  raw_data_list <- list()
  #for (i in c(1:81,84:99,102:176)) {
  for (i in nums) {
    raw_data <- ifelse(i < 10, get_raw_episode(paste0("0",i)),get_raw_episode(i))
    raw_data_list[[i]] <- raw_data
    save(raw_data,file=paste0(getwd(),"/data-raw/",i,".Rda"))
  }
  #Getting double episodes
  for (i in c(82,100,177,179)) {
    raw_data <- get_raw_episode(paste0(i,"and",i+1))
    raw_data_list[[i]] <- raw_data
    save(raw_data,file=paste0(getwd(),"/data-raw/",i,".Rda"))
  }
  raw_data_list
}

get_list_from_rda_files <- function(thepath,nums) {
  raw_data_list <- list()
  for (i in nums) {
    f_name <- paste0(thepath,"/",i,".Rda")
    if (file.exists(f_name)) {
      load(file=f_name)
      raw_data_list[[i]] <- raw_data
    }
  }
  raw_data_list
}
