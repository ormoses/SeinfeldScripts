#' Clean the raw data of a script
#'
#' The function takes a raw data of a script (html) and cleans it that only the script is left.
#' @param ep The raw data of a single script
#' @importFrom  stringr str_locate_all str_replace_all
#' @return a string that is a cleaned script
clean_episode <- function(ep) {

  # Get rid of sum html stuff
  ep <- gsub("\\t","",ep)
  #ep <- gsub("\\n","",ep)
  ep <- gsub("\\\\","",ep)
  ep <- gsub("\\*","",ep)
  ep <- gsub("\\[Opening Monologue\\]","MONOLOUGUE:",ep)

  #Take out everything after "The End"
  end <- str_locate_all(ep,"The End")
  end <- end[[1]]
  end <- end[nrow(end),]
  ep <- substr(ep,1,end[1]-1)

  #Extract the season #, episode #, episode name

  #Take out everything before "========================"
  start <- str_locate_all(ep,"===")
  start <- start[[1]]
  start <- start[nrow(start),]
  ep <- substr(ep,start[2]+1,str_length(ep))

  #Delete all that inside () and inside [] and <>.
  ep <- str_replace_all(ep,"\\(.*?\\)|\\[.*?\\]|\\<.*?\\>","")
  #Merge multiple space into one
  ep <- gsub("\\s+"," ",ep)
  #Delete extra spaces in the beginning and end
  ep <- str_trim(ep)
  ep
}


#Find all the capital words with : after, which is the beginning of
# a someones qoute
#str_extract_all(ep,"[A-Z]+:")

