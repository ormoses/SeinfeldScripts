#' Clean the raw data of a script
#'
#' The function takes a raw data of a script (html) and cleans it that only the script is left.
#' @param ep The raw data of a single script
#' @import stringr
#' @return a string that is a cleaned script
clean_the_episode <- function(ep) {


  #Change the monologues to MONOLOGUE:
  ep <- gsub("\\[Opening Monologue\\]","MONOLOUGUE:",ep)
  ep <- gsub("\\(?Jerry is on stage, performing.\\)?]","MONOLOUGUE:",ep)

  #Get rid of locations
  ep <- str_replace_all(ep,"INT.+","")
  # Get rid of sum html stuff
  ep <- gsub("\\t","",ep)
  ep <- gsub("\\n","",ep)
  ep <- gsub("\\\\","",ep)
  ep <- gsub("\\*","",ep)


  #Take out everything after "The End"
  end <- str_locate_all(ep,"Copyright 2006 seinology.com")
  end <- end[[1]]
  end <- end[nrow(end),]
  ep <- substr(ep,1,end[1]-1)

  #Extract the season #, episode #, episode name

  #Extract the episode total number
  ep_loc <- str_locate(ep,"Episode [0-9&]+")
  episode <- substr(ep,ep_loc[1],ep_loc[2])
  tot_episode <- str_extract(episode,"[0-9&]+")
  #Extract the season and the episode in the season
  season_loc <- str_locate(ep,"season [0-9], episode [0-9&]+")
  season_ep <- substr(ep,season_loc[1],season_loc[2])
  season_ep <- str_extract_all(season_ep,"[0-9&]+")[[1]]
  season <- season_ep[1]
  episode <- season_ep[2]

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
  list(season=season,episode=episode,tot_episode=tot_episode,script=ep)
}


#Find all the capital words with : after, which is the beginning of
# a someones qoute
#str_extract_all(ep,"[A-Z][A-Z ,&-]+:")

