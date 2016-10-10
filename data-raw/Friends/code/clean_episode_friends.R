

get_raw_data_friends <- function(filename) {
  x <- htmlToText(paste0(getwd(),"/data-raw/Friends/",filename))
  if (str_length(filename)==9) {
    season=as.numeric(substr(filename,1,2))
    episode=as.numeric(substr(filename,3,4))
  } else if (str_length(filename)==14) {
    season=as.numeric(substr(filename,1,2))
    episode=paste0(as.numeric(substr(filename,3,4)),"&",as.numeric(substr(filename,8,9)))
  }
  list(ep=x,season=season,episode=episode)
}

clean_episode_friends <- function(ep) {
  x <- ep
  ep <- ep$ep
  #remove after "END"
  if (!is.na(str_locate(ep," E[Nn][Dd]"))[1]) {
    ep <- remove_after(ep," E[Nn][Dd]")
  }
  #remove everything in () and [] and <>
  ep <- remove_between(ep,matrix(c("\\(","\\)","\\[","\\]"),ncol=2,byrow=TRUE))
  #remove white spaces
  ep <- remove_white_spaces(ep)
  list(script = ep, season = x$season, episode = x$episode)
}


arrange_episode_friends <- function(ep) {
  print(ep$season)
  print(ep$episode)
  x <- ep$script
  #Find all the words with : after, which is the beginning of a someones qoute
  locs <- str_locate_all(x,"[A-Z][a-zA-Z]+ ?:")[[1]]
  #get a vector of all speakers
  spks <- vapply(seq_len(nrow(locs)),function(a) substr(x,locs[a,1],locs[a,2]-1),character(1))
  #get a vector of all content
  content <- vapply(seq_len(nrow(locs)-1),function(a) str_trim(substr(x,locs[a,2]+1,locs[a+1,1]-1)),character(1))
  content[nrow(locs)] <- str_trim(substr(x,locs[nrow(locs),2]+1,str_length(x)))
  num_words <- vapply(content,function(a) nrow(str_locate_all(a," ")[[1]])+1,numeric(1))
  n <- length(content)
  res <- data.frame(season = rep(ep$season,n), episode = rep(ep$episode,n), speaker=spks,content=content,
                    num_words = num_words,stringsAsFactors = FALSE)
  res
}

all_episode_list <- function(path) {
  files <- dir(path,pattern=".html")
  raw_list <- lapply(files,get_raw_data_friends)
  clean_list <- lapply(raw_list,clean_episode_friends)
  arranged_list <- lapply(clean_list,arrange_episode_friends)
}

merge_into_big_df_friends <- function(arranged_list) {
  x <- do.call(rbind,arranged_list)
}

arrange_df_friends <- function(df) {
  #change speaker into upper case
  dplyr::mutate(df,speaker=toupper(speaker))
  #remove extra spaces
  dplyr::mutate(df,speaker=stringr::str_trim(speaker))
  #remove punctuation
  df$content <- vapply(df$content,remove_punc,character(1))
  df
}
