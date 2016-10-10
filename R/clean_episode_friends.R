get_raw_data_friends <- function(filename) {
  x <- readLines(con=paste0(getwd(),"/data-raw/Friends/",filename))
  x <- paste(x,sep=" ",collapse=" ")
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
  #extract episode name
  start <- str_locate(ep,"<title>")[2]
  end <- str_locate(ep,"</title>")[1]
  episode_name <- substr(ep,start+1,end-1)
  #remove after "END"
  ep <- remove_after(ep,"E[Nn][Dd]\\<")
  #remove before beginning of the script
  ep <- remove_before(ep,"<hr",occur="last")
  #remove strong captions
  ep <- remove_between(ep,matrix(c(paste("<p align=","center","",sep='"'),"</p>"),ncol=2,byrow=TRUE))
  #remove everything in () and [] and <>
  ep <- remove_between(ep,matrix(c("\\(","\\)","\\[","\\]"),ncol=2,byrow=TRUE))
  #remove all &#number
  ep <- remove_exact(ep,"\\&\\#[0-9]+;",sep="")
  #remove white spaces
  ep <- remove_white_spaces(ep)
  #split by paragraph or brake
  ep <- str_split(ep,"<p>|<p align|<br>")[[1]]
  #split to speaker and content
  #Speaker
  speaker <- vapply(ep,function(a) str_extract(a,"[a-zA-Z]+:"),character(1))
  speaker <- vapply(speaker,function(a) remove_between(a,matrix(c("\\<","\\>"),ncol=2,byrow=TRUE)),character(1))
  speaker <- vapply(speaker,remove_punc,character(1))
  speaker <- vapply(speaker,remove_white_spaces,character(1))
  speaker <- vapply(speaker,tolower,character(1))
  names(speaker) <- NULL
  #content
  content <- vapply(ep,function(a) str_extract(a,"(?<=[a-zA-z]:).*$"),character(1))
  content <- vapply(content,function(a) remove_between(a,matrix(c("\\<","\\>"),ncol=2,byrow=TRUE)),character(1))
  content <- vapply(content,remove_punc,character(1))
  content <- vapply(content,remove_white_spaces,character(1))
  content <- vapply(content,tolower,character(1))
  names(content) <- NULL
  df <- data.frame(season=x$season,episode=x$episode,speaker=speaker,content=content,episode_name=episode_name)
  df <- df[complete.cases(df),]
  df
}

all_episode_list <- function(path) {
  files <- dir(path,pattern=".html")
  raw_list <- lapply(files,get_raw_data_friends)
  clean_list <- lapply(raw_list,clean_episode_friends)
  clean_list
}
