#' Arrange a script into a dataframe
#'
#' A function to arrange a cleaned script (the list that gets back from \code{\link{clean_the_episode}}).
#' Every row in the dataframe is a different person speaking.
#' @param ep a list. The list returned from \code{\link{clean_the_episode}} of a single episode.
#' @return a dataframe containing arranged episode script
arrange_episode <- function(ep) {
  x <- ep$script
  #Find all the capital words with : after, which is the beginning of a someones qoute
  locs <- str_locate_all(x,"[A-Z][A-Z ,&-]+:")[[1]]
  #get a vector of all speakers
  spks <- vapply(seq_len(nrow(locs)),function(a) substr(x,locs[a,1],locs[a,2]-1),character(1))
  #get a vector of all content
  content <- vapply(seq_len(nrow(locs)-1),function(a) substr(x,locs[a,2]+1,locs[a+1,1]-1),character(1))
  content[nrow(locs)] <- substr(x,locs[nrow(locs),2]+1,str_length(x))
  n <- length(content)
  data.frame(season = rep(ep$season,n), episode = rep(ep$episode,n), speaker=spks,content=content,
             total_episode = rep(ep$tot_episode,n))
}
