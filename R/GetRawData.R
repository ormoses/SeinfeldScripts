get_raw_episode <- function(ep_num) {
  url <- paste0("http://www.seinology.com/scripts/script-",ep_num,".shtml")

  # evaluate input and convert to text
  txt <- htmlToText(url)
  txt
}

get_raw_data <- function() {
  raw_data <- list()
  for (i in c(1:81,84:99,102:176)) {
    raw_data[[i]] <- get_raw_episode(i)
  }
  raw_data[[82]] <- get_raw_episode("82and83")
  raw_data[[100]] <- get_raw_episode("100and101")
  raw_data[[177]] <- get_raw_episode("177and178")
  raw_data[[178]] <- get_raw_episode("179and180")
}
