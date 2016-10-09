#' Count who speakes most by number of times
#'
#' A function that counts who speaks the most number of times
#' @param all_scripts a dataframe with all the scripts data arranged
#' @param the_season a number indicates the season. for all season can omit or "all"
#' @param the_episode a number indicates the episode. for all season can omit or "all"
#' @importFrom plyr count
#' @importFrom dplyr arrange filter
#' @return a frequency table
count_the_speakers <- function(all_scripts,the_season="all",the_episode="all") {
  #filter by season
  if (the_season!="all") {
    all_scripts <- dplyr::filter(all_scripts,season==the_season)
  }

  #filter by episode
  if (the_episode!="all") {
    all_scripts <- dplyr::filter(all_scripts,episode==the_episode)
  }

  counts <- count(all_scripts,'speaker')
  freq <- arrange(counts,desc(freq))
  freq
}

#' Count who speakes most by number of words
#'
#' A function that counts who speaks the most number of words
#' @inheritParams count_the_speakers
#' @importFrom dplyr arrange select filter
#' @return a frequency table
count_num_words <- function(all_scripts,the_season="all",the_episode="all") {
  #filter by season
  #filter by season
  if (the_season!="all") {
    all_scripts <- dplyr::filter(all_scripts,season==the_season)
  }

  #filter by episode
  if (the_episode!="all") {
    all_scripts <- dplyr::filter(all_scripts,episode==the_episode)
  }
  x <- aggregate(num_words ~ speaker,data=all_scripts,sum)
  x <- arrange(x,desc(num_words))
  x <- select(x,speaker,freq=num_words)
  x
}
#' Plot a frequency plot
#'
#' Plot a frequency plot for the speakers
#' @param freq a frequency table
#' @param num a number indicates the number of bars in the plot
#' @import ggplot2
#' @return a bar plot
plot_the_speakers <- function(freq,num) {
  freq <- head(freq,num)
  highest <- head(freq,1)$freq
  #order the graph
  freq$speaker <- factor(freq$speaker,levels=freq$speaker[order(freq$freq,decreasing = TRUE)],ordered = TRUE)
  ggplot(data=freq,aes(x=speaker,y=freq))+geom_bar(stat="identity",color="black",fill="grey")+
    labs(title = "Seinfeld Speaker\n", x = "\nSpeaker", y = "Frequency\n") +
    geom_text(size=2.5,data=freq,aes(x=speaker,y=freq+highest/20,label=formatC(freq,format="d",big.mark=','))) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
