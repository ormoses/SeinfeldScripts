#' Count who speakes most number of times
#'
#' A function that counts who speaks the most number of times
#' @param all_scripts a dataframe with all the scripts data arranged
#' @importFrom plyr count
#' @importFrom dplyr arrange
#' @return a frequency table
count_the_speakers <- function(all_scripts) {
  counts <- count(all_scripts,'speaker')
  freq <- arrange(counts,desc(freq))
  freq
}

plot_the_speakers <- function(freq,num) {
  freq <- head(freq,num)
  #order the graph
  freq$speaker <- factor(freq$speaker,levels=freq$speaker[order(freq$freq,decreasing = TRUE)],ordered = TRUE)
  ggplot(data=freq,aes(x=speaker,y=freq))+geom_bar(stat="identity",color="black",fill="grey")+
    labs(title = "Seinfeld Speaker\n", x = "\nSpeaker", y = "Frequency\n") +
    theme_classic()
}
