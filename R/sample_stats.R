#' Count who speakes most by number of times
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
#' Count who speakes most by number of words
#'
#' A function that counts who speaks the most number of words
#' @inheritParams count_the_speakers
#' @importFrom dplyr arrange select
#' @return a frequency table
count_num_words <- function(all_scripts) {
  x <- aggregate(num_words ~ speaker,data=all_scripts,sum)
  x <- arrange(x,desc(num_words))
  x <- select(x,speaker,freq=num_words)
  x
}
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
