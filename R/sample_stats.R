#' filter by name, season, episode
#'
#' A helper function to choose to filter by name/season/episode
#'
#' @param all_scripts dataframe with all the scripts arranged
#' @param the_name a character vector. ("all") will mean no filter
#' @param the_season a numeric vector. ("all") will mean no filter
#' @param the_episode a numeric vector. ("all") will mean no filter
#' @importFrom dplyr filter
#' @return dataframe filtered
filter_by_choose <- function(all_scripts,the_name="all",the_season="all",
                             the_episode="all") {
  #filter by name
  if (the_name!="all") {
    #change to upper case
    name <- toupper(the_name)
    all_scripts <- dplyr::filter(all_scripts,speaker %in% the_name)
  }
  #filter by season
  if (the_season!="all") {
    all_scripts <- dplyr::filter(all_scripts,season %in% the_season)
  }
  #filter by episode
  if (the_episode!="all") {
    all_scripts <- dplyr::filter(all_scripts,episode %in% the_episode)
  }
  all_scripts
}


#' Counts number of episode each character appears
#'
#' A function that counts how many episodes a character appears
#' @param all_scripts a dataframe with all the scripts data arranged
#' @param season a number indicates the season. for all season can omit or "all"
#' @importFrom plyr count
#' @importFrom dplyr arrange filter select mutate
#' @return a frequency table
#' @export
count_episodes_appear <- function(all_scripts,season="all") {
  #filter
  all_scripts <- filter_by_choose(all_scripts,the_season=season)
  if (season == "all") season <- as.numeric(unique(all_scripts$season))
  #Make a list of all characters
  characters <- unique(as.character(all_scripts$speaker))
  #Count for each character the number of episodes
  df <- data.frame(speaker = as.character(characters), num_episodes = vapply(characters,
         function(ch_name) length(unique(all_scripts[all_scripts$speaker==ch_name & all_scripts$season %in% season,]$episode)),
         numeric(1)),stringsAsFactors = FALSE)
}

#' Count who speakes most by number of times
#'
#' A function that counts who speaks the most number of times
#' @param all_scripts a dataframe with all the scripts data arranged
#' @param type a character indicates if to check number of times a charcter
#' speaks ("num_speaks") or number of words ("num_words")
#' @param season a number indicates the season. for all season can omit or "all"
#' @param episode a number indicates the episode. for all season can omit or "all"
#' @param scaled logical. Weather to scale the frequency by number of episodes the character involved.
#' @importFrom plyr count
#' @importFrom dplyr arrange filter select mutate
#' @importFrom stats aggregate
#' @return a frequency table
#' @export
count_the_speakers <- function(all_scripts, type, season = "all",episode = "all", scaled = FALSE) {
  #filter
  all_scripts <- filter_by_choose(all_scripts,the_season=season,
                                  the_episode=episode)

  if (type=="num_speaks") {
    counts <- count(all_scripts,'speaker')
  } else if (type=="num_words") {
    num_words <- vapply(all_scripts$content,function(a) nrow(str_locate_all(a," ")[[1]])+1,numeric(1))
    all_scripts <- mutate(all_scripts,num_words = num_words)
    counts <- aggregate(num_words ~ speaker,data=all_scripts,sum)
    counts <- select(counts,speaker,freq=num_words)
  }

  if (scaled == TRUE) {
    epis_appear <- count_episodes_appear(all_scripts, season)
    new_count <- merge(counts, epis_appear)
    new_count <- new_count %>%
                 filter(num_episodes>0) %>%
                 mutate(freq_by_episode = freq / num_episodes)
    counts <- select(new_count, speaker, freq = freq_by_episode)

  }
    freq <- arrange(counts,desc(freq))
    freq
}
#' Count frequency of a given list of words
#'
#' A function that counts the frequency of a given list of words in the show
#' @param word_vec a character vector where each item is a word to count
#' @inheritParams filter_by_choose
#' @return a frequency table
#' @importFrom plyr count
#' @importFrom dplyr arrange %>%
#' @export
count_list_of_words <- function(all_scripts,word_vec,name="all",season="all",episode="all") {
  #get only the content with filter
  all_scripts <- filter_by_choose(all_scripts,name,season,episode)$content
  #combine to one string
  the_words <- paste(all_scripts,sep=" ",collapse=" ")
  the_words <- the_words %>%
                remove_punc %>% remove_white_spaces %>% split_to_words
  the_words <- the_words[the_words %in% word_vec]
  the_words <- count(the_words)
  names(the_words)[1] <- "speaker"
  arrange(the_words,desc(freq))
}


#' Plot a frequency plot
#'
#' Plot a frequency plot for the speakers
#' @param freq a frequency table
#' @param num a number indicates the number of bars in the plot
#' @param x_name the name of the x axis
#' @import ggplot2
#' @importFrom utils head
#' @return a bar plot
#' @export
plot_the_speakers <- function(freq,num,x_name) {
  freq <- head(freq,num)
  if (nrow(freq)==0) stop("Can't find any matches")
  highest <- head(freq,1)$freq
  #order the graph
  freq$speaker <- factor(freq$speaker,levels=freq$speaker[order(freq$freq,decreasing = TRUE)],ordered = TRUE)
  ggplot(data=freq,aes(x=speaker,y=freq))+geom_bar(stat="identity",color="black",fill="skyblue3",alpha=0.7)+
    labs(title = "Word Frequencies\n", x = paste0("\n",x_name), y = "Frequency\n") +
    geom_text(size=2.5,data=freq,aes(x=speaker,y=freq+highest/20,label=formatC(freq,format="d",big.mark=','))) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
