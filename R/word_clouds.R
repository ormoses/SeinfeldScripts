#' Make a word cloud from a dataframe
#'
#' Make a word cloud from a dataframe
#' @param all_scripts a dataframe to make cloud from
#' @param logical. Indicates weather to remove stopwords or not.
#' @inheritParams filter_by_choose
#' @import tm RColorBrewer wordcloud SnowballC
#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter
#' @return A word cloud chart
#' @export
make_cloud_by_name <- function(all_scripts,name,stopwords=FALSE,season="all",episode="all") {

  #filter
  df <- filter_by_choose(all_scripts,the_name = name,the_season = season,
                   the_episode = episode)

  df <- df$content

  #make a string
  df <- paste(df,sep=" ",collapse="")
  #remove stop words
  if (stopwords==FALSE) {
    stopwords_regex <-  paste(stopwords('en'), collapse = '\\b|\\b')
    stopwords_regex <-  paste0('\\b', stopwords_regex, '\\b')
    df <-  str_replace_all(df, stopwords_regex, '')
  }
  a <- Corpus(VectorSource(df))
  a <- tm_map(a,stripWhitespace)
  a <- tm_map(a,removeNumbers)
  a <- tm_map(a,removePunctuation)
  a <- tm_map(a,content_transformer(tolower))
  #a <- tm_map(a,removeWords,stopwords)
  a <- tm_map(a,stemDocument)

  wordcloud(a, scale=c(4,0.5), max.words=150, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,colors=brewer.pal(8, "Set2"))
}
