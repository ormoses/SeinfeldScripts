#' Get a raw list and return a cleaned one using \code{\link{clean_the_episode}}.
#'
#' A function that takes a raw data list and returns a cleaned list.
#' @param raw_data_list a list with the raw data.
#' @return a list with cleaned data
make_a_cleaned_list <- function(raw_data_list) {
  clean_list <- lapply(raw_data_list,clean_the_episode)
  clean_list <- clean_list[lapply(clean_list,is.null)==FALSE]
  clean_list
}

#' Arrange a cleaned list into a list of arranged dataframes
#'
#' Arrange a cleaned list into a list of arranged dataframes
#' @param clean_list a list of cleaned episodes' scripts
#' @return a list of dataframes
arrange_a_cleaned_list <- function(clean_list) {
  lapply(clean_list,arrange_episode)
}

#' Merge an arranged list into one big dataframe
#'
#' Merge an arranged list into one big dataframe
#' @param arranged_list a list of arranged dataframes
#' @return a dataframe with the arranged script of all episodes.
merge_into_big_df <- function(arranged_list) {
  x <- do.call(rbind,arranged_list)
}
