#' Remove a pattern
#'
#' Remove a pattern from a string with a type (before, after, between)
#' @param string a string to remove pattern from
#' @param pattern a regular expression that indicates the pattern to remove
#' @param type a string indicates wheater "before","after"
#' @param occur string indicates weather to remove from/till "first" or "last" occurance
#' @importFrom stringr str_length str_locate_all
#' @return string after removing the pattern
remove_pattern <- function(string,pattern,type,occur) {
  first <- 1
  last <- str_length(string)
  #get the locations of all occurances
  locs <- str_locate_all(string,pattern)[[1]]
  print(locs)
  #pick the first/last according to occur
  if (occur=="first") {
    locs <- locs[1,]
  } else if (occur=="last") {
    locs <- locs[nrow(locs),]
  }
  print(locs)
  #accordiong to type - pick the range to remove
  if (type=="before") {
    last <- locs[1]-1
  } else if (type=="after") {
    first <- locs[2]+1
  }
  substr(string,first,last)
}

#' Remove everything before a pattern
#'
#' Remove everything before a pattern in a string
#' @inheritParams remove_pattern
#' @return
#' @export a string after removing everything before the pattern
remove_before <- function(string,pattern,occur="first") {
  remove_pattern(string,pattern,type="before",occur)
}
