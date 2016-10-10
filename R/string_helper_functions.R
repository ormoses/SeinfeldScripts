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
#' @return a string after removing everything before the pattern
#' @export
remove_before <- function(string,pattern,occur="first") {
  remove_pattern(string,pattern,type="before",occur)
}

#' Remove everything after a pattern
#'
#' Remove everything after a pattern in a string
#' @inheritParams remove_pattern
#' @return a string after removing everything after the pattern
#' @export
remove_after <- function(string,pattern,occur="last") {
  remove_pattern(string,pattern,type="after",occur)
}

#' Remove everything that matches a pattern
#'
#' Remove everything that matches a pattern in a string
#' @inheritParams remove_pattern
#' @return a string after removing everything that matches the pattern
#' @export
remove_exact <- function(string,pattern) {
  gsub(pattern,"",string)
}

#' Remove everything between two patterns
#'
#' Remove everything between two patterns in a string
#' @param pattern a character matrix with 2 columns. The first element is the left pattern and the
#' second is the right (if the pattern needs escaping with backslash then add it.
#' @inheritParams remove_pattern
#' @importFrom stringr str_replace_all
#' @return a string after removing everything between the two patterns
#' @export
remove_between <- function(string,pattern) {
  #get all the patterns
  new_pat <- vapply(seq_len(nrow(pattern)),function(a) paste0(pattern[a,1],".*?",pattern[a,2]),character(1))
  #add | (or) to make a large pattern
  new_pat <- paste(new_pat,sep="",collapse="|")
  str_replace_all(string,new_pat,"")
}

