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
  #pick the first/last according to occur
  if (occur=="first") {
    locs <- locs[1,]
  } else if (occur=="last") {
    locs <- locs[nrow(locs),]
  }
  #accordiong to type - pick the range to remove
  if (type=="after") {
    last <- locs[1]-1
  } else if (type=="before") {
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
#' @param sep a seperator instead of the removed pattern
#' @inheritParams remove_pattern
#' @return a string after removing everything that matches the pattern
#' @export
remove_exact <- function(string,pattern,sep=" ") {
  gsub(pattern,sep,string)
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

#' Remove white spaces
#'
#' Remove white spaces from a string
#' @param string a string to remove spaces from
#' @return a string after removing punctuation
#' @export
remove_punc <- function(string) {
  remove_exact(string,"[.]|[,]|[?]|[!]|[:]|[;]|[-]|[_]|[']|[ֲ]",sep="")
}

#' Remove punctuation
#'
#' Remove punctuation from a string
#' @param string a string to remove punctuation from
#' @importFrom stringr str_replace_all
#' @return a string after removing white spaces
#' @export
remove_white_spaces <- function(string) {
  str_trim(gsub(" +"," ",string))
}

#' Split a string into vector of words
#'
#' Split a string into character vector of words
#' @param string a string to split into words
#' @param sep a seperator
#' @importFrom stringr str_split
#' @return a character vector where each word is an item
#' @export
split_to_words <- function(string,sep=" ") {
  unlist(str_split(string,sep))
}

#' Stem words using SnowballC
#'
#' Split a string into character vector of words
#' @param string a character vector of words
#' @importFrom SnowballC wordStem
#' @return a character vector where each word is stemmed
#' @export
stem_words <- function(string) {
  wordStem(string)
}
