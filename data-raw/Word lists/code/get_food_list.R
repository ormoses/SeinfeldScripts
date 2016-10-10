get_food <- function() {
  url <- "http://www.enchantedlearning.com/wordlist/food.shtml"
  txt <- readLines(url)
  txt
}

clean_food <- function(food) {
  #take just the words
  food <- food[70:938]
  #remove html <>
  food <- vapply(food,function(a) remove_between(a,matrix(c("\\<","\\>"),ncol=2,byrow=T)),character(1))
  #change to lower case
  food <- vapply(food,tolower,character(1))
  #remove white spaces
  food <- vapply(food,remove_white_spaces,character(1))
  #remove punctuation
  food <- vapply(food,remove_punc,character(1))
  #remove spaces and blanks and just characters
  food <- food[vapply(food,str_length,numeric(1))>1]
  #remove some words
  food <- food[!food %in% c("eat","dinner","hot","food","lunch","drink","order","glasses","restaurant",
                            "fire","fat","dry","fast","sweet","hungry","ice")]
  names(food) <- NULL
  food
}

