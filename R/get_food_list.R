get_food <- function() {
  url <- "http://www.enchantedlearning.com/wordlist/food.shtml"
  txt <- htmlToText(url)
  txt
}
