# TVscriptsR
Statistics and charts from TV scripts.
Currently has Seinfeld and Friends.

## Shiny app using the package:
https://ormoses.shinyapps.io/TVscripts_app/

## Package's key subjects:
1. Statistics for TV scripts
1. Charts for TV scripts' statistics
1. Shiny App using the statistics and charts mentioned above
1. String manipulation helper functions (To get and clean raw data from the web)

### Statistics for TV scripts:
There are a few functions in the package to compute statistics of a series:

(Each function can be filtered by season and episode and character name where appropriate) 

1. ```
count_episodes_appear
``` - Count for each character in how many episode he was in.
1. ```
count_the_speakers
``` - Count for each character the number of times he speaks / the number of word he speaks.
1. ```
count_list_of_words
``` - Counts the frequency of words in the series from a list of words

### Charts functions:
1. ```
plot_the_speakers
``` - plot function to use with the "```count_```" family of functions (above)
1. ```
make_cloud_by_name
``` - Creates a wordcloud of the words in the scripts.

