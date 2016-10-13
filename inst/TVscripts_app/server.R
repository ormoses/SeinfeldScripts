library(SeinfeldScripts)
library(dplyr)
library(stringr)
load("data/seinfeld.rda")
load("data/friends.rda")
load("data/food_words.rda")

shinyServer(function(input, output,session) {
  #x <- reactiveValues()

  observeEvent(input$series,{
    #update the number of season according to series
   x <-  switch(input$series,
            "Seinfeld"=seinfeld,
            "Friends"=friends)

    season_list <- as.list(c("all",sort(unique(x$season))))
    max_season <- max(as.numeric(sapply(season_list[-1],max)))
    if (input$season == "all" | suppressWarnings(as.numeric(input$season)) <= max_season) {
      season_selected <- input$season
    } else {
      season_selected <- "all"
    }
    updateSelectInput(session,"season",
                      choices=season_list,
                      selected=season_selected
    )
  })

    #update the episodes on the season
  observeEvent(input$season,{
    x <-  switch(input$series,
                 "Seinfeld"=seinfeld,
                 "Friends"=friends)
    if (input$season=="all") {
      episode_list <- unique(x$episode)

    } else {
    episode_list <- x %>%
                    filter(season==as.numeric(input$season)) %>%
                    select(episode) %>%
                    unique
    episode_list <- episode_list$episode

    }

    updateSelectInput(session,"episode",
                      choices=as.list(c("all",episode_list)),
                      selected = "all"
    )
  })

  output$freq_plot <- renderPlot({
    x <-  switch(input$series,
                 "Seinfeld"=seinfeld,
                 "Friends"=friends)

    freq <- count_the_speakers(x, type=input$by_what,
                               season=input$season,
                               episode=input$episode)
    plot_the_speakers(freq,15,"Speaker")
  })

  output$word_plot <- renderPlot({

    x <-  switch(input$series,
                 "Seinfeld"=seinfeld,
                 "Friends"=friends)

    freq <- count_list_of_words(x, word_vec=food_words,
                               season=input$season,
                               episode=input$episode)
    plot_the_speakers(freq,15,"Food")
  })

  }
)
