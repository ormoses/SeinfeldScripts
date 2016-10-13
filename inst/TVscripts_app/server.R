library(SeinfeldScripts)
library(dplyr)
library(stringr)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)

load("data/seinfeld.rda")
load("data/friends.rda")
load("data/food_words.rda")

shinyServer(function(input, output,session) {
  x <- reactive({
    ser <- switch(input$series,
           "Seinfeld"=seinfeld,
           "Friends"=friends)
    ser
  })

  observeEvent(input$series,{
    #update the number of season according to series
    scripts <- x()
    season_list <- as.list(c("all",sort(unique(scripts$season))))
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
    #update the character names according to series
    spks <- count_the_speakers(scripts,type="num_speaks",season="all",episode="all")
    spks <- filter(spks,freq>1)
    updateSelectInput(session,"name",
                      choices=as.list(c("all",spks$speaker)))
  })

    #update the episodes on the season
  observeEvent(input$season,{
    scripts <- x()
    if (input$season=="all") {
      episode_list <- unique(scripts$episode)

    } else {
    episode_list <- scripts %>%
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
    scripts <- x()
    freq <- count_the_speakers(scripts, type=input$by_what,
                               season=input$season,
                               episode=input$episode)
    plot_the_speakers(freq,input$bins,"Speaker")
  })

  output$word_plot <- renderPlot({
    scripts <- x()
    freq <- count_list_of_words(scripts, word_vec=food_words,
                                name=input$name,
                                season=input$season,
                                episode=input$episode)
    plot_the_speakers(freq,input$bins,"Food")
  })

  output$cloud_plot <- renderPlot({
    scripts <- x()
    make_cloud_by_name(scripts, input$name, season = input$season, episode = input$episode,
                       stopwords = input$stopwords, stemming = input$stem)
  })

  }
)
