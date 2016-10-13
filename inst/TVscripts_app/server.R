library(SeinfeldScripts)
load("data/seinfeld.rda")
load("data/friends.rda")

shinyServer(function(input, output,session) {

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
    #updateSelectInput(session,"episode",
                      #label = h4("Select a season"),
     #                 choices=as.list(sort(unique(x$season)))
    #)
 # })

  output$freq_plot <- renderPlot({

    x <-  switch(input$series,
                 "Seinfeld"=seinfeld,
                 "Friends"=friends)

    freq <- count_the_speakers(x, type=input$by_what,
                               season=input$season,
                               episode=input$episode)
    plot_the_speakers(freq,15,"Speaker")
  })
  }
)
