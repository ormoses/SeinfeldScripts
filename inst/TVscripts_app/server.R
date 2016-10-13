library(SeinfeldScripts)
load("data/seinfeld.rda")
load("data/friends.rda")

shinyServer(function(input, output,session) {

#  if (input$series=="Seinfeld") {
#    x <- seinfeld
#  } else if (input$series=="Friends") {
#    x <- friends
# }

  observe({
    #update the number of season according to series
    if (input$series=="Seinfeld") {
          x <- seinfeld
        } else if (input$series=="Friends") {
          x <- friends
       }
    season_list <- as.list(c("all",sort(unique(x$season))))
    max_season <- max(as.numeric(sapply(season_list[-1],max)))
    if (input$season == "all" | as.numeric(input$season) <= max_season) {
      season_selected <- input$season
    } else {
      season_selected <- as.chracter(c("all"))
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
    if (input$series=="Seinfeld") {
      x <- seinfeld
    } else if (input$series=="Friends") {
      x <- friends
    }

    freq <- count_the_speakers(x, type=input$by_what,
                               season=input$season,
                               episode=input$episode)
    plot_the_speakers(freq,15,"Speaker")
  })
  }
)
