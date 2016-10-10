library(SeinfeldScripts)
seinfeld <- readRDS("data/seinfeld.rda")
friends <- readRDS("data/friends.rda")

shinyServer(function(input, output) {
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
