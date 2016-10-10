library(SeinfeldScripts)
load("seinfeld.Rda")
load("friends.Rda")

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
