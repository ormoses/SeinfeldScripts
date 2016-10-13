shinyUI(fluidPage(
    titlePanel("TV Series scripts analysis"),

    sidebarLayout(
      sidebarPanel(h3("Plot who speaks most"),
        selectInput("series",
                    label = h3("Select a series"),
                    choices = list("Seinfeld" = "Seinfeld", "Friends" = "Friends"),
                    selected = "Seinfeld"),
        selectInput("season",
                     label = h4("Select a season"),
                    choices=as.list(c("all",1:9))),
        selectInput("episode",
                     label = h4("Select an episode"),
                     choices=as.list(c("all",1:24))),
        radioButtons("by_what",
                     label = h4("by what to plot?"),
                     choices=list("Number of time character speaks" = "num_speaks",
                                  "Number of words a character speaks" = "num_words"))
      ),


      mainPanel(
        tabsetPanel(type="tabs",
          tabPanel("Character Speaking",plotOutput("freq_plot")),
          tabPanel("Food Word Frequency",plotOutput("word_plot"))
        )
      )
    )
))
