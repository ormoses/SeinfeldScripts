shinyUI(fluidPage(
    titlePanel("TV Series scripts analysis"),

    sidebarLayout(
      sidebarPanel(h3("Plot Frequency Table"),
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
        conditionalPanel(condition = "input.tabs1 == 'speak'",
          radioButtons("by_what",
                       label = h4("by what to plot?"),
                       choices = list("Number of time character speaks" = "num_speaks",
                                  "Number of words a character speaks" = "num_words"))
        ),
        conditionalPanel(condition = ("input.tabs1 == 'speak'"),
                         checkboxInput("scale",
                                       label = h5("Scale by episode appeared"),
                                       value = FALSE)
        ),
        conditionalPanel(condition = ("input.tabs1 == 'words' | input.tabs1 == 'cloud'"),
                         selectInput("name",
                                      label = h4("Select Character Name"),
                                      choices = "all")
        ),
        conditionalPanel(condition = ("input.tabs1 == 'speak' | input.tabs1 == 'words'"),
                         sliderInput("bins",
                                     label = h4("Select number of bins"),
                                     min = 1,max = 25,value = 10)
        ),
        conditionalPanel(condition = ("input.tabs1 == 'cloud'"),
                         checkboxInput("stopwords",
                                     label = h5("Remove stop words"),
                                     value = FALSE),
                         checkboxInput("stem",
                                       label = h5("Use stemming algorithm"),
                                       value = FALSE)
        )
      ),


      mainPanel(
        tabsetPanel(type="tabs",id="tabs1",
          tabPanel("Character Speaking", plotOutput("freq_plot"), value = "speak"),
          tabPanel("Food Word Frequency", plotOutput("word_plot"), value = "words"),
          tabPanel("Word Cloud", plotOutput("cloud_plot"), value = "cloud")
        )
      )
    )
))
