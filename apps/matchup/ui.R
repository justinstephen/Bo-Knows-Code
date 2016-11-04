library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(paste("Week", week)),

  # Sidebar
  fluidRow(
    column(3, 
        selectInput("var", label = "Pick a Matchup",
                    choices = matchups$matchup)),
    column(9,
        fluidRow(
          column(6,
                 textOutput("teamA"),
                 textOutput("teamAper")),
          column(6,
                 textOutput("teamB"),
                 textOutput("teamBper")),
        plotOutput("plot"))
    )
  )
))