library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(paste("Week")),

  # Sidebar
  fluidRow(
    column(3, 
           uiOutput("var")),
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