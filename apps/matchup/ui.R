library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Week", textOutput("week")),
  
  # Sidebar
  fluidRow(
    column(3, 
           uiOutput("var")),
    column(9,
           fluidRow(
             column(6, align="center",
                    span(
                      h3(textOutput("teamA")),
                      h1(textOutput("teamAper")),
                      style = "color:#ef8a62")),
             column(6, align="center",
                    span(
                      h3(textOutput("teamB")),
                      h1(textOutput("teamBper")),
                      style = "color:#67a9cf")),
             plotOutput("l.plot"),
             br(), br(), br(), br(), br(), br(),
             plotOutput("d.plot"))
    )
  )
))
