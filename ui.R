## ui.R ##
library(shiny)
library(shinythemes)
library(dplyr)
library(shinydashboard)

values = reactiveValues(players = NULL,
                        notes = NULL,
                        gameStart = NULL,
                        people_looking_at_notes = NULL,
                        endOfRound = NULL)

ui <- fluidPage(theme = shinytheme("superhero"),
                tags$style("body {font-family: 'Comic Sans MS' !important;}"),
                tags$style(".btn {border-radius: 5% !important}"),
                tags$style(type='text/css', "#add_notes {margin-top: 23px;}"),
                #shinyalert::useShinyalert(),  # Set up shinyalert
                div(style="padding:50px",
                    img(src='logo.png', align = "center", height = "150px")
                ),
                uiOutput("showJoinButton"),
                uiOutput("showNotesButton"),
                uiOutput("hatForNotes"),
                uiOutput("gameBanner")
)