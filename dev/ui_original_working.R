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
                tags$style("#reset_button {font-size: 50% !important}"),
                tags$style("#start {margin-top: 23px; !important}"),
                tags$style(type='text/css', "#add_notes {margin-top: 23px;}"),
                div(style="padding:45px",
                    img(src='logo.png', align = "center", height = "150px")
                ),
                # add tiny refresh button for when shit hits the fan
                absolutePanel(top = 20, right = 25,
                              actionButton("reset_button", "", icon = icon("refresh"))
                ),
                uiOutput("showJoinButton"),
                uiOutput("showNotesButton"),
                uiOutput("hatForNotes"),
                uiOutput("gameBanner")
)