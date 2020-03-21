## app.R ##
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

server <- function(input, output, session) { 
  # ================================ #
  # 1) Add new player to the game ####
  # ================================ #
  output$showJoinButton <- renderUI({
    # Hide button when player is already in the game
    if (session$token %in% values$players$session_token) {return(NULL)}
    if (is.null(values$gameStart)) {
      actionButton("add_player","Join Game")
    } else {
      HTML('<em>Sorry, the game started without you...</em></br>')
    }
  })
  observeEvent(input$add_player, {
    # see here: https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(modalDialog(title = "New Player",
                          textInput("player_name","NAME"),
                          # You can select one of the variables that is actually being plotted (maybe we should actually just add all?)
                          textInput("team_name","TEAM NAME"),
                          footer = tagList(
                            modalButton("Cancel"),
                            actionButton("ok", "OK")
                          )
    ))
  })
  # When the OK button is pressed, update labels in RDS database and the values$labels object in the session
  observeEvent(input$ok, {
    player <- as.data.frame(input$player_name)
    names(player) <- "player"
    player$team <- input$team_name
    player$session_token <- session$token
    player$score = 0
    values$players <- rbind(values$players, player)
    print(values$players)
    removeModal()
    
    # =========================== #
    # 2) Add notes to the game ####
    # =========================== #
    # see here: https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(modalDialog(title = "Add Notes",
                          textInput("note_1",label = ""),
                          textInput("note_2",label = ""),
                          textInput("note_3",label = ""),
                          uiOutput("warning_msg"),
                          footer = tagList(
                            actionButton("add_notes","Add Notes")
                          )
    ))
  })
  observeEvent(input$add_notes, {
    notes <- c(input$note_1,input$note_2,input$note_3)
    print(notes)
    
    # If some one forgot to fill a note, show warning and stop - just validate was not enough sadly for some reason :(
    if (length(notes[notes == ""])!=0) {
      output$warning_msg <- renderUI(
        HTML("<span style = 'color: white'> Oops, you missed a note!</span>"))
    } else {
      output$warning_msg <- renderUI(return(NULL))
    }
    shiny::validate(need(length(notes[notes == ""])==0, 
                         "Oops, you missed a note!"))
    
    # Otherwise continue :)
    names(notes) <- rep(session$token, length.out =length(notes))
    values$notes <- c(values$notes, notes)
    message("Saved!")
    removeModal()
  })
  
  # ========================================= #
  # 3) Hat screen, waiting for all players ####
  # ========================================= #
  output$hatForNotes <- renderUI({
    # Hide when there are no notes yet
    if (is.null(values$notes)) {return(NULL)}
    # Hide when player did not add notes yet
    if (!is.null(values$notes)) {
      if (!session$token %in% names(values$notes)) {return(NULL)}
    }
    number_of_players <- nrow(values$players)
    number_of_notes <- length(values$notes)
    
    # Start game button when number of notes is 100%
    if (number_of_notes/(number_of_players*3) == 1) {
      div(style="padding:50px; text-align: center",
          p("All set, press the button to..."),
          actionButton("start_game", "START THE GAME!"),
          h1(paste0(round((number_of_notes/(number_of_players*3))*100,0),"%")),
          img(src='hat.png', align = "center", height = "600px"))
    } else {
      div(style="padding:50px; text-align: center",
          p("Waiting for other players..."),
          actionButton('remove_players','Remove players'),
          h1(paste0(round((number_of_notes/(number_of_players*3))*100,0),"%")),
          h3(paste0('Players: ',paste(values$players$player, collapse = ', '))),
          img(src='hat.png', align = "center", height = "600px"))
    }
  })
  observeEvent(input$remove_players, {
    # see here: https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(modalDialog(title = "Delete player",
                          selectInput("player_to_be_deleted","Player to be deleted:",
                                      choices= values$players$player, multiple = TRUE),
                          footer = tagList(
                            modalButton("Cancel"),
                            actionButton("ok2", "OK")
                          )
    ))
  })
  # When OK button is pressed, remove the player that should be deleted from values$players
  observeEvent(input$ok2, {
    print(input$player_to_be_deleted)
    players <- values$players
    players <- subset(players, ! player %in% input$player_to_be_deleted)
    print(players)
    values$players <- players
    message("players updated.")
    print(values$players)
    removeModal()
  })
  
  # ========================== #
  # 4) Let the games begin! ####
  # ========================== #
  observeEvent(input$start_game, {
    # Shuffle the notes
    values$notes <- values$notes[sample(1:length(values$notes),length(values$notes))]
    
    # turn the notes into a df, so we can assign a column that indicates if they have already been guessed
    df <- as.data.frame(values$notes)
    names(df) <- "notes"
    df$guessed <- 0
    values$notes_df <- df
    
    # trigger game start
    values$gameStart = Sys.time()
  })
  observeEvent(values$gameStart, {
    # Let's remove that ugly hat
    output$hatForNotes <- renderUI(return(NULL))
    output$gameBanner <- renderUI({
      # Don't show to people not in the game
      if (!session$token %in% values$players$session_token) {return(NULL)}
      
      # Add the scores on the left hand side
      htmlText <- values$players %>%
        group_by(team) %>%
        summarise(
          summary = paste0("<summary><b>",unique(team),"</b>: ",sum(score)," points</summary>"),
          details = paste0(paste(paste0("<li>",player,"</li>"),
                                 collapse=" ")),
          txt = paste0("<details>",summary,"<ul style ='list-style-type: none;'>",details,"</ul></details>"))
      
      div(box(width = 3, HTML(htmlText$txt)),
          box(width = 9, div(actionButton("take_note","Take a note"),
                             uiOutput('someone_took_a_note'))))
    })
  })
  output$someone_took_a_note <- renderUI({
    if (is.null(values$people_looking_at_notes)) {return(NULL)}
    if (length(values$people_looking_at_notes) == 0) {return(NULL)}
    players <- paste(values$players$player[values$players$session_token %in% values$people_looking_at_notes], collapse = ' & ')
    HTML('<em>',players,
         ifelse(length(values$people_looking_at_notes) > 1, ' are looking at notes...', ' is looking at notes...'),'</em>')
  })
  observeEvent(input$take_note, {
    # keep track of who is looking at notes...
    values$people_looking_at_notes <- c(values$people_looking_at_notes, session$token)
    i <- sample(1:nrow(values$notes_df[values$notes_df$guessed==0,]),1)
    # see here: https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(modalDialog(title = "Your Note",
                          h1(values$notes_df$notes[values$notes_df$guessed==0][i]),
                          footer = tagList(
                            actionButton("cancel", "Cancel"),
                            actionButton("point", "Someone guessed it!")
                          )
    ))
  })
  observeEvent(input$cancel, {
    values$people_looking_at_notes <- values$people_looking_at_notes[!values$people_looking_at_notes %in% session$token]
    removeModal()
  })
  observeEvent(input$point, {
    # One point for the player/team
    values$players$score[values$players$session_token == session$token] <- 1 + values$players$score[values$players$session_token == session$token]
    values$notes_df$guessed[values$notes_df$guessed==0][1] = 1
    values$people_looking_at_notes <- values$people_looking_at_notes[!values$people_looking_at_notes %in% session$token]
    removeModal()
    
    if (nrow(values$notes_df[values$notes_df$guessed==0,]) == 0) {
      values$endOfRound <- Sys.time()
    } 
  })
  
  # ==================== #
  # 5) End of a round ####
  # ==================== #
  observeEvent(values$endOfRound, {
    output$gameBanner <- renderUI({
      # Don't show to people not in the game
      if (!session$token %in% values$players$session_token) {return(NULL)}
      
      winner <- values$players %>%
        group_by(team) %>%
        summarise(score = sum(score)) %>%
        ungroup() %>%
        filter(score == max(score)) 
      winner <- paste(winner$team, collapse = ' & ')
      
      # Add the scores on the left hand side
      htmlText <- values$players %>%
        group_by(team) %>%
        summarise(
          summary = paste0("<summary><b>",unique(team),"</b>: ",sum(score)," points</summary>"),
          details = paste0(paste(paste0("<li>",player,"</li>"),
                                 collapse=" ")),
          txt = paste0("<details>",summary,"<ul style ='list-style-type: none;'>",details,"</ul></details>"))
      
      div(box(width = 3, HTML(htmlText$txt)),
          box(width = 9, div(h1('Halleluja! We have a winner!'),
                             h3(paste0('Congratulations team ',winner,'!')),
                             actionButton('next_round','Next Round'),
                             actionButton('end_game','End Game'))
          ))
    })
  })
  observeEvent(input$next_round, {
    # Reset all notes again to zero
    values$notes_df$guessed <- 0
    values$endOfRound = NULL
    values$gameStart = Sys.time()
  })
  observeEvent(input$end_game, {
    # Reset all notes again to zero
    values$notes_df <- NULL
    values$notes <- NULL
    values$players <- NULL
    values$people_looking_at_notes = NULL
    values$endOfRound = NULL
    values$gameStart = NULL
  })
}

shinyApp(ui, server)


# observeEvent(input$start_timer, {
#   values$timerValue <- 0
# })
# output$timerGlobal <- renderText({
#   invalidateLater(1000)
#   print("Running...")
#   if (is.null(values$timerValue)) {return(NULL)}
#   if (values$timerValue == 30) {return("Your time is up, buddy...")}
#   remainingTime = 30 - values$timerValue
#   values$timerValue = values$timerValue + 1
#   paste("00:", remainingTime)
# })
# for (note in values$notes) {
#   one = values$players$session_token[1]
#   if (one == session$token) {
#     showNotification(note, closeButton = TRUE, type = 'warning')
#   } else {
#     showNotification("Some one else is playing", closeButton = TRUE)
#   }
#   Sys.sleep(30)
# }
# # testing different themes
# shinyApp(
#   ui = fluidPage(
#     shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
#     sidebarPanel(
#       textInput("txt", "Text input:", "text here"),
#       sliderInput("slider", "Slider input:", 1, 100, 30),
#       actionButton("action", "Button"),
#       actionButton("action2", "Button2", class = "btn-primary")
#     ),
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Tab 1"),
#         tabPanel("Tab 2")
#       )
#     )
#   ),
#   server = function(input, output) {}
# )
# =========================== #
# 2) Add notes to the game ####
# =========================== #
# output$showNotesButton <- renderUI({
#   # Hide button when player did not sign up yet...
#   if (!session$token %in% values$players$session_token) {return(NULL)}
#   # Hide button when player already added 3 notes
#   if (!is.null(values$notes)) {
#     if (session$token %in% names(values$notes)) {return(NULL)}
#   }
#   div(
#     h3("Please add your three notes to the game..."),
#     fluidRow(
#       column(3, textInput("note_1",label = "")),
#       column(3, textInput("note_2",label = "")),
#       column(3, textInput("note_3",label = "")),
#       actionButton("add_notes","Add Notes")),
#     uiOutput("warning_msg")
#     )
# })
#shinyalert::shinyalert("Yay!", paste0("Welcome to the game ", input$player_name,"!"), type = "success")
