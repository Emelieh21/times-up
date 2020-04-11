## server.R ##
library(shiny)
library(shinythemes)
library(dplyr)
library(shinydashboard)

values = reactiveValues(players = NULL,
                        notes = NULL,
                        gameStart = NULL,
                        people_looking_at_notes = NULL,
                        endOfRound = NULL,
                        timer = "-")

server <- function(input, output, session) { 
  # refresh button for when stuff gets weird
  observeEvent(input$reset_button, {
    # Reset all notes again to zero
    values$notes_df <- NULL
    values$notes <- NULL
    values$players <- NULL
    values$people_looking_at_notes = NULL
    values$endOfRound = NULL
    values$gameStart = NULL
  }) 
  # ================================ #
  # 1) Add new player to the game ####
  # ================================ #
  output$showJoinButton <- renderUI({
    # Hide button when player is already in the game
    if (session$token %in% values$players$session_token) {return(NULL)}
    if (is.null(values$gameStart)) {
      div(actionButton("add_player","Join Game"),
          tags$br(),
          actionButton("rejoin","What the hell, I was already there?!"))
    } else {
      div(HTML('<em>The game already started without you...</em></br>'),
          tags$br(),
          actionButton("rejoin","Bullshit, I was already in. Let me in again."))
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
  
  # When the OK button is pressed, update values$players
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
  
  # =================================== #
  # 2b) Rejoin after session refresh ####
  # =================================== #
  # If someone was already playing but the session refreshed, we want to update the sessionid in the values$players
  observeEvent(input$rejoin, {
    # see here: https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(modalDialog(title = "Existing Player",
                          # In rejoin you can pick from names that we already have in our table
                          selectInput("player_name","NAME", choices=values$players$player),
                          selectInput("team_name","TEAM NAME", choices=values$players$team),
                          footer = tagList(
                            modalButton("Cancel"),
                            actionButton("ok_rejoin", "OK")
                          )
    ))
  })
  # When the OK rejoin button is pressed, update sessiontoken in values$players
  observeEvent(input$ok_rejoin, {
    players <- values$players
    # update the token
    former_token <- players$session_token[players$player == input$player_name & players$team == input$team_name]
    players$session_token <- ifelse(players$player == input$player_name & players$team == input$team_name,
                                    session$token, players$session_token)
    values$players <- players
    print(values$players)
    # update the notes (if they exist)
    if (!is.null(values$notes)) {
      if (!former_token %in% names(values$notes)) {
        # Another chance to add your notes people!
        showModal(modalDialog(title = "Add Notes",
                              textInput("note_1",label = ""),
                              textInput("note_2",label = ""),
                              textInput("note_3",label = ""),
                              uiOutput("warning_msg"),
                              footer = tagList(
                                actionButton("add_notes_rejoin","Add Notes")
                              )
        ))
      } else {
        notes <- values$notes
        names(notes) <- gsub(former_token, session$token, names(notes))
        values$notes <- notes
        removeModal()
      }
    } else {
      # Another chance to add your notes people!
      showModal(modalDialog(title = "Add Notes",
                            textInput("note_1",label = ""),
                            textInput("note_2",label = ""),
                            textInput("note_3",label = ""),
                            uiOutput("warning_msg"),
                            footer = tagList(
                              actionButton("add_notes_rejoin","Add Notes")
                            )
      ))
    }
  })
  # for some reason I could not get this to work with the existing observeEvent :(
  observeEvent(input$add_notes_rejoin, {
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
  # needed to wrap this in an observeEvent otherwise this screen would not reappear after refresh
  observeEvent(values$notes, {
    output$hatForNotes <- renderUI({
      # Hide when there are no notes yet
      if (is.null(values$notes)) {return(NULL)}
      # Hide when game is already running and someone just refreshed the page
      if (!is.null(values$gameStart)) {
        values$gameStart = Sys.time()
        return(NULL)}
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
  observeEvent(values$gameStart, ignoreNULL = TRUE, {
    # Let's remove that ugly hat
    output$hatForNotes <- renderUI(return(NULL))
    output$gameBanner <- renderUI({
      if (is.null(values$notes_df)) {return(NULL)}
      
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
          box(width = 9, fluidRow(column(4,div(actionButton("take_note","Pick a note"),
                                               uiOutput('someone_took_a_note'))),
                                  column(8,uiOutput('timer')))
                             ))
    })
  })
  
  # Stopwatch: https://stackoverflow.com/questions/49250167/how-to-create-a-countdown-timer-in-shiny
  # Initialize the timer, 30 seconds, not active.
  timer <- reactiveVal(30)
  active <- reactiveVal(FALSE)
  # Initialize the timer, 30 seconds, not active.
  output$timer <- renderUI({
    #div(style='background-color:orange;margin-left:5px;border-style:solid;border-color:green;border-spacing:20px;border-width:1px',
    wellPanel(style = "width: 250px; background-color:#a1a1a1; border-radius: 5%", 
              h2('Stopwatch'),  
              fluidRow(column(6,numericInput('seconds','Seconds:',value=30,min=0,max=99999,step=1)), 
                       column(6,actionButton('start','Start'))),
              uiOutput('timeleft'))
  })
  # Output the time left.
  output$timeleft <- renderUI({
    HTML(paste0("<p>Time left: <b style='color:red;font-size:20px'>", values$timer,"</b></span>"))
  })
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000)
    isolate({
      if(active())
      { timer(timer()-1)
        values$timer <- timer()
        if(timer()<1)
        { active(FALSE)
          values$people_looking_at_notes <- values$people_looking_at_notes[!values$people_looking_at_notes %in% session$token]
          showModal(modalDialog(
            title = "Sorry buddy...",
            "YOUR TIME IS UP!"
          ))
        }
      }
    })
  })
  # observers for actionbuttons
  observeEvent(input$start, {
    timer(input$seconds)
    active(TRUE)
  })
  
  # Show who is looking at notes
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