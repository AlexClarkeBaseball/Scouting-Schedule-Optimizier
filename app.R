library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(DT)
library(lpSolve)

playerDataPath <- "C:/Users/alexa/Downloads/DraftedD1PlayerList.xlsx"
scheduleDataPath <- "C:/Users/alexa/Downloads/2024Schedules.xlsx"

playerData <- tryCatch({
  df <- read_excel(playerDataPath)
  
  actual_colnames <- colnames(df)
  message("Actual column names in player data file: ", paste(actual_colnames, collapse = ", "))
  
  if(ncol(df) < 5) {
    stop("Player data file doesn't have the expected number of columns (at least 5 needed)")
  }
  
  names(df) <- make.names(names(df), unique = TRUE)
  
  if("name" %in% tolower(colnames(df))) {
    name_col <- which(tolower(colnames(df)) == "name")[1]
    colnames(df)[name_col] <- "Player_Name"
  } else {
    colnames(df)[1] <- "Player_Name"
  }
  
  if("school" %in% tolower(colnames(df))) {
    school_col <- which(tolower(colnames(df)) == "school")[1]
    colnames(df)[school_col] <- "School"
  } else {
    colnames(df)[2] <- "School"
  }
  
  
  bonus_col <- NULL
  if(any(grepl("signing|bonus", tolower(colnames(df))))) {
    bonus_col <- which(grepl("signing|bonus", tolower(colnames(df))))[1]
  } else {
    
    bonus_col <- min(5, ncol(df))
  }
  
  if(!is.null(bonus_col)) {
    colnames(df)[bonus_col] <- "Signing_Bonus"
  }
  
  
  if(ncol(df) >= 3) colnames(df)[3] <- "Position"
  if(ncol(df) >= 4) colnames(df)[4] <- "Role"
  
  
  if("Signing_Bonus" %in% colnames(df)) {
    if(!is.numeric(df$Signing_Bonus)) {
      df$Signing_Bonus <- as.numeric(gsub("[^0-9.]", "", df$Signing_Bonus))
    }
  } else {
  }
  
  
  df$Player_Name <- as.character(df$Player_Name)
  df$School <- as.character(df$School)
  
  message("Final column names: ", paste(colnames(df), collapse = ", "))
  df
}, error = function(e) {
  message("Error reading player data: ", e$message)
  data.frame(
    Player_Name = character(),
    School = character(),
    Position = character(),
    Role = character(),
    Signing_Bonus = numeric()
  )
})

scheduleData <- tryCatch({
  message("Reading schedule data from: ", scheduleDataPath)
  df <- read_excel(scheduleDataPath)
  
  
  message("Raw schedule columns: ", paste(colnames(df), collapse=", "))
  
  
  names(df) <- make.names(names(df), unique = TRUE)
  
  
  
  if(ncol(df) >= 9) {
    standardColumns <- c("Date", "Day_of_Week", "Away_Team", "Home_Team", "Location",
                         "Game_Title", "Game_ID", "Weeks_Since_Season", "Double_Header")
    names(df)[1:9] <- standardColumns
    
    if("Weeks_Since_Season" %in% names(df)) {
      message("Found Weeks_Since_Season column with classes: ", paste(class(df$Weeks_Since_Season), collapse=", "))
      message("Sample values: ", paste(head(df$Weeks_Since_Season), collapse=", "))
      
      tryCatch({
        
        df$Weeks_Since_Season <- as.character(df$Weeks_Since_Season)
        
        df$Weeks_Since_Season <- suppressWarnings(as.numeric(df$Weeks_Since_Season))
        
        df$Weeks_Since_Season[is.na(df$Weeks_Since_Season)] <- 0
        message("Successfully converted Weeks_Since_Season to numeric")
        message("Final sample values: ", paste(head(df$Weeks_Since_Season), collapse=", "))
      }, error = function(e) {
        message("Error converting Weeks_Since_Season to numeric: ", e$message)
        
        df$Weeks_Since_Season <- 0
      })
    } else {
      message("WARNING: Weeks_Since_Season column not found after standardization")
      df$Weeks_Since_Season <- 0
    }
    
    
    if("Double_Header" %in% names(df)) {
      message("Found Double_Header column with classes: ", paste(class(df$Double_Header), collapse=", "))
      message("Sample Double_Header values: ", paste(head(df$Double_Header), collapse=", "))
      
      
      df$Double_Header <- sapply(df$Double_Header, function(x) {
        is_dh <- FALSE
        if (is.logical(x)) {
          is_dh <- x
        } else if (!is.na(x)) {
          
          x_str <- tolower(as.character(x))
          is_dh <- x_str %in% c("yes", "y", "true", "t", "1", "doubleheader", "double header")
        }
        return(is_dh)
      })
      message("Processed Double_Header values: ", paste(head(df$Double_Header), collapse=", "))
    } else {
      message("WARNING: Double_Header column not found, adding default column")
      df$Double_Header <- FALSE
    }
    
    if(!inherits(df$Date, "Date")) {
      df$Date <- as.Date(df$Date)
    }
    
    df$Display <- paste(df$Game_Title, "at", df$Location)
  } else {
    stop("Schedule file doesn't have the expected 9 columns")
  }
  
  message("Final schedule columns: ", paste(colnames(df), collapse=", "))
  df
}, error = function(e) {
  message("Error reading schedule data: ", e$message)
  data.frame(Date = as.Date(character()),
             Day_of_Week = character(),
             Away_Team = character(),
             Home_Team = character(),
             Location = character(),
             Game_Title = character(),
             Game_ID = character(),
             Weeks_Since_Season = numeric(),
             Double_Header = logical(),
             Display = character())
})

isFridayGame <- function(game) {
  if("Day_of_Week" %in% colnames(game)) {
    day <- tolower(as.character(game$Day_of_Week))
    return(day == "friday")
  }
  
  if("Date" %in% colnames(game) && inherits(game$Date, "Date")) {
    return(weekdays(game$Date) == "Friday")
  }
  
  return(FALSE)
}


ui <- dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Selection", tabName = "player_selection", icon = icon("user")),
      menuItem("Optimal Schedule", tabName = "schedule", icon = icon("calendar"))
    ),
    br(),
    fluidRow(
      column(12, align = "center",
             actionButton("optimize", "Generate Optimal Schedule",
                          style = "color: #fff; background-color:rgb(183, 126, 51); border-color:rgb(183, 126, 51);")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style("
        /* Hide sidebar toggle */
        .sidebar-toggle { display: none !important; }
       
        /* Make title span full width */
        .main-header .logo {
          width: 100% !important;
          font-size: 24px !important;
          font-weight: bold !important;
          text-align: center !important;
        }
       
        /* Adjust navbar right section */
        .main-header .navbar-custom-menu, .main-header .navbar-right {
          display: none;
        }
      ")
    ),
    div(style = "width: 100%; background-color: #3c8dbc; padding: 15px; margin-bottom: 15px;",
        h1("Scouting Schedule Optimizer", style = "color: white; text-align: center; margin: 0; font-weight: bold;")
    ),
    
    tabItems(
      tabItem(tabName = "player_selection",
              
              fluidRow(
                box(
                  title = "Available Players", width = 12, status = "primary",
                  DTOutput("playersTable")
                )
              ),
              fluidRow(
                box(
                  title = "Selected Players",
                  status = "primary",
                  width = 12,
                  DTOutput("selectedPlayersTable"),
                  actionButton("clearPlayers", "Clear All Selected Players",
                               class = "btn-warning", style = "margin-top: 10px;")
                )
              )
      ),
      
      tabItem(tabName = "schedule",
              fluidRow(
                box(
                  title = "Optimization Summary", width = 12, status = "info",
                  textOutput("optimizationSummary")
                )
              ),
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel("Optimal Schedule", DTOutput("optimalSchedule")),
                  tabPanel("Player Coverage", DTOutput("playerCoverage"))
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  selectedPlayers <- reactiveVal(data.frame())
  fri_starter_times_seen <- reactiveVal(list())
  sun_starter_times_seen <- reactiveVal(list())
  sat_starter_times_seen <- reactiveVal(list())
  top_pos_player_times_seen <- reactiveVal(list())
  top_reliever_times_seen <- reactiveVal(list())
  player_appearances <- reactiveValues()
  filteredPlayers <- reactiveVal(playerData)
  optimalSchedule <- reactiveVal(NULL)
  
  trackFridayStarterAppearance <- function(player_name, game_date, current_tracking = NULL) {
    cat("Player:", player_name, "on date:", game_date, "\n")
    
    if (!player_name %in% names(player_appearances)) {
      player_appearances[[player_name]] <- list(appearances = list())
      cat("Initialized new tracking for player:", player_name, "\n")
    }
    
    player_data <- player_appearances[[player_name]]
    prior_appearances <- player_data$appearances
    
    appearances_before_this_game <- 0
    if (length(prior_appearances) > 0) {
      prior_dates <- names(prior_appearances)
      
      appearances_before_this_game <- sum(prior_dates < game_date, na.rm = TRUE)
      
      cat("Found", length(prior_dates), "previous appearance dates for", player_name, "\n")
      cat("Previous dates:", paste(prior_dates, collapse=", "), "\n")
      cat("Appearances before", game_date, ":", appearances_before_this_game, "\n")
    } else {
      cat("No previous appearances found for", player_name, "\n")
    }
    
    current_times_seen <- appearances_before_this_game + 1
    
    player_data$appearances[[game_date]] <- list(
      date = game_date,
      appearance_number = current_times_seen
    )
    
    player_appearances[[player_name]] <- player_data
    
    times_seen_decay <- 1 / (current_times_seen ^ 0.75)
    
    cat("COMPLETELY ISOLATED PLAYER-SPECIFIC TRACKING:\n")
    cat("Player:", player_name, "\n")
    cat("Times seen (including today):", current_times_seen, "\n")
    cat("Calculated decay factor: 1/", current_times_seen, "^0.75 =", round(times_seen_decay, 4), "\n")
    
    return(list(
      times_seen = current_times_seen,
      decay_factor = times_seen_decay
    ))
  }
  
  trackSundayStarterAppearance <- function(player_name, game_date, current_tracking = NULL) {
    cat("Player:", player_name, "on date:", game_date, "\n")
    
    if (!player_name %in% names(player_appearances)) {
      player_appearances[[player_name]] <- list(appearances = list(), sunday_appearances = list())
      cat("Initialized new tracking for player:", player_name, "\n")
    } else if (!"sunday_appearances" %in% names(player_appearances[[player_name]])) {
      player_appearances[[player_name]]$sunday_appearances <- list()
    }
    
    player_data <- player_appearances[[player_name]]
    prior_appearances <- player_data$sunday_appearances
    
    appearances_before_this_game <- 0
    if (length(prior_appearances) > 0) {
      prior_dates <- names(prior_appearances)
      
      appearances_before_this_game <- sum(prior_dates < game_date, na.rm = TRUE)
      
      cat("Found", length(prior_dates), "previous Sunday appearance dates for", player_name, "\n")
      cat("Previous Sunday dates:", paste(prior_dates, collapse=", "), "\n")
      cat("Sunday appearances before", game_date, ":", appearances_before_this_game, "\n")
    } else {
      cat("No previous Sunday appearances found for", player_name, "\n")
    }
    
    current_times_seen <- appearances_before_this_game + 1
    
    player_data$sunday_appearances[[game_date]] <- list(
      date = game_date,
      appearance_number = current_times_seen
    )
    
    player_appearances[[player_name]] <- player_data
    
    times_seen_decay <- 1 / (current_times_seen ^ 0.75)
    
    cat("COMPLETELY ISOLATED PLAYER-SPECIFIC SUNDAY TRACKING:\n")
    cat("Player:", player_name, "\n")
    cat("Sunday times seen (including today):", current_times_seen, "\n")
    cat("Calculated Sunday decay factor: 1/", current_times_seen, "^0.75 =", round(times_seen_decay, 4), "\n")
    
    return(list(
      times_seen = current_times_seen,
      decay_factor = times_seen_decay
    ))
  }
  
  trackSaturdayStarterAppearance <- function(player_name, game_date, current_tracking = NULL) {
    cat("Player:", player_name, "on date:", game_date, "\n")
    
    if (!player_name %in% names(player_appearances)) {
      player_appearances[[player_name]] <- list(appearances = list(), sunday_appearances = list(), saturday_appearances = list())
      cat("Initialized new tracking for player:", player_name, "\n")
    } else if (!"saturday_appearances" %in% names(player_appearances[[player_name]])) {
      player_appearances[[player_name]]$saturday_appearances <- list()
    }
    
    player_data <- player_appearances[[player_name]]
    prior_appearances <- player_data$saturday_appearances
    
    appearances_before_this_game <- 0
    if (length(prior_appearances) > 0) {
      prior_dates <- names(prior_appearances)
      
      appearances_before_this_game <- sum(prior_dates < game_date, na.rm = TRUE)
      
      cat("Found", length(prior_dates), "previous Saturday appearance dates for", player_name, "\n")
      cat("Previous Saturday dates:", paste(prior_dates, collapse=", "), "\n")
      cat("Saturday appearances before", game_date, ":", appearances_before_this_game, "\n")
    } else {
      cat("No previous Saturday appearances found for", player_name, "\n")
    }
    
    current_times_seen <- appearances_before_this_game + 1
    
    player_data$saturday_appearances[[game_date]] <- list(
      date = game_date,
      appearance_number = current_times_seen
    )
    
    player_appearances[[player_name]] <- player_data
    
    times_seen_decay <- 1 / (current_times_seen ^ 0.75)
    
    cat("COMPLETELY ISOLATED PLAYER-SPECIFIC SATURDAY TRACKING:\n")
    cat("Player:", player_name, "\n")
    cat("Saturday times seen (including today):", current_times_seen, "\n")
    cat("Calculated Saturday decay factor: 1/", current_times_seen, "^0.75 =", round(times_seen_decay, 4), "\n")
    
    return(list(
      times_seen = current_times_seen,
      decay_factor = times_seen_decay
    ))
  }
  
  trackTopPosPlayerAppearance <- function(player_name, game_date, is_weekend, current_tracking = NULL) {
    cat("Player:", player_name, "on date:", game_date, "is weekend:", is_weekend, "\n")
    
    if (!player_name %in% names(player_appearances)) {
      player_appearances[[player_name]] <- list(
        appearances = list(),
        sunday_appearances = list(),
        saturday_appearances = list(),
        top_pos_appearances = list()
      )
      cat("Initialized new tracking for player:", player_name, "\n")
    } else if (!"top_pos_appearances" %in% names(player_appearances[[player_name]])) {
      player_appearances[[player_name]]$top_pos_appearances <- list()
    }
    
    player_data <- player_appearances[[player_name]]
    prior_appearances <- player_data$top_pos_appearances
    
    appearances_before_this_game <- 0
    if (length(prior_appearances) > 0) {
      prior_dates <- names(prior_appearances)
      
      appearances_before_this_game <- sum(prior_dates < game_date, na.rm = TRUE)
      
      cat("Found", length(prior_dates), "previous TopPosPlayer appearance dates for", player_name, "\n")
      cat("Previous TopPosPlayer dates:", paste(prior_dates, collapse=", "), "\n")
      cat("TopPosPlayer appearances before", game_date, ":", appearances_before_this_game, "\n")
    } else {
      cat("No previous TopPosPlayer appearances found for", player_name, "\n")
    }
    
    current_times_seen <- appearances_before_this_game + 1
    
    player_data$top_pos_appearances[[game_date]] <- list(
      date = game_date,
      appearance_number = current_times_seen
    )
    
    player_appearances[[player_name]] <- player_data
    
    if (is_weekend) {
      times_seen_decay <- 1 / (current_times_seen ^ 0.75)
    } else {
      times_seen_decay <- 1 / (0.97 * current_times_seen ^ 0.75)
    }
    
    cat("COMPLETELY ISOLATED PLAYER-SPECIFIC TOPPOSPLAYER TRACKING:\n")
    cat("Player:", player_name, "\n")
    cat("TopPosPlayer times seen (including today):", current_times_seen, "\n")
    cat("Is weekend:", is_weekend, "\n")
    if (is_weekend) {
      cat("Calculated TopPosPlayer decay factor: 1/", current_times_seen, "^0.75 =", round(times_seen_decay, 4), "\n")
    } else {
      cat("Calculated TopPosPlayer decay factor: 1/(0.97 * ", current_times_seen, "^0.75) =", round(times_seen_decay, 4), "\n")
    }
    return(list(
      times_seen = current_times_seen,
      decay_factor = times_seen_decay
    ))
  }
  
  trackTopRelieverAppearance <- function(player_name, game_date, day_type, current_tracking = NULL) {
    cat("Player:", player_name, "on date:", game_date, "day type:", day_type, "\n")
    
    if (!player_name %in% names(player_appearances)) {
      player_appearances[[player_name]] <- list(
        appearances = list(),
        sunday_appearances = list(),
        saturday_appearances = list(),
        top_pos_appearances = list(),
        top_reliever_appearances = list()
      )
      cat("Initialized new tracking for player:", player_name, "\n")
    } else if (!"top_reliever_appearances" %in% names(player_appearances[[player_name]])) {
      player_appearances[[player_name]]$top_reliever_appearances <- list()
    }
    
    player_data <- player_appearances[[player_name]]
    prior_appearances <- player_data$top_reliever_appearances
    
    appearances_before_this_game <- 0
    if (length(prior_appearances) > 0) {
      prior_dates <- names(prior_appearances)
      
      appearances_before_this_game <- sum(prior_dates < game_date, na.rm = TRUE)
      
      cat("Found", length(prior_dates), "previous TopReliever appearance dates for", player_name, "\n")
      cat("Previous TopReliever dates:", paste(prior_dates, collapse=", "), "\n")
      cat("TopReliever appearances before", game_date, ":", appearances_before_this_game, "\n")
    } else {
      cat("No previous TopReliever appearances found for", player_name, "\n")
    }
    
    current_times_seen <- appearances_before_this_game + 1
    
    player_data$top_reliever_appearances[[game_date]] <- list(
      date = game_date,
      appearance_number = current_times_seen
    )
    
    player_appearances[[player_name]] <- player_data
    
    
    playing_probability <- 1.0 
    if (day_type == "friday") {
      playing_probability <- 0.65
      times_seen_decay <- 1 / (playing_probability * current_times_seen ^ 0.75)
    } else if (day_type == "saturday") {
      playing_probability <- 0.55
      times_seen_decay <- 1 / (playing_probability * current_times_seen ^ 0.75)
    } else if (day_type == "sunday") {
      playing_probability <- 0.50
      times_seen_decay <- 1 / (playing_probability * current_times_seen ^ 0.75)
    } else {
      playing_probability <- 0.25
      times_seen_decay <- 1 / (playing_probability * current_times_seen ^ 0.75)
    }
    
    cat("COMPLETELY ISOLATED PLAYER-SPECIFIC TOPRELIEVER TRACKING:\n")
    cat("Player:", player_name, "\n")
    cat("TopReliever times seen (including today):", current_times_seen, "\n")
    cat("Day type:", day_type, "\n")
    cat("Playing probability:", playing_probability, "\n")
    cat("Calculated TopReliever decay factor: 1/(", playing_probability, " * ", current_times_seen, "^0.75) =",
        round(times_seen_decay, 4), "\n")
    
    return(list(
      times_seen = current_times_seen,
      decay_factor = times_seen_decay
    ))
  }
  
  output$playersTable <- renderDT({
    if(nrow(playerData) > 0) {
      display_players <- playerData
      
      if("Signing_Bonus" %in% names(display_players)) {
        display_players$Expected_Value <- format(display_players$Signing_Bonus, big.mark = ",", scientific = FALSE)
        display_players <- display_players[, !names(display_players) %in% c("Signing_Bonus")]
      }
      
      datatable(display_players,
                selection = 'multiple',
                options = list(pageLength = nrow(playerData),
                               dom = 't',
                               scrollY = "400px",
                               scrollCollapse = TRUE,
                               columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                rownames = FALSE)
    }
  })
  
  observeEvent(input$playersTable_rows_selected, {
    selected_indices <- input$playersTable_rows_selected
    if(length(selected_indices) > 0) {
      players_to_add <- playerData[selected_indices, ]
      players_to_add <- playerData[selected_indices, ]
      available_cols <- names(playerData)
      players_to_add <- playerData[selected_indices, available_cols, drop=FALSE]
      
      if(!"Player_Name" %in% names(players_to_add)) {
        players_to_add$Player_Name <- paste("Player", 1:nrow(players_to_add))
      }
      
      if(!"School" %in% names(players_to_add)) {
        players_to_add$School <- NA
        showNotification("Warning: School information missing for selected players", type="warning")
      }
      
      if(!"Role" %in% names(players_to_add) && "Role" %in% available_cols) {
        players_to_add$Role <- playerData[selected_indices, "Role"]
      }
      
      if(!"Role" %in% names(players_to_add)) {
        players_to_add$Role <- "TopPosPlayer"
      }
      
      cat("\nAdding players with roles:\n")
      for(i in 1:nrow(players_to_add)) {
        cat("  ", as.character(players_to_add$Player_Name[i]), ": ",
            as.character(players_to_add$Role[i]), "\n")
      }
      
      if(!"Signing_Bonus" %in% names(players_to_add)) {
        players_to_add$Signing_Bonus <- 0  
      }
      
      current <- selectedPlayers()
      
      if(nrow(current) == 0) {
        selectedPlayers(players_to_add)
      } else {
        new_players <- anti_join(players_to_add, current, by = "Player_Name")
        if(nrow(new_players) > 0) {
          common_cols <- intersect(names(current), names(new_players))
          if(length(common_cols) > 0) {
            selectedPlayers(rbind(current[, common_cols], new_players[, common_cols]))
          } else {
            showNotification("Could not add players - column mismatch", type = "error")
          }
        }
      }
    }
  })
  
  output$selectedPlayersTable <- renderDT({
    players <- selectedPlayers()
    if(nrow(players) > 0) {
      player_names <- character(nrow(players))
      schools <- character(nrow(players))
      positions <- character(nrow(players))
      roles <- character(nrow(players))
      expected_values <- character(nrow(players))
      
      for(i in 1:nrow(players)) {
        if("Player_Name" %in% names(players)) {
          name_val <- players[i, "Player_Name"]
          player_names[i] <- if(is.list(name_val)) as.character(name_val[[1]]) else as.character(name_val)
        } else {
          player_names[i] <- paste0("Player ", i)
        }
        
        if("School" %in% names(players)) {
          school_val <- players[i, "School"]
          schools[i] <- if(is.list(school_val)) as.character(school_val[[1]]) else as.character(school_val)
        } else {
          schools[i] <- "N/A"
        }
        
        if("Position" %in% names(players)) {
          pos_val <- players[i, "Position"]
          positions[i] <- if(is.list(pos_val)) as.character(pos_val[[1]]) else as.character(pos_val)
        } else {
          positions[i] <- "N/A"
        }
        
        if("Role" %in% names(players)) {
          role_val <- players[i, "Role"]
          roles[i] <- if(is.list(role_val)) as.character(role_val[[1]]) else as.character(role_val)
        } else {
          roles[i] <- "TopPosPlayer"
        }
        
        if("Signing_Bonus" %in% names(players)) {
          bonus <- players[i, "Signing_Bonus"]
          if(is.list(bonus)) {
            if(length(bonus) > 0) {
              bonus_num <- as.numeric(bonus[[1]])
            } else {
              bonus_num <- 0
            }
          } else {
            bonus_num <- as.numeric(as.character(bonus))
            if(is.na(bonus_num)) bonus_num <- 0
          }
          expected_values[i] <- format(bonus_num, big.mark = ",", scientific = FALSE)
        } else {
          expected_values[i] <- "N/A"
        }
      }
      
      display_df <- data.frame(
        "Player Name" = player_names,
        "School" = schools,
        "Position" = positions,
        "Role" = roles,
        "Expected Value" = expected_values,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      datatable(display_df,
                options = list(
                  pageLength = nrow(players),
                  dom = 't',
                  scrollY = "300px",
                  columnDefs = list(list(className = 'dt-center', targets = '_all'))
                ),
                rownames = FALSE)
    }
  })
  
  observeEvent(input$clearPlayers, {
    empty_df <- data.frame(
      Player_Name = character(0),
      School = character(0),
      Position = character(0),
      Signing_Bonus = numeric(0),
      stringsAsFactors = FALSE
    )
    selectedPlayers(empty_df)
  })
  
  observeEvent(input$optimize, {
    fri_starter_times_seen(list())
    sat_starter_times_seen(list())
    sun_starter_times_seen(list())
    top_pos_player_times_seen(list())
    
    for (player_name in names(player_appearances)) {
      player_appearances[[player_name]] <- NULL
    }
    
    players <- selectedPlayers()
    if(nrow(players) == 0) {
      showNotification("Please select players first", type = "error")
      return()
    }
    
    req_columns <- c("Player_Name", "School", "Signing_Bonus")
    missing_columns <- req_columns[!req_columns %in% names(players)]
    if(length(missing_columns) > 0) {
      if("Player_Name" %in% missing_columns && ncol(players) >= 1) {
        players$Player_Name <- players[[1]]
        missing_columns <- setdiff(missing_columns, "Player_Name")
      }
      
      if("School" %in% missing_columns && ncol(players) >= 2) {
        players$School <- players[[2]]
        missing_columns <- setdiff(missing_columns, "School")
      }
      
      if("Signing_Bonus" %in% missing_columns && ncol(players) >= 5) {
        players$Signing_Bonus <- as.numeric(gsub("[^0-9.]", "", players[[5]]))
        missing_columns <- setdiff(missing_columns, "Signing_Bonus")
      }
      
      if(length(missing_columns) > 0) {
        showNotification(paste0("Missing required columns in player data: ",
                                paste(missing_columns, collapse=", ")), type = "error")
        return()
      }
    }
    
    req_game_columns <- c("Away_Team", "Home_Team")
    missing_game_columns <- req_game_columns[!req_game_columns %in% names(scheduleData)]
    if(length(missing_game_columns) > 0) {
      showNotification(paste0("Missing required columns in schedule data: ",
                              paste(missing_game_columns, collapse=", ")), type = "error")
      return()
    }
    
    if("School" %in% names(players)) {
      players <- players[!is.na(players$School), ]
    }
    
    if(!("Player_Name" %in% names(players))) players$Player_Name <- paste("Player", 1:nrow(players))
    if(!("School" %in% names(players))) stop("School column must be present for optimization")
    if(!("Signing_Bonus" %in% names(players))) players$Signing_Bonus <- 0 
    
    relevantGames <- scheduleData
    
    if(nrow(relevantGames) == 0) {
      showNotification("No games found in the schedule data", type = "error")
      return()
    }
    
    relevantGames$Bonus <- numeric(nrow(relevantGames))
    
    for(i in 1:nrow(relevantGames)) {
      game_schools <- c(relevantGames$Away_Team[i], relevantGames$Home_Team[i])
      game_schools <- game_schools[!is.na(game_schools)]
      
      if(all(c("School", "Signing_Bonus") %in% names(players))) {
        game_players <- players[players$School %in% game_schools, ]
        
        if(!("Role" %in% names(game_players)) && nrow(game_players) > 0) {
          game_players$Role <- "TopPosPlayer"
        }
        
        cat("Roles for players in game", i, ":\n")
        for(p in 1:nrow(game_players)) {
          player_name <- as.character(game_players$Player_Name[p])
          player_role <- if("Role" %in% names(game_players)) {
            as.character(game_players$Role[p])
          } else {
            "Unknown"
          }
          cat("  ", player_name, ": ", player_role, "\n")
        }
      } else {
        game_players <- data.frame(Player_Name=character(), School=character(), Signing_Bonus=numeric())
      }
      
      relevantGames$Bonus[i] <- 0.5 
      if(nrow(game_players) > 0) {
        is_friday <- FALSE
        if("Day_of_Week" %in% names(relevantGames)) {
          day <- tolower(as.character(relevantGames$Day_of_Week[i]))
          if(length(day) == 1) {
            is_friday <- (day == "friday")
          } else if(length(day) > 1) {
            is_friday <- any(day == "friday", na.rm = TRUE)
            cat("Warning: Multiple day values found for game ", i, ", using first value\n")
          }
        }
        
        total_bonus <- 0
        
        for(j in 1:nrow(game_players)) {
          
          player_name <- if("Player_Name" %in% names(game_players)) {
            as.character(game_players$Player_Name[j])
          } else {
            paste("Player", j)
          }
          
          player_bonus_raw <- game_players$Signing_Bonus[j]
          if(is.list(player_bonus_raw)) {
            player_bonus <- tryCatch({
              as.numeric(player_bonus_raw[[1]])
            }, error = function(e) {
              cat("Error processing bonus for player", j, ":", e$message, "\n")
              0
            })
          } else {
            player_bonus <- as.numeric(player_bonus_raw)
          }
          if(is.na(player_bonus)) player_bonus <- 0
          
          is_friday_starter <- FALSE
          is_saturday_starter <- FALSE
          is_sunday_starter <- FALSE
          is_friday_starter <- FALSE
          is_saturday_starter <- FALSE
          is_sunday_starter <- FALSE
          if("Role" %in% names(game_players)) {
            player_role <- as.character(game_players$Role[j])
            is_friday_starter <- !is.na(player_role) && grepl("FriStarter", player_role, fixed=TRUE)
            is_saturday_starter <- !is.na(player_role) && grepl("SatStarter", player_role, fixed=TRUE)
            is_sunday_starter <- !is.na(player_role) && grepl("SunStarter", player_role, fixed=TRUE)
            
            cat(paste0("Player ", player_name, " has role: ", player_role, ", Is Friday starter: ", is_friday_starter, "\n"))
          }
          is_saturday <- FALSE
          is_sunday <- FALSE
          is_weekend <- FALSE  
          if("Day_of_Week" %in% names(relevantGames)) {
            day <- tolower(as.character(relevantGames$Day_of_Week[i]))
            if(length(day) == 1) {
              is_saturday <- (day == "saturday")
              is_sunday <- (day == "sunday")
              is_weekend <- (day == "friday" || day == "saturday" || day == "sunday")
            } else if(length(day) > 1) {
              is_saturday <- any(day == "saturday", na.rm = TRUE)
              is_sunday <- any(day == "sunday", na.rm = TRUE)
              is_weekend <- any(day == "friday" || day == "saturday" || day == "sunday", na.rm = TRUE)
              cat("Warning: Multiple day values found for game ", i, ", using first value\n")
            }
          }
          
          if(isTRUE(is_friday_starter)) {
            week_of_season <- 0
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              if("Weeks_Since_Season" %in% names(games) && i <= nrow(games)) {
                week_of_season <- as.numeric(games$Weeks_Since_Season[i])
                if(is.na(week_of_season)) {
                  cat("WARNING: Week value converted to NA for FriStarter, defaulting to 0\n")
                  week_of_season <- 0
                }
              }
            }
            
            injury_factor <- 1 - (0.0115 * week_of_season)
            cat("Week ", week_of_season, " FriStarter injury factor: ", round(injury_factor, 4), "\n")
            
            if(isTRUE(is_friday)) {
              game_date <- "0"
              if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Date[i])) {
                  game_date <- as.character(relevantGames$Date[i])
                }
              } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Game_Date[i])) {
                  game_date <- as.character(relevantGames$Game_Date[i])
                }
              }
              
              if(game_date == "0" || game_date == "") {
                game_date <- sprintf("%04d", i)
              }
              
              result <- trackFridayStarterAppearance(player_name, game_date)
              
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              
              final_bonus <- player_bonus * injury_factor * times_seen_decay
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/", current_times_seen, "^0.75): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              total_bonus <- total_bonus + final_bonus
            } else {
              cat(paste0("Total bonus for FriStarter ", player_name, ": ", player_bonus, " * 0 = 0\n"))
            }
          } else if(isTRUE(is_saturday_starter)) {
            week_of_season <- 0
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              if("Weeks_Since_Season" %in% names(games) && i <= nrow(games)) {
                week_of_season <- as.numeric(games$Weeks_Since_Season[i])
                if(is.na(week_of_season)) {
                  cat("WARNING: Week value converted to NA for SatStarter, defaulting to 0\n")
                  week_of_season <- 0
                }
              }
            }
            
            injury_factor <- 1 - (0.0115 * week_of_season)
            cat("Week ", week_of_season, " SatStarter injury factor: ", round(injury_factor, 4), "\n")
            
            if(isTRUE(is_saturday)) {
              game_date <- "0"
              if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Date[i])) {
                  game_date <- as.character(relevantGames$Date[i])
                }
              } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Game_Date[i])) {
                  game_date <- as.character(relevantGames$Game_Date[i])
                }
              }
              
              if(game_date == "0" || game_date == "") {
                game_date <- sprintf("%04d", i)
              }
              
              result <- trackSaturdayStarterAppearance(player_name, game_date)
              
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              
              final_bonus <- player_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/", current_times_seen, "^0.75): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              catn")
              
              cat(paste0("Total bonus for SatStarter ", player_name, " (week ", week_of_season, "): ",
                         player_bonus, " * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4), " = ", round(final_bonus, 4), "\n"))
            } else {
              cat(paste0("Total bonus for SatStarter ", player_name, ": ", player_bonus, " * 0 = 0\n"))
            }
          } else if(isTRUE(is_sunday_starter)) {
            week_of_season <- 0
            week_of_season <- 0
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              if("Weeks_Since_Season" %in% names(games) && i <= nrow(games)) {
                week_of_season <- as.numeric(games$Weeks_Since_Season[i])
                if(is.na(week_of_season)) {
                  cat("WARNING: Week value converted to NA for SunStarter, defaulting to 0\n")
                  week_of_season <- 0
                }
              }
            }
            
            injury_factor <- 1 - (0.0115 * week_of_season)
            cat("Week ", week_of_season, " SunStarter injury factor: ", round(injury_factor, 4), "\n")
            
            if(isTRUE(is_sunday)) {
              game_date <- "0"
              if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                game_date <- as.character(relevantGames$Date[i])
                if(is.na(game_date)) game_date <- "0"
              }
              
              result <- trackSundayStarterAppearance(player_name, game_date)
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              
              final_bonus <- player_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Times seen decay factor: ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat(paste0("Total bonus for SunStarter ", player_name, " (week ", week_of_season, "): ",
                         player_bonus, " * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4), " = ", round(final_bonus, 4), "\n"))
            } else {
              cat(paste0("Total bonus for SunStarter ", player_name, ": ", player_bonus, " * 0 = 0\n"))
            }
          } else if(grepl("TopPosPlayer", player_role, fixed=TRUE)) {
            week_of_season <- 0
            
            week_of_season <- 0
            
            cat("DEBUG: Checking for week column in games data frame...\n")
            
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              cat("Available columns in games: ", paste(names(games), collapse=", "), "\n")
              
              if("Weeks_Since_Season" %in% names(games)) {
                cat("Weeks_Since_Season column FOUND. Raw values: ",
                    paste(head(games$Weeks_Since_Season), collapse=", "), "\n")
                
                if(i <= nrow(games)) {
                  raw_week <- games$Weeks_Since_Season[i]
                  cat("Raw week value for game ", i, ": '", raw_week, "' (class: ", class(raw_week), ")\n")
                  
                  week_of_season <- suppressWarnings(as.numeric(as.character(raw_week)))
                  cat("Converted week value: ", week_of_season, " (is.na: ", is.na(week_of_season), ")\n")
                  
                  if(is.na(week_of_season)) {
                    cat("WARNING: Week value converted to NA, defaulting to 0\n")
                    week_of_season <- 0  
                  }
                } else {
                  cat("ERROR: Index ", i, " out of bounds for games data frame with ", nrow(games), " rows\n")
                }
              } else {
                cat("ERROR: Weeks_Since_Season column NOT FOUND in games data frame\n")
                
                week_cols <- grep("week|season", tolower(names(games)), value = TRUE)
                if(length(week_cols) > 0) {
                  cat("Possible week-related columns found: ", paste(week_cols, collapse=", "), "\n")
                }
              }
            } else {
              cat("ERROR: games data frame is NULL, empty, or doesn't exist\n")
            }
            
            injury_factor <- 1 - (0.0038 * week_of_season)
            
            cat("Week ", week_of_season, ": 1 - (0.0038 * ", week_of_season, ") = ", round(injury_factor, 4), "\n")
            
            cat("Using injury factor ", round(injury_factor, 4), " for week ", week_of_season, "\n")
            
            game_date <- "0"
            if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
              if(!is.na(relevantGames$Date[i])) {
                game_date <- as.character(relevantGames$Date[i])
              }
            } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
              if(!is.na(relevantGames$Game_Date[i])) {
                game_date <- as.character(relevantGames$Game_Date[i])
              }
            }
            
            if(game_date == "0" || game_date == "") {
              game_date <- sprintf("%04d", i)
            }
            
            result <- trackTopPosPlayerAppearance(player_name, game_date, is_weekend)
            result <- trackTopPosPlayerAppearance(player_name, game_date, is_weekend)
            current_times_seen <- result$times_seen
            times_seen_decay <- result$decay_factor
            
            if(isTRUE(is_weekend)) {
              day_adjusted_bonus <- player_bonus * 1 
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/", current_times_seen, "^0.75): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Total bonus for TopPosPlayer ", player_name, " (weekend, week ", week_of_season, "): ",
                         player_bonus, " * 1 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4), " = ", round(final_bonus, 4), "\n"))
            } else {
              day_adjusted_bonus <- player_bonus * 0.97 
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.97 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Total bonus for TopPosPlayer ", player_name, " (weekday, week ", week_of_season, "): ",
                         player_bonus, " * 0.97 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4), " = ", round(final_bonus, 4), "\n"))
            }
          } else if(grepl("TopReliever", player_role, fixed=TRUE)) {
            week_of_season <- 0
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              if("Weeks_Since_Season" %in% names(games) && i <= nrow(games)) {
                week_of_season <- as.numeric(games$Weeks_Since_Season[i])
                if(is.na(week_of_season)) {
                  cat("WARNING: Week value converted to NA for TopReliever, defaulting to 0\n")
                  week_of_season <- 0
                }
              }
            }
            
            injury_factor <- 1 - (0.0115 * week_of_season)
            cat("Week ", week_of_season, " TopReliever injury factor: ", round(injury_factor, 4), "\n")
            
            game_date <- "0"
            if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
              if(!is.na(relevantGames$Date[i])) {
                game_date <- as.character(relevantGames$Date[i])
              }
            } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
              if(!is.na(relevantGames$Game_Date[i])) {
                game_date <- as.character(relevantGames$Game_Date[i])
              }
            }
            
            if(game_date == "0" || game_date == "") {
              game_date <- sprintf("%04d", i)
            }
            
            if(isTRUE(is_friday)) {
              result <- trackTopRelieverAppearance(player_name, game_date, "friday")
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              
              day_adjusted_bonus <- player_bonus * 0.65
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.65 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Total bonus for TopReliever ", player_name, " (Friday, week ", week_of_season, "): ",
                         player_bonus, " * 0.65 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            } else if(isTRUE(is_saturday)) {
              result <- trackTopRelieverAppearance(player_name, game_date, "saturday")
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              day_adjusted_bonus <- player_bonus * 0.55
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.55 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Total bonus for TopReliever ", player_name, " (Saturday, week ", week_of_season, "): ",
                         player_bonus, " * 0.55 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            } else if(isTRUE(is_sunday)) {
              result <- trackTopRelieverAppearance(player_name, game_date, "sunday")
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              
              day_adjusted_bonus <- player_bonus * 0.50
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.50 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
          
              
              cat(paste0("Total bonus for TopReliever ", player_name, " (Sunday, week ", week_of_season, "): ",
                         player_bonus, " * 0.50 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            } else {
              result <- trackTopRelieverAppearance(player_name, game_date, "weekday")
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              
              day_adjusted_bonus <- player_bonus * 0.25
              day_adjusted_bonus <- player_bonus * 0.25
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.25 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Total bonus for TopReliever ", player_name, " (weekday, week ", week_of_season, "): ",
                         player_bonus, " * 0.25 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            }
          } else {
            total_bonus <- total_bonus + player_bonus
          }
        }
        
        unadjusted_bonus <- total_bonus / 10000
        relevantGames$Bonus[i] <- relevantGames$Bonus[i] + unadjusted_bonus
        relevantGames$Bonus[i] <- as.numeric(relevantGames$Bonus[i])
        
        cat(paste0("Final score for game ", i, ": ", round(relevantGames$Bonus[i], 4),
                   " (unadjusted bonus: ", round(unadjusted_bonus, 4), ")\n"))
      }
    }
    
    relevantGames$DateLocation <- paste(relevantGames$Date, relevantGames$Location, sep = "_")
    uniqueDateLocations <- unique(relevantGames$DateLocation)
    
    uniqueDateLocations <- unique(relevantGames$DateLocation)
    
    n <- length(uniqueDateLocations)
    locationValues <- data.frame(
      DateLocation = uniqueDateLocations,
      TotalBonus = rep(0, n),
      Date = rep(as.Date("1970-01-01"), n)
    )
    
    for(i in 1:nrow(locationValues)) {
      dl_parts <- strsplit(locationValues$DateLocation[i], "_")[[1]]
      if(length(dl_parts) >= 1) {
        locationValues$Date[i] <- as.Date(dl_parts[1])
      }
      
      games_at_dl <- relevantGames[relevantGames$DateLocation == locationValues$DateLocation[i], ]
      if(nrow(games_at_dl) > 0 && "Bonus" %in% names(games_at_dl)) {
        locationValues$TotalBonus[i] <- sum(as.numeric(games_at_dl$Bonus), na.rm = TRUE)
        cat(paste0("Location ", locationValues$DateLocation[i], " total score: ", round(locationValues$TotalBonus[i], 4), "\n"))
      }
    }
    
    for(i in 1:nrow(locationValues)) {
      dl <- locationValues$DateLocation[i]
      games <- relevantGames[relevantGames$DateLocation == dl,]
      
      schoolsPlaying <- unique(c(as.character(games$Away_Team), as.character(games$Home_Team)))
      schoolsPlaying <- schoolsPlaying[!is.na(schoolsPlaying)]
      
      if(all(c("School", "Signing_Bonus") %in% names(players))) {
        playersAtGames <- players[players$School %in% schoolsPlaying, ]
        
        if(!("Role" %in% names(playersAtGames)) && nrow(playersAtGames) > 0) {
          playersAtGames$Role <- "TopPosPlayer"
        }
        
        cat("Roles for players at location", dl, ":\n")
        for(p in 1:nrow(playersAtGames)) {
          player_name <- as.character(playersAtGames$Player_Name[p])
          player_role <- if("Role" %in% names(playersAtGames)) {
            as.character(playersAtGames$Role[p])
          } else {
            "Unknown"
          }
          cat("  ", player_name, ": ", player_role, "\n")
        }
      } else {
        playersAtGames <- data.frame(Player_Name=character(), School=character(), Signing_Bonus=numeric())
      }
      
      locationValues$TotalBonus[i] <- 0.5  
      
      if(nrow(playersAtGames) > 0) {
        is_friday <- FALSE
        is_saturday <- FALSE
        is_sunday <- FALSE
        is_weekend <- FALSE  
        
        if(nrow(games) > 0 && "Day_of_Week" %in% names(games)) {
          for(g in 1:nrow(games)) {
            day_value <- games$Day_of_Week[g]
            if(length(day_value) == 1) {
              day <- tolower(as.character(day_value))
              if(!is.na(day)) {
                if(day == "friday") {
                  is_friday <- TRUE
                  is_weekend <- TRUE
                }
                if(day == "saturday") {
                  is_saturday <- TRUE
                  is_weekend <- TRUE
                }
                if(day == "sunday") {
                  is_sunday <- TRUE
                  is_weekend <- TRUE
                }
              }
            } else if(length(day_value) > 1) {
              days <- sapply(day_value, function(d) {
                tolower(as.character(d))
              })
              if(any(days == "friday", na.rm = TRUE)) {
                is_friday <- TRUE
                is_weekend <- TRUE
              }
              if(any(days == "saturday", na.rm = TRUE)) {
                is_saturday <- TRUE
                is_weekend <- TRUE
              }
              if(any(days == "sunday", na.rm = TRUE)) {
                is_sunday <- TRUE
                is_weekend <- TRUE
              }
            }
          }
        }
        
        cat(paste0("\nLocation ", dl, " is Friday: ", is_friday, ", is Saturday: ", is_saturday, ", is Sunday: ", is_sunday, ", is Weekend: ", is_weekend, "\n"))
        
        total_bonus <- 0
        
        for(j in 1:nrow(playersAtGames)) {
          player_name <- if("Player_Name" %in% names(playersAtGames)) {
            as.character(playersAtGames$Player_Name[j])
          } else {
            paste("Player", j)
          }
          
          player_bonus_raw <- playersAtGames$Signing_Bonus[j]
          if(is.list(player_bonus_raw)) {
            player_bonus <- tryCatch({
              as.numeric(player_bonus_raw[[1]])
            }, error = function(e) {
              cat("Error processing bonus for location player", j, ":", e$message, "\n")
              0
            })
          } else {
            player_bonus <- as.numeric(player_bonus_raw)
          }
          if(is.na(player_bonus)) player_bonus <- 0
          
          is_friday_starter <- FALSE
          is_saturday_starter <- FALSE
          is_sunday_starter <- FALSE
          is_friday_starter <- FALSE
          is_saturday_starter <- FALSE
          is_sunday_starter <- FALSE
          if("Role" %in% names(playersAtGames)) {
            player_role <- as.character(playersAtGames$Role[j])
            is_friday_starter <- !is.na(player_role) && grepl("FriStarter", player_role, fixed=TRUE)
            is_saturday_starter <- !is.na(player_role) && grepl("SatStarter", player_role, fixed=TRUE)
            is_sunday_starter <- !is.na(player_role) && grepl("SunStarter", player_role, fixed=TRUE)
            
            cat(paste0("Location check - Player ", player_name, " has role: ", player_role, ", Is Friday starter: ", is_friday_starter, "\n"))
          }
          
          if(isTRUE(is_friday_starter)) {
            max_week_of_season <- 0
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              if("Weeks_Since_Season" %in% names(games)) {
                weeks <- as.numeric(games$Weeks_Since_Season)
                max_week_of_season <- max(weeks, na.rm = TRUE)
                if(is.na(max_week_of_season) || is.infinite(max_week_of_season)) {
                  max_week_of_season <- 0
                }
              }
            }
            
            injury_factor <- 1 - (0.0115 * max_week_of_season)
            cat("Location max week ", max_week_of_season, " FriStarter injury factor: ", round(injury_factor, 4), "\n")
            
            if(isTRUE(is_friday)) {
              current_tracking <- fri_starter_times_seen()
              
              game_date <- "0"
              if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Date[i])) {
                  game_date <- as.character(relevantGames$Date[i])
                }
              } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Game_Date[i])) {
                  game_date <- as.character(relevantGames$Game_Date[i])
                }
              }
              
              if(game_date == "0" || game_date == "") {
                game_date <- sprintf("%04d", i)
              }
              
              dated_player_key <- paste0(player_name, "_FriStarter_", game_date)
              
              appearances_before_this_game <- 0
              
              player_keys <- character(0)
              if(length(current_tracking) > 0) {
                all_keys <- names(current_tracking)
                if(length(all_keys) > 0) {
                  player_keys <- character(0)
                  for (key in all_keys) {
                    parts <- strsplit(key, "_FriStarter_")[[1]]
                    if (length(parts) > 0 && parts[1] == player_name) {
                      player_keys <- c(player_keys, key)
                    }
                  }
                  cat("Player:", player_name, "- Found", length(player_keys), "previous appearances\n")
                }
              }
              
              if(length(player_keys) > 0) {
                player_dates <- sapply(player_keys, function(key) {
                  parts <- strsplit(key, "_FriStarter_")[[1]]
                  if(length(parts) >= 2) return(parts[2]) else return("")
                })
                
                appearances_before_this_game <- sum(player_dates < game_date, na.rm = TRUE)
              }
              
              current_times_seen <- appearances_before_this_game + 1
              
              current_tracking[[dated_player_key]] <- current_times_seen
              fri_starter_times_seen(current_tracking)
              
              times_seen_decay <- 1 / (current_times_seen ^ 0.75)
              
              final_bonus <- player_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              cat(paste0("Location bonus for FriStarter ", player_name, " (week ", max_week_of_season, "): ",
                         player_bonus, " * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4), " = ", round(final_bonus, 4), "\n"))
            } else {
              cat(paste0("Location bonus for FriStarter ", player_name, ": ", player_bonus, " * 0 = 0\n"))
            }
          } else if(isTRUE(is_saturday_starter)) {
            max_week_of_season <- 0
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              if("Weeks_Since_Season" %in% names(games)) {
                weeks <- as.numeric(games$Weeks_Since_Season)
                max_week_of_season <- max(weeks, na.rm = TRUE)
                if(is.na(max_week_of_season) || is.infinite(max_week_of_season)) {
                  max_week_of_season <- 0
                }
              }
            }
            
            injury_factor <- 1 - (0.0115 * max_week_of_season)
            cat("Location max week ", max_week_of_season, " SatStarter injury factor: ", round(injury_factor, 4), "\n")
            
            if(isTRUE(is_saturday)) {
              current_tracking <- sat_starter_times_seen()
              
              game_date <- "0"
              if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Date[i])) {
                  game_date <- as.character(relevantGames$Date[i])
                }
              } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Game_Date[i])) {
                  game_date <- as.character(relevantGames$Game_Date[i])
                }
              }
              
              if(game_date == "0" || game_date == "") {
                game_date <- sprintf("%04d", i)
              }
              
              dated_player_key <- paste0(player_name, "_SatStarter_", game_date)
              
              appearances_before_this_game <- 0
              
              player_keys <- character(0)
              if(length(current_tracking) > 0) {
                all_keys <- names(current_tracking)
                if(length(all_keys) > 0) {
                  player_keys <- character(0)
                  for (key in all_keys) {
                    parts <- strsplit(key, "_SatStarter_")[[1]]
                    if (length(parts) > 0 && parts[1] == player_name) {
                      player_keys <- c(player_keys, key)
                    }
                  }
                  cat("Player:", player_name, "- Found", length(player_keys), "previous Saturday appearances\n")
                }
              }
              
              if(length(player_keys) > 0) {
                player_dates <- sapply(player_keys, function(key) {
                  parts <- strsplit(key, "_SatStarter_")[[1]]
                  if(length(parts) >= 2) return(parts[2]) else return("")
                })
                
                appearances_before_this_game <- sum(player_dates < game_date, na.rm = TRUE)
              }
              
              current_times_seen <- appearances_before_this_game + 1
              
              current_tracking[[dated_player_key]] <- current_times_seen
              sat_starter_times_seen(current_tracking)
              
              times_seen_decay <- 1 / (current_times_seen ^ 0.75)
              
              final_bonus <- player_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/", current_times_seen, "^0.75): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Location bonus for SatStarter ", player_name, " (week ", max_week_of_season, "): ",
                         player_bonus, " * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4), " = ", round(final_bonus, 4), "\n"))
            } else {
              cat(paste0("Location bonus for SatStarter ", player_name, ": ", player_bonus, " * 0 = 0\n"))
            }
          } else if(isTRUE(is_sunday_starter)) {
            max_week_of_season <- 0
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              if("Weeks_Since_Season" %in% names(games)) {
                weeks <- as.numeric(games$Weeks_Since_Season)
                max_week_of_season <- max(weeks, na.rm = TRUE)
                if(is.na(max_week_of_season) || is.infinite(max_week_of_season)) {
                  max_week_of_season <- 0
                }
              }
            }
            
            injury_factor <- 1 - (0.0115 * max_week_of_season)
            cat("Location max week ", max_week_of_season, " SunStarter injury factor: ", round(injury_factor, 4), "\n")
            
            if(isTRUE(is_sunday)) {
              current_tracking <- sun_starter_times_seen()
              
              game_date <- "0"
              if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Date[i])) {
                  game_date <- as.character(relevantGames$Date[i])
                }
              } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
                if(!is.na(relevantGames$Game_Date[i])) {
                  game_date <- as.character(relevantGames$Game_Date[i])
                }
              }
              
              if(game_date == "0" || game_date == "") {
                game_date <- sprintf("%04d", i)
              }
              
              dated_player_key <- paste0(player_name, "_SunStarter_", game_date)
              
              appearances_before_this_game <- 0
              
              player_keys <- character(0)
              if(length(current_tracking) > 0) {
                all_keys <- names(current_tracking)
                if(length(all_keys) > 0) {
                  player_keys <- character(0)
                  for (key in all_keys) {
                    parts <- strsplit(key, "_SunStarter_")[[1]]
                    if (length(parts) > 0 && parts[1] == player_name) {
                      player_keys <- c(player_keys, key)
                    }
                  }
                  cat("Player:", player_name, "- Found", length(player_keys), "previous Sunday appearances\n")
                }
              }
              
              if(length(player_keys) > 0) {
                player_dates <- sapply(player_keys, function(key) {
                  parts <- strsplit(key, "_SunStarter_")[[1]]
                  if(length(parts) >= 2) return(parts[2]) else return("")
                })
                
                appearances_before_this_game <- sum(player_dates < game_date, na.rm = TRUE)
              }
              
              current_times_seen <- appearances_before_this_game + 1
              
              current_tracking[[dated_player_key]] <- current_times_seen
              sun_starter_times_seen(current_tracking)
              
              times_seen_decay <- 1 / (current_times_seen ^ 0.75)
              
              final_bonus <- player_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              cat(paste0("Location bonus for SunStarter ", player_name, " (week ", max_week_of_season, "): ",
                         player_bonus, " * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            } else {
              cat(paste0("Location bonus for SunStarter ", player_name, ": ", player_bonus, " * 0 = 0\n"))
            }
          } else if(grepl("TopPosPlayer", player_role, fixed=TRUE)) {
            max_week_of_season <- 0
            
            max_week_of_season <- 0
            
            cat("LOCATION DEBUG: Checking for week column in location games data frame...\n")
            
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              cat("Available columns in location games: ", paste(names(games), collapse=", "), "\n")
              
              if("Weeks_Since_Season" %in% names(games)) {
                cat("Weeks_Since_Season column FOUND in location analysis. Raw values: ",
                    paste(head(games$Weeks_Since_Season), collapse=", "), "\n")
                
                raw_weeks <- games$Weeks_Since_Season
                cat("Raw week values from location (first 10): ", paste(head(raw_weeks, 10), collapse=", "), "\n")
                cat("Class of raw weeks: ", class(raw_weeks), "\n")
                
                weeks <- suppressWarnings(as.numeric(as.character(raw_weeks)))
                cat("Converted week values (first 10): ", paste(head(weeks, 10), collapse=", "), "\n")
                cat("Number of NA values after conversion: ", sum(is.na(weeks)), " of ", length(weeks), "\n")
                
                weeks <- weeks[!is.na(weeks)]  
                
                if(length(weeks) > 0) {
                  max_week_of_season <- max(weeks)  
                  cat("Maximum week found for location: ", max_week_of_season, "\n")
                } else {
                  cat("WARNING: No valid week values found for location analysis\n")
                }
              } else {
                cat("ERROR: Weeks_Since_Season column NOT FOUND in location games data frame\n")
                
                week_cols <- grep("week|season", tolower(names(games)), value = TRUE)
                if(length(week_cols) > 0) {
                  cat("Possible week-related columns found in location: ", paste(week_cols, collapse=", "), "\n")
                }
              }
            } else {
              cat("ERROR: Games data frame is NULL, empty, or doesn't exist for location analysis\n")
            }
            
            injury_factor <- 1 - (0.0038 * max_week_of_season)
            
            cat("Location max week ", max_week_of_season, ": 1 - (0.0038 * ", max_week_of_season, ") = ", round(injury_factor, 4), "\n")
            
            cat("Using location injury factor ", round(injury_factor, 4), " for max week ", max_week_of_season, "\n")
            
            game_date <- "0"
            if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
              if(!is.na(relevantGames$Date[i])) {
                game_date <- as.character(relevantGames$Date[i])
              }
            } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
              if(!is.na(relevantGames$Game_Date[i])) {
                game_date <- as.character(relevantGames$Game_Date[i])
              }
            }
            
            if(game_date == "0" || game_date == "") {
              game_date <- sprintf("%04d", i)
            }
            
            result <- trackTopPosPlayerAppearance(player_name, game_date, is_weekend)
            current_times_seen <- result$times_seen
            times_seen_decay <- result$decay_factor
            
            if(isTRUE(is_weekend)) {
              day_adjusted_bonus <- player_bonus * 1 
              
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/", current_times_seen, "^0.75): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Location bonus for TopPosPlayer ", player_name, " (weekend, week ", max_week_of_season, "): ",
                         player_bonus, " * 1 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            } else {
              day_adjusted_bonus <- player_bonus * 0.97  
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.97 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Location bonus for TopPosPlayer ", player_name, " (weekday, week ", max_week_of_season, "): ",
                         player_bonus, " * 0.97 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            }
          } else if(grepl("TopReliever", player_role, fixed=TRUE)) {
            max_week_of_season <- 0
            if(exists("games") && !is.null(games) && nrow(games) > 0) {
              if("Weeks_Since_Season" %in% names(games)) {
                weeks <- as.numeric(games$Weeks_Since_Season)
                max_week_of_season <- max(weeks, na.rm = TRUE)
                if(is.na(max_week_of_season) || is.infinite(max_week_of_season)) {
                  max_week_of_season <- 0
                }
              }
            }
            
            injury_factor <- 1 - (0.0115 * max_week_of_season)
            cat("Location max week ", max_week_of_season, " TopReliever injury factor: ", round(injury_factor, 4), "\n")
            
            game_date <- "0"
            if("Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
              if(!is.na(relevantGames$Date[i])) {
                game_date <- as.character(relevantGames$Date[i])
              }
            } else if("Game_Date" %in% names(relevantGames) && i <= nrow(relevantGames)) {
              if(!is.na(relevantGames$Game_Date[i])) {
                game_date <- as.character(relevantGames$Game_Date[i])
              }
            }
            
            if(game_date == "0" || game_date == "") {
              game_date <- sprintf("%04d", i)
            }
            
            if(isTRUE(is_friday)) {
              result <- trackTopRelieverAppearance(player_name, game_date, "friday")
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              day_adjusted_bonus <- player_bonus * 0.65
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.65 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Location bonus for TopReliever ", player_name, " (Friday, week ", max_week_of_season, "): ",
                         player_bonus, " * 0.65 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            } else if(isTRUE(is_saturday)) {
              result <- trackTopRelieverAppearance(player_name, game_date, "saturday")
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              day_adjusted_bonus <- player_bonus * 0.55
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.55 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Location bonus for TopReliever ", player_name, " (Saturday, week ", max_week_of_season, "): ",
                         player_bonus, " * 0.55 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            } else if(isTRUE(is_sunday)) {
              result <- trackTopRelieverAppearance(player_name, game_date, "sunday")
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              day_adjusted_bonus <- player_bonus * 0.50
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.50 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Location bonus for TopReliever ", player_name, " (Sunday, week ", max_week_of_season, "): ",
                         player_bonus, " * 0.50 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            } else {
              result <- trackTopRelieverAppearance(player_name, game_date, "weekday")
              current_times_seen <- result$times_seen
              times_seen_decay <- result$decay_factor
              day_adjusted_bonus <- player_bonus * 0.25
              final_bonus <- day_adjusted_bonus * injury_factor * times_seen_decay
              total_bonus <- total_bonus + final_bonus
              
              cat("Player: ", player_name, "\n")
              cat("Game date: ", game_date, "\n")
              cat("Times seen: ", current_times_seen, "\n")
              cat("Decay factor (1/(0.25 * ", current_times_seen, "^0.75)): ", round(times_seen_decay, 4), "\n")
              cat("Injury factor: ", round(injury_factor, 4), "\n")
              cat("Original bonus: ", format(player_bonus, big.mark=","), "\n")
              cat("Final bonus: ", format(round(final_bonus, 2), big.mark=","), "\n")
              
              cat(paste0("Location bonus for TopReliever ", player_name, " (weekday, week ", max_week_of_season, "): ",
                         player_bonus, " * 0.25 * ", round(injury_factor, 4), " * ", round(times_seen_decay, 4),
                         " = ", round(final_bonus, 4), "\n"))
            }
          } else {
            total_bonus <- total_bonus + player_bonus
          }
        }
        
        locationValues$TotalBonus[i] <- locationValues$TotalBonus[i] + (total_bonus / 10000)
        cat(paste0("Final location ", dl, " score: ", locationValues$TotalBonus[i], "\n"))
      }
    }
    
    
    source("stage_two.R")
    cat("Using two-stage optimization approach...\n")
    
    locationValues$Date <- sapply(strsplit(locationValues$DateLocation, "_"), `[`, 1)
    locationValues$Location <- sapply(strsplit(locationValues$DateLocation, "_"), function(x) paste(x[-1], collapse="_"))
    
    cat("Checking for doubleheader games...\n")
    locationValues$IsDoubleHeader <- FALSE
    
    for(i in 1:nrow(locationValues)) {
      dl <- locationValues$DateLocation[i]
      game_idx <- which(relevantGames$DateLocation == dl)
      
      if(length(game_idx) > 0 && "Double_Header" %in% names(relevantGames)) {
        is_dh <- relevantGames$Double_Header[game_idx[1]]
        locationValues$IsDoubleHeader[i] <- is_dh
        
        if(is_dh) {
          cat("Found doubleheader game at", dl, "\n")
        }
      }
    }
    
    locationValues$DateLocationKey <- paste(locationValues$Date, locationValues$Location)
    
    dateLocCounts <- table(locationValues$DateLocationKey)
    
    for(i in 1:nrow(locationValues)) {
      key <- locationValues$DateLocationKey[i]
      if(dateLocCounts[key] > 1) {
        idx <- which(locationValues$DateLocationKey == key)
        has_doubleheader <- any(locationValues$IsDoubleHeader[idx])
        
        if(has_doubleheader) {
          cat("Confirmed doubleheader at", key, "with", dateLocCounts[key], "games\n")
        } else {
          cat("Found multiple games at", key, "but none marked as doubleheader\n")
        }
      }
    }
    
    uniqueDates <- unique(locationValues$Date)
    
    obj <- as.numeric(locationValues$TotalBonus)
    cat("Objective vector for optimization:\n")
    cat(paste(round(obj, 4), collapse=", "), "\n")
    
    constr_list <- list()
    
    for(date in uniqueDates) {
      date_rows <- which(locationValues$Date == date)
      
      date_locs <- unique(locationValues$Location[date_rows])
      
      if(length(date_locs) > 1) {
        for(loc in date_locs) {
          date_loc_rows <- which(locationValues$Date == date & locationValues$Location == loc)
          
          has_doubleheader <- any(locationValues$IsDoubleHeader[date_loc_rows])
          
          if(!has_doubleheader && length(date_loc_rows) > 1) {
            constr_row <- rep(0, nrow(locationValues))
            constr_row[date_loc_rows] <- 1
            constr_list[[length(constr_list) + 1]] <- constr_row
            cat("Adding constraint for multiple games at", paste(date, loc), "(not doubleheaders)\n")
          }
        }
        
        constr_row <- rep(0, nrow(locationValues))
        constr_row[date_rows] <- 1
        constr_list[[length(constr_list) + 1]] <- constr_row
        cat("Adding constraint for date", date, "to visit at most one location\n")
      }
    }
    
    if(length(constr_list) > 0) {
      constr <- do.call(rbind, constr_list)
      cat("Created", nrow(constr), "constraint rows\n")
    } else {
      constr <- matrix(0, nrow = 1, ncol = nrow(locationValues))
      cat("No constraints needed\n")
    }
    
    num_constraints <- nrow(constr)
    dir <- rep("<=", num_constraints)
    rhs <- rep(1, num_constraints)
    
    cat("Created", num_constraints, "constraints with direction <=\n")
    
    lp_result <- lp("max", obj, constr, dir, rhs, all.bin = TRUE)
    
    if(lp_result$status == 0) {
      selectedLocations <- which(lp_result$solution == 1)
      
      optimalDateLocations <- locationValues$DateLocation[selectedLocations]
      
      initialGames <- relevantGames %>%
        filter(DateLocation %in% optimalDateLocations) %>%
        arrange(Date, Location)
      
      cat("Processing final schedule to calculate accurate player appearances...\n")
      
      processedGames <- processFinalSchedule(initialGames, players)
      
      optimalSchools <- unique(c(processedGames$Away_Team, processedGames$Home_Team))
      
      if(all(c("School") %in% names(players))) {
        coveredPlayers <- players[players$School %in% optimalSchools, ]
      } else {
        coveredPlayers <- data.frame(
          Player_Name = character(),
          School = character(),
          Signing_Bonus = numeric()
        )
      }
      
      accurateTotalBonus <- sum(processedGames$Bonus, na.rm = TRUE)
      cat("Total bonus after accurate player appearance calculation:", round(accurateTotalBonus, 4), "\n")
      
      selectedDates <- unique(as.character(processedGames$Date))
      selectedLocations <- unique(as.character(processedGames$Location))
      
      optimalSchedule(list(
        games = processedGames,  
        totalBonus = accurateTotalBonus,  
        coveredPlayers = coveredPlayers,
        nCoveredPlayers = nrow(coveredPlayers),
        datesSelected = length(unique(selectedDates)),
        locationsSelected = length(unique(selectedLocations))
      ))
      
      showNotification("Optimal schedule generated!", type = "message")
    } else {
      showNotification("Failed to generate optimal schedule", type = "error")
    }
  })
  
  output$optimizationSummary <- renderText({
    schedule <- optimalSchedule()
    if(is.null(schedule)) {
      return("Click 'Generate Optimal Schedule' to find the best schedule.")
    }
    
    totalGameScore <- if(!is.null(schedule$games) && is.numeric(schedule$games$Bonus)) {
      bonus_values <- as.numeric(schedule$games$Bonus)
      round(sum(bonus_values, na.rm = TRUE), 1)
    } else {
      0
    }
    
    paste0(
      "Optimization complete! The optimal schedule covers a total of ", schedule$nCoveredPlayers,
      " players with a cumulative Season Game Score of ", totalGameScore, ". "
    )
  })
  
  output$optimalSchedule <- renderDT({
    schedule <- optimalSchedule()
    if(is.null(schedule)) {
      return(NULL)
    }
    
    result_df <- schedule$games
    
    if("Bonus" %in% names(result_df)) {
      cat("Debug - game scores in final schedule:\n")
      for(i in 1:nrow(result_df)) {
        if(!is.numeric(result_df$Bonus[i])) {
          result_df$Bonus[i] <- as.numeric(result_df$Bonus[i])
        }
        cat(paste0("Game ", i, ", Date: ", result_df$Date[i], ", Location: ", result_df$Location[i], ", Score: ", round(result_df$Bonus[i], 4)))
        
        if("Double_Header" %in% names(result_df) && result_df$Double_Header[i]) {
          cat(", Doubleheader: Yes")
        }
        cat("\n")
      }
    }
    
    if("Date" %in% names(result_df)) {
      if(inherits(result_df$Date, "Date")) {
        result_df$Date <- format(result_df$Date, "%a %b %d, %Y")
      }
    }
    
    if("Double_Header" %in% names(result_df)) {
      dh_count <- sum(result_df$Double_Header, na.rm = TRUE)
      cat("Found", dh_count, "doubleheader games in the optimal schedule\n")
      
      result_df$DoubleHeader <- ifelse(result_df$Double_Header == TRUE, "Yes", "No")
    } else {
      cat("No Double_Header column in the results\n")
      result_df$DoubleHeader <- "No"
    }
    
    if("Bonus" %in% names(result_df)) {
      result_df$Bonus <- sapply(result_df$Bonus, function(x) {
        val <- suppressWarnings(as.numeric(x))
        if(is.na(val)) return(0) else return(val)
      })
      
      result_df$GameScore <- result_df$Bonus
      
      result_df$GameScore <- round(result_df$GameScore, 1)
      
      cat("Final GameScore values for table display (rounded to 1 decimal):\n")
      cat(paste(result_df$GameScore, collapse=", "), "\n")
      
      cat("Pre-rounded game scores:\n")
      cat(paste(round(result_df$Bonus, 4), collapse=", "), "\n")
    } else {
      cat("WARNING: No Bonus column found in result_df\n")
      result_df$GameScore <- 0
    }
    
    final_df <- data.frame(
      Date = result_df$Date,
      Game = result_df$Game_Title,  
      Location = result_df$Location,
      GameScore = result_df$GameScore,  
      Week = result_df$Weeks_Since_Season,  
      DoubleHeader = result_df$DoubleHeader,
      stringsAsFactors = FALSE
    )
    
    cat("Final table columns: ", paste(names(final_df), collapse=", "), "\n")
    cat("Game scores in final table: ", paste(round(final_df$GameScore, 3), collapse=", "), "\n")
    
    datatable(final_df,
              options = list(
                pageLength = 525,
                dom = 't',  
                scrollY = "400px",  
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              ),
              rownames = FALSE,
              caption = "Optimal Scout Schedule")
  })
  
  output$playerCoverage <- renderDT({
    schedule <- optimalSchedule()
    if(is.null(schedule)) {
      return(NULL)
    }
    
    if(!is.null(schedule$games) && nrow(schedule$games) > 0 &&
       !is.null(schedule$coveredPlayers) && nrow(schedule$coveredPlayers) > 0) {
      
      playerAppearances <- data.frame(
        Player_Name = character(0),
        School = character(0),
        Position = character(0),
        TimesEvaluated = numeric(0),
        stringsAsFactors = FALSE
      )
      
      for(i in 1:nrow(schedule$coveredPlayers)) {
        player <- schedule$coveredPlayers[i,]
        
        new_row <- data.frame(
          Player_Name = as.character(player$Player_Name),
          School = as.character(player$School),
          Position = ifelse("Position" %in% names(player), as.character(player$Position), "Unknown"),
          TimesEvaluated = 0,
          stringsAsFactors = FALSE
        )
        
        playerAppearances <- rbind(playerAppearances, new_row)
      }
      
      for(i in 1:nrow(schedule$games)) {
        game <- schedule$games[i,]
        game_schools <- c(as.character(game$Away_Team), as.character(game$Home_Team))
        game_schools <- game_schools[!is.na(game_schools)]
        
        is_friday <- FALSE
        is_saturday <- FALSE
        is_sunday <- FALSE
        is_weekend <- FALSE
        
        if("Day_of_Week" %in% names(game)) {
          day <- tolower(as.character(game$Day_of_Week))
          is_friday <- (day == "friday")
          is_saturday <- (day == "saturday")
          is_sunday <- (day == "sunday")
          is_weekend <- (is_friday || is_saturday || is_sunday)
        }
        
        for(j in 1:nrow(playerAppearances)) {
          if(playerAppearances$School[j] %in% game_schools) {
            player_name <- playerAppearances$Player_Name[j]
            player_role <- if("Role" %in% names(schedule$coveredPlayers)) {
              idx <- which(schedule$coveredPlayers$Player_Name == player_name)
              if(length(idx) > 0) as.character(schedule$coveredPlayers$Role[idx[1]]) else ""
            } else ""
            
            is_friday_starter <- !is.na(player_role) && grepl("FriStarter", player_role, fixed=TRUE)
            is_saturday_starter <- !is.na(player_role) && grepl("SatStarter", player_role, fixed=TRUE)
            is_sunday_starter <- !is.na(player_role) && grepl("SunStarter", player_role, fixed=TRUE)
            is_top_pos_player <- !is.na(player_role) && grepl("TopPosPlayer", player_role, fixed=TRUE)
            is_top_reliever <- !is.na(player_role) && grepl("TopReliever", player_role, fixed=TRUE)
            
            evaluated <- FALSE
            evaluation_weight <- 0
            
            is_doubleheader <- FALSE
            is_first_doubleheader_game <- TRUE
            
            if("Double_Header" %in% names(game)) {
              is_doubleheader <- isTRUE(game$Double_Header)
            }
            
            if(is_doubleheader && "Is_First_DH_Game" %in% names(game)) {
              is_first_doubleheader_game <- isTRUE(game$Is_First_DH_Game)
            }
            
            is_pitcher <- is_friday_starter || is_saturday_starter || is_sunday_starter || is_top_reliever
            
            if(is_friday_starter && is_friday) {
              if(!is_doubleheader || is_first_doubleheader_game) {
                evaluated <- TRUE
                evaluation_weight <- 1.0
              }
            } else if(is_saturday_starter && is_saturday) {
              if(!is_doubleheader || is_first_doubleheader_game) {
                evaluated <- TRUE
                evaluation_weight <- 1.0
              }
            } else if(is_sunday_starter && is_sunday) {
              if(!is_doubleheader || is_first_doubleheader_game) {
                evaluated <- TRUE
                evaluation_weight <- 1.0
              }
            } else if(is_top_pos_player) {
              evaluated <- TRUE
              base_weight <- if(is_weekend) 1.0 else 0.97
              
              if(is_doubleheader) {
                evaluation_weight <- base_weight * 2.0  
                cat(paste0("Doubling evaluation weight for position player ", player_name, " in doubleheader game\n"))
              } else {
                evaluation_weight <- base_weight
              }
            } else if(is_top_reliever) {
              if(!is_doubleheader || is_first_doubleheader_game) {
                evaluated <- TRUE
                if(is_friday) {
                  evaluation_weight <- 0.65
                } else if(is_saturday) {
                  evaluation_weight <- 0.55
                } else if(is_sunday) {
                  evaluation_weight <- 0.50
                } else {
                  evaluation_weight <- 0.25  
                }
              }
            } else if(!(is_friday_starter || is_saturday_starter || is_sunday_starter)) {
              
              if(is_doubleheader && !grepl("pitcher|reliever", player_role, ignore.case = TRUE)) {
                evaluation_weight <- base_weight * 2.0  
              } else {
                evaluation_weight <- base_weight
              }
            }
            
            if(evaluated) {
              playerAppearances$TimesEvaluated[j] <- playerAppearances$TimesEvaluated[j] + evaluation_weight
            }
          }
        }
      }
      
      playerAppearances <- playerAppearances[order(-playerAppearances$TimesEvaluated),]
      
      playerAppearances$TimesEvaluated <- round(playerAppearances$TimesEvaluated, 1)
      
      datatable(playerAppearances,
                options = list(
                  pageLength = 125,  
                  dom = 't',  
                  scrollY = "400px",
                  columnDefs = list(list(className = 'dt-center', targets = '_all'))
                ),
                rownames = FALSE,
                caption = "Players You'll See and Times Evaluated")
    } else {
      return(NULL)
    }
  })
}

shinyApp(ui = ui, server = server)
