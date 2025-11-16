initializePlayerAppearances <- function() {
  list(
    friday_starters = list(),
    saturday_starters = list(),
    sunday_starters = list(),
    top_pos_players = list(),
    top_pos_player_game_count = list(),
    top_relievers = list(),
    top_reliever_game_count = list()
  )
}

calculateAccurateGameScore <- function(game, players, player_appearances) {
  game_score <- 0.5
  
  game_schools <- c(game$Away_Team, game$Home_Team)
  game_schools <- game_schools[!is.na(game_schools)]
  
  game_players <- players[players$School %in% game_schools, ]
  
  if(nrow(game_players) == 0) {
    return(game_score)
  }
  
  game_date <- as.character(game$Date)
  if(is.na(game_date)) game_date <- as.character(game$Game_Date)
  
  is_doubleheader <- FALSE
  if("Double_Header" %in% names(game)) {
    is_doubleheader <- isTRUE(game$Double_Header)
  }
  
  is_first_doubleheader_game <- TRUE  
  if(is_doubleheader && "Is_First_DH_Game" %in% names(game)) {
    is_first_doubleheader_game <- isTRUE(game$Is_First_DH_Game)
  }
  
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
  
  week_of_season <- 0
  if("Weeks_Since_Season" %in% names(game)) {
    week_of_season <- as.numeric(game$Weeks_Since_Season)
    if(is.na(week_of_season)) week_of_season <- 0
  }
  
  total_bonus <- 0
  
  for(i in 1:nrow(game_players)) {
    player <- game_players[i,]
    player_name <- as.character(player$Player_Name)
    
    player_bonus <- as.numeric(player$Signing_Bonus)
    if(is.na(player_bonus)) player_bonus <- 0
    
    player_role <- if("Role" %in% names(player)) as.character(player$Role) else "TopPosPlayer"
    
    is_friday_starter <- !is.na(player_role) && grepl("FriStarter", player_role, fixed=TRUE)
    is_saturday_starter <- !is.na(player_role) && grepl("SatStarter", player_role, fixed=TRUE)
    is_sunday_starter <- !is.na(player_role) && grepl("SunStarter", player_role, fixed=TRUE)
    is_top_pos_player <- !is.na(player_role) && grepl("TopPosPlayer", player_role, fixed=TRUE)
    is_top_reliever <- !is.na(player_role) && grepl("TopReliever", player_role, fixed=TRUE)
    
    if(is_friday_starter && is_friday) {
      if(!is_doubleheader || is_first_doubleheader_game) {
        injury_factor <- 1
        
        appearances <- 0
        if(player_name %in% names(player_appearances$friday_starters)) {
          prior_dates <- names(player_appearances$friday_starters[[player_name]])
          appearances <- sum(prior_dates < game_date, na.rm = TRUE)
        }
        
        times_seen <- appearances + 1
        times_seen_decay <- 1 / (times_seen ^ 0.75)
        
        player_game_bonus <- player_bonus * injury_factor * times_seen_decay
        total_bonus <- total_bonus + player_game_bonus
      }
      
    } else if(is_saturday_starter && is_saturday) {
      if(!is_doubleheader || is_first_doubleheader_game) {
        injury_factor <- 1
        
        appearances <- 0
        if(player_name %in% names(player_appearances$saturday_starters)) {
          prior_dates <- names(player_appearances$saturday_starters[[player_name]])
          appearances <- sum(prior_dates < game_date, na.rm = TRUE)
        }
        
        times_seen <- appearances + 1
        times_seen_decay <- 1 / (times_seen ^ 0.75)
        
        player_game_bonus <- player_bonus * injury_factor * times_seen_decay
        total_bonus <- total_bonus + player_game_bonus
      }
      
    } else if(is_sunday_starter && is_sunday) {
      if(!is_doubleheader || is_first_doubleheader_game) {
        injury_factor <- 1
        
        appearances <- 0
        if(player_name %in% names(player_appearances$sunday_starters)) {
          prior_dates <- names(player_appearances$sunday_starters[[player_name]])
          appearances <- sum(prior_dates < game_date, na.rm = TRUE)
        }
        
        times_seen <- appearances + 1
        times_seen_decay <- 1 / (times_seen ^ 0.75)
        
        player_game_bonus <- player_bonus * injury_factor * times_seen_decay
        total_bonus <- total_bonus + player_game_bonus
      }
      
    } else if(is_top_pos_player) {
      injury_factor <- 1
      
      is_doubleheader <- FALSE
      if("Double_Header" %in% names(game)) {
        is_doubleheader <- isTRUE(game$Double_Header)
      }
      
      game_appearances <- 0
      if(player_name %in% names(player_appearances$top_pos_player_game_count)) {
        game_appearances <- player_appearances$top_pos_player_game_count[[player_name]]
      }
      
      playing_prob <- 1.0
      if(!is_weekend) {
        playing_prob <- 0.97
      }
      
      times_seen <- game_appearances + 1
      
      if(game_appearances == 0) {
        times_seen_effective <- 1
        times_seen_decay <- 1
      } else {
        effective_prior_appearances <- 0
        
        if(player_name %in% names(player_appearances$top_pos_player_game_count)) {
          if(player_name %in% names(player_appearances$top_pos_players)) {
            prior_dates <- names(player_appearances$top_pos_players[[player_name]])
            
            for(prior_date in prior_dates) {
              day_type <- player_appearances$top_pos_players[[player_name]][[prior_date]]
              prior_prob <- 0.97  
              if(day_type == "weekend") prior_prob <- 1.0  
              
              effective_prior_appearances <- effective_prior_appearances + prior_prob
            }
          } else {
            effective_prior_appearances <- game_appearances * 0.98
          }
        }
        
        current_viewing_number <- times_seen
        times_seen_effective <- effective_prior_appearances * current_viewing_number
        times_seen_decay <- 1 / (times_seen_effective ^ 0.75)
      }
      
      player_game_bonus <- player_bonus * injury_factor * times_seen_decay
      total_bonus <- total_bonus + player_game_bonus
    } else if(is_top_reliever) {
      if(!is_doubleheader || is_first_doubleheader_game) {
        playing_prob <- 0.25
        if(is_friday) {
          playing_prob <- 0.65
        } else if(is_saturday) {
          playing_prob <- 0.55
        } else if(is_sunday) {
          playing_prob <- 0.50
        }
        
        game_appearances <- 0
        if(player_name %in% names(player_appearances$top_reliever_game_count)) {
          game_appearances <- player_appearances$top_reliever_game_count[[player_name]]
        }
        
        times_seen <- game_appearances + 1
        
        if(game_appearances == 0) {
          times_seen_effective <- 1
          times_seen_decay <- 1
        } else {
          effective_prior_appearances <- 0
          
          if(player_name %in% names(player_appearances$top_reliever_game_count) && 
             player_name %in% names(player_appearances$top_relievers)) {
            
            prior_dates <- names(player_appearances$top_relievers[[player_name]])
            
            for(prior_date in prior_dates) {
              day_type <- player_appearances$top_relievers[[player_name]][[prior_date]]
              prior_prob <- 0.25
              if(day_type == "friday") prior_prob <- 0.65
              else if(day_type == "saturday") prior_prob <- 0.55
              else if(day_type == "sunday") prior_prob <- 0.50
              
              effective_prior_appearances <- effective_prior_appearances + prior_prob
            }
          }
          
          current_viewing_number <- times_seen
          times_seen_effective <- effective_prior_appearances * current_viewing_number
          times_seen_decay <- 1 / (times_seen_effective ^ 0.75)
        }
        
        injury_factor <- 1
        
        
        base_bonus <- player_bonus * playing_prob
        
        player_game_bonus <- base_bonus * injury_factor * times_seen_decay
        total_bonus <- total_bonus + player_game_bonus
      }
    }
  }
  
  game_score <- game_score + (total_bonus / 10000)
  
  return(game_score)
}

updatePlayerAppearances <- function(game, players, player_appearances) {
  game_schools <- c(game$Away_Team, game$Home_Team)
  game_schools <- game_schools[!is.na(game_schools)]
  
  game_players <- players[players$School %in% game_schools, ]
  
  if(nrow(game_players) == 0) {
    return(player_appearances)
  }
  
  game_date <- as.character(game$Date)
  if(is.na(game_date)) game_date <- as.character(game$Game_Date)
  
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
  
  for(i in 1:nrow(game_players)) {
    player <- game_players[i,]
    player_name <- as.character(player$Player_Name)
    
    player_role <- if("Role" %in% names(player)) as.character(player$Role) else "TopPosPlayer"
    
    is_friday_starter <- !is.na(player_role) && grepl("FriStarter", player_role, fixed=TRUE)
    is_saturday_starter <- !is.na(player_role) && grepl("SatStarter", player_role, fixed=TRUE)
    is_sunday_starter <- !is.na(player_role) && grepl("SunStarter", player_role, fixed=TRUE)
    is_top_pos_player <- !is.na(player_role) && grepl("TopPosPlayer", player_role, fixed=TRUE)
    is_top_reliever <- !is.na(player_role) && grepl("TopReliever", player_role, fixed=TRUE)
    
    if(is_friday_starter && is_friday) {
      if(!(player_name %in% names(player_appearances$friday_starters))) {
        player_appearances$friday_starters[[player_name]] <- list()
      }
      player_appearances$friday_starters[[player_name]][[game_date]] <- TRUE
      
    } else if(is_saturday_starter && is_saturday) {
      if(!(player_name %in% names(player_appearances$saturday_starters))) {
        player_appearances$saturday_starters[[player_name]] <- list()
      }
      player_appearances$saturday_starters[[player_name]][[game_date]] <- TRUE
      
    } else if(is_sunday_starter && is_sunday) {
      if(!(player_name %in% names(player_appearances$sunday_starters))) {
        player_appearances$sunday_starters[[player_name]] <- list()
      }
      player_appearances$sunday_starters[[player_name]][[game_date]] <- TRUE
      
    } else if(is_top_pos_player) {
      is_doubleheader <- FALSE
      if("Double_Header" %in% names(game)) {
        is_doubleheader <- isTRUE(game$Double_Header)
      }
      
      if(!(player_name %in% names(player_appearances$top_pos_players))) {
        player_appearances$top_pos_players[[player_name]] <- list()
      }
      player_appearances$top_pos_players[[player_name]][[game_date]] <- TRUE
      
      if(!(player_name %in% names(player_appearances$top_pos_player_game_count))) {
        player_appearances$top_pos_player_game_count[[player_name]] <- 0
      }
      
      current_count <- player_appearances$top_pos_player_game_count[[player_name]]
      if(is_doubleheader) {
        player_appearances$top_pos_player_game_count[[player_name]] <- current_count + 2
      } else {
        player_appearances$top_pos_player_game_count[[player_name]] <- current_count + 1
      }
      
    } else if(is_top_reliever) {
      is_doubleheader <- FALSE
      if("Double_Header" %in% names(game)) {
        is_doubleheader <- isTRUE(game$Double_Header)
      }
      
      is_first_doubleheader_game <- TRUE
      if("Is_First_DH_Game" %in% names(game)) {
        is_first_doubleheader_game <- game$Is_First_DH_Game
      }
      
      if(!(player_name %in% names(player_appearances$top_relievers))) {
        player_appearances$top_relievers[[player_name]] <- list()
      }
      
      day_type <- "weekday"
      if(is_friday) day_type <- "friday"
      else if(is_saturday) day_type <- "saturday"
      else if(is_sunday) day_type <- "sunday"
      
      player_appearances$top_relievers[[player_name]][[game_date]] <- day_type
      
      if(!is_doubleheader || is_first_doubleheader_game) {
        if(!(player_name %in% names(player_appearances$top_reliever_game_count))) {
          player_appearances$top_reliever_game_count[[player_name]] <- 0
        }
        
        current_count <- player_appearances$top_reliever_game_count[[player_name]]
        player_appearances$top_reliever_game_count[[player_name]] <- current_count + 1
      }
    }
  }
  
  return(player_appearances)
}

processFinalSchedule <- function(optimalGames, players) {
  player_appearances <- initializePlayerAppearances()
  
  optimalGames <- optimalGames[order(as.Date(optimalGames$Date)), ]
  
  optimalGames$Is_First_DH_Game <- TRUE  
  
  dh_dates <- unique(optimalGames$Date[optimalGames$Double_Header == TRUE])
  for(dh_date in dh_dates) {
    date_games <- optimalGames[optimalGames$Date == dh_date & optimalGames$Double_Header == TRUE, ]
    
    if(nrow(date_games) > 1) {
      sorted_games <- date_games[order(as.character(date_games$Away_Team), as.character(date_games$Home_Team)), ]
      game_ids <- as.character(sorted_games$Game_ID)
      
      if(length(game_ids) > 1) {
        for(i in 2:length(game_ids)) {
          optimalGames$Is_First_DH_Game[optimalGames$Game_ID == game_ids[i]] <- FALSE
        }
      }
    }
  }
  
  for(i in 1:nrow(optimalGames)) {
    optimalGames$Bonus[i] <- calculateAccurateGameScore(optimalGames[i,], players, player_appearances)
    
    player_appearances <- updatePlayerAppearances(optimalGames[i,], players, player_appearances)
  }
  
  return(optimalGames)
}

