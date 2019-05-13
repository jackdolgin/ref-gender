library("tidyr")
library("rvest")
library("dplyr")
library("tools")
library("stringr")
library("tools")
library("magrittr")


games_df <- setNames(data.frame(matrix(ncol=39)), c("teamcount", "gamecount", "away_total_techs", "home_total_techs",
                                                  "away_total_points", "home_total_points",
                                                  "away_total_fouls", "home_total_fouls",
                                                  "away_1st_techs", "home_1st_techs",
                                                  "away_2nd_techs", "home_2nd_techs",
                                                  "away_1st_points", "home_1st_points",
                                                  "away_2nd_points", "home_2nd_points",
                                                  "away_1st_fouls", "home_1st_fouls",
                                                  "away_2nd_fouls", "home_2nd_fouls",
                                                  "ref_pre_aux", "ref_1_first_name", "ref_1_last_name",
                                                  "ref_2_first_name", "ref_2_last_name",
                                                  "ref_3_first_name", "ref_3_last_name",
                                                  "ref_post_aux_1", "ref_post_aux_2", "ref_post_aux_3",
                                                  "ref_post_aux_4", "ref_post_aux_5", "Date",
                                                  "City_State_Arena", "Attendance",
                                                  "Away_Team", "Home_Team", "Ref_Name_Length", "Team_Num"))

games_df <- games_df[FALSE,]

gamecount <- 0
teamcount <- 0

for (seasoncount in 0:8){
  
  landing_page <- read_html(paste0("https://stats.ncaa.org/team/inst_team_list?academic_year=201", seasoncount, "&conf_id=-1&division=1&sport_code=WBB"))
  season <- html_session(paste0("https://stats.ncaa.org/team/inst_team_list?academic_year=201", seasoncount, "&conf_id=-1&division=1&sport_code=WBB"))
  
  all_teams <- landing_page %>%
    html_nodes("table") %>%
    html_text()
  yom <- strsplit(all_teams, "\n")[[1]]
  teamslooping <- str_trim(yom[which(str_detect(yom, "[A-z]"))])
  if (seasoncount < 6){
    add_to_lower_years <- 1
  } else{
    add_to_lower_years <- 0
  }
  
  for(i in 1:length(teamslooping)){
    teamcount <- teamcount + 1
    tryCatch(
      {
        if ((seasoncount == 0 & (i == 282 | i == 315 | i == 327)) |
            (seasoncount < 2 & i == 156) | (between(seasoncount, 2, 3) & i == 155) |
            (between(seasoncount, 1, 2) & (i == 281 | i == 314 | i == 326)) |
            (seasoncount == 3 & (i == 328 | i == 283 | i == 316)) |
            (seasoncount > 3 & (i == 158 | i == 286 | i == 320 | i == 332))){
          item <- i + 180 + add_to_lower_years
        } else{
          item <- teamslooping[i]
        }
        team_home <- season %>% follow_link(item) %>% read_html()
        marshall <- team_home %>%
          html_nodes("legend") %>%
          html_text()
        
        #finds out how many games the team played that season, which gives the number of lines in the data table to loop through until we've gone through all the games
        firstparens <- gregexpr(pattern ='\\(',marshall[1])
        firstparens <- firstparens[[1]][length(firstparens[[1]])]
        secondparens <- gregexpr(pattern ='\\)',marshall[1])
        secondparens <- secondparens[[1]][length(secondparens[[1]])]
        dash <- gregexpr(pattern ='-',marshall[1])
        dash <- dash[[1]][length(dash[[1]])]
        winstotal <- as.integer(substr(marshall, firstparens+1, dash-1)[1])
        losstotal <- as.integer(substr(marshall, dash+1, secondparens-1)[1])
        totalgames <- winstotal+losstotal
        
        the_team_linkforpaste <- html_attr(html_nodes(landing_page, "a"), "href")[i + 180 + add_to_lower_years]
        the_team_link <- paste("http://stats.ncaa.org", the_team_linkforpaste, sep ="")
        teamcountinput <- teamcount
      },
      error = function(cond){
        teamcountinput = "Team Skipped"
      }
    )
    additional_rows <- 0
    for (j in 1:(totalgames)){ 
      gamecount = gamecount + 1
      tryCatch(
        {
          contesthtml <- paste0("tr:nth-child(", j + additional_rows + 2, ")")
          contest <- team_home %>%
            html_nodes(contesthtml) %>%
            html_text()
          
          #some rows in the website's table don't have a game, but just like a line saying 'this was a tournament'; and if we count this row as a game as the for loop is currently doing, then the for loop won't reach the last game
          placeholding_which <- which(strsplit(contest[[1]], "\\s+")[[1]] == "-")
          if (length(placeholding_which) == 0){
            additional_rows <- additional_rows + 1
            contesthtml <- paste0("tr:nth-child(", j + additional_rows + 2, ")")
            contest <- team_home %>%
              html_nodes(contesthtml) %>%
              html_text()
          }
          dash_index <- max(which(strsplit(contest[[1]], "\\s+")[[1]] == "-"))
          final_score <- paste(strsplit(contest[[1]], "\\s+")[[1]][(dash_index-1):(dash_index+1)], collapse = " ")    # only drawback is, if a team wins a game twice by the exact same score or loses by the exact same score twice in a season, the first of these games will be double counted and the second game won't get counted
          gamepage <- html_session(the_team_link) %>%   # I need to copy the url from the team page, the team page is an intermediary; also after that, I'll want to write down the coaches' names[]
            follow_link(final_score) %>%
            read_html()
          boxscore <- gamepage %>% html_nodes("td:nth-child(1), td:nth-child(10), td:nth-child(18)") %>%
            html_text()
          game_header <- gamepage %>%
            html_nodes(".boldtext+ td") %>%
            html_text()
          techrows <- which(boxscore == "\nTEAM \n" | boxscore == "\nteam \n" | boxscore == "\nTeam \n") + 2
          
          firsthalflinkforpaste <- html_attr(html_nodes(gamepage, "a"), "href")[9]
          # some games (I think particularly closer to 2010) don't have data for each half, and I believe this function instead would take them to the play_by_play data without this if function; so we're saying if you have half data, then we'll take it, but if not (the first part of this if else clause) then just give us the game totals and a filler for the half columns
          if (grepl("/game/play_by_play", firsthalflinkforpaste)){
            halfdata <- rep("No boxscores by half supplies", 12)
          } else {
            firsthalflink <- paste("http://stats.ncaa.org", firsthalflinkforpaste, sep ="")
            firsthalfpage <-  html_session(firsthalflink) %>% read_html()
            firsthalfboxscore <- firsthalfpage %>% html_nodes("td:nth-child(1), td:nth-child(10), td:nth-child(18)") %>%
              html_text()
            firsthalftechrows <- which(firsthalfboxscore == "\nTEAM \n" | firsthalfboxscore == "\nteam \n" | firsthalfboxscore == "\nTeam \n") + 2
            
            secondhalflinkforpaste <- html_attr(html_nodes(gamepage, "a"), "href")[10]
            secondhalflink <- paste("http://stats.ncaa.org", secondhalflinkforpaste, sep ="")
            secondhalfpage <-  html_session(secondhalflink) %>% read_html()
            secondhalfboxscore <- secondhalfpage %>% html_nodes("td:nth-child(1), td:nth-child(10), td:nth-child(18)") %>%
              html_text()
            secondhalftechrows <- which(secondhalfboxscore == "\nTEAM \n" | secondhalfboxscore == "\nteam \n" | secondhalfboxscore == "\nTeam \n") + 2  
            
            halfdata <- c(substr(firsthalfboxscore[firsthalftechrows], 33, 33),
                          substr(secondhalfboxscore[secondhalftechrows], 33, 33),
                          substr(firsthalfboxscore[techrows + 2], 33, 34),
                          substr(secondhalfboxscore[techrows + 2], 33, 34),
                          substr(firsthalfboxscore[techrows + 3], 33, 34),
                          substr(secondhalfboxscore[techrows + 3], 33, 34))
          }         
          
          
          
          games_df[(nrow(games_df) + 1),] <- c(teamcount, gamecount,
                                           substr(boxscore[techrows], 33, 33),
                                           substr(boxscore[techrows + 2], 33, 34),
                                           substr(boxscore[techrows + 3], 33, 34),
                                           halfdata,
                                           strsplit(game_header[4], "\\s+")[[1]][1],
                                           strsplit(game_header[4], "\\s+")[[1]][2],
                                           strsplit(game_header[4], "\\s+")[[1]][3],
                                           strsplit(game_header[4], "\\s+")[[1]][4],
                                           strsplit(game_header[4], "\\s+")[[1]][5],
                                           strsplit(game_header[4], "\\s+")[[1]][6],
                                           strsplit(game_header[4], "\\s+")[[1]][7],
                                           strsplit(game_header[4], "\\s+")[[1]][8],
                                           strsplit(game_header[4], "\\s+")[[1]][9],
                                           strsplit(game_header[4], "\\s+")[[1]][10],
                                           strsplit(game_header[4], "\\s+")[[1]][11],
                                           strsplit(game_header[4], "\\s+")[[1]][12],
                                           game_header[c(1,2,3)],
                                           boxscore[c(2,3)],
                                           length(strsplit(game_header[4], "\\s+")[[1]]),
                                           i)
          
        },
        error = function(cond){
          games_df[(nrow(games_df) + 1),] <- c(teamcount, gamecount, rep("Trial Error", ncol(games_df) - 2))
        })
    }
  }
  write.csv(games_df, file = "games_df.csv", row.names=FALSE)
}


games_df <- read.csv("tommy3.csv", skip = 1, col.names = c("junk", "away_total_techs", "home_total_techs",
                                                         "away_1st_techs", "home_1st_techs",
                                                         "away_2nd_techs", "home_2nd_techs",
                                                         "away_total_points", "home_total_points",
                                                         "away_1st_points", "home_1st_points",
                                                         "away_2nd_points", "home_2nd_points",
                                                         "away_total_fouls", "home_total_fouls",
                                                         "away_1st_fouls", "home_1st_fouls",
                                                         "away_2nd_fouls", "home_2nd_fouls",
                                                         "ref_pre_aux", "ref_1_first_name", "ref_1_last_name",
                                                         "ref_2_first_name", "ref_2_last_name",
                                                         "ref_3_first_name", "ref_3_last_name",
                                                         "ref_post_aux_1", "ref_post_aux_2", "ref_post_aux_3",
                                                         "ref_post_aux_4", "ref_post_aux_5", "Date",
                                                         "City_State_Arena", "Attendance",
                                                         "Away_Team", "Home_Team", "Ref_Name_Length", "Team_Num"))

games_df %<>%
  filter(!str_detect(as.character(games_df$Away_Team), "Do Not Use") |       # Removes any games with the title "Do Not Use"
           !str_detect(as.character(games_df$Home_Team), "Do Not Use")) %>%
  rowwise() %>%
  filter(any(grepl(Away_Team,coach_and_team$School, ignore.case = TRUE)), #this line and line below filters data to include games with only DI teams
         any(grepl(Home_Team,coach_and_team$School, ignore.case = TRUE))) %>%
  select(-junk, -Team_Num)
games_df <- unique(games_df)  #Removes duplicate rows / games
