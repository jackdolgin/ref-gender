library("tidyr")
library("rvest")
library("dplyr")
library("tools")
library("stringr")
library("tools")

coach_df <- setNames(data.frame(matrix(ncol=7)),  c("team", "season", "coach_first_name", "coach_last_name",
                                                       "coach_aux_1", "coach_aux_2", "grad_yr"))

coach_df <- coach_df[FALSE,]

teamcount <- 0

for (seasoncount in 0:8){
  
  landing_page <- read_html(paste0("https://stats.ncaa.org/team/inst_team_list?academic_year=201", seasoncount, "&conf_id=-1&division=1&sport_code=WBB"))
  season <- html_session(paste0("https://stats.ncaa.org/team/inst_team_list?academic_year=201", seasoncount, "&conf_id=-1&division=1&sport_code=WBB"))
  all_teams <- landing_page %>%
    html_nodes("table") %>%
    html_text()
  yom <- strsplit(all_teams, "\n")[[1]]
  teamslooping <- str_trim(yom[which(str_detect(yom, "[A-z]"))])
  
  for (i in 1:length(teamslooping)){
    teamcount <- teamcount + 1
    if ((seasoncount == 0 & (i == 282 | i == 315 | i == 327)) |
        (seasoncount < 2 & i == 156) | (between(seasoncount, 2, 3) & i == 155) |
        (between(seasoncount, 1, 2) & (i == 281 | i == 314 | i == 326)) |
        (seasoncount == 3 & (i == 328 | i == 283 | i == 316)) |
        (seasoncount > 3 & (i == 158 | i == 286 | i == 320 | i == 332))){
      team <- i + 180 + add_to_lower_years
    } else{
      team <- teamslooping[i]
    }
    tryCatch(
      {
        coachbox <- (season %>% follow_link(team) %>% read_html() %>% html_nodes("fieldset") %>% html_text())[3]
        grad_yr_temp <- str_locate(coachbox, "Seasons")[1]
        grad_yr <- as.integer(substr(coachbox, grad_yr_temp - 4, grad_yr_temp - 1))
        if (is.na(grad_yr)){  #this if statement is for coaches with graduation listed as just the last two digits, like 88 and not 1988
          grad_yr <- as.integer(substr(coachbox, grad_yr_temp - 2, grad_yr_temp - 1))
          if (!is.na(grad_yr)){
            ifelse(grad_yr > 18, grad_yr + 1900, grad_yr + 2000)
          }
        }
        coach_exp_temp <- str_locate(coachbox, "Record:")[1]
        
        coachname <- strsplit((season %>% follow_link(team) %>% read_html() %>% html_nodes("fieldset a") %>% html_text())[4], "\\s")[[1]]
        coach_df[(nrow(coach_df) + 1),] <- c(teamslooping[i], paste(2009 + seasoncount, 2010 + seasoncount, sep = "-"), coachname[1:4], grad_yr)
      })
  }
}


write.csv(coach_df, file = "coaches.csv", row.names=FALSE)