library("tidyr")
library("rvest")
library("dplyr")
library("tools")
library("stringr")
library("tools")

coachtommy2 <- setNames(data.frame(matrix(ncol=7)),  c("team", "season", "coach_first_name", "coach_last_name",
                                                       "coach_aux_1", "coach_aux_2", "grad_yr"))

coachtommy2 <- coachtommy2[FALSE,]

teamcount <- 0

for (seasoncount in 0:8){
  
  landing_page <- read_html(paste0("https://stats.ncaa.org/team/inst_team_list?academic_year=201", seasoncount, "&conf_id=-1&division=1&sport_code=WBB"))
  s <- html_session(paste0("https://stats.ncaa.org/team/inst_team_list?academic_year=201", seasoncount, "&conf_id=-1&division=1&sport_code=WBB"))
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
        coachbox <- (s %>% follow_link(team) %>% read_html() %>% html_nodes("fieldset") %>% html_text())[3]
        grad_yr_temp <- str_locate(coachbox, "Seasons")[1]
        grad_yr <- as.integer(substr(coachbox, grad_yr_temp - 4, grad_yr_temp - 1))
        if (is.na(grad_yr)){  #this if statement is for coaches with graduation listed as just the last two digits, like 88 and not 1988
          grad_yr <- as.integer(substr(coachbox, grad_yr_temp - 2, grad_yr_temp - 1))
          if (!is.na(grad_yr)){
            ifelse(grad_yr > 18, grad_yr + 1900, grad_yr + 2000)
          }
        }
        coach_exp_temp <- str_locate(coachbox, "Record:")[1]
        
        coachname <- strsplit((s %>% follow_link(team) %>% read_html() %>% html_nodes("fieldset a") %>% html_text())[4], "\\s")[[1]]
        coachtommy2[(nrow(coachtommy2) + 1),] <- c(teamslooping[i], paste(2009 + seasoncount, 2010 + seasoncount, sep = "-"), coachname[1:4], grad_yr)
      })
  }
}






# landing_page <- read_html("http://stats.ncaa.org/team/inst_team_list?academic_year=2018&conf_id=-1&division=1&sport_code=WBB")
# s <- html_session("http://stats.ncaa.org/team/inst_team_list?academic_year=2018&conf_id=-1&division=1&sport_code=WBB")



coach_and_team <- data.frame(coachtommy2[2:nrow(coachtommy2),], c(pogo[,1]))
#write.csv(coach_and_team, file = "coach_and_team.csv", row.names=FALSE)
coach_and_team <- read.csv("coach_and_team.csv", col.names = c("coach_first_name", "team"), stringsAsFactors = FALSE)
empvec <- c()
for (i in 1:nrow(coach_and_team)){
  empvec[i] <- strsplit(coach_and_team$team, split = "                ")[[i]][2]
}
coach_and_team <- data.frame("Coach" = coach_and_team$coach_first_name, "School" = empvec) %>%
  rowwise() %>%
  mutate(coachmale = any(grepl(str_c("^",Coach,"$"), usnamesmale$first_name, ignore.case = TRUE)),
         coachfemale = any(grepl(str_c("^",Coach,"$"), usnamesfemale$first_name, ignore.case = TRUE)),
         coachgender = ifelse(coachmale, "Male", ifelse(coachfemale, "Female", "Neither"))) %>%
  select(-coachmale, -coachfemale)




#Data cleaning

coach_data <- read.csv("coach_and_team2.csv")

coach_data2 <- coach_data %>%
  mutate_at(vars(grad_yr), funs(as.numeric(as.character(.)))) %>%
  mutate_at(vars(grad_yr), funs(ifelse(. < 100, . + 1900, .))) %>%
  rowwise() %>%
  mutate(coach_age = ifelse(as.integer(strsplit(as.character(season), "-")[[1]][1]) - grad_yr < 2, NA, 22 + as.integer(strsplit(as.character(season), "-")[[1]][1]) - grad_yr),
         coach_gender = usnames$gender[match(coach_first_name, usnames$first_name)])

coach_data_home <- coach_data2
colnames(coach_data_home) <- paste(colnames(coach_data2), "home", sep = "_")
coach_data_away <- coach_data2
colnames(coach_data_away) <- paste(colnames(coach_data2), "away", sep = "_")