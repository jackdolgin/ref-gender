library("tidyr")
library("dplyr")
library("tools")
library("stringr")
library("tools")
library("magrittr")

coach_data <- read.csv("coaches.csv") %>%
  mutate_at(vars(grad_yr), list(~as.numeric(as.character(.)))) %>%
  mutate_at(vars(grad_yr), list(~ifelse(. < 100, . + 1900, .))) %>%
  rowwise() %>%
  mutate(coach_age = ifelse(as.integer(strsplit(as.character(season), "-")[[1]][1]) - grad_yr < 2, NA, 22 + as.integer(strsplit(as.character(season), "-")[[1]][1]) - grad_yr),
         coach_gender = usnames$gender[match(coach_first_name, usnames$first_name)])

coach_data_home <- coach_data
colnames(coach_data_home) <- paste(colnames(coach_data2), "home", sep = "_")
coach_data_away <- coach_data
colnames(coach_data_away) <- paste(colnames(coach_data2), "away", sep = "_")
