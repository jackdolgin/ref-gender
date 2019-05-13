library("tidyr")
library("dplyr")
library("tools")
library("stringr")
library("tools")
library("lubridate")

togroupby <- c("ref_1_first_name", "ref_1_last_name", "ref_2_first_name", "ref_2_last_name", "ref_3_first_name", "ref_3_last_name", "ref_post_aux_1", "ref_post_aux_2", "ref_post_aux_3", "ref_post_aux_4", "ref_post_aux_5")
togroupby2 <- c("ref_1_first_name2", "ref_1_last_name2", "ref_2_first_name2", "ref_2_last_name2", "ref_3_first_name2", "ref_3_last_name2", "ref_post_aux_12", "ref_post_aux_22", "ref_post_aux_32", "ref_post_aux_42", "ref_post_aux_52")

allgames <- read.csv("games_df.csv", strip.white=TRUE)

first_ref_col <- grep("ref_1_first_name", colnames(allgames))



shift_left_paste <- function(x, y1, y2, z){
  x %>%
    mutate(ref_1_first_name = ifelse(grepl(y1, ref_1_first_name, ignore.case = TRUE) & grepl(y2, ref_1_last_name, ignore.case = TRUE), paste0(ref_1_first_name, z), ref_1_first_name),
  ref_1_last_name = ifelse(grepl(y1, ref_1_last_name, ignore.case = TRUE) & grepl(y2, ref_2_first_name, ignore.case = TRUE), paste0(ref_1_last_name, z), ref_1_last_name),
ref_2_first_name = ifelse(grepl(y1, ref_2_first_name, ignore.case = TRUE) & grepl(y2, ref_2_last_name, ignore.case = TRUE), paste0(ref_2_first_name, z), ref_2_first_name),
ref_2_last_name = ifelse(grepl(y1, ref_2_last_name, ignore.case = TRUE) & grepl(y2, ref_3_first_name, ignore.case = TRUE), paste0(ref_2_last_name, z), ref_2_last_name),
ref_3_first_name = ifelse(grepl(y1, ref_3_first_name, ignore.case = TRUE) & grepl(y2, ref_3_last_name, ignore.case = TRUE), paste0(ref_3_first_name, z), ref_3_first_name),
ref_3_last_name = ifelse(grepl(y1, ref_3_last_name, ignore.case = TRUE) & grepl(y2, ref_post_aux_1, ignore.case = TRUE), paste0(ref_3_last_name, z), ref_3_last_name),
ref_post_aux_1 = ifelse(grepl(y1, ref_post_aux_1, ignore.case = TRUE) & grepl(y2, ref_post_aux_2, ignore.case = TRUE), paste0(ref_post_aux_1, z), ref_post_aux_1),
ref_post_aux_2 = ifelse(grepl(y1, ref_post_aux_2, ignore.case = TRUE) & grepl(y2, ref_post_aux_3, ignore.case = TRUE), paste0(ref_post_aux_2, z), ref_post_aux_2),
ref_post_aux_3 = ifelse(grepl(y1, ref_post_aux_3, ignore.case = TRUE) & grepl(y2, ref_post_aux_4, ignore.case = TRUE), paste0(ref_post_aux_3, z), ref_post_aux_3),
ref_post_aux_4 = ifelse(grepl(y1, ref_post_aux_4, ignore.case = TRUE) & grepl(y2, ref_post_aux_5, ignore.case = TRUE), paste0(ref_post_aux_4, z), ref_post_aux_4))
}

shift_left <- function(x, y1, y2, z){
  x %>%
    mutate(ref_1_first_name = ifelse(grepl(y1, ref_1_first_name, ignore.case = TRUE) & grepl(y2, ref_1_last_name, ignore.case = TRUE), z, ref_1_first_name),
  ref_1_last_name = ifelse(grepl(y1, ref_1_last_name, ignore.case = TRUE) & grepl(y2, ref_2_first_name, ignore.case = TRUE), z, ref_1_last_name),
ref_2_first_name = ifelse(grepl(y1, ref_2_first_name, ignore.case = TRUE) & grepl(y2, ref_2_last_name, ignore.case = TRUE), z, ref_2_first_name),
ref_2_last_name = ifelse(grepl(y1, ref_2_last_name, ignore.case = TRUE) & grepl(y2, ref_3_first_name, ignore.case = TRUE), z, ref_2_last_name),
ref_3_first_name = ifelse(grepl(y1, ref_3_first_name, ignore.case = TRUE) & grepl(y2, ref_3_last_name, ignore.case = TRUE), z, ref_3_first_name),
ref_3_last_name = ifelse(grepl(y1, ref_3_last_name, ignore.case = TRUE) & grepl(y2, ref_post_aux_1, ignore.case = TRUE), z, ref_3_last_name),
ref_post_aux_1 = ifelse(grepl(y1, ref_post_aux_1, ignore.case = TRUE) & grepl(y2, ref_post_aux_2, ignore.case = TRUE), z, ref_post_aux_1),
ref_post_aux_2 = ifelse(grepl(y1, ref_post_aux_2, ignore.case = TRUE) & grepl(y2, ref_post_aux_3, ignore.case = TRUE), z, ref_post_aux_2),
ref_post_aux_3 = ifelse(grepl(y1, ref_post_aux_3, ignore.case = TRUE) & grepl(y2, ref_post_aux_4, ignore.case = TRUE), z, ref_post_aux_3),
ref_post_aux_4 = ifelse(grepl(y1, ref_post_aux_4, ignore.case = TRUE) & grepl(y2, ref_post_aux_5, ignore.case = TRUE), z, ref_post_aux_4))
}


shift_right_paste <- function(x, y1, z){
  x %>%
    mutate(ref_1_last_name = ifelse(grepl(y1, ref_1_first_name, ignore.case = TRUE), paste0(z, ref_1_last_name), ref_1_last_name),
           ref_2_first_name = ifelse(grepl(y1, ref_1_last_name, ignore.case = TRUE), paste0(z, ref_2_first_name), ref_2_first_name),
           ref_2_last_name = ifelse(grepl(y1, ref_2_first_name, ignore.case = TRUE), paste0(z, ref_2_last_name), ref_2_last_name),
           ref_3_first_name = ifelse(grepl(y1, ref_2_last_name, ignore.case = TRUE), paste0(z, ref_3_first_name), ref_3_first_name),
           ref_3_last_name = ifelse(grepl(y1, ref_3_first_name, ignore.case = TRUE), paste0(z, ref_3_last_name), ref_3_last_name),
           ref_post_aux_1 = ifelse(grepl(y1, ref_3_last_name, ignore.case = TRUE), paste0(z, ref_post_aux_1), ref_post_aux_1),
           ref_post_aux_2 = ifelse(grepl(y1, ref_post_aux_1, ignore.case = TRUE), paste0(z, ref_post_aux_2), ref_post_aux_2),
           ref_post_aux_3 = ifelse(grepl(y1, ref_post_aux_2, ignore.case = TRUE), paste0(z, ref_post_aux_3), ref_post_aux_3),
           ref_post_aux_4 = ifelse(grepl(y1, ref_post_aux_3, ignore.case = TRUE), paste0(z, ref_post_aux_4), ref_post_aux_4),
           ref_post_aux_5 = ifelse(grepl(y1, ref_post_aux_4, ignore.case = TRUE), paste0(z, ref_post_aux_5), ref_post_aux_5))
}

shift_right_paste2 <-  function(x, y, y2, z){
  x %>%
    mutate(ref_1_last_name = ifelse(grepl(y, ref_1_first_name, ignore.case = TRUE) & grepl(y2, ref_1_last_name, ignore.case = TRUE), paste0(ref_1_last_name, z), ref_1_last_name),
           ref_2_first_name = ifelse(grepl(y, ref_1_last_name, ignore.case = TRUE) & grepl(y2, ref_2_first_name, ignore.case = TRUE), paste0(ref_2_first_name, z), ref_2_first_name),
           ref_2_last_name = ifelse(grepl(y, ref_2_first_name, ignore.case = TRUE) & grepl(y2, ref_2_last_name, ignore.case = TRUE), paste0(ref_2_last_name, z), ref_2_last_name),
           ref_3_first_name = ifelse(grepl(y, ref_2_last_name, ignore.case = TRUE) & grepl(y2, ref_3_first_name, ignore.case = TRUE), paste0(ref_3_first_name, z), ref_3_first_name),
           ref_3_last_name = ifelse(grepl(y, ref_3_first_name, ignore.case = TRUE) & grepl(y2, ref_3_last_name, ignore.case = TRUE), paste0(ref_3_last_name, z), ref_3_last_name),
           ref_post_aux_1 = ifelse(grepl(y, ref_3_last_name, ignore.case = TRUE) & grepl(y2, ref_post_aux_1, ignore.case = TRUE), paste0(ref_post_aux_1, z), ref_post_aux_1),
           ref_post_aux_2 = ifelse(grepl(y, ref_post_aux_1, ignore.case = TRUE) & grepl(y2, ref_post_aux_2, ignore.case = TRUE), paste0(ref_post_aux_2, z), ref_post_aux_2),
           ref_post_aux_3 = ifelse(grepl(y, ref_post_aux_2, ignore.case = TRUE) & grepl(y2, ref_post_aux_3, ignore.case = TRUE), paste0(ref_post_aux_3, z), ref_post_aux_3),
           ref_post_aux_4 = ifelse(grepl(y, ref_post_aux_3, ignore.case = TRUE) & grepl(y2, ref_post_aux_4, ignore.case = TRUE), paste0(ref_post_aux_4, z), ref_post_aux_4),
           ref_post_aux_5 = ifelse(grepl(y, ref_post_aux_4, ignore.case = TRUE) & grepl(y2, ref_post_aux_5, ignore.case = TRUE), paste0(ref_post_aux_5, z), ref_post_aux_5))
}


allgames2 <- allgames %>%
  mutate_at(vars(Attendance), funs(gsub(",|\n|\n|^\\s+", "", .))) %>%
  mutate(ref_1_first_name = ifelse(grepl("[A-z]", Attendance), as.character(Attendance), as.character(ref_1_first_name)),
         Attendance = as.numeric(as.character(Attendance)),
         Date = as.Date(gsub("\n", "", Date), "%m/%d/%Y")) %>%
  separate(ref_1_first_name, into = togroupby2, remove = FALSE) %>%
  mutate(ref_1_first_name = coalesce(ref_1_first_name, ref_1_first_name2),
         ref_1_last_name = coalesce(as.character(ref_1_last_name), as.character(ref_1_last_name2)),
         ref_2_first_name = coalesce(as.character(ref_2_first_name), as.character(ref_2_first_name2)),
         ref_2_last_name = coalesce(as.character(ref_2_last_name), as.character(ref_2_last_name2)),
         ref_3_first_name = coalesce(as.character(ref_3_first_name), as.character(ref_3_first_name2)),
         ref_3_last_name = coalesce(as.character(ref_3_last_name), as.character(ref_3_last_name2)),
         ref_post_aux_1 = coalesce(as.character(ref_post_aux_1), as.character(ref_post_aux_12)),
         ref_post_aux_2 = coalesce(as.character(ref_post_aux_2), as.character(ref_post_aux_22)),
         ref_post_aux_3 = coalesce(as.character(ref_post_aux_3), as.character(ref_post_aux_32)),
         ref_post_aux_4 = coalesce(as.character(ref_post_aux_4), as.character(ref_post_aux_42)),
         ref_post_aux_5 = coalesce(as.character(ref_post_aux_5), as.character(ref_post_aux_52))) %>%
  select(-!!togroupby2) %>%
  mutate_at(vars(ref_1_first_name), funs(gsub("(\\s|\\.).*", "", .))) %>%
  mutate_at(vars(!!togroupby), funs(gsub("O\\'C(o)?o(n)?n(o|e)(r|n)(e)?-", "", .))) %>%
  mutate_at(vars(!!togroupby),
            funs(gsub(",|;|:|Referee|\\b(Ref|\\&|and)\\b|(<|\\()([A-z]|ref|(S|s)(B|b)|(A|a)lt.?)[0-9]?(>|\\))|-[A-z][0-9]|[A-z][0-9]-|\\b([A-z]|(S|s)(B|b)|(A|a)lt)-|-([A-z]|(S|s)(B|b)|(A|a)lt)\\b|(R|r|U|u)[0-9]|(A|a)lt\\.|^\\(|\\)$|^[^A-z]*$|\\b(A|a)lt\\.?\\b", "", .))) %>%
  shift_left_paste(".", "Jr\\.?", " Jr.") %>%
  shift_left_paste(".", "Sr\\.?", " Sr.") %>%
  shift_left_paste(".", "^II$", " II") %>%
  shift_left_paste(".", "^III$", " III") %>%
  shift_left_paste(".", "^IV$", " IV") %>%
  shift_left_paste("^Kay$", "^Bradley$", "junk") %>%
  shift_left_paste("^Ellen$", "^Roach$", "junk") %>%
  shift_left_paste("^Ann$", "^Garcia$", "junk") %>%
  shift_left_paste("^Allen$", "^Brown$", "junk") %>%
  shift_left_paste("^James$", "^Clarke?$", "junk") %>%
  shift_left_paste("^Watt$", "^Cruse$", "junk") %>%
  shift_left_paste("^Jo$", "^Smith$", "junk") %>%
  shift_left_paste("^Dean$", "^Ledington$", "junk") %>%
  shift_left_paste("^Lajwan$", "^Ren(n|e){1,2}$", "junk") %>%
  shift_left_paste("^Jamie$", "^Coleen$", "junk") %>%
  shift_left_paste("^Benito$", "^Benny$", "junk") %>%
  shift_left_paste("^Mich?al$", "^Ann$", "junk") %>%
  shift_left_paste("^FongRO-Bill$", "^Stricker$", "junk") %>%
  shift_right_paste2("^Chu$", "^Gonzalez$", "junk") %>%
  shift_left("^Jamie$", "^Broderick$", "Coleen") %>%
  shift_left("^Tiki$","^Kahoano$","Ethan") %>%
  shift_left("^Shelley$","^Russi$","Michelle") %>%
  shift_left("^Shelley$", "^Russi$", "Michelle") %>%
  shift_left("^Lajwan$", "^Robinson$", "Renee") %>%
  shift_left("^[[:punct:]]?Buts?ch[[:punct:]]?$", "^Die?t?s?ch(e|u)r$", "James") %>%
  shift_right_paste2("^Michelle$", "^Shelley$", "junk") %>%
  shift_right_paste2("^Ethan$", "^Tiki$", "junk") %>%
  shift_right_paste2("^Harding$", "^JR$", "junk") %>%
  shift_right_paste2("^Murray$", "Broomfield$", "junk") %>%
  shift_right_paste2("^Laura$", "^Lynne?$", "junk") %>%
  shift_right_paste2("^C(a|i)ss(i|o)(c|k)o$", "^Ste((ph)|v)ens$", "junk") %>%
  shift_right_paste2("^Shannon$", "^Linette$", "junk") %>%
  shift_right_paste2("^James$", "^[[:punct:]]?Buts?ch[[:punct:]]?$", "junk") %>%
  shift_right_paste2("^Deutsch$", "^East$", "junk") %>%
  shift_right_paste2("^Teresa$", "^Ann$", "junk") %>%
  shift_right_paste2("^Marissa$", "^Monique$", "junk") %>%
  shift_right_paste2("^Mary$", "^Elizabeth$", "junk") %>%
  shift_right_paste2("^JANA$", "^KAY$", "junk") %>%
  shift_right_paste2("^Brad(ley)?$", "^Edward$", "junk") %>%
  shift_right_paste2("^Adria$", "^Maria$", "junk") %>%
  shift_right_paste2("^Ann$", "^Marie$", "junk") %>%
  shift_right_paste2("^Michelle$", "^Michelle$", "junk") %>%
  shift_right_paste2("^Monroe$", "^Melot$", "junk") %>%
  shift_right_paste2("^Cameron$", "^Vannessa$", "junk") %>%
  shift_right_paste2("^James$", "^James$", "junk") %>%
  shift_right_paste("^St\\.?$", "St. ") %>%
  shift_right_paste("^La\\.?$", "La ") %>%
  shift_right_paste("^De\\.?$", "De ") %>%
  shift_right_paste("^(d|D)el\\.?$", "Del ") %>%
  shift_right_paste("^Van\\.?$", "Van ") %>%
  shift_right_paste("^Do\\.?$", "Do") %>%
  mutate(ref_1_last_name =ifelse(grepl("^Shelley$", ref_1_last_name) & grepl("^Russi$", ref_2_first_name), "Michelle", ref_1_last_name),
         ref_2_first_name = ifelse(grepl("^Shelley$", ref_2_first_name) & grepl("^Russi$", ref_2_last_name), "Michelle", ref_2_first_name),
         ref_2_last_name = ifelse(grepl("^Shelley$", ref_2_last_name) & grepl("^Russi$", ref_3_first_name), "Michelle", ref_2_last_name),
         ref_3_first_name = ifelse(grepl("^Shelley$", ref_3_first_name) & grepl("^Russi$", ref_3_last_name), "Michelle", ref_3_first_name),
         ref_3_last_name = ifelse(grepl("^Shelley$", ref_3_last_name) & grepl("^Russi$", ref_post_aux_1), "Michelle", ref_3_last_name),
         ref_post_aux_1 = ifelse(grepl("^Shelley$", ref_post_aux_1) & grepl("^Russi$", ref_post_aux_2), "Michelle", ref_post_aux_1),
         ref_post_aux_2 = ifelse(grepl("^Shelley$", ref_post_aux_2) & grepl("^Russi$", ref_post_aux_3), "Michelle", ref_post_aux_2),
         ref_post_aux_3 = ifelse(grepl("^Shelley$", ref_post_aux_3) & grepl("^Russi$", ref_post_aux_4), "Michelle", ref_post_aux_3),
         ref_post_aux_4 = ifelse(grepl("^Shelley$", ref_post_aux_4) & grepl("^Russi$", ref_1_last_name), "Michelle", ref_post_aux_4),
         
         
         
         ref_1_last_name = ifelse(grepl("^Banal$", ref_1_first_name) & grepl("^Hoyt$", ref_1_last_name), paste(ref_1_first_name, ref_1_last_name, sep = "-"), ref_1_last_name),
         ref_2_first_name =ifelse(grepl("^Banal$", ref_1_last_name) & grepl("^Hoyt$", ref_2_first_name), paste(ref_1_last_name, ref_2_first_name, sep = "-"), ref_2_first_name),
         ref_2_last_name = ifelse(grepl("^Banal$", ref_2_first_name) & grepl("^Hoyt$", ref_2_last_name), paste(ref_2_first_name, ref_2_last_name, sep = "-"), ref_2_last_name),
         ref_3_first_name = ifelse(grepl("^Banal$", ref_2_last_name) & grepl("^Hoyt$", ref_3_first_name), paste(ref_2_last_name, ref_3_first_name, sep = "-"), ref_3_first_name),
         ref_3_last_name = ifelse(grepl("^Banal$", ref_3_first_name) & grepl("^Hoyt$", ref_3_last_name), paste(ref_3_first_name, ref_3_last_name, sep = "-"), ref_3_last_name),
         ref_post_aux_1 = ifelse(grepl("^Banal$", ref_3_last_name) & grepl("^Hoyt$", ref_post_aux_1), paste(ref_3_last_name, ref_post_aux_1, sep = "-"), ref_post_aux_1),
         ref_post_aux_2 = ifelse(grepl("^Banal$", ref_post_aux_1) & grepl("^Hoyt$", ref_post_aux_2), paste(ref_post_aux_1, ref_post_aux_2, sep = "-"), ref_post_aux_2),
         ref_post_aux_3 = ifelse(grepl("^Banal$", ref_post_aux_2) & grepl("^Hoyt$", ref_post_aux_3), paste(ref_post_aux_2, ref_post_aux_3, sep = "-"), ref_post_aux_3),
         ref_post_aux_4 = ifelse(grepl("^Banal$", ref_post_aux_3) & grepl("^Hoyt$", ref_post_aux_4), paste(ref_post_aux_3, ref_post_aux_4, sep = "-"), ref_post_aux_4),
         ref_post_aux_5 = ifelse(grepl("^Banal$", ref_post_aux_4) & grepl("^Hoyt$", ref_1_last_name), paste(ref_post_aux_4, ref_1_last_name, sep = "-"), ref_post_aux_5)) %>%
         
  mutate_at(vars(!!togroupby), funs(gsub("^(|[[:punct:]]?Ify[[:punct:]]?|[A-z]?|(Ellen|Kay|East|Ann|Allen|James|Watt|Jo|Dean|Lajwan|Shelley|Jamie|Tiki|JR|Broomfield|Lynne?|Ste((ph)|v)ens|Benito|Linette|Monique|Mich?al|Elizabeth|KAY|Edward|Stricker|Mari(a|e)|Michelle|Melot|Vannessa|Chu|[[:punct:]]?Buts?ch[[:punct:]]?)junk|Banal|La|Do|(d|D)el?|Van|Jr|Sr|St|III?)\\.?$", "", ., ignore.case = T)))

#converts blank cells to NA's
allgames2[,togroupby][allgames2[,togroupby] == ""] <- NA      

allgamessample <- allgames2[,22:28]
allgamessample[] <- t(apply(allgamessample, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))) 

allgames3 <- cbind(allgames2[,1:21], allgamessample, allgames2[,29:39])

allgames3 <- allgames3 %>% filter(!(is.na(ref_1_first_name) | is.na(ref_3_last_name)))


usnames <- read.csv("usnames.csv")
  
allgames4 <- allgames3 %>%
  mutate(ref_1_gender = usnames$gender[match(ref_1_first_name, usnames$first_name)],
         ref_2_gender = usnames$gender[match(ref_2_first_name, usnames$first_name)],
         ref_3_gender = usnames$gender[match(ref_3_first_name, usnames$first_name)]) %>%
  filter_at(vars(ref_1_gender, ref_2_gender, ref_3_gender), all_vars(!is.na(.))) %>%
  mutate_at(vars(away_total_fouls), funs(as.numeric(as.character(.)))) %>%
  mutate_at(vars(away_total_techs, home_total_techs), funs(ifelse(is.na(.), 0, .))) %>%
  mutate(total_fouls = home_total_fouls + away_total_fouls,
         total_techs = home_total_techs + away_total_techs,
         refs_together = paste(ref_1_gender, ref_2_gender, ref_3_gender),
         numfemalerefs = str_count(refs_together, "F"))

allgames5 <- allgames4 %>%
  select(-c(teamcount, gamecount, Team_Num)) %>%
  distinct()


allgames6 <- allgames5 %>%
  mutate(season = ifelse(month(Date) < 7, paste(year(Date)-1, year(Date), sep = "-"), paste(year(Date), year(Date) + 1, sep = "-"))) %>%
  inner_join(coach_data_home, by = c("Home_Team" = "team_home", "season" = "season_home")) %>%
  inner_join(coach_data_away, by = c("Away_Team" = "team_away", "season" = "season_away")) %>%
  filter_at(vars(coach_gender_home, coach_gender_away), all_vars(!is.na(.))) %>%
  mutate(coaches_genders = paste(coach_gender_home, coach_gender_away),
         numfemalecoaches = str_count(coaches_genders, "F"),
         numfemaletechs = ifelse(coach_gender_home == "F",  home_total_techs, 0),
         numfemaletechs = ifelse(coach_gender_away == "F", numfemaletechs + away_total_techs, numfemaletechs),
         nummaletechs = ifelse(coach_gender_home == "M", home_total_techs, 0),
         nummaletechs = ifelse(coach_gender_away == "M", nummaletechs + away_total_techs, nummaletechs),
         numfemaletotalfouls = ifelse(coach_gender_home == "F",  home_total_fouls, 0),
         numfemaletotalfouls = ifelse(coach_gender_away == "F", numfemaletotalfouls + away_total_fouls, numfemaletotalfouls),
         nummaletotalfouls = ifelse(coach_gender_home == "M", home_total_fouls, 0),
         nummaletotalfouls = ifelse(coach_gender_away == "M", nummaletotalfouls + away_total_fouls, nummaletotalfouls))

