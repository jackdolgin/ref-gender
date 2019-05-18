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

#Remove rows with blanks for all ref name columns (I think)
allgamessample <- allgames2[,22:28]
allgamessample[] <- t(apply(allgamessample, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))) 

allgames3 <- cbind(allgames2[,1:21], allgamessample, allgames2[,29:39])

allgames3 <- allgames3 %>% filter(!(is.na(ref_1_first_name) | is.na(ref_3_last_name)))


allgames3 <- allgames3 %>%
  select(-c(teamcount, gamecount, Team_Num)) %>%
  distinct()


usnames <- read.csv("usnames.csv")

ref_gender_more <- function(x, y1, y2, z, g){
  x %>%
    mutate(ref_1_first_name = ifelse(grepl(y1, ref_1_first_name, ignore.case = TRUE) & grepl(y2, ref_1_last_name, ignore.case = TRUE), z, ref_1_first_name),
           ref_2_first_name = ifelse(grepl(y1, ref_2_first_name, ignore.case = TRUE) & grepl(y2, ref_2_last_name, ignore.case = TRUE), z, ref_2_first_name),
           ref_3_first_name = ifelse(grepl(y1, ref_3_first_name, ignore.case = TRUE) & grepl(y2, ref_3_last_name, ignore.case = TRUE), z, ref_3_first_name),
           ref_1_gender = ifelse(grepl(y1, ref_1_first_name, ignore.case = TRUE) & grepl(y2, ref_1_last_name, ignore.case = TRUE), as.character(g), as.character(ref_1_gender)),
           ref_2_gender = ifelse(grepl(y1, ref_2_first_name, ignore.case = TRUE) & grepl(y2, ref_2_last_name, ignore.case = TRUE), as.character(g), as.character(ref_2_gender)),
           ref_3_gender = ifelse(grepl(y1, ref_3_first_name, ignore.case = TRUE) & grepl(y2, ref_3_last_name, ignore.case = TRUE), as.character(g), as.character(ref_3_gender)))
}

ref_gender_more2 <- function(x, y1, y2, z, g){
  x %>%
    mutate(ref_1_last_name = ifelse(grepl(y1, ref_1_first_name, ignore.case = TRUE) & grepl(y2, ref_1_last_name, ignore.case = TRUE), z, ref_1_last_name),
           ref_2_last_name = ifelse(grepl(y1, ref_2_first_name, ignore.case = TRUE) & grepl(y2, ref_2_last_name, ignore.case = TRUE), z, ref_2_last_name),
           ref_3_last_name = ifelse(grepl(y1, ref_3_first_name, ignore.case = TRUE) & grepl(y2, ref_3_last_name, ignore.case = TRUE), z, ref_3_last_name),
           ref_1_gender = ifelse(grepl(y1, ref_1_first_name, ignore.case = TRUE) & grepl(y2, ref_1_last_name, ignore.case = TRUE), as.character(g), as.character(ref_1_gender)),
           ref_2_gender = ifelse(grepl(y1, ref_2_first_name, ignore.case = TRUE) & grepl(y2, ref_2_last_name, ignore.case = TRUE), as.character(g), as.character(ref_2_gender)),
           ref_3_gender = ifelse(grepl(y1, ref_3_first_name, ignore.case = TRUE) & grepl(y2, ref_3_last_name, ignore.case = TRUE), as.character(g), as.character(ref_3_gender)))
}

allgames4 <- allgames3 %>%
  mutate(ref_1_gender = usnames$gender[match(ref_1_first_name, usnames$first_name)],
         ref_2_gender = usnames$gender[match(ref_2_first_name, usnames$first_name)],
         ref_3_gender = usnames$gender[match(ref_3_first_name, usnames$first_name)]) %>%
  ref_gender_more("A((\\.)?J(\\.)?)|(nn?-?Jamil?n?ah?)", "Gregory", "An-Jamilah", "F") %>%
  ref_gender_more("^A(\\.)?V(\\.)?)", "Cervantes", "AV", "F") %>%
  ref_gender_more("(Albert)|(Bill)", "Titt?us", "Robert", "M") %>%
  ref_gender_more("Aliberti", "Janice", "Janice", "F") %>%
  ref_gender_more("All?yn", "Richardson", "Allyn", "M") %>%
  ref_gender_more2("Angel", "St(r?anton/?)|(ockton)", "", "") %>%
  ref_gender_more("Angel", "Kent", "Angel", "F") %>%
  ref_gender_more("Aniita", "Ortega", "Anita", "F") %>%
  ref_gender_more("(Anjinea)|(L(a|e)\\s?Sh((on)|(a'?)))", "(LaSha)|(Hopson)", "LaSha", "F") %>%
  ref_gender_more("Antoive", "Antoine", "Love", "M") %>%
  ref_gender_more("Bar", "Smith", "Barb", "F") %>%
  ref_gender_more("((Barl)|(Dan))a", "Foutz", "Darla", "F") %>%
  ref_gender_more("Beeny", "Luna", "Benny", "M") %>%
  ref_gender_more("Better", "Booker-Parks", "Betty", "F") %>%
  ref_gender_more("Bil", "Larance", "", "Bill") %>%
  ref_gender_more("Bobbu", "Jarmon", "Bobby", "M") %>%
  ref_gender_more("Boo?nn?ies?", "Pettus", "Bonnie", "F") %>%
  ref_gender_more("Booby", "Deavus", "Bobby", "M") %>%
  ref_gender_more("Brab", "Smith", "Brad", "M") %>%
  ref_gender_more2("Britton", "S((herr)|(tor))y/?", "Sherry", "M") %>%
  ref_gender_more("Caet", "Martin", "Chet", "M") %>%
  ref_gender_more("Car?lr?o(s|z)", "Alvarez", "Carlos", "M") %>%
  ref_gender_more("Car?l(r|o)a", "Fountain", "Carla", "F") %>%
  ref_gender_more("(U|R)?C(a|o)(m|n)erona?", "Inouye", "Cameron", "F") %>%
  ref_gender_more("Carme(nt)?", "Pottorreal", "Carmen", "F") %>%
  ref_gender_more("Caroal", "Comanita", "Carol", "F") %>%
  ref_gender_more("Ch(a|e)(n|m)n?c?ey", "Mu?e?u?ns?ch", "Chaney", "F") %>%
  ref_gender_more("Charisee", "Okamoto", "Charisse", "F") %>%
  ref_gender_more("Char(l|k)i?e?(y|r|s)?", "Hur?st", "Charlie", "M") %>%
  ref_gender_more("Cherll", "Blue", "Cheryll", "F") %>%
  ref_gender_more("Chris", "Gonzales", "Chris", "M") %>%
  ref_gender_more2("Clarke", "St?e((ph)|v)ens?", "Stevens", "M") %>%
  ref_gender_more("Collen", "O'Connor", "Colleen", "F") %>%
  ref_gender_more("corma", "Jones", "Norma", "F") %>%
  ref_gender_more("D'Juan", "Chapman", "D'Juan", "M") %>%
  ref_gender_more("D'?Lynn?", "Sc?hw?(a|e)r?tze?", "D'Lynn", "F") %>%
  ref_gender_more("(I|(Da))'?Fini", "Robinson", "In'Fini", "F") %>%
  ref_gender_more("Daili", "Kimura", "Kaili", "F") %>%
  ref_gender_more("Dana", "Pysz", "Dana", "M") %>%
  ref_gender_more("Danyel", "Bittle", "Daniel", "M") %>%
  ref_gender_more("Daqrren", "Krzesnik", "Darren", "M") %>%
  ref_gender_more("Darchy?", "Schlabhach", "Darcy", "F") %>%
  ref_gender_more("Darei", "Doll", "Dari", "F") %>%
  ref_gender_more("Darly", "Humphrey", "Daryl", "M") %>%
  ref_gender_more("Darrrel", "Mitchel", "Darell", "M") %>%
  ref_gender_more("Dawh", "Marsh", "Dawn", "F") %>%
  ref_gender_more("Demoya", "Williams", "Demoya", "F") %>%
  ref_gender_more("Denanna", "Blakenberg", "Deanna", "F") %>%
  ref_gender_more("DeSha(u|w)n", "Sav(a|ed)ge", "DeShawn", "M") %>%
  ref_gender_more("Desiree'", "Peterkin", "Desiree", "F") %>%
  ref_gender_more("D(o|eu)rr?(a|e)n", "Pullins", "Deron", "M") %>%
  ref_gender_more("Digana", "Harris", "Dijana", "F") %>%
  ref_gender_more("Do(glas|vy)", "Cloud", "Douglas", "M") %>%
  ref_gender_more("Dorr(a|e)n", "Stewart", "Dorran", "M") %>%
  ref_gender_more("R?Dough?", "Williams", "Doug", "M") %>%
  ref_gender_more("Dou(p|z)", "Zipp", "Doug", "M") %>%
  ref_gender_more("Drw", "Judge", "Drew", "M") %>%
  ref_gender_more("Duanne", "Mack", "Duane", "M") %>%
  ref_gender_more("E(c|d)itry(m|n)m?", "LaMm?arr?", "Ecitrym", "M") %>%
  ref_gender_more("Edddie", "Gray", "Eddie", "M") %>%
  ref_gender_more("Eizika", "Herriman", "Erika", "F") %>%
  ref_gender_more("Ereck", "Thome", "Erick", "M") %>%
  ref_gender_more("Eric\\.", "Brewerton", "Erick", "M") %>%
  ref_gender_more("Esreal", "Silva", "Esrael", "M") %>%
  ref_gender_more("(F|G)(a|o)tt?u?i?o?u?(-Cissok)?", "(Cissoko(-)?)?(Stephens)?", "Fatou", "F") %>%
  ref_gender_more("R?Ga(d|t)or", "Parrish", "Gator", "M") %>%
  ref_gender_more("G(a|e)ra?la?dine", "Smith", "Geraldine", "F") %>%
  ref_gender_more("Gerry", "Pollard", "Gerry", "M") %>%
  ref_gender_more("R?Gi(n|r)a", "Cross", "Gina", "F") %>%
  ref_gender_more("Hea(r|tc)her", "Browne", "Heather", "F") %>%
  ref_gender_more("Herbet", "Jackson", "Herbert", "M") %>%
  ref_gender_more("Herome", "Skrine", "Jerome", "M") %>%
  ref_gender_more("Hurb", "Burl", "Herb", "M") %>%
  ref_gender_more("Iffy", "Seales", "Ifeyinwa", "F") %>%
  ref_gender_more("Issa", "Johnson", "Isaac", "M") %>%
  ref_gender_more("J(\\.)?B(\\.)?", "De(\\s)?Rosa", "JB", "M") %>%
  ref_gender_more("Jackie", "Sanders", "Jackie", "M") %>%
  ref_gender_more("Jai?mi?e", "Br(a|o)deric(h|k)", "Jamie", "F") %>%
  ref_gender_more("J(a|u)ll?es", "Gai?ll?i?(e|o|u)(m|n|s)", "Jules", "M") %>%
  ref_gender_more("JamesJr\\.", "Perry.*", "James", "M") %>%
  ref_gender_more("Je(li)?ss(i(ss)?e)?", "Oliver", "Jelisse", "F") %>%
  ref_gender_more2("Jen", "Rezac?k?", "Rezac", "F") %>%
  ref_gender_more("Jenane", "Pence", "Jeneane", "F") %>%
  ref_gender_more("R?Jessi?e", "Dickerson", "Jesse", "M") %>%
  ref_gender_more("Jin", "Levinson", "Jon", "M") %>%
  ref_gender_more("Joae?nnn?e?", "Au?ldri(ch|ge)", "Joanne", "F") %>%
  ref_gender_more("Jody", "Taylor", "Jody", "M") %>%
  ref_gender_more("Jule", "Krommenhoek", "Jules", "M") %>%
  ref_gender_more("Kanielle", "Johnson", "Danielle", "F") %>%
  ref_gender_more("Kark", "Ijames", "Mark", "M") %>%
  ref_gender_more("Karleens", "Tobin", "Karleena", "F") %>%
  ref_gender_more("KC", "Nash", "Ken", "M") %>%
  ref_gender_more("Keeneth", "Kelly", "Kenneth", "M") %>%
  ref_gender_more("Keither", "Miller", "Keith", "M") %>%
  ref_gender_more2("Kelcey", "Roe?gi?ers.*", "", "F") %>%
  ref_gender_more("Keneitha", "Shoulder", "Kenethia", "F") %>%
  ref_gender_more("Kennethy", "Kelly", "Kenneth", "M") %>%
  ref_gender_more("Kethleen", "Lynch", "Kathleen", "F") %>%
  ref_gender_more("Kev", "Ruland", "Kevin", "M") %>%
  ref_gender_more("Ki(im)?", "Thebo", "Kim", "F") %>%
  ref_gender_more("Kie?mbers?ly", "Hobbs", "Kimberly", "F") %>%
  ref_gender_more2("Kris", "Den?son", "Denson", "F") %>%
  ref_gender_more("Kristein", "Bell", "Kristen", "F") %>%
  ref_gender_more("Krys(ti)?le", "Apellaniz", "Krystle", "F") %>%
  ref_gender_more("Kyile", "Galloway", "Kylie", "F") %>%
  ref_gender_more("Kyke", "Bacon", "Kyle", "M") %>%
  ref_gender_more("Kykesha", "Thompson", "Nykesha", "F") %>%
  ref_gender_more("L\\.Renee", "Robinson", "Renee", "F") %>%
  ref_gender_more("L(a|e|c)(\\s)?S?h?(a|i|o|u)?(m|n)d?r?a", "(Miller-)?(R|K)idd", "LaShonda", "F") %>%
  ref_gender_more("L(a|e)Son(i|y)a", "Bakee?r", "LaSonia", "F") %>%
  ref_gender_more("Lavrie", "Porter", "Laurie", "F") %>%
  ref_gender_more("Laweson", "Newton", "Lawson", "M") %>%
  ref_gender_more("Leslie", "Jones", "Leslie", "M") %>%
  ref_gender_more2("Leslie", "Pors?chen", "Porschen", "F") %>%
  ref_gender_more("Lipya", "Rentz", "Libya", "F") %>%
  ref_gender_more("Lorenza", "Ahumada", "Lorena", "F") %>%
  ref_gender_more("Loti", "Francescon", "Lori", "F") %>%
  ref_gender_more("Macr", "Merritt", "Marc", "M") %>%
  ref_gender_more2("Maj", "F(r?o|a)r?r?e?st?b(e|u)r(g|t)", "Forsberg", "F") %>%
  ref_gender_more("My?a?y?", "Forsberg", "May", "F") %>%
  ref_gender_more("Mantrel", "Simmons", "Montrel", "M") %>%
  ref_gender_more("march", "(Merritt)|(Resch)", "Mark", "M") %>%
  ref_gender_more("R?Me?a?(tt|rr)a", "Roberts", "Metta", "F") %>%
  ref_gender_more("Mazett(a|e)", "Garr?ett?", "Mazetta", "F") %>%
  ref_gender_more("Mead\\.", "Overstreet", "Meadow", "F") %>%
  ref_gender_more("Micahael", "Schmidt", "Michael", "M") %>%
  ref_gender_more("Michael", "Murray", "Michol", "F") %>%
  ref_gender_more("Michal", "McConnell", "Michael", "M") %>%
  ref_gender_more("Michel(-St)?", ".*Pierre", "Michele", "F") %>%
  ref_gender_more("Mich(el)?(lle)?", "De(l\\s|i)duco", "Michelle", "F") %>%
  ref_gender_more("MIichele", "Eglinger", "Michele", "F") %>%
  ref_gender_more("R?Miss?y?y", "Brooks", "Missy", "F") %>%
  ref_gender_more("Mitchelle", "Russi", "Michelle", "F") %>%
  ref_gender_more("Naneth", "Sisk", "Nan", "F") %>%
  ref_gender_more("Nark", "Hardcastle", "Mark", "M") %>%
  ref_gender_more("Natashai", "Camy", "Natash", "F") %>%
  ref_gender_more("Nathosha", "Harris", "Natosha", "F") %>%
  ref_gender_more("N(e|i)(a|o)n?(ri|t)a", ".*", "Neonta", "F") %>%
  ref_gender_more("Ni(cr)?e", "Cappel", "Nic", "M") %>%
  ref_gender_more("Nyu?(k|il)ea?sha", "Thompson", "Nykesha", "F") %>%
  ref_gender_more(".*", "^Spurl(a|oc)k$", "Pualani", "F") %>%
  ref_gender_more("Pachel", "Monroe", "Rachael", "F") %>%
  ref_gender_more("Pas?a?tr?o?e?r?n?", "Torres", "Pastor", "M") %>%
  ref_gender_more("Pat", "Gebhart|Deforte", "Pat", "M") %>%
  ref_gender_more("Petti", "Fernandez", "Patti", "F") %>%
  ref_gender_more("Pheniee", "Ransom", "Phenizee", "M") %>%
  ref_gender_more("Phili", "Freels", "Phillip", "M") %>%
  ref_gender_more("Pucho", "Figueroa", "Pucho", "M") %>%
  ref_gender_more("R\\.J\\.", "Meyer", "RJ", "M") %>%
  ref_gender_more("Rac?s?ha?e?n", "Michel", "Rashan", "M") %>%
  ref_gender_more("Radny", "Robinson", "Randy", "M") %>%
  ref_gender_more("RaeAnna?", "Cas(a|si)dy", "RaeAnn", "F") %>%
  ref_gender_more2("Ramsey", "Marlin", "Marlin", "M") %>%
  ref_gender_more("Rander", "Robertson", "Randy", "M") %>%
  ref_gender_more("Raulette", "Franklin", "Paulette", "F") %>%
  ref_gender_more("RBeverly", "Roberts", "Beverly", "F") %>%
  ref_gender_more("RBob", "Trammell", "Bob", "M") %>%
  ref_gender_more("RCharles", "Carroll", "Charles", "M") %>%
  ref_gender_more("Rck", "Morris", "Rick", "M") %>%
  ref_gender_more("RDave", "Rittman", "Dave", "M") %>%
  ref_gender_more("RDebbie", "Adams", "Debbie", "F") %>%
  ref_gender_more("RDenise", "Brooks", "Denise", "F") %>%
  ref_gender_more("REachelle", "Jones", "Rachelle", "F") %>%
  ref_gender_more("RFelicia", "Grinter", "Felicia", "F") %>%
  ref_gender_more("RGreg", "Small", "Greg", "M") %>%
  ref_gender_more("Riachrd", "Waters", "Richard", "M") %>%
  ref_gender_more("Rib", "Fessler", "Rob", "M") %>%
  ref_gender_more("Rid", "Creech", "Rod", "M") %>%
  ref_gender_more("RJoe", "Cunningham", "Joe", "M") %>%
  ref_gender_more("(U|)RJohn", "Fletcher", "John", "M") %>%
  ref_gender_more("RKelly", "Johnson", "Kelly", "F") %>%
  ref_gender_more("RKevin", "Britt", "Kevin", "M") %>%
  ref_gender_more("RLisa", "Mattingly", "Lisa", "F") %>%
  ref_gender_more("Rmark", "Zentz", "Mark", "M") %>%
  ref_gender_more("RMichael", "Price", "Michael", "M") %>%
  ref_gender_more("Roger.*", "Brown.*", "Roger", "M") %>%
  ref_gender_more("Ro(i|r)y", "Gulbeyan", "Roy", "M") %>%
  ref_gender_more("Ronalds", "Ledington", "Ronald", "M") %>%
  ref_gender_more("RRachel", "Melot", "Rachel", "F") %>%
  ref_gender_more("RRobert", "Strong", "Robert", "M") %>%
  ref_gender_more("RSue", "Blauch", "Sue", "F") %>%
  ref_gender_more("RTim", "Daley", "Tim", "M") %>%
  ref_gender_more("RTimothy", "Bryant", "Timothy", "M") %>%
  ref_gender_more("RTina", "Napier", "Tina", "F") %>%
  ref_gender_more("Rysan", "Nunes", "Ryan", "M") %>%
  ref_gender_more("Sawn", "Marsh", "Dawn", "F") %>%
  ref_gender_more("Saye", "Wilson", "Faye", "F") %>%
  ref_gender_more("Scoot", "Starkey", "Scott", "M") %>%
  ref_gender_more("Scottt", "Fruehling", "Scott", "M") %>%
  ref_gender_more("Scotty", "Hermann", "Scoty", "M") %>%
  ref_gender_more("Shannon", "Boone", "Shanon", "F") %>%
  ref_gender_more("Sharv?(a|u)?ez", "Bra?own", "Sharvez", "M") %>%
  ref_gender_more("Sha(ui|w)ne?", "Goode", "Shawn", "M") %>%
  ref_gender_more("Sheeley", "Nakasone", "Shelley", "F") %>%
  ref_gender_more("Shoalin", "Crawford", "Shaolin", "F") %>%
  ref_gender_more("Sie", "Blauch", "Sue", "F") %>%
  ref_gender_more2("Skelly", "Win?gard?", "Wingard", "F") %>%
  ref_gender_more("Sterlinig", "Higgins", "Sterling", "M") %>%
  ref_gender_more("Stevve", "Matuszewski", "Steve", "M") %>%
  ref_gender_more("Sunny", "Scyphers", "Sonny", "M") %>%
  ref_gender_more("Symbri", "Tuttle", "Symbri", "F") %>%
  ref_gender_more("Taacha", "Brown-Drummond", "Taacha", "F") %>%
  ref_gender_more("T(a(i|l)|e|ia)qu(a|e|i|o)(a|l)?", "Stewar(d|t)", "Taiqua", "F") %>%
  ref_gender_more("Th?eo?d?tis?d?", "Tidwell", "Theodis", "M") %>%
  ref_gender_more("Teeresa", "Turner", "Teresa", "F") %>%
  ref_gender_more("Terr-Tubbs", "Carbone", "Terri", "F") %>%
  ref_gender_more("Ter?ry", "Funk", "Terry", "F") %>%
  ref_gender_more("Theromiles", "Flowers", "Theromiles", "M") %>%
  ref_gender_more("Thr?umann?", "Leggs.*", "Thurman", "M") %>%
  ref_gender_more("Tiffan", "Jump", "Tiffany", "F") %>%
  ref_gender_more("Timother", "Buckner", "Timothy", "M") %>%
  ref_gender_more("Toma", "Gillen", "Toma", "M") %>%
  ref_gender_more2("Tombi", "(Be|Ha)ll|Bates", "Bell", "F") %>%
  ref_gender_more("Tommie", "Paris", "Tommi", "F") %>%
  ref_gender_more("Tont", "Feroah", "Tony", "M") %>%
  ref_gender_more("Topy", "Jones", "Toby", "M") %>%
  ref_gender_more("Tory", "Winders", "Troy", "M") %>%
  ref_gender_more("Trayn", "Towns", "Taryn", "F") %>%
  ref_gender_more("Trooy", "Ramey", "Troy", "M") %>%
  ref_gender_more("Tunita", "Dumas", "Tunita", "F") %>%
  ref_gender_more("Victory", "Roy|Terrgiano", "Victor", "M") %>%
  ref_gender_more("Vida?mant(a|i)s", "Petrai?ti?s", "Vidmantas", "M") %>%
  ref_gender_more("Wallis", "Bryan", "Wallis", "F") %>%
  ref_gender_more("Warey", "Day", "Mary", "F") %>%
  ref_gender_more("Warr(ren|ing)", "Harding.*", "Warren", "M") %>%
  ref_gender_more("Wedny", "Dailey", "Wendy", "F") %>%
  ref_gender_more("Whtney", "Armstrong", "Whiteney", "F") %>%
  ref_gender_more("Xiom(a|e)rr?a", "Cruz", "Xiomara", "F") %>%
  
  filter_at(vars(ref_1_gender, ref_2_gender, ref_3_gender), all_vars(!is.na(.))) %>%
  mutate_at(vars(away_total_fouls), funs(as.numeric(as.character(.)))) %>%
  mutate_at(vars(away_total_techs, home_total_techs), funs(ifelse(is.na(.), 0, .))) %>%
  mutate(total_fouls = home_total_fouls + away_total_fouls,
         total_techs = home_total_techs + away_total_techs,
         refs_together = paste(ref_1_gender, ref_2_gender, ref_3_gender),
         numfemalerefs = str_count(refs_together, "F"),
         nummalerefs = str_count(refs_together, "M"))


coach_gender_more <- function(x, y1, y2, g){
  x %>%
    mutate(coach_gender_home = ifelse(grepl(y1, coach_first_name_home, ignore.case = TRUE) & grepl(y2, coach_last_name_home, ignore.case = TRUE), as.character(g), coach_gender_home),
           coach_gender_away = ifelse(grepl(y1, coach_first_name_away, ignore.case = TRUE) & grepl(y2, coach_last_name_away, ignore.case = TRUE), as.character(g), coach_gender_away))
}


allgames6 <- allgames4 %>%
  mutate(season = ifelse(month(Date) < 7, paste(year(Date)-1, year(Date), sep = "-"), paste(year(Date), year(Date) + 1, sep = "-"))) %>%
  inner_join(coach_data_home, by = c("Home_Team" = "team_home", "season" = "season_home")) %>%
  inner_join(coach_data_away, by = c("Away_Team" = "team_away", "season" = "season_away")) %>%
  mutate_at(vars(coach_gender_home, coach_gender_away), funs(as.character(.))) %>%
  coach_gender_more("Adell", "Harris", "F") %>%
  coach_gender_more("Agnus", "Berenato", "F") %>%
  coach_gender_more("Ali", "Jaques", "F") %>%
  coach_gender_more("Altherias", "Warmley", "F") %>%
  coach_gender_more("Angel", "Elderkin", "F") %>%
  coach_gender_more("AnnMarie", "Gilbert", "F") %>%
  coach_gender_more("Ardie", "McInelly", "F") %>%
  coach_gender_more("Bunky", "Harkleroad", "M") %>%
  coach_gender_more("C\\.", "Vivian", "F") %>%
  coach_gender_more("Carey", "Green", "M") %>%
  coach_gender_more("Coquese", "Washington", "F") %>%
  coach_gender_more("Dana", "Takahara-Dias", "F") %>%
  coach_gender_more("Daynia", "LaForce-Mann", "F") %>%
  coach_gender_more("DeLisha", "Milton-Jones", "F") %>%
  coach_gender_more("DeUnna", "Hendrix", "F") %>%
  coach_gender_more("DeWayne", "Burroughs", "M") %>%
  coach_gender_more("DoBee", "Plaisance", "F") %>%
  coach_gender_more("Itoro", "Coleman", "F") %>%
  coach_gender_more("Jackie", "Smith", "F") %>%
  coach_gender_more("Jaime", "White", "F") %>%
  coach_gender_more("Jamelle", "Elliott", "F") %>%
  coach_gender_more("Jamie", "Craighead", "F") %>%
  coach_gender_more("JD", "Gravina", "M") %>%
  coach_gender_more("Jenerrie", "Harris", "F") %>%
  coach_gender_more("Jody", "Wynn", "F") %>%
  coach_gender_more("Jody", "Adams", "F") %>%
  coach_gender_more("Jordan", "Dupuy", "M") %>%
  coach_gender_more("JR", "Payne", "F") %>%
  coach_gender_more("Kerry", "Cremeans", "F") %>%
  coach_gender_more("LaVonda", "Wagner", "F") %>%
  coach_gender_more("LeDawn", "Gibson", "F") %>%
  coach_gender_more("Lee", "Ann", "F") %>%
  coach_gender_more("Lee", "Buchanan", "M") %>%
  coach_gender_more("Leslie", "Crane", "F") %>%
  coach_gender_more("Lubomyr", "Lichonczak", "M") %>%
  coach_gender_more("MaChelle", "Joseph", "F") %>%
  coach_gender_more("Muffet", "McGraw", "F") %>%
  coach_gender_more("Niecee", "Nelson", "F") %>%
  coach_gender_more("Oties", "Epps", "M") %>%
  coach_gender_more("Pat", "Summitt", "F") %>%
  coach_gender_more("Pat", "Coyle", "F") %>%
  coach_gender_more("Ravon", "Justice", "F") %>%
  coach_gender_more("Shann", "Hart", "F") %>%
  coach_gender_more("Shimmy", "Gray-Miller", "F") %>%
  coach_gender_more("Sytia", "Messer", "F") %>%
  coach_gender_more("Tajama", "Abraham", "F") %>%
  coach_gender_more("Tooey", "Loy", "M") %>%
  coach_gender_more("Tory", "Verdi", "M") %>%
  coach_gender_more("Toyelle", "Wilson", "F") %>%
  coach_gender_more("Yolett", "McPhee-McCuin", "F") %>%
  coach_gender_more("Zenarae", "Antoine", "F") %>%
  
  filter_at(vars(coach_gender_home, coach_gender_away), all_vars(!is.na(.))) %>%
  mutate(coaches_genders = paste(coach_gender_home, coach_gender_away),
         numfemalecoaches = str_count(coaches_genders, "F"),
         nummalecoaches = str_count(coaches_genders, "M"),
         numfemaletechs = ifelse(coach_gender_home == "F",  home_total_techs, 0),
         numfemaletechs = ifelse(coach_gender_away == "F", numfemaletechs + away_total_techs, numfemaletechs),
         nummaletechs = ifelse(coach_gender_home == "M", home_total_techs, 0),
         nummaletechs = ifelse(coach_gender_away == "M", nummaletechs + away_total_techs, nummaletechs),
         numfemaletotalfouls = ifelse(coach_gender_home == "F",  home_total_fouls, 0),
         numfemaletotalfouls = ifelse(coach_gender_away == "F", numfemaletotalfouls + away_total_fouls, numfemaletotalfouls),
         nummaletotalfouls = ifelse(coach_gender_home == "M", home_total_fouls, 0),
         nummaletotalfouls = ifelse(coach_gender_away == "M", nummaletotalfouls + away_total_fouls, nummaletotalfouls))

write.csv(allgames6, "games_and_coaches_for_stats.csv")
