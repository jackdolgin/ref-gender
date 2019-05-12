library("tidyr")
library("dplyr")
library("tools")
library("stringr")
library("tools")

togroupby <- c("ref_1_first_name", "ref_1_last_name", "ref_2_first_name", "ref_2_last_name", "ref_3_first_name", "ref_3_last_name", "ref_post_aux_1", "ref_post_aux_2", "ref_post_aux_3", "ref_post_aux_4", "ref_post_aux_5")
togroupby2 <- c("ref_1_first_name2", "ref_1_last_name2", "ref_2_first_name2", "ref_2_last_name2", "ref_3_first_name2", "ref_3_last_name2", "ref_post_aux_12", "ref_post_aux_22", "ref_post_aux_32", "ref_post_aux_42", "ref_post_aux_52")

allgames <- read.csv("tommy2_in_progress_march_5.csv", strip.white=TRUE)

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
  mutate(
    
    ref_1_first_name = ifelse(grepl("Jr\\.?", ref_1_last_name), paste(ref_1_first_name, "Jr."), ref_1_first_name),
    ref_1_last_name = ifelse(grepl("Jr\\.?", ref_2_first_name), paste(ref_1_last_name, "Jr."), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("Jr\\.?", ref_2_last_name), paste(ref_2_first_name, "Jr."), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("Jr\\.?", ref_3_first_name), paste(ref_2_last_name, "Jr."), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("Jr\\.?", ref_3_last_name), paste(ref_3_first_name, "Jr."), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("Jr\\.?", ref_post_aux_1), paste(ref_3_last_name, "Jr."), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("Jr\\.?", ref_post_aux_2), paste(ref_post_aux_1, "Jr."), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("Jr\\.?", ref_post_aux_3), paste(ref_post_aux_2, "Jr."), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("Jr\\.?", ref_post_aux_4), paste(ref_post_aux_3, "Jr."), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("Jr\\.?", ref_post_aux_5), paste(ref_post_aux_4, "Jr."), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("Sr\\.?", ref_1_last_name), paste(ref_1_first_name, "Sr."), ref_1_first_name),
    ref_1_last_name = ifelse(grepl("Sr\\.?", ref_2_first_name), paste(ref_1_last_name, "Sr."), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("Sr\\.?", ref_2_last_name), paste(ref_2_first_name, "Sr."), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("Sr\\.?", ref_3_first_name), paste(ref_2_last_name, "Sr."), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("Sr\\.?", ref_3_last_name), paste(ref_3_first_name, "Sr."), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("Sr\\.?", ref_post_aux_1), paste(ref_3_last_name, "Sr."), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("Sr\\.?", ref_post_aux_2), paste(ref_post_aux_1, "Sr."), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("Sr\\.?", ref_post_aux_3), paste(ref_post_aux_2, "Sr."), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("Sr\\.?", ref_post_aux_4), paste(ref_post_aux_3, "Sr."), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("Sr\\.?", ref_post_aux_5), paste(ref_post_aux_4, "Sr."), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^II$", ref_1_last_name), paste(ref_1_first_name, "II"), ref_1_first_name),
    ref_1_last_name = ifelse(grepl("^II$", ref_2_first_name), paste(ref_1_last_name, "II"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^II$", ref_2_last_name), paste(ref_2_first_name, "II"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^II$", ref_3_first_name), paste(ref_2_last_name, "II"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^II$", ref_3_last_name), paste(ref_3_first_name, "II"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^II$", ref_post_aux_1), paste(ref_3_last_name, "II"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^II$", ref_post_aux_2), paste(ref_post_aux_1, "II"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^II$", ref_post_aux_3), paste(ref_post_aux_2, "II"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^II$", ref_post_aux_4), paste(ref_post_aux_3, "II"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^II$", ref_post_aux_5), paste(ref_post_aux_4, "II"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^III$", ref_1_last_name), paste(ref_1_first_name, "III"), ref_1_first_name),
    ref_1_last_name = ifelse(grepl("^III$", ref_2_first_name), paste(ref_1_last_name, "III"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^III$", ref_2_last_name), paste(ref_2_first_name, "III"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^III$", ref_3_first_name), paste(ref_2_last_name, "III"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^III$", ref_3_last_name), paste(ref_3_first_name, "III"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^III$", ref_post_aux_1), paste(ref_3_last_name, "III"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^III$", ref_post_aux_2), paste(ref_post_aux_1, "III"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^III$", ref_post_aux_3), paste(ref_post_aux_2, "III"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^III$", ref_post_aux_4), paste(ref_post_aux_3, "III"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^III$", ref_post_aux_5), paste(ref_post_aux_4, "III"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^IV$", ref_1_last_name), paste(ref_1_first_name, "IV"), ref_1_first_name),
    ref_1_last_name = ifelse(grepl("^IV$", ref_2_first_name), paste(ref_1_last_name, "IV"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^IV$", ref_2_last_name), paste(ref_2_first_name, "IV"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^IV$", ref_3_first_name), paste(ref_2_last_name, "IV"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^IV$", ref_3_last_name), paste(ref_3_first_name, "IV"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^IV$", ref_post_aux_1), paste(ref_3_last_name, "IV"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^IV$", ref_post_aux_2), paste(ref_post_aux_1, "IV"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^IV$", ref_post_aux_3), paste(ref_post_aux_2, "IV"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^IV$", ref_post_aux_4), paste(ref_post_aux_3, "IV"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^IV$", ref_post_aux_5), paste(ref_post_aux_4, "IV"), ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^St\\.?$", ref_1_first_name), paste("St.", ref_1_last_name), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^St\\.?$", ref_1_last_name), paste("St.", ref_2_first_name), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^St\\.?$", ref_2_first_name), paste("St.", ref_2_last_name), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^St\\.?$", ref_2_last_name), paste("St.", ref_3_first_name), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^St\\.?$", ref_3_first_name), paste("St.", ref_3_last_name), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^St\\.?$", ref_3_last_name), paste("St.", ref_post_aux_1), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^St\\.?$", ref_post_aux_1), paste("St.", ref_post_aux_2), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^St\\.?$", ref_post_aux_2), paste("St.", ref_post_aux_3), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^St\\.?$", ref_post_aux_3), paste("St.", ref_post_aux_4), ref_post_aux_4),
    ref_post_aux_5 = ifelse(grepl("^St\\.?$", ref_post_aux_4), paste("St.", ref_post_aux_5), ref_post_aux_5),
    
    ref_1_last_name = ifelse(grepl("^La\\.?$", ref_1_first_name), paste("La", ref_1_last_name), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^La\\.?$", ref_1_last_name), paste("La", ref_2_first_name), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^La\\.?$", ref_2_first_name), paste("La", ref_2_last_name), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^La\\.?$", ref_2_last_name), paste("La", ref_3_first_name), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^La\\.?$", ref_3_first_name), paste("La", ref_3_last_name), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^La\\.?$", ref_3_last_name), paste("La", ref_post_aux_1), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^La\\.?$", ref_post_aux_1), paste("La", ref_post_aux_2), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^La\\.?$", ref_post_aux_2), paste("La", ref_post_aux_3), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^La\\.?$", ref_post_aux_3), paste("La", ref_post_aux_4), ref_post_aux_4),
    ref_post_aux_5 = ifelse(grepl("^La\\.?$", ref_post_aux_4), paste("La", ref_post_aux_5), ref_post_aux_5),
    
    ref_1_last_name = ifelse(grepl("^De\\.?$", ref_1_first_name), paste("De", ref_1_last_name), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^De\\.?$", ref_1_last_name), paste("De", ref_2_first_name), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^De\\.?$", ref_2_first_name), paste("De", ref_2_last_name), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^De\\.?$", ref_2_last_name), paste("De", ref_3_first_name), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^De\\.?$", ref_3_first_name), paste("De", ref_3_last_name), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^De\\.?$", ref_3_last_name), paste("De", ref_post_aux_1), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^De\\.?$", ref_post_aux_1), paste("De", ref_post_aux_2), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^De\\.?$", ref_post_aux_2), paste("De", ref_post_aux_3), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^De\\.?$", ref_post_aux_3), paste("De", ref_post_aux_4), ref_post_aux_4),
    ref_post_aux_5 = ifelse(grepl("^De\\.?$", ref_post_aux_4), paste("De", ref_post_aux_5), ref_post_aux_5),
    
    ref_1_last_name = ifelse(grepl("^(d|D)el\\.?$", ref_1_first_name), paste("Del", ref_1_last_name), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^(d|D)el\\.?$", ref_1_last_name), paste("Del", ref_2_first_name), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^(d|D)el\\.?$", ref_2_first_name), paste("Del", ref_2_last_name), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^(d|D)el\\.?$", ref_2_last_name), paste("Del", ref_3_first_name), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^(d|D)el\\.?$", ref_3_first_name), paste("Del", ref_3_last_name), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^(d|D)el\\.?$", ref_3_last_name), paste("Del", ref_post_aux_1), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^(d|D)el\\.?$", ref_post_aux_1), paste("Del", ref_post_aux_2), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^(d|D)el\\.?$", ref_post_aux_2), paste("Del", ref_post_aux_3), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^(d|D)el\\.?$", ref_post_aux_3), paste("Del", ref_post_aux_4), ref_post_aux_4),
    ref_post_aux_5 = ifelse(grepl("^(d|D)el\\.?$", ref_post_aux_4), paste("Del", ref_post_aux_5), ref_post_aux_5),
    
    ref_1_last_name = ifelse(grepl("^Van\\.?$", ref_1_first_name), paste("Van", ref_1_last_name), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Van\\.?$", ref_1_last_name), paste("Van", ref_2_first_name), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Van\\.?$", ref_2_first_name), paste("Van", ref_2_last_name), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Van\\.?$", ref_2_last_name), paste("Van", ref_3_first_name), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Van\\.?$", ref_3_first_name), paste("Van", ref_3_last_name), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Van\\.?$", ref_3_last_name), paste("Van", ref_post_aux_1), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Van\\.?$", ref_post_aux_1), paste("Van", ref_post_aux_2), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Van\\.?$", ref_post_aux_2), paste("Van", ref_post_aux_3), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Van\\.?$", ref_post_aux_3), paste("Van", ref_post_aux_4), ref_post_aux_4),
    ref_post_aux_5 = ifelse(grepl("^Van\\.?$", ref_post_aux_4), paste("Van", ref_post_aux_5), ref_post_aux_5),
    
    ref_1_last_name = ifelse(grepl("^Do\\.?$", ref_1_first_name), paste0("Do", ref_1_last_name), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Do\\.?$", ref_1_last_name), paste0("Do", ref_2_first_name), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Do\\.?$", ref_2_first_name), paste0("Do", ref_2_last_name), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Do\\.?$", ref_2_last_name), paste0("Do", ref_3_first_name), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Do\\.?$", ref_3_first_name), paste0("Do", ref_3_last_name), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Do\\.?$", ref_3_last_name), paste0("Do", ref_post_aux_1), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Do\\.?$", ref_post_aux_1), paste0("Do", ref_post_aux_2), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Do\\.?$", ref_post_aux_2), paste0("Do", ref_post_aux_3), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Do\\.?$", ref_post_aux_3), paste0("Do", ref_post_aux_4), ref_post_aux_4),
    ref_post_aux_5 = ifelse(grepl("^Do\\.?$", ref_post_aux_4), paste0("Do", ref_post_aux_5), ref_post_aux_5),
    
    ref_1_last_name = ifelse(grepl("^Banal$", ref_1_first_name) & grepl("^Hoyt$", ref_1_last_name), paste(ref_1_first_name, ref_1_last_name, sep = "-"), ref_1_last_name),
    ref_2_first_name =ifelse(grepl("^Banal$", ref_1_last_name) & grepl("^Hoyt$", ref_2_first_name), paste(ref_1_last_name, ref_2_first_name, sep = "-"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Banal$", ref_2_first_name) & grepl("^Hoyt$", ref_2_last_name), paste(ref_2_first_name, ref_2_last_name, sep = "-"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Banal$", ref_2_last_name) & grepl("^Hoyt$", ref_3_first_name), paste(ref_2_last_name, ref_3_first_name, sep = "-"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Banal$", ref_3_first_name) & grepl("^Hoyt$", ref_3_last_name), paste(ref_3_first_name, ref_3_last_name, sep = "-"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Banal$", ref_3_last_name) & grepl("^Hoyt$", ref_post_aux_1), paste(ref_3_last_name, ref_post_aux_1, sep = "-"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Banal$", ref_post_aux_1) & grepl("^Hoyt$", ref_post_aux_2), paste(ref_post_aux_1, ref_post_aux_2, sep = "-"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Banal$", ref_post_aux_2) & grepl("^Hoyt$", ref_post_aux_3), paste(ref_post_aux_2, ref_post_aux_3, sep = "-"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Banal$", ref_post_aux_3) & grepl("^Hoyt$", ref_post_aux_4), paste(ref_post_aux_3, ref_post_aux_4, sep = "-"), ref_post_aux_4),
    ref_post_aux_5 = ifelse(grepl("^Banal$", ref_post_aux_4) & grepl("^Hoyt$", ref_1_last_name), paste(ref_post_aux_4, ref_1_last_name, sep = "-"), ref_post_aux_5),
    
    ref_1_first_name = ifelse(grepl("^Kay$", ref_1_first_name) & grepl("^Bradley$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Kay$", ref_1_last_name) & grepl("^Bradley$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Kay$", ref_2_first_name) & grepl("^Bradley$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Kay$", ref_2_last_name) & grepl("^Bradley$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Kay$", ref_3_first_name) & grepl("^Bradley$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Kay$", ref_3_last_name) & grepl("^Bradley$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Kay$", ref_post_aux_1) & grepl("^Bradley$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Kay$", ref_post_aux_2) & grepl("^Bradley$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Kay$", ref_post_aux_3) & grepl("^Bradley$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Kay$", ref_post_aux_4) & grepl("^Bradley$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Ellen$", ref_1_first_name) & grepl("^Roach$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Ellen$", ref_1_last_name) & grepl("^Roach$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Ellen$", ref_2_first_name) & grepl("^Roach$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Ellen$", ref_2_last_name) & grepl("^Roach$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Ellen$", ref_3_first_name) & grepl("^Roach$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Ellen$", ref_3_last_name) & grepl("^Roach$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Ellen$", ref_post_aux_1) & grepl("^Roach$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Ellen$", ref_post_aux_2) & grepl("^Roach$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Ellen$", ref_post_aux_3) & grepl("^Roach$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Ellen$", ref_post_aux_4) & grepl("^Roach$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Deutsch$", ref_1_first_name) & grepl("^East$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Deutsch$", ref_1_last_name) & grepl("^East$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Deutsch$", ref_2_first_name) & grepl("^East$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Deutsch$", ref_2_last_name) & grepl("^East$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Deutsch$", ref_3_first_name) & grepl("^East$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Deutsch$", ref_3_last_name) & grepl("^East$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Deutsch$", ref_post_aux_1) & grepl("^East$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Deutsch$", ref_post_aux_2) & grepl("^East$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Deutsch$", ref_post_aux_3) & grepl("^East$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Ann$", ref_1_first_name) & grepl("^Garcia$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Ann$", ref_1_last_name) & grepl("^Garcia$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Ann$", ref_2_first_name) & grepl("^Garcia$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Ann$", ref_2_last_name) & grepl("^Garcia$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Ann$", ref_3_first_name) & grepl("^Garcia$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Ann$", ref_3_last_name) & grepl("^Garcia$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Ann$", ref_post_aux_1) & grepl("^Garcia$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Ann$", ref_post_aux_2) & grepl("^Garcia$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Ann$", ref_post_aux_3) & grepl("^Garcia$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Ann$", ref_post_aux_4) & grepl("^Garcia$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Allen$", ref_1_first_name) & grepl("^Brown$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Allen$", ref_1_last_name) & grepl("^Brown$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Allen$", ref_2_first_name) & grepl("^Brown$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Allen$", ref_2_last_name) & grepl("^Brown$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Allen$", ref_3_first_name) & grepl("^Brown$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Allen$", ref_3_last_name) & grepl("^Brown$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Allen$", ref_post_aux_1) & grepl("^Brown$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Allen$", ref_post_aux_2) & grepl("^Brown$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Allen$", ref_post_aux_3) & grepl("^Brown$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Allen$", ref_post_aux_4) & grepl("^Brown$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^James$", ref_1_first_name) & grepl("^Clarke?$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^James$", ref_1_last_name) & grepl("^Clarke?$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^James$", ref_2_first_name) & grepl("^Clarke?$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^James$", ref_2_last_name) & grepl("^Clarke?$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^James$", ref_3_first_name) & grepl("^Clarke?$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^James$", ref_3_last_name) & grepl("^Clarke?$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^James$", ref_post_aux_1) & grepl("^Clarke?$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^James$", ref_post_aux_2) & grepl("^Clarke?$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^James$", ref_post_aux_3) & grepl("^Clarke?$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^James$", ref_post_aux_4) & grepl("^Clarke?$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    
    ref_1_first_name = ifelse(grepl("^Watt$", ref_1_first_name) & grepl("^Cruse$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Watt$", ref_1_last_name) & grepl("^Cruse$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Watt$", ref_2_first_name) & grepl("^Cruse$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Watt$", ref_2_last_name) & grepl("^Cruse$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Watt$", ref_3_first_name) & grepl("^Cruse$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Watt$", ref_3_last_name) & grepl("^Cruse$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Watt$", ref_post_aux_1) & grepl("^Cruse$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Watt$", ref_post_aux_2) & grepl("^Cruse$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Watt$", ref_post_aux_3) & grepl("^Cruse$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Watt$", ref_post_aux_4) & grepl("^Cruse$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Jo$", ref_1_first_name) & grepl("^Smith$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Jo$", ref_1_last_name) & grepl("^Smith$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Jo$", ref_2_first_name) & grepl("^Smith$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Jo$", ref_2_last_name) & grepl("^Smith$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Jo$", ref_3_first_name) & grepl("^Smith$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Jo$", ref_3_last_name) & grepl("^Smith$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Jo$", ref_post_aux_1) & grepl("^Smith$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Jo$", ref_post_aux_2) & grepl("^Smith$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Jo$", ref_post_aux_3) & grepl("^Smith$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Jo$", ref_post_aux_4) & grepl("^Smith$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Dean$", ref_1_first_name) & grepl("^Ledington$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Dean$", ref_1_last_name) & grepl("^Ledington$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Dean$", ref_2_first_name) & grepl("^Ledington$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Dean$", ref_2_last_name) & grepl("^Ledington$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Dean$", ref_3_first_name) & grepl("^Ledington$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Dean$", ref_3_last_name) & grepl("^Ledington$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Dean$", ref_post_aux_1) & grepl("^Ledington$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Dean$", ref_post_aux_2) & grepl("^Ledington$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Dean$", ref_post_aux_3) & grepl("^Ledington$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Dean$", ref_post_aux_4) & grepl("^Ledington$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Lajwan$", ref_1_first_name) & grepl("^Ren(n|e)e$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Lajwan$", ref_1_last_name) & grepl("^Ren(n|e)e$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Lajwan$", ref_2_first_name) & grepl("^Ren(n|e)e$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Lajwan$", ref_2_last_name) & grepl("^Ren(n|e)e$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Lajwan$", ref_3_first_name) & grepl("^Ren(n|e)e$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Lajwan$", ref_3_last_name) & grepl("^Ren(n|e)e$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Lajwan$", ref_post_aux_1) & grepl("^Ren(n|e)e$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Lajwan$", ref_post_aux_2) & grepl("^Ren(n|e)e$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Lajwan$", ref_post_aux_3) & grepl("^Ren(n|e)e$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Lajwan$", ref_post_aux_4) & grepl("^Ren(n|e)e$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Lajwan$", ref_1_first_name) & grepl("^Robinson$", ref_1_last_name), "Renee", ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Lajwan$", ref_1_last_name) & grepl("^Robinson$", ref_2_first_name), "Renee", ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Lajwan$", ref_2_first_name) & grepl("^Robinson$", ref_2_last_name), "Renee", ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Lajwan$", ref_2_last_name) & grepl("^Robinson$", ref_3_first_name), "Renee", ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Lajwan$", ref_3_first_name) & grepl("^Robinson$", ref_3_last_name), "Renee", ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Lajwan$", ref_3_last_name) & grepl("^Robinson$", ref_post_aux_1), "Renee", ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Lajwan$", ref_post_aux_1) & grepl("^Robinson$", ref_post_aux_2), "Renee", ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Lajwan$", ref_post_aux_2) & grepl("^Robinson$", ref_post_aux_3), "Renee", ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Lajwan$", ref_post_aux_3) & grepl("^Robinson$", ref_post_aux_4), "Renee", ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Lajwan$", ref_post_aux_4) & grepl("^Robinson$", ref_1_last_name), "Renee", ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Michelle$", ref_1_first_name) & grepl("^Shelley$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Michelle$", ref_1_last_name) & grepl("^Shelley$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Michelle$", ref_2_first_name) & grepl("^Shelley$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Michelle$", ref_2_last_name) & grepl("^Shelley$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Michelle$", ref_3_first_name) & grepl("^Shelley$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Michelle$", ref_3_last_name) & grepl("^Shelley$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Michelle$", ref_post_aux_1) & grepl("^Shelley$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Michelle$", ref_post_aux_2) & grepl("^Shelley$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Michelle$", ref_post_aux_3) & grepl("^Shelley$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Shelley$", ref_1_first_name) & grepl("^Russi$", ref_1_last_name), "Michelle", ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Shelley$", ref_1_last_name) & grepl("^Russi$", ref_2_first_name), "Michelle", ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Shelley$", ref_2_first_name) & grepl("^Russi$", ref_2_last_name), "Michelle", ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Shelley$", ref_2_last_name) & grepl("^Russi$", ref_3_first_name), "Michelle", ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Shelley$", ref_3_first_name) & grepl("^Russi$", ref_3_last_name), "Michelle", ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Shelley$", ref_3_last_name) & grepl("^Russi$", ref_post_aux_1), "Michelle", ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Shelley$", ref_post_aux_1) & grepl("^Russi$", ref_post_aux_2), "Michelle", ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Shelley$", ref_post_aux_2) & grepl("^Russi$", ref_post_aux_3), "Michelle", ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Shelley$", ref_post_aux_3) & grepl("^Russi$", ref_post_aux_4), "Michelle", ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Shelley$", ref_post_aux_4) & grepl("^Russi$", ref_1_last_name), "Michelle", ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Jamie$", ref_1_first_name) & grepl("^Coleen$", ref_1_last_name), paste0(ref_1_first_name, "junk"), ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Jamie$", ref_1_last_name) & grepl("^Coleen$", ref_2_first_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Jamie$", ref_2_first_name) & grepl("^Coleen$", ref_2_last_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Jamie$", ref_2_last_name) & grepl("^Coleen$", ref_3_first_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Jamie$", ref_3_first_name) & grepl("^Coleen$", ref_3_last_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Jamie$", ref_3_last_name) & grepl("^Coleen$", ref_post_aux_1), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Jamie$", ref_post_aux_1) & grepl("^Coleen$", ref_post_aux_2), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Jamie$", ref_post_aux_2) & grepl("^Coleen$", ref_post_aux_3), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Jamie$", ref_post_aux_3) & grepl("^Coleen$", ref_post_aux_4), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Jamie$", ref_post_aux_4) & grepl("^Coleen$", ref_1_last_name), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Jamie$", ref_1_first_name) & grepl("^Broderick$", ref_1_last_name), "Coleen", ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Jamie$", ref_1_last_name) & grepl("^Broderick$", ref_2_first_name), "Coleen", ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Jamie$", ref_2_first_name) & grepl("^Broderick$", ref_2_last_name), "Coleen", ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Jamie$", ref_2_last_name) & grepl("^Broderick$", ref_3_first_name), "Coleen", ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Jamie$", ref_3_first_name) & grepl("^Broderick$", ref_3_last_name), "Coleen", ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Jamie$", ref_3_last_name) & grepl("^Broderick$", ref_post_aux_1), "Coleen", ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Jamie$", ref_post_aux_1) & grepl("^Broderick$", ref_post_aux_2), "Coleen", ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Jamie$", ref_post_aux_2) & grepl("^Broderick$", ref_post_aux_3), "Coleen", ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Jamie$", ref_post_aux_3) & grepl("^Broderick$", ref_post_aux_4), "Coleen", ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Jamie$", ref_post_aux_4) & grepl("^Broderick$", ref_1_last_name), "Coleen", ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Ethan$", ref_1_first_name) & grepl("^Tiki$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Ethan$", ref_1_last_name) & grepl("^Tiki$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Ethan$", ref_2_first_name) & grepl("^Tiki$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Ethan$", ref_2_last_name) & grepl("^Tiki$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Ethan$", ref_3_first_name) & grepl("^Tiki$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Ethan$", ref_3_last_name) & grepl("^Tiki$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Ethan$", ref_post_aux_1) & grepl("^Tiki$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Ethan$", ref_post_aux_2) & grepl("^Tiki$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Ethan$", ref_post_aux_3) & grepl("^Tiki$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^Tiki$", ref_1_first_name) & grepl("^Kahoano$", ref_1_last_name), "Ethan", ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^Tiki$", ref_1_last_name) & grepl("^Kahoano$", ref_2_first_name), "Ethan", ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Tiki$", ref_2_first_name) & grepl("^Kahoano$", ref_2_last_name), "Ethan", ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Tiki$", ref_2_last_name) & grepl("^Kahoano$", ref_3_first_name), "Ethan", ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Tiki$", ref_3_first_name) & grepl("^Kahoano$", ref_3_last_name), "Ethan", ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Tiki$", ref_3_last_name) & grepl("^Kahoano$", ref_post_aux_1), "Ethan", ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Tiki$", ref_post_aux_1) & grepl("^Kahoano$", ref_post_aux_2), "Ethan", ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Tiki$", ref_post_aux_2) & grepl("^Kahoano$", ref_post_aux_3), "Ethan", ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Tiki$", ref_post_aux_3) & grepl("^Kahoano$", ref_post_aux_4), "Ethan", ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Tiki$", ref_post_aux_4) & grepl("^Kahoano$", ref_1_last_name), "Ethan", ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Harding$", ref_1_first_name) & grepl("^JR$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Harding$", ref_1_last_name) & grepl("^JR$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Harding$", ref_2_first_name) & grepl("^JR$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Harding$", ref_2_last_name) & grepl("^JR$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Harding$", ref_3_first_name) & grepl("^JR$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Harding$", ref_3_last_name) & grepl("^JR$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Harding$", ref_post_aux_1) & grepl("^JR$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Harding$", ref_post_aux_2) & grepl("^JR$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Harding$", ref_post_aux_3) & grepl("^JR$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Murray$", ref_1_first_name) & grepl("^Broomfield$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Murray$", ref_1_last_name) & grepl("^Broomfield$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Murray$", ref_2_first_name) & grepl("^Broomfield$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Murray$", ref_2_last_name) & grepl("^Broomfield$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Murray$", ref_3_first_name) & grepl("^Broomfield$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Murray$", ref_3_last_name) & grepl("^Broomfield$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Murray$", ref_post_aux_1) & grepl("^Broomfield$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Murray$", ref_post_aux_2) & grepl("^Broomfield$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Murray$", ref_post_aux_3) & grepl("^Broomfield$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Laura$", ref_1_first_name) & grepl("^Lynne?$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Laura$", ref_1_last_name) & grepl("^Lynne?$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Laura$", ref_2_first_name) & grepl("^Lynne?$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Laura$", ref_2_last_name) & grepl("^Lynne?$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Laura$", ref_3_first_name) & grepl("^Lynne?$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Laura$", ref_3_last_name) & grepl("^Lynne?$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Laura$", ref_post_aux_1) & grepl("^Lynne?$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Laura$", ref_post_aux_2) & grepl("^Lynne?$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Laura$", ref_post_aux_3) & grepl("^Lynne?$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Cisso(k|c)o$", ref_1_first_name) & grepl("^Ste((ph)|v)ens$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Cisso(k|c)o$", ref_1_last_name) & grepl("^Ste((ph)|v)ens$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Cisso(k|c)o$", ref_2_first_name) & grepl("^Ste((ph)|v)ens$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Cisso(k|c)o$", ref_2_last_name) & grepl("^Ste((ph)|v)ens$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Cisso(k|c)o$", ref_3_first_name) & grepl("^Ste((ph)|v)ens$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Cisso(k|c)o$", ref_3_last_name) & grepl("^Ste((ph)|v)ens$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Cisso(k|c)o$", ref_post_aux_1) & grepl("^Ste((ph)|v)ens$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Cisso(k|c)o$", ref_post_aux_2) & grepl("^Ste((ph)|v)ens$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Cisso(k|c)o$", ref_post_aux_3) & grepl("^Ste((ph)|v)ens$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Benito$", ref_1_first_name) & grepl("^Benny$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Benito$", ref_1_last_name) & grepl("^Benny$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Benito$", ref_2_first_name) & grepl("^Benny$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Benito$", ref_2_last_name) & grepl("^Benny$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Benito$", ref_3_first_name) & grepl("^Benny$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Benito$", ref_3_last_name) & grepl("^Benny$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Benito$", ref_post_aux_1) & grepl("^Benny$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Benito$", ref_post_aux_2) & grepl("^Benny$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Benito$", ref_post_aux_3) & grepl("^Benny$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^Shannon$", ref_1_first_name) & grepl("^Linette$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^Shannon$", ref_1_last_name) & grepl("^Linette$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^Shannon$", ref_2_first_name) & grepl("^Linette$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^Shannon$", ref_2_last_name) & grepl("^Linette$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^Shannon$", ref_3_first_name) & grepl("^Linette$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^Shannon$", ref_3_last_name) & grepl("^Linette$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^Shannon$", ref_post_aux_1) & grepl("^Linette$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^Shannon$", ref_post_aux_2) & grepl("^Linette$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^Shannon$", ref_post_aux_3) & grepl("^Linette$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_last_name = ifelse(grepl("^James$", ref_1_first_name) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_1_last_name), paste0(ref_1_last_name, "junk"), ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^James$", ref_1_last_name) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_2_first_name), paste0(ref_2_first_name, "junk"), ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^James$", ref_2_first_name) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_2_last_name), paste0(ref_2_last_name, "junk"), ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^James$", ref_2_last_name) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_3_first_name), paste0(ref_3_first_name, "junk"), ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^James$", ref_3_first_name) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_3_last_name), paste0(ref_3_last_name, "junk"), ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^James$", ref_3_last_name) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_post_aux_1), paste0(ref_post_aux_1, "junk"), ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^James$", ref_post_aux_1) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_post_aux_2), paste0(ref_post_aux_2, "junk"), ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^James$", ref_post_aux_2) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_post_aux_3), paste0(ref_post_aux_3, "junk"), ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^James$", ref_post_aux_3) & grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_post_aux_4), paste0(ref_post_aux_4, "junk"), ref_post_aux_4),
    
    ref_1_first_name = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_1_first_name) & grepl("^Die?t?s?ch(e|u)r$", ref_1_last_name), "James", ref_1_first_name),
    ref_1_last_name =ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_1_last_name) & grepl("^Die?t?s?ch(e|u)r$", ref_2_first_name), "James", ref_1_last_name),
    ref_2_first_name = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_2_first_name) & grepl("^Die?t?s?ch(e|u)r$", ref_2_last_name), "James", ref_2_first_name),
    ref_2_last_name = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_2_last_name) & grepl("^Die?t?s?ch(e|u)r$", ref_3_first_name), "James", ref_2_last_name),
    ref_3_first_name = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_3_first_name) & grepl("^Die?t?s?ch(e|u)r$", ref_3_last_name), "James", ref_3_first_name),
    ref_3_last_name = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_3_last_name) & grepl("^Die?t?s?ch(e|u)r$", ref_post_aux_1), "James", ref_3_last_name),
    ref_post_aux_1 = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_post_aux_1) & grepl("^Die?t?s?ch(e|u)r$", ref_post_aux_2), "James", ref_post_aux_1),
    ref_post_aux_2 = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_post_aux_2) & grepl("^Die?t?s?ch(e|u)r$", ref_post_aux_3), "James", ref_post_aux_2),
    ref_post_aux_3 = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_post_aux_3) & grepl("^Die?t?s?ch(e|u)r$", ref_post_aux_4), "James", ref_post_aux_3),
    ref_post_aux_4 = ifelse(grepl("^[[:punct:]]?Buts?ch[[:punct:]]?$", ref_post_aux_4) & grepl("^Die?t?s?ch(e|u)r$", ref_1_last_name), "James", ref_post_aux_4)
    
    
    
    
    
    # ,
    # 
    # ref_1_last_name = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_1_first_name), paste(ref_1_first_name, ref_1_last_name), ref_1_last_name),
    # ref_2_first_name = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_1_last_name), paste(ref_1_last_name, ref_2_first_name), ref_2_first_name),
    # ref_2_last_name = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_2_first_name), paste(ref_2_first_name, ref_2_last_name), ref_2_last_name),
    # ref_3_first_name = ifelse(grepl("\\b[A-z]{2,3}", ref_2_last_name), paste(ref_2_last_name, ref_3_first_name), ref_3_first_name),
    # ref_3_last_name = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_3_first_name), paste(ref_3_first_name, ref_3_last_name), ref_3_last_name),
    # ref_post_aux_1 = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_3_last_name), paste(ref_3_last_name, ref_post_aux_1), ref_post_aux_1),
    # ref_post_aux_2 = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_post_aux_1), paste(ref_post_aux_1, ref_post_aux_2), ref_post_aux_2),
    # ref_post_aux_3 = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_post_aux_2), paste(ref_post_aux_2, ref_post_aux_3), ref_post_aux_3),
    # ref_post_aux_4 = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_post_aux_3), paste(ref_post_aux_3, ref_post_aux_4), ref_post_aux_4),
    # ref_post_aux_5 = ifelse(grepl("\\b[A-z]{2,3}\\b", ref_post_aux_4), paste(ref_post_aux_4, ref_post_aux_5), ref_post_aux_5)
    
  ) %>%
  mutate_at(vars(!!togroupby), funs(gsub("^(|[[:punct:]]?Ify[[:punct:]]?|[A-z]?|(Ellen|Kay|East|Ann|Allen|James|Watt|Jo|Dean|Lajwan|Shelley|Jamie|Tiki|JR|Broomfield|Lynne?|Ste((ph)|v)ens|Benito|Linette|[[:punct:]]?Buts?ch[[:punct:]]?)junk|Banal|La|Do|(d|D)el?|Van|Jr|Sr|St|III?)\\.?$", "", .)))

#converts blank cells to NA's
allgames2[,togroupby][allgames2[,togroupby] == ""] <- NA      

allgamessample <- allgames2[,22:28]
allgamessample[] <- t(apply(allgamessample, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))) 

allgames3 <- cbind(allgames2[,1:21], allgamessample, allgames2[,29:39])

allgames3 <- allgames3 %>% filter(!(is.na(ref_1_first_name) | is.na(ref_3_last_name)))


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

