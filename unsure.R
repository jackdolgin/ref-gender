ref_data_scraped <- read.csv("tommy2.csv")
ref_data_scraped2 <- ref_data_scraped %>%
  mutate(X7 = toTitleCase(tolower(X7)),
         X8 = toTitleCase(tolower(X8)),
         X9 = toTitleCase(tolower(X9)),
         ref1male = (X7 %in%usnamesmale[,1]),
         ref1female = (X7 %in%usnamesfemale[,1]),
         ref2male = (X8 %in%usnamesmale[,1]),
         ref2female = (X8 %in%usnamesfemale[,1]),
         ref3male = (X9 %in%usnamesmale[,1]),
         ref3female = (X9 %in%usnamesfemale[,1]),
         ref1gender = ifelse(ref1male, "Male", ifelse(ref1female, "Female", "Neither")),
         ref2gender = ifelse(ref2male, "Male", ifelse(ref2female, "Female", "Neither")),
         ref3gender = ifelse(ref3male, "Male", ifelse(ref3female, "Female", "Neither"))) %>%
  select(-ref1male, -ref1female, -ref2male, -ref2female, -ref3male, -ref3female) # %>%
# filter(X10 == 7,
#        ((nchar(X7) > 2) | (nchar(X8) > 2) | (nchar(X9) > 2)))
ref_data_scraped3 <- ref_data_scraped2      
ref_data_scraped3[,2] <- as.character(ref_data_scraped3[,2])
ref_data_scraped3[,3] <- as.character(ref_data_scraped3[,3])




ref_data_scraped4 <- ref_data_scraped3 %>%
  rowwise() %>%
  mutate(coach1 = coach_and_team[which(X1 == coach_and_team[,2]), 1],
         coach1gen = coach_and_team3[which(X1 == coach_and_team[,2]), 3],
         coach2 = coach_and_team[which(X2 == coach_and_team[,2]), 1],
         coach2gen = coach_and_team[which(X2 == coach_and_team[,2]), 3],
         Date = as.Date(strsplit(as.character(X3), " ")[[1]][1], "%m/%d/%Y"),
         Time = strsplit(as.character(X3), " ")[[1]][3],
         Time = strsplit(Time, "pm")[[1]][1],
         Time = strsplit(Time, "PM")[[1]][1],
         Time = strsplit(Time, "p.m.")[[1]][1],
         Time = strsplit(Time, "am")[[1]][1],
         Time = paste(Time, ":00", sep = ""),
         Time = paste(strsplit(Time, ":")[[1]][1], ":", strsplit(Time, ":")[[1]][2], sep = ""))