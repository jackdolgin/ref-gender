library("tidyr")
library("dplyr")
library("tools")
library("stringr")
library("tools")
library("parallel")
library("magrittr")

lowyear <- 1940
highyear <- 2019
file_list <- list.files("usnames") #list of the files of us names from each year
  
years_combine <- function(file){
  fileyear <- as.numeric(substr(file, 4, 7))
  if (fileyear > lowyear & fileyear < highyear){
      read.table(file.path("usnames", file), sep=",", col.names = c("first_name", "gender", "first_name_popularity"))
    }
  }

usnames <- do.call(rbind, mclapply(file_list, years_combine)) %>%
  group_by(first_name, gender) %>%
  summarise(Tot_count = sum(first_name_popularity)) %>% # combines popularity of the diff names from all of the diff years; so one name only has one row (at least for each gender it's associated with)
  arrange(first_name, Tot_count) %>%
  mutate(both_gender = duplicated(first_name)) %>%
  arrange(first_name, desc(Tot_count)) %>%
  mutate(gender_comparison = lead (Tot_count))

usnames <- usnames[!duplicated(usnames$first_name),] #this and line above combine eliminate the less-popular gender version of each name (i.e. the Aaron for females is eliminated)
usnames %<>% filter(Tot_count > 4*gender_comparison | (both_gender == FALSE)) #reduces name from 97,310 entries to 68,638 entries (92,452 if you include names shared by less than 10 ppl) (first half of the filter is saying, how many more names associated with more popular gender do we need to count the name as significantly more associated with one gender vs another; second half is saying, if there's only one gender associated with a name, how many name appearances at minimum are needed for us to keep the name in the dataset; like if a name is only listed as five people having it, then is that enough data to assume a ref with that name will also be of that gender?)
write.csv(usnames, "usnames.csv")
