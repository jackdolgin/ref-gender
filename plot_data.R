library("dplyr")
library("tidyr")
library("ggplot2")
library("pracma")
library("scales")
library("ggthemes")
library("viridis")
library("gifski")
library("gganimate")





my_df <- data.frame(matrix(fread('games_and_coaches_for_stats.csv') %>%
                             mutate(anyfemaletech = ifelse(numfemaletechs > 0, 1, 0),
                                    anymaletech = ifelse(nummaletechs > 0, 1, 0)) %>%
                             summarise_at(vars(numfemalecoaches, nummalecoaches, anyfemaletech, anymaletech), list(~sum(.))),
                           ncol = 2, byrow = TRUE, dimnames = list(c(), c("Female","Male")))) %>%
  mutate(game_type = ifelse(row_number() == 1, "All", "Technical Foul Occurring")) %>%
  gather("Coach Gender", "Total", Female, Male) %>%
  mutate_at(vars(Total), as.numeric) %>%
  arrange(desc(`Coach Gender`)) %>%
  mutate(forfacet = c(1, 2, 4, 3),
         y_max = ifelse(game_type == "All", 60000, 3000))


some_df <- as.data.frame(t(do.call(rbind, lapply(unique(my_df$Total), function(x) seq(0, x, 1000)))))
colnames(some_df) <- c("A", "B", "D", "C")
some_df <- some_df %>%
  mutate_at(vars(A), list(~ifelse(row_number() > which(some_df$A == max(some_df$A)), max(some_df$A), .))) %>%
  mutate_at(vars(A), list(~ifelse(. == max(.), my_df$Total[1], .))) %>%
  mutate_at(vars(B), list(~ifelse(. == max(.), my_df$Total[2], .))) %>%
  mutate_at(vars(D), list(~ifelse(. == max(.), my_df$Total[3], .))) %>%
  mutate_at(vars(C), list(~ifelse(. == max(.), my_df$Total[4], .))) %>%
  mutate(time = row_number())%>%
  gather("Letter", "Total", A, B, C, D) %>%
  mutate(`Coach Gender` = ifelse(Letter == "A" | Letter == "B", "Male", "Female"),
         game_type = ifelse(Letter == "A" | Letter == "D", "All", "Technical Foul Occuring"),
         y_max = ifelse(game_type == "All", 60000, 3000))


my_animation <- ggplot(data = some_df, aes(x = `Coach Gender`, y = Total, fill = game_type)) +
  theme_wsj() +
  geom_bar(stat = "identity") +
  labs(x=NULL, y=NULL, fill=NULL, title="{closest_state}") +
  transition_states(time, transition_length = 2, state_length = 1) +
  geom_text(aes(label=comma_format()(Total)), position=position_dodge(width=0.9), vjust=-0.25, fontface = 'bold' ) +
  facet_wrap(~ Letter, ncol = 4, scales = "free") +
  scale_fill_viridis_d(option = "D", end = .7) +
  geom_blank(aes(y = y_max)) +
  theme(strip.text.x = element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.spacing.x = unit(.5, 'cm')) +
  labs(title = "Number of Games Coached by Each Gender,\n as a Function of Technical Fouls\n",
       fill = "Type of Game   ",
       caption = "\nMale coaches received technical fouls in slightly more games,\neven though they coached in almost 20,000 fewer games.")

animation_done <- animate(my_animation, renderer = gifski_renderer(loop = FALSE), height = 600, width =900, nframes = 110)

anim_save("gender_coaches.gif", animation = last_animation())








data.frame(matrix(fread('games_and_coaches_for_stats.csv') %>%
                    mutate(anyfemaletech = ifelse(numfemaletechs > 0, 1, 0),
                           anymaletech = ifelse(nummaletechs > 0, 1, 0)) %>%
                    summarise_at(vars(numfemalecoaches, nummalecoaches, anyfemaletech, anymaletech), list(~sum(.))),
                  ncol = 2, byrow = TRUE, dimnames = list(c(), c("Female","Male")))) %>%
  mutate(game_type = ifelse(row_number() == 1, "All", "Technical Foul Occurring")) %>%
  gather("Coach Gender", "Total", Female, Male) %>%
  mutate_at(vars(Total), as.numeric) %>%
  arrange(desc(`Coach Gender`)) %>%
  mutate(forfacet = c(1, 2, 4, 3),
         y_max = ifelse(game_type == "All", 60000, 3000)) %>%
  ggplot(aes(x = `Coach Gender`, y = Total, fill = game_type)) +
  theme_wsj() +
  geom_bar(stat = "identity") +
  geom_text(aes(label=comma_format()(Total)), position=position_dodge(width=0.9), vjust=-0.25, fontface = 'bold' ) +
  facet_wrap(~ forfacet, ncol = 4, scales = "free") +
  scale_fill_viridis_d(option = "D", end = .7) +
  geom_blank(aes(y = y_max)) +
  theme(strip.text.x = element_blank(),
              axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.spacing.x = unit(.5, 'cm')) +
  labs(title = "Number of Games Coached by Each Gender,\n as a Function of Technical Fouls\n",
       fill = "Type of Game   ",
       caption = "\nMale coaches received technical fouls in slightly more games,\neven though they coached in almost 20,000 fewer games.")


data.frame(matrix(fread('games_and_coaches_for_stats.csv') %>%
                    mutate(anyfemaletech = ifelse(numfemaletechs > 0, 1, 0),
                           anymaletech = ifelse(nummaletechs > 0, 1, 0)) %>%
                    summarise_at(vars(numfemalecoaches, nummalecoaches, anyfemaletech, anymaletech), list(~sum(.))),
                  ncol = 2, byrow = TRUE, dimnames = list(c(), c("Female","Male")))) %>%
  mutate(game_type = ifelse(row_number() == 1, "All", "Technical Foul Occurring")) %>%
  gather("Coach Gender", "Total", Female, Male) %>%
  mutate_at(vars(Total), as.numeric) %>%
  arrange(desc(`Coach Gender`)) %>%
  mutate(forfacet = c(1, 2, 4, 3),
         y_max = ifelse(game_type == "All", 60000, 3000)) %>%
  ggplot(aes(x = `Coach Gender`, y = Total, fill = game_type)) +
  theme_wsj() +z
  geom_bar(stat = "identity") +
  geom_text(aes(label=comma_format()(Total)), position=position_dodge(width=0.9), vjust=-0.25, fontface = 'bold' ) +
  facet_wrap(~ forfacet, ncol = 4, scales = "free") +
  scale_fill_viridis_d(option = "D", end = .7) +
  geom_blank(aes(y = y_max)) +
  theme(strip.text.x = element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.spacing.x = unit(.5, 'cm')) +
  labs(title = "Number of Games Coached by Each Gender,\n as a Function of Technical Fouls\n",
       fill = "Type of Game   ",
       caption = "\nMale coaches received technical fouls in slightly more games,\neven though they coached in almost 20,000 fewer games.")

  
  raw.data %>%
    group_by(nummaletechs > 0, nummaletechs > -1) %>%
    summarise(totals = sum(nummalerefs)/ sum(nummalerefs + numfemalerefs))



# for whatever reason there are more men refs at men's games than at women's games, and women tend to call
  # fewer techs; need to explore this more /account for this in future analyses
  
  raw.data %>%
    mutate(malecoachrefs = ifelse(nummalecoaches > 0, nummalerefs, NA),
           malecoachrefstechs = ifelse(nummalecoaches > 0 & total_techs > 0, nummalerefs, NA),
           femalecoachrefs = ifelse(numfemalecoaches > 0, nummalerefs, NA),
           femalecoachrefstechs = ifelse(numfemalecoaches > 0 & total_techs > 0, nummalerefs, NA)) %>%
    summarise_at(vars(malecoachrefs, malecoachrefstechs, femalecoachrefs, femalecoachrefstechs), list(~mean(., na.rm = TRUE)))
  
  ##

 





















fread('games_and_coaches_for_stats.csv') %>%
  group_by(numfemalerefs, nummalerefs) %>%
  summarise(totalgames = n(), Techs_handed = sum(total_techs)) %>%
  mutate(Techs_per_game = Techs_handed/totalgames)
