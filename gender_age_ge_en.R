rm(list=ls())    # clear the workspace


#=== Temur Gugushvii === 
# Author https://temurgugushvili.ge/
# Data source:
############ CSS4D:gender & age ###########

# activate packages

library(ggplot2)
library(tidyverse)


#import data

StudentsSurvey <- readr::read_csv("data/survey_feedback.csv", show_col_types = FALSE)


#modify data for viz

SurveyAgeGender <- StudentsSurvey |>
  select(Gender, Age) 



Figure_GenderAge_2 <- ggplot(SurveyAgeGender, aes(Age, group = Gender, fill = Gender))+
  geom_histogram(binwidth = 5, position="dodge")+
  theme_minimal(base_family="Sylfaen") +
  theme(axis.text.x=element_text(angle = 0, hjust=1, size=9, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=1, size=9, colour="black"))+
  labs(x = "ასაკი",
       y = "აბსოლუტური რაოდენობა")+
  scale_fill_manual(name = "სქესი",
    values = c("#450369", "#964eed", "#0d31d1"))+
  scale_y_continuous(breaks=seq(0, 75, 10), limits = c(0,75))+
  scale_x_continuous(breaks=seq(0, 100, 5))
  



Figure_GenderAge_2

ggsave("visualisation/Figure_GenderAge_2.png", 
       Figure_GenderAge_2,
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)







#modify data for viz

SurveyAgeGender_EN <- StudentsSurvey |>
  select(Gender, Age) |>
  mutate(Gender_EN  = recode(Gender, 
                           "მდედრობითი" = "Female",
                           "მამრობითი" = "Male",
                           "უარი პასუხზე" = "Refuse to answer "))


Figure_GenderAge_2_EN <- ggplot(SurveyAgeGender_EN, aes(Age, group = Gender_EN, fill = Gender_EN))+
  geom_histogram(binwidth = 5, position="dodge")+
  theme_minimal(base_family="Sylfaen") +
  theme(axis.text.x=element_text(angle = 0, hjust=1, size=9, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=1, size=9, colour="black"))+
  labs(x = "Age",
       y = "Absolute Number")+
  scale_fill_manual(name = "Gender",
                    values = c("#450369", "#964eed", "#0d31d1"))+
  scale_y_continuous(breaks=seq(0, 75, 10), limits = c(0,75))+
  scale_x_continuous(breaks=seq(0, 100, 5))




Figure_GenderAge_2_EN

ggsave("visualisation/Figure_GenderAge_2_EN.png", 
       Figure_GenderAge_2_EN,
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)