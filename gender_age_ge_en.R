rm(list=ls())    # clear the workspace



# Temur Gugushvii
# Author: https://temurgugushvili.ge/
# Data source:
# Project: CSS4D
# Topic: gender & age



# activate packages

library(ggplot2)
library(tidyverse)


#import data

StudentsSurvey <- readr::read_csv("data/student_alumni_survey.csv", 
                                  show_col_types = FALSE)


#modify data for viz

SurveyAgeGender <- StudentsSurvey |>
                   dplyr::select(Gender, Age) 



Figure_2_gender_age_ge <- ggplot(SurveyAgeGender, aes(Age, group = Gender, fill = Gender))+
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
  



Figure_2_gender_age_ge

ggsave("visualisation/Figure_2_gender_age_ge.png", 
       Figure_2_gender_age_ge,
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


Figure_2_gender_age_en <- ggplot(SurveyAgeGender_EN, aes(Age, group = Gender_EN, fill = Gender_EN))+
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




Figure_2_gender_age_en

ggsave("visualisation/Figure_2_gender_age_en.png", 
       Figure_2_gender_age_en,
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)