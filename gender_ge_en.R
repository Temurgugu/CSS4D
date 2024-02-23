rm(list=ls())    # clear the workspace


#=== Temur Gugushvii === 
# Author https://temurgugushvili.ge/
# Data source:
############ CSS4D:gender ###########

# activate packages

library(ggplot2)
library(tidyverse)


#import data

StudentsSurvey <- readr::read_csv("data/survey_feedback.csv", show_col_types = FALSE)


#modify data for viz

SurveyGender <- StudentsSurvey |>
  select(Gender) |>
  count(Gender)


# Compute percentages
SurveyGender$fraction <- SurveyGender$n / sum(SurveyGender$n)

# Compute the cumulative percentages (top of each rectangle)
SurveyGender$ymax <- cumsum(SurveyGender$fraction)

# Compute the bottom of each rectangle
SurveyGender$ymin <- c(0, head(SurveyGender$ymax, n=-1))

# Compute label position
SurveyGender$labelPosition <- (SurveyGender$ymax + SurveyGender$ymin) / 2

# Compute a good label
SurveyGender$label <- paste0(SurveyGender$Gender, "\n რაოდენობა: ", SurveyGender$n, " (", round(SurveyGender$fraction *100,1), "%)")

# Make the plot
Figure_Gender_1_ge <- ggplot(SurveyGender, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Gender)) +
  geom_rect() +
  geom_text(x=1.5, aes(y=labelPosition, label=label, color=Gender), size=4.5) + # x here controls label position (inner / outer)
  scale_fill_manual(values=c("#450369", "#964eed", "#0d31d1")) +
  scale_color_manual(values=c("#450369", "#964eed", "#0d31d1")) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")


Figure_Gender_1_ge

ggsave("visualisation/Figure_Gender_1_ge.png", 
       Figure_Gender_1_ge,
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)



#modify data for viz_en

SurveyGender_en <- StudentsSurvey |>
  select(Gender) |>
  count(Gender)  |>
  mutate(Gender_EN  = recode(Gender, 
                            "მდედრობითი" = "Female",
                            "მამრობითი" = "Male",
                            "უარი პასუხზე" = "Refuse to answer "))



# Compute percentages
SurveyGender_en$fraction <- SurveyGender_en$n / sum(SurveyGender_en$n)

# Compute the cumulative percentages (top of each rectangle)
SurveyGender_en$ymax <- cumsum(SurveyGender_en$fraction)

# Compute the bottom of each rectangle
SurveyGender_en$ymin <- c(0, head(SurveyGender_en$ymax, n=-1))

# Compute label position
SurveyGender_en$labelPosition <- (SurveyGender_en$ymax + SurveyGender_en$ymin) / 2

# Compute a good label
SurveyGender_en$label <- paste0(SurveyGender_en$Gender_EN, "\n Number: ", SurveyGender_en$n, " (", round(SurveyGender_en$fraction *100,1), "%)")

# Make the plot
Figure_Gender_1_EN <- ggplot(SurveyGender_en, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Gender_EN)) +
  geom_rect() +
  geom_text(x=1.5, aes(y=labelPosition, label=label, color=Gender_EN), size=4.5) + # x here controls label position (inner / outer)
  scale_fill_manual(values=c("#450369", "#964eed", "#0d31d1")) +
  scale_color_manual(values=c("#450369", "#964eed", "#0d31d1")) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")


Figure_Gender_1_EN

ggsave("visualisation/Figure_Gender_1_EN.png", 
       Figure_Gender_1_EN,
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)




