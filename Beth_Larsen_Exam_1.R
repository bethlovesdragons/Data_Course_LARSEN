#I'm working from my Data_Course_LARSEN folder, in the exam 1 sub directory.

#Task I. 
covdat <-read.csv("cleaned_covid_data.csv")

#Task II
library(tidyverse)
library(dplyr)

#Subsetting into rows that start with A
A_states <- covdat %>%
  filter(startsWith(toupper(Province_State), "A"))

#Task III
library(ggplot2)
#subsetting A_states into deaths over time
deaths <-A_states$Deaths

#okay, so the description on the website says, 
#"“Deaths” 	The date the DNA was originally extracted in the format YYYY-MM-DD
#but the "Deaths" doesn't seem to actually be written like that, and instead appears to be quantity
#So instead I'm going to base this off the Last_Update column instead for time, I hope that's right

ggplot(A_states, aes(x = Last_Update, y = Deaths, color=Province_State)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  scale_x_discrete(breaks = unique(A_states$Last_Update))+
  labs(x="Deaths over time", y="Number of deaths in states")+
  theme_minimal()

#I don't seem to be able ot get numbers on the X axis to show up, so you get this I guess.

#Task IV.

#subsetting again
fatalrate <- covdat$Case_Fatality_Ratio

state_max_fatality_rate <- aggregate(fatalrate ~ covdat$Province_State, data = covdat, function(x) max(x, na.rm = TRUE))
colnames(state_max_fatality_rate) <- c("Providence_State", "Case_Fatality_Ratio")
state_max_fatality_rate <- state_max_fatality_rate[order(-state_max_fatality_rate$Case_Fatality_Ratio), ]

#Task V.

#making covdat a factor by descending fatality rates
state_max_fatality_rate <- state_max_fatality_rate %>%
  arrange(desc(state_max_fatality_rate$Case_Fatality_Ratio)) %>%             # Make sure descending
  mutate(State = factor(state_max_fatality_rate$Providence_State, levels = state_max_fatality_rate$Providence_State))

#Creating the bar plot I guess
ggplot(state_max_fatality_rate, aes(x = State, y = Case_Fatality_Ratio)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Peak Fatality Rate by State",
    x = "State",
    y = "Fatality Rate "
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
