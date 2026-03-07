
#urelraw - Untidied version of Utah_Religions_by_County (u - utah, rel - relgions)
#urelclean - clean version of Utah_Religions
#urellong - longer version that's being tidied up
#urmajor - only the major religons
#nonrel - Non relgious
#ucomp - comparing relgious vs non-religous. (U - utah, comp - comparison)

#Loading in the data set
urelraw<-read.csv("Utah_Religions_by_County.csv")

#Loading the libraries from the janitor tutorial
library(readxl)
library(janitor)
library(dplyr)
library(here)
library(tidyverse)


#Cleaning up the names of the columns. They seemed odd to me.
urelclean<-read.csv("Utah_Religions_by_County.csv") |> 
  clean_names()

#Making it easier to analyise. Even if it looks really odd to a human.
urellong<- urelclean|>
  pivot_longer(
    cols = -c(county, pop_2010),
    names_to = "religion",
    values_to = "proportion"
  )

#Now let's do the exploring you mentioned.


#Adressing question 1.

##“Does population of a county correlate with the proportion of any ##
##specific religious group in that county?”##

#I'll make a big one, but I don't think it'll tell me much.
ggplot(urellong, aes(x = pop_2010, y = proportion)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "forestgreen") +
  facet_wrap(~ religion) +
  labs(
    x = "County Population (2010)",
    y = "Proportion of Population",
    title = "Potenial Relationship Between County Population and Religious Proportion"
  ) +
  theme_minimal()

#Let's also make a chart for just the major religons to get rid of like the ones with
#almost no population

urmajor <- urellong |>
  filter(religion %in% c("lds", "catholic", "evangelical", "non_religious"))

ggplot(urmajor, aes(pop_2010, proportion, color = religion)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "County Population",
    y = "Proportion",
    title = "County Population vs Religious Composition"
  ) +
  theme_minimal()

#Yeah, maybe I'm doing osmething wrong, because this doesn't really seem to be doing much
#It doens't like, mean anything when I look at this really. It's just sort of there.
#I asked chat.gpt and it helped me make these plots too. Like it gave me pretty mucht he same


#On to question 2
##“Does proportion of any specific religion in a given county correlate with## 
##the proportion of non-religious people?”##

#With the help of chatgpt I made another column for the dataframe

nonrel <- urellong |>
  filter(religion == "non_religious") |>
  select(county, non_religious = proportion)

ucomp <- urellong |>
  left_join(nonrel, by = "county") |>
  filter(religion != "non_religious")

#So now we have the propotions to compare
#Doing very similar to the previous question with my stuff
ggplot(ucomp, aes(x = non_religious, y = proportion)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  facet_wrap(~ religion) +
  labs(
    x = "Proportion Non-Religious",
    y = "Proportion of Religion",
    title = "Potential Relationship Between Non-Religious Population and Religious Groups"
  ) +
  theme_minimal()
#The only one I'm really seeing a correlation with is the LDS, which makes sense considering
#that it's by far the biggest religion in the state. If you're not LDS, there's a good chance
#that you're just non religious to begin with.


