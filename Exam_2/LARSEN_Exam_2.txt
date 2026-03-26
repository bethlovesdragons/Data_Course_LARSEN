####Exam 2####

###Loading in the Libraries
library(readxl)
library(janitor)
library(dplyr)
library(here)
library(ggplot2)
library(tidyverse)


###Variable List:
#unicef - the whole data csv file.
#contmean - the means of the u5mr score for each country by year grouped by continents.
#my - Model for year
#myc - model for year and continent
#mi - Model for the interaction of year and continent
#predat - prediction data
#pinfo - plot info


####Task 1####
#Loading in the unicef data. 

unicef <- read.csv("unicef-u5mr.csv")

glimpse(unicef)

####Task 2####
#tidying the data.
#I originally had this as like three seperate steps, but this is just so much nicer
#But I originally did the names in one command, then the pivot_longer command
#And then the mutate command in the next two. 
unicef<- unicef %>% clean_names() %>%
  pivot_longer(
    cols = starts_with("u5mr_"),
    names_to = "year",
    values_to = "u5mr"
  ) %>%
  mutate(
    year = str_remove(year, "u5mr_"),
    year = as.integer(year)
  ) %>%
  drop_na(u5mr)
#I'm going to assume we drop the missing values, just because I don't really know what to 
#do with them otherwise in all honesty.

####Task 3 ####
#Multiple graphs like on the html page. 
#Oh boy, my favorite. ggplot.
#It's not that bad when I actually sit down and think about it
#I'm just complaining cause I spent WAY too long on Withered Foxy
ggplot(unicef, aes(x = year, y = u5mr, group = country_name)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ continent, ncol = 3) +
  labs(
    title = "Under-5 Mortality Rate Over Time by Country",
    x = "Year",
    y = "U5MR"
  ) +
  theme_minimal()

#I'm going to sneak in a random note in here in the middle of my exam.
#Walmart Eclairs are Not Very Good tbh

####Task 4 ####
#Oh, it's just downloading the plot.
#I just used the export button over there. That seemed easier than code

####Task 5 ####
#So now I think I have to subset the data

contmean<-unicef %>%
  group_by(continent, year) %>%
  summarise(
    mean_u5mr = mean(u5mr, na.rm = TRUE)
  ) %>%
  ungroup()

#Then making this plot
ggplot(contmean, aes(x = year, y = mean_u5mr, color = continent)) +
  geom_line(alpha = 2) +
  labs(
    title = "Under-5 Mortality Rate by Continent over Time",
    x = "Year",
    y = "U5MR"
  ) +
  theme_minimal()

####Task 6 #####
#I'm just saving the graph png again. Yay.

####Task 7####
#Making three predictive models

#Model 1, year only
my <- lm(u5mr ~ year, data = unicef)
summary(my)

#Model 2, Year and Continent
myc <-lm(u5mr ~ year + continent, data = unicef)
summary(myc)

#Model 3, year and continent interaction
mi <- lm(u5mr~ year * continent, data = unicef)
summary(mi)


####Task 8####
#Comparing the models.
AIC(my, myc, mi)
anova(my, myc, mi)

#I will be fully honest, I don't think we went over this properly in class,
#At least regarding interpritation. Chat told me to use these commands, and I did
#I don't know what the numbers mean.
#But I would assume that model three, mi, would be the best since it shows interaction
#And that would likely give us more data. 

#Using predition models, but Idk what these are either really
predat <- unicef %>%
  distinct(year, continent) %>%
  arrange(year, continent)

predat <- predat %>%
  mutate(
    pred1 = predict(my, newdata = predat),
    pred2 = predict(myc, newdata = predat),
    pred3 = predict(mi, newdata = predat)
  )
predat
#I think my previous conclusion remains. It seems to be model three as the best.

####Task 9####
#Making them into a faceted graph?


#Let's prep the stuff for the plot
pinfo <- predat %>%
  pivot_longer(
    cols = starts_with("pred"),
    names_to = "model",
    values_to = "u5mr_pred"
  ) %>%
  mutate(
    model = recode(model,
                   pred1 = "Model 1: Year only",
                   pred2 = "Model 2: Year + Continent",
                   pred3 = "Model 3: Interaction"
    )
  )

ggplot(pinfo, aes(x = year, y = u5mr_pred, color = continent)) +
  geom_line(size = 1) +
  facet_wrap(~ model) +
  labs(
    title = "Predicted Under-5 Mortality Rate (u5mr)",
    x = "Year",
    y = "Predicted u5mr",
    color = "Continent"
  ) +
  theme_minimal()
