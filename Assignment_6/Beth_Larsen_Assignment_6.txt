#Assignment 6

#loading in tidyverse and gganimate as well as the csv
library(tidyverse)
library(gganimate)
plate<-read.csv("../../Data/BioLog_Plate_Data.csv")

#variables:
#plate - the whole csv, since it's plate data
#long_plate - the tidy verison of plate
#meanplate - the verison of plate where I've taken the mean of the absorbance. 
#meanplate is specifically going to be fore the substrate Tween 40 since I chose that one.
#twe4, the animated graph variabe
#twegif, for exporting the gif


# 1. Cleans this data into tidy (long) form
# I think the only thing that needed adjustment was combining the time (in hours)
#into one column and also the absorbency for each one.
plate_long <- plate %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance"
  )
plate_long <- plate_long %>%
  mutate(
    Time = str_remove(Time, "Hr_"),
    Time = as.numeric(Time)
  )

unique(plate_long$Sample.ID)
# 2. Creates a new column specifying whether a sample is from soil or water
#I took wayy too long figuring out what we meant the Sample.ID colmun and not the substrate one
plate_long <- plate_long %>%
  mutate(
    Sample_Type = if_else(
      str_detect(Sample.ID, regex("creek|water", ignore_case = TRUE)),
      "Water",
      "Soil"
    )
  )

# 3. Generates a plot that matches this one (note just plotting dilution == 0.1):
#The plot is from the website.
#I don't know why mine is boxy and yours is not, and I'm not sure why the data doesn't
#exactly align, I'm assuming you meant match as in like, match what you did, not the information exactly
#Cause idk where I went wrong if it's the second option.
ggplot(plate_long, aes(x = Time,
                      y = Absorbance,
                      color = Sample_Type,
                      group = Sample_Type)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  facet_wrap(~ Substrate) +
  theme_minimal() +
  labs(
    x = "Time (hours)",
    y = "Absorbance",
    color = "Sample Type")

# 4. Generates an animated plot that matches this one 
#(absorbance values are mean of all 3 replicates for each group):
#See plot on website to compare

#okay, so first I need to get the mean
meanplate <- plate_long %>%
  filter(Substrate == "Tween 40") %>%
  group_by(Sample.ID, Dilution, Time) %>%
  summarise(
    mean_absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop"
  )

#Now we actually make the plot
twe4 <- ggplot(meanplate,
            aes(x = Time,
                y = mean_absorbance,
                color = Sample.ID,
                group = Sample.ID)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~Dilution)+
  labs(
    title = paste("Tween 40"),
    x = "Time (hours)",
    y = "Mean Absorbance",
    color = "Sample ID"
  ) +
  theme_minimal() +
  transition_reveal(Time)

animate(twe4, width = 1000, height = 600, fps = 10)


#Now we save the gif
twegif <- animate(twe4, renderer = gifski_renderer())
anim_save(animation =  twegif,  "Beth_Larsen_pt4_Plot.gif")

#thanks to chat gpt for helping me trouble shoot some of my issues