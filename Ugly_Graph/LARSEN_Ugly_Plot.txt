####Ugly plot time whohoo!!!####
#We're going to make a Withered Foxy humpscare. Because why the hell not?
#I needed help from ChatGBT on this one because of that, so if there's code I can't explain
#that's why.

#according to google, I need the ggimage package and magick
install.packages("ggimage")
install.packages("magick")
install.packages("png")

####Loading libraries####
#So I installed those, and now load them into my library
library(ggplot2)
library(ggimage)
library(magick)
library(dplyr)
library(gganimate)
library(grid)
library(png)


####Variables and data####
#up - Ugly Plot.
#office - FNAF 2 Office Background png
#offgrop - the rasterGrob version of the office.
#foxyall - the data set I'm using for all of Foxy's animations.
#n - number of frames
#jumpscare - the animated plot.

office <- data.frame(
  x = 0,
  y = 0,
  image = "background.png")
#Chat helped me streamline this so I didn't have a bajillion variables.
n <- 14
foxyall <-  data.frame(
  frame = 1:n,
  x = rep(5, n),   # same x (change if needed)
  y = rep(5, n),  # same y
  img = paste0("frame", 1:n, ".png")  # img1.png ... img14.png
)
foxyall$x <- 0
foxyall$y <- -0.2



####Making the plot itself####
#with the help of ChatGBT
up <- ggplot() +
  geom_image(
    data = office,
    aes(x = x, y = y, image = image),
    size = 3
  ) +
  geom_image(
    data = foxyall,
    aes(x = x, y = y, image = img),
    size = 1.5
  ) +
  coord_cartesian(
    xlim = c(-1, 1),
    ylim = c(-1.5, 1),
    clip = "off"
  ) +
  
  theme_void()



#Actual animated plot
jumpscare <- up +transition_manual(frame)
animate(jumpscare,width = 1600, height = 786, units = "px", fps = 80, 
        renderer = gifski_renderer("LARSEN_Ugly_Plot.gif"))  



####Garbage, useless stuff that didn't work####
#ggplot(foxyall, aes(x = x, y = y))+
  #geom_image(aes(image = img), size = 0.1) + geom_image(aes(image = img), size = 2)
#up + coord_cartesian(xlim = c(-1, 1), ylim = c(-1.5, 1), clip = "off")
#Now that we've got the background up and running, let's add Foxy.
#Making a data point that we can use to anchor Foxy to.
#animate(jumpscare, nframes = 14, fps = 2, width = 1600,height = 786, units = "px")
#annotation_custom(
# offgrob,
#xmin = -Inf, xmax = Inf,
#ymin = -Inf, ymax = Inf
#) +
# geom_image(aes(image = img), size = 0.2)
#office<- "background.png"
#offgrob <- rasterGrob(office, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
