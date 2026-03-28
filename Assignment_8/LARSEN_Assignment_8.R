library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)


####Tutorial section####
data("mtcars")
mod1 = lm(mpg ~ disp, data = mtcars)
summary(mod1)

ggplot(mtcars, aes(x=disp,y=mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mod2 = lm(mpg ~ qsec, data = mtcars)
ggplot(mtcars, aes(x=disp,y=qsec)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mean(mod1$residuals^2)
mean(mod2$residuals^2)

df <- mtcars %>% 
  add_predictions(mod1) 
df %>% dplyr::select("mpg","pred")

newdf = data.frame(disp = c(500,600,700,800,900))
pred = predict(mod1, newdata = newdf)
hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"
fullpreds <- full_join(df,hyp_preds)
ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=mpg),color="Black") +
  theme_minimal()

mod3 <- glm(data=mtcars,
            formula = mpg ~ hp + disp + factor(am) + qsec)
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3)
map(mods,performance) %>% reduce(full_join)
mtcars %>% 
  gather_residuals(mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()
mtcars %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal() +
  annotate("text",x=250,y=32,label=mod1$call) +
  annotate("text",x=250,y=30,label=mod2$call) +
  annotate("text",x=250,y=28,label=mod3$call)


report(mod3)


####Actual assignment####

#mush - musroom_growth.csv
#mushmod1-5 - their respective models.
#newmush - preparing the predition
#mushpred - mushroom prediction
#hyppred - hypothetical mushrooms
#fullmush - mushrooms with hypothetical mushrooms

#1
mush<-read.csv("../../Data/mushroom_growth.csv")

#2
ggplot(mush, aes(x=Light,y=Humidity)) + 
  geom_point() + 
  theme_minimal()
#That's useless lmao

ggplot(mush, aes(x=Humidity,y=Light)) + 
  geom_point() + 
  theme_minimal()
#Useless, but to the side (bonus useless!)

ggplot(mush, aes(x=Humidity,y=GrowthRate)) + 
  geom_point() + 
  theme_minimal()

ggplot(mush, aes(x=Light,y=GrowthRate)) + 
  geom_point() + 
  theme_minimal()

ggplot(mush, aes(x=Nitrogen,y=GrowthRate)) + 
  geom_point() + 
  theme_minimal()

ggplot(mush, aes(x=Temperature,y=GrowthRate)) + 
  geom_point() + 
  theme_minimal()

#3
mushmod1 = lm(GrowthRate ~ Light, data = mush)
mushmod2 = lm(GrowthRate ~ Nitrogen, data = mush)
mushmod3 = lm(GrowthRate ~ Humidity, data = mush)
mushmod4 = lm(GrowthRate ~ Temperature, data = mush)
mushmod5 <- glm(data=mush,
            formula = GrowthRate ~ Light + Nitrogen + Humidity + Temperature)

#4
mean(mushmod1$residuals^2)
mean(mushmod2$residuals^2)
mean(mushmod3$residuals^2)
mean(mushmod4$residuals^2)
mean(mushmod5$residuals^2)

#5
#The lowest mean was by far model 5, unsurprisingly
#So we're using that.

#6
max(mush$GrowthRate)
#I think I'm doing this right.
newmush = data.frame(Light = c(5, 15, 20, 25, 30),
                     Nitrogen = c(45, 50, 60, 40, 35),
                     Humidity = c("Low", "Low", "Low", "Low", "Low"),
                     Temperature = c(20, 25, 25, 20, 20))
newmush$Humidity <- factor(newmush$Humidity, levels = levels(mush$Humidity))
mushpred = predict(mushmod5, newdata = newmush)
hyppreds <- data.frame(Light = newmush$Light, Nitrogen = newmush$Nitrogen,
                       Humidity = newmush$Humidity, Temperature = newmush$Temperature,
                        pred = mushpred)

mush$PredictionType <- "Real"
hyppreds$PredictionType <- "Hypothetical"
mush$pred <- predict(mushmod5)
fullmush <- bind_rows(mush, hyppreds)


#7
ggplot(fullmush, aes(x = Nitrogen, y = pred, color = PredictionType)) +
  geom_point() +
  geom_point(aes(y = GrowthRate), color = "black") +
  theme_minimal()

ggplot(mush, aes(x = GrowthRate, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal()


####non-linear####
nonlin<-read.csv("../../Data/non_linear_relationship.csv")

mod_poly <- lm(response ~ poly(predictor, 2), data = nonlin)
ggplot(nonlin, aes(x = predictor, y = response)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +
  theme_minimal()
