library(table1)
library(dplyr)
library(broom)
library(knitr)
library(lme4)
library(ggplot2)
library(forcats)

data = read.csv("Data.csv")

# omit the columns we don't need
d = data[,1:8]
d = na.omit(d)
colnames(d) = c("Person", "trt", "trtTime","Sex","Age",
                "Weight","BodyTemp","OutsideTemp")

# releveling trtTime factor
d$trtTime <- fct_relevel(d$trtTime, "before", "after", "3_months_after")

# spelling out sex column
d$Sex = ifelse(d$Sex == "M", "Male", "Female")

# data frame that's one row per person
allBefore = d[seq(1,140,3),]

# cleaning
indexes = complete.cases(d)
d_clean = d[indexes,]

#table of level 2 covariates
lvl2 = table1(~ Sex + Age + Weight, data = allBefore)
lvl2

#table of level 2 covariates split by treatment
lvl2bytrt = table1(~ Sex + Age + Weight | trt, data = allBefore)
lvl2bytrt

#plot of level 2 covariates
lvl2plot = ggplot(allBefore, aes(x = Age, y = Weight, color = Sex)) + 
  geom_point() + facet_wrap(~trt) + labs(title = "Weight vs Age", y = "Weight(kg)")
lvl2plot

# outside temperature vs body temperature
p1 = ggplot(d_clean, aes(x = OutsideTemp, y = BodyTemp, color = trt)) + 
  geom_point() + facet_wrap(~trtTime) + 
  labs(title = "Outside Temperauture vs Body Temperature", 
       x = "Outside Temperature", y = "Body Temperature") + 
  scale_color_manual(values = c("None" = "red", "OneGy" = "blue"), name = "Treatment")
p1

# response per person
pplPlot = ggplot(d_clean, aes(x = trtTime, y = BodyTemp, group = Person, color = Person)) +
  geom_point() + geom_line() + facet_grid(Sex~trt) + theme(legend.position = "none") + 
  labs(title = "Body Temperature vs Time Relative to Treatment by Treatment", 
       x = "Time Relative to Treatment", y = "Body Temperature(C)")
pplPlot

# model fitting with REML
model <- lmer(BodyTemp ~ trt * trtTime + Sex + Age + Weight + OutsideTemp + (1 | Person), data = d)
summary(model)
library(pbkrtest)

# confidence intervals for fixed effects
confint(model)

# full and reduced model without the treatment effect. Fitted using ML
full = lmer(BodyTemp ~ trt * trtTime + Sex + Age + Weight + OutsideTemp + (1 | Person), data = d, REML = FALSE)
reduced = lmer(BodyTemp ~ trtTime + Sex + Age + Weight + OutsideTemp + (1 | Person), data = d, REML = FALSE)

# parametric bootstrap to compare full and reduced model
test <- PBmodcomp(full, reduced, nsim = 10000)

summary(test)








