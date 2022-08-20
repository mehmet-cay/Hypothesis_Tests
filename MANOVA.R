library(tidyverse)
library(rstatix)
library(mvnormtest)

heart <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/heart.csv")

heart %>% group_by(cp) %>% summarise(Shapiro = mshapiro_test(cbind(chol,thalach))$p.value)

identify_outliers(heart["chol"])
identify_outliers(heart["thalach"])
quantile(heart$thalach)

heart <- heart %>% filter( chol < 394 & thalach > 71)

heart %>% group_by(cp) %>% summarise(Shapiro = mshapiro_test(cbind(chol,thalach))$p.value)

library(heplots)

bartlettTests(y=heart[c("chol","thalach")],group = heart$cp)
leveneTests(y = heart[c("chol" , "thalach")] , group = heart$cp )

box_m(heart[c("chol" , "thalach")] , group = heart$cp)

manova <- manova(cbind(chol,thalach)~ cp, data=heart)
summary(manova)

grouped_heart <- heart %>% gather(key="variable",value="value",thalach , chol) %>% group_by(variable)
grouped_heart %>% convert_as_factor(cp) %>% welch_anova_test(value ~ cp)
grouped_heart %>% convert_as_factor(cp) %>% 
  tukey_hsd(value ~ cp)
grouped_heart %>% convert_as_factor(cp) %>% 
  games_howell_test(value ~ cp)