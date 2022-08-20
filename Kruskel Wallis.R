library(tidyverse)

heart <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/heart.csv")

heart %>% group_by(cp) %>% summarise(Normality = shapiro.test(oldpeak)$p.value)
bartlett.test(heart$oldpeak ~ heart$cp)

kruskal.test(x=heart$oldpeak,g=as.factor(heart$cp))
library(dunn.test)

dunn.test(x=heart$oldpeak,g=heart$cp)
dunn.test(x=heart$oldpeak,g=heart$cp,method="bonferroni")