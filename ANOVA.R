library(tidyverse)
library(rstatix)

heart <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/heart.csv")

outliers <- identify_outliers(heart["chol"])
outliers$chol
outliers$is.outlier

data <- heart %>% filter(chol <394)

data %>% group_by(sex) %>% summarise(ShapiroResults = shapiro.test(chol)$p.value)

bartlett.test(data$chol ~ data$sex)

anov <- aov(data$chol ~ data$sex)
anov
summary(anov)