library(tidyverse)
Movies <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/Movies.csv")

data <- Movies %>% select(popularity)
set.seed(920)
Index <- sample(1:nrow(data),size=5000)
smallData <- data[Index,]
hist(smallData)

shapiro.test(smallData)

dataset <- iris[1:25,]
nrow(dataset)
hist(dataset$Sepal.Length)
ks.test(dataset$Sepal.Length,pnorm)