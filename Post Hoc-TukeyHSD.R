library(tidyverse)
library(rstatix)

DesignTest <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/DesignTest.csv")

data <- DesignTest %>% gather(key="Groups",value="Scores",ADesign,BDesign,CDesign) %>% convert_as_factor(Groups)

data %>% group_by(Groups) %>% summarise(ShapiroResults = shapiro.test(Scores)$p.value)

ids <- c(rep(seq(1, 100) , 3))
data$wid = ids
anova <- anova_test(data , dv = "Scores" , within = "Groups" , wid = "wid")

tukey_hsd(data,Scores ~ Groups)