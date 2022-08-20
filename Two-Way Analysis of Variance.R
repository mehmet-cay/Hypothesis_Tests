library(tidyverse)
library(rstatix)
heart <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/heart.csv")

data <- heart %>% filter(chol<394)
data %>% group_by(cp,fbs) %>% summarise(ShapiroResults = shapiro.test(chol)$p.value)

bartlett.test(data$chol ~interaction(data$cp,data$fbs))

anova <-aov(data$chol ~ as.factor(data$cp)*as.factor(data$fbs))
summary(anova)

identify_outliers(heart["trestbps"])

data2 <- heart %>% filter(trestbps < 172)
int_groups <- apply(data2, MARGIN = 1 , FUN = function(x){  
  r <- paste0( x["cp"] , ' - ' , x["fbs"])
  return(r)
} )

data2$int_groups <- int_groups

library(ggpubr)
ggboxplot(data2, x = "int_groups" , y = "trestbps" , title = "Graph",
          color = "green")

data2 %>% group_by(cp , fbs) %>% 
  summarise(Shapiro = shapiro.test(trestbps)$p.value)

bartlett.test(data2$trestbps ~ interaction(data2$cp , data2$fbs)) 


anova2 <- aov(data2$trestbps ~ as.factor(data2$cp) * as.factor(data2$fbs))
summary(anova2)

TukeyHSD(anova2)
