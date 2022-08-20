weatherAUS <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/weatherAUS.csv")

Albury <- data.frame(weatherAUS$Temp9am[weatherAUS$Location=="Albury"])
Albury <- Albury[1:2988,]
Cobar <- data.frame(weatherAUS$Temp9am[weatherAUS$Location=="Cobar"])
Cobar <- Cobar[1:2988,]
df <- c(Albury,Cobar)
groups <- c(rep("Albury",2988),rep("Cobar",2988))

shapiro.test(Albury)
shapiro.test(Cobar)

data <- data.frame("Temp"=df,"groups"=groups)

wilcox.test(data$Temp ~ data$groups,mu=0)