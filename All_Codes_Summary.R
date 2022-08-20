# Normality Tests

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

#T-Test

Heights <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/Height of Male and Female by Country 2022.csv")

shapiro.test(Heights$Male.Height.in.Cm)

set.seed(115)
sample <- sample(Heights$Male.Height.in.Cm, size=100)
shapiro.test(sample)
t.test(sample,mu=180)

t.test(sample,mu=173)

# Dependent Two-Sample T-test

InsPost <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/InsPostComparison.csv")

RandomPosts <- InsPost$RPostsDuration
RelatedPosts <- InsPost$LPostsDuration
duration <- c(RandomPosts,RelatedPosts)
groups <- c(rep("Random",50),rep("Liked",50))
df <- data.frame("duration" = duration,"groups"=groups)
View(df)

shapiro.test(df$duration[df$groups=="Random"])
shapiro.test(df$duration[df$groups=="Liked"])

bartlett.test(df$duration ~ df$groups)
t.test(df$duration ~ df$groups , mu = 0 , paired = T)

# Independent 2-Sample T-test

df <- subset(iris , subset = (Species != "virginica"))
df$Species <- as.character(df$Species)
par(mfrow = c(1,2))
hist(df$Sepal.Width[df$Species == "setosa"])
hist(df$Sepal.Width[df$Species == "versicolor"])

shapiro.test(df$Sepal.Width[df$Species == "setosa"])
shapiro.test(df$Sepal.Width[df$Species == "versicolor"])

bartlett.test(df$Sepal.Width ~ df$Species)

t.test(df$Sepal.Width ~ df$Species , mu = 0 , var.equal = TRUE  )

# Wilcoxon Rank Sums Test

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

# Wilcoxon Signed-Rank Test

shapiro.test(cars$dist)

wilcox.test(cars$dist,mu=40)

# ANOVA

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

# Repeated Measures ANOVA

library(ggpubr)
library(tidyverse)
library(rstatix)

DesignTest <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/DesignTest.csv")

data <- DesignTest %>% gather(key="Groups",value="Scores",ADesign,BDesign,CDesign) %>% convert_as_factor(Groups)

data %>% group_by(Groups) %>% summarise(ShapiroResults = shapiro.test(Scores)$p.value)

ggboxplot(data, x="Groups",y="Scores",color="green")

ids <- c(rep(seq(1, 100) , 3))
data$wid = ids
anova_test(data , dv = "Scores" , within = "Groups" , wid = "wid")

# Post Hoc-TukeyHSD

library(tidyverse)
library(rstatix)

DesignTest <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/DesignTest.csv")

data <- DesignTest %>% gather(key="Groups",value="Scores",ADesign,BDesign,CDesign) %>% convert_as_factor(Groups)

data %>% group_by(Groups) %>% summarise(ShapiroResults = shapiro.test(Scores)$p.value)

ids <- c(rep(seq(1, 100) , 3))
data$wid = ids
anova <- anova_test(data , dv = "Scores" , within = "Groups" , wid = "wid")

tukey_hsd(data,Scores ~ Groups)

# Two-Way Analysis of Variance

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

# Kruskel Wallis

library(tidyverse)

heart <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/heart.csv")

heart %>% group_by(cp) %>% summarise(Normality = shapiro.test(oldpeak)$p.value)
bartlett.test(heart$oldpeak ~ heart$cp)

kruskal.test(x=heart$oldpeak,g=as.factor(heart$cp))
library(dunn.test)

dunn.test(x=heart$oldpeak,g=heart$cp)
dunn.test(x=heart$oldpeak,g=heart$cp,method="bonferroni")

# MANOVA

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

# Categorical Data Tests

survey <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/survey.csv")

question1 <- survey$treatment

q1 <- question1[question1=="Yes"]
q2 <- append(q1, question1[question1=="No"]) 

table(q2)
binom.test(x=637,n=1259,p=0.5)

work_interfere <- survey$work_interfere
table(work_interfere)

result <- chisq.test(table(work_interfere))
result$expected
result$observed
result$p.value

data <- data.frame(x=survey$remote_work,y=survey$seek_help)
tbl <- table(data)

prop.table(table(data)) #table of probability

chisq.test(tbl)

data2 <- data.frame(x=survey$treatment,y=survey$tech_company)
tbl2 <- table(data2)

fisher.test(tbl2)

mydata <- data.frame(
  once = c('yes','yes','yes','yes','no','no'),
  sonra = c('no','yes','yes','no','no','no')
)

mcnemar.test(table(mydata))

# Correlation
shapiro.test(iris$Sepal.Width)
shapiro.test(iris$Petal.Width)
shapiro.test(iris$Sepal.Length)

hist(iris$Sepal.Width)
hist(iris$Sepal.Length)

cor(iris$Sepal.Width , iris$Sepal.Length , method = "pearson")

cor.test(iris$Sepal.Width , iris$Sepal.Length , method = "pearson")

hist(iris$Petal.Width)
hist(iris$Petal.Length)

cor.test(iris$Petal.Width , iris$Petal.Length , method = "kendall")

x <-  c(2,3,4,1,1,3,4,5,1,2,2,3)
y <-  c(3,4,4,2,1,2,5,3,1,3,3,2)

cor.test(x , y , method = "spearman" )

cov(iris$Sepal.Length , iris$Sepal.Width)
cov(iris$Petal.Length , iris$Petal.Width)
