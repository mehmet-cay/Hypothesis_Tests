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