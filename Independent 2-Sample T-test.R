df <- subset(iris , subset = (Species != "virginica"))
df$Species <- as.character(df$Species)
par(mfrow = c(1,2))
hist(df$Sepal.Width[df$Species == "setosa"])
hist(df$Sepal.Width[df$Species == "versicolor"])

shapiro.test(df$Sepal.Width[df$Species == "setosa"])
shapiro.test(df$Sepal.Width[df$Species == "versicolor"])

bartlett.test(df$Sepal.Width ~ df$Species)

t.test(df$Sepal.Width ~ df$Species , mu = 0 , var.equal = TRUE  )