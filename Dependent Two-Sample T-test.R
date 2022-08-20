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