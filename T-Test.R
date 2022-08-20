Heights <- read.csv("C:/Users/Mehmet ÇAY/Desktop/R/Hypothesis_Tests/Height of Male and Female by Country 2022.csv")

shapiro.test(Heights$Male.Height.in.Cm)

set.seed(115)
sample <- sample(Heights$Male.Height.in.Cm, size=100)
shapiro.test(sample)
t.test(sample,mu=180)

t.test(sample,mu=173)