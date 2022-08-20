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
