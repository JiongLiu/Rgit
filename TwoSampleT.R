###### Two independent sample t-test
###### dataset:eclsk_revised
###### chosen independent variable:female
###### chosen dependent variable:math
library(readr)
eclsk_revised <- read.csv("Desktop/eclsk_revised.csv",header=TRUE)
data <- eclsk_revised
#1. Research question: Are female students performing as good as male students on math?
#2. The independent variable I choose is gender (named as female in the dataset), which is nominal data; the dependent variable I choose is math score, which is ordinal data.
my_data <- data.frame(female=c(data$female),math=c(data$math))
#head(my_data)
library("ggpubr")
ggboxplot(my_data, x = "female", y="math", color="black", ylab="Math", xlab="Female")
#H0: Yes, the two groups performing as good as each other, mu_male = mu_female
#Ha: No, one group has better performance than the other, mu_male != mu_female
#Assumption discussion:
#1. Independence: these two sample sets are grouped by gender, female, and male, so we can # consider they are independent of each other, since they are collected from
# different groups of people.
#2. Population normally distributed
male_math <- my_data$math[my_data$female == 0]
female_math <- my_data$math[my_data$female == 1]
shapiro.test(female_math)
#data: female_math
#W = 0.95475, p-value < 2.2e-16
shapiro.test(male_math)
#data: male_math
#W = 0.97749, p-value = 5.291e-13
### Based on the relatively high values of W and very small values of p, both samples are
### normally distributed.
#3. Homogeneity of Variance
myData = stack(list(maleMath=male_math, femaleMath=female_math))
library(car)
leveneTest(values ~ ind, myData)
# p-value ~ 0.0012 indicates that we should reject the null hypothesis,
# i.e., this assumption isn't met.
# For the two-independent t test, Welch's t test is used as below!
result <- t.test(female_math, male_math, var.equal = FALSE)
result
# Welch Two Sample t-test
#data: female_math and male_math
#t = 0.42989, df = 2366.7, p-value = 0.6673
#Alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -0.7493704 1.1701874
#Sample estimates:
# Mean of x mean of y
#38.00643 37.79602
#Conclusion: based on the t-test value (~ 0.43) and the p-value (~ 0.667), we can conclude that
# there is no big difference between the math performance of male and female # students, i.e. they are as good as one another averagely. So, we can accept our
# null hypothesis that their performance in math are as good as each other!
#Cohen's D
library(lsr)
cohensD(male_math, female_math)
# 0.01747362
# The d ~ 0.02 which indicates a very small effective sample size. The reason could be that
# the correlation between these two samples are relatively high, as they could be students from
# the same class, no matter which gender, besides the size of the sample we use in the test
# is not large enough.