#todo - check if need to set working directory by ourselves with some command
library(tidyverse)
library(effsize)
library(emmeans)
# question 4
# 1 - load data.
data = read.csv("tiktok-misinfo-data.csv")
data = drop_na(data)
data$condition = as.factor(data$condition)
# 2 - filter samples in which participant didn't get accuracy 1 in the attention task
data = filter(data, screenacc == "1")
# 3 - filter samples in which videos shown in second part of the experiment were neutral 
data =   filter(data, veracity != "none")
# 4 - adding a feature with the value of the average rating of the sample, based on
# the unbiased, accuracy & reliable (the dependant variables)
data = mutate(data, mean_rating = (reliable + accuracy + unbiased)/3)
# 5 - test the influence of the experiment condition on the the mean rating
aov.out = aov(formula = mean_rating ~ condition, data)
summary(aov.out)
# we saw in class that a measure to effect size in one-way ANOVA is called eta-squared
effectsize::eta_squared(aov.out, alternative = "two.sided")
# 6.b test the planned contrast
emm = emmeans(aov.out, specs = ~condition) # table of means
levels(data$condition) # check the order of the levels 
cont1 = c(-0.5, -0.5, 1)
cont2 = c(1, -1, 0)
contrast(emm, method = list(cont1, cont2)) # to do check if they are orthogonal!! to check!!!!
# 7
res = aov(mean_rating ~ condition * veracity, data)
summary(res)
