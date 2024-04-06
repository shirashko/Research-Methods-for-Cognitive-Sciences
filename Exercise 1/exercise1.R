# question 1
possible_results = 1:6 # 2
set.seed(61677) # 4
# 5
probablities = c(2/15, 0.2, 2/15, 0.2, 2/15, 0.2) 
number_of_tosses = 15 
sample = sample(possible_results, number_of_tosses, replace=TRUE, prob=probablities)



pnorm(-0.5) # question 3, e
# question 4
# a
sample_a = rnorm(1, 100,10) 
sample_b = rnorm(1,90,2)
# c
sample_a10 = rnorm(10, 100,10)
sample_b10 = rnorm(10, 90,2)
# d
mean_a = mean(sample_a10)
mean_b = mean(sample_b10)
print("which values are greater than the mean? first a, then b")
print(sample_a10[sample_a10>100])
print(sample_b10[sample_b10>90])
# e
print("what is the distance between the sample average and the distibution mean?")
print(abs(100-mean(sample_a10)))
print(abs(90-mean(sample_b10)))
# f
sample_a1000 = rnorm(1000,100,10)
sample_b1000 = rnorm(1000,90,2)
h1 = hist(sample_a1000, main = "species a- 1000 samples from N(100,100) distibution", 
          xlab = "Values", col="lightgreen", labels= TRUE, cex.main=1)
hist(sample_b1000, main = "species b- 1000 samples from N(90,4) distibution", 
     xlab = "Values", col= "lightblue", labels= TRUE, cex.main=1)

