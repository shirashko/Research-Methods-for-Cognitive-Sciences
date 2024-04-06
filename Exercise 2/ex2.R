# exercise 2

# question 1
# 1
n = 5
sample = rnorm(n, 10, 2)
# R's std function calculates a *corrects* std estimate. The same goes for var. 
# so if we want the variance of the sample and not the corrected estimator, we 
# need to multiply by n-1/n
var = var(sample)*(n-1)/n # get the empirical variance of the sample
hist(sample, col="lightpink", main=paste("five samples from N(10,4) with variance of", round(var, 3))) 
# 2 
sizes = c(5, 10, 50, 100, 250, 500)
means = c()
j = 1
# for each n in sizes, find the average variance of sample size n
for (n in sizes){
  n_vars = c()
  for (i in 1:10000){
    sample = rnorm(n, 10, 2)
    n_vars[i] = var(sample)*(n-1)/n
  }
  means[j] = mean(n_vars)
  j = j + 1
}
# 3
plot(sizes, means, ylim = range(3,4), main="Biased Variance Average As Function Of Sample Size
  (over 10,000 samples from N(10,4) distribution)", xlab="n = sample size", 
     ylab="average variance", cex.main=0.8)
abline(h=4, col="blue") 

# 5
means = c()
j = 1
for (n in sizes){
  n_vars = c()
  for (i in 1:10000){
    sample = rnorm(n, 10, 2)
    n_vars[i] = var(sample)
  }
  means[j] = mean(n_vars)
  j = j + 1
}
plot(sizes, means, ylim = range(3,4), main="Unbiased Variance Average As Function Of Sample Size
  (over 10,000 samples from N(10,4) distribution)", xlab="n = sample size", 
     ylab="average variance", cex.main=0.9)
abline(h=4, col="blue") 


# question 2
sizes = c(1, 3, 15, 30)
j = 1
for (n in sizes){
  n_mean = c()
  for (i in 1:10000){
    sample_exp = rexp(n, rate=5)
    n_mean[i] = mean(sample_exp)
  }
  hist(n_mean, xlab= "mean", main=paste("Distribution Of Sample Mean For Sample 
  Size =", n, "For Samples From Exp(5)"), cex.main = 0.9, col="orange")
}


# question 4
# 2
domesticated_dogs_height_sample = rnorm(5, 90, 2)
# 3
H0_mu = 90
t.test(domesticated_dogs_height_sample, mu = H0_mu, alternative = "two.sided") 
  
# 7
domesticated_dogs_height_sample = rnorm(5, 95, 2)
t.test(domesticated_dogs_height_sample, mu = H0_mu, alternative = "two.sided")
