install.packages("tidyverse")
library(tidyverse)
install.packages("effectsize")
library(effectsize)
library(ggplot2)

# question 1
# 1
data = read.csv("pokedex.csv")

# 2 
# change the type of the categorical variables into factor type by using across() 
# within mutate(), so multiple variables can be modified at once
data = mutate(data, across(c(pokedex_number, name, type, generation, is_legendary), as.factor))
data = mutate(data, across(c(attack, defense, height_m, hp, percentage_male, 
      sp_attack, sp_defense, speed, weight_kg), as.numeric))

# 3
data = drop_na(data)

# 4
# group the pokemons by their type, then create a df with 2 columns - the type 
# and the mean attack by the type feature, and call this column 
# attack_mean_by_type. reminder: when summarize is used on grouped data, 
# to summary statistics that are computed by group
attack_df = summarize(group_by(data, type), attack_mean_by_type = mean(attack))

# 5
# in order to find the two types of pokemons with best attack, we sort in 
# decreasing order the rows by the mean column
sorted_data = attack_df[order(attack_df$attack_mean_by_type, decreasing = TRUE),]
# this way we found out that 2 best attackers are steel and dragon

# examine if the means of steel and dragon populations are different, using the 
# samples of these types in the data
steel_sample = filter(data, type == "steel")$attack
dragon_sample = filter(data, type == "dragon")$attack
# using t.test because the variance is unknown
t.test(steel_sample, dragon_sample, type = "two-tailed", var.equal = T)
# sanity_check:
# sorted_data = data[order(data$type),]

# 6
data$sturdiness = (data$defense) * (data$sp_defense)
sturdiness_mean = mean(data$sturdiness)

# 7 - checking whether the sturdiness of the pokemons from generation 1 is smaller
# than the the average sturdiness of all pokemons
first_generation_sample = filter(data, generation == 1)$sturdiness
t.test(first_generation_sample, mu = sturdiness_mean, alternative = "less")


# question 2
# 5a
df = read.csv("GambleWalker.csv", sep="\t")
# need to group by condition (with helmet vs hat), and than to take the score column
helmet_sample = df[df$Condition == 1, "BART"] # condition1 is helmet (because there is 39)
cap_sample = df[df$Condition == 2, "BART"] # condition2 is cap (because there is 41) 
t_test = t.test(helmet_sample, cap_sample, type = "two-tailed", var.equal = T)
# also need to find the Cohen's d value
t_test
cohens_d(helmet_sample, cap_sample)
sd(helmet_sample)
sd(cap_sample)


# question 3
# 1 
shapes = c(0.1, 0.15, 0.2, 0.35, 0.5, 0.7, 1, 1.25, 1.5, 2, 2.5, 3, 4, 5, 7.5, 10, 15)
sample_size = 20
population_var = 2
# 2
x_list = seq(0, 20, 0.1)
for (s in shapes) {
  rate = sqrt(s/population_var)
  pdf = dgamma(x_list, rate=rate, shape=s)
  plot(x_list, pdf, main=paste("Simulation of Gamma PDF with shape = ", s), xlab="x", 
       ylab="pdf(x)", cex.main=0.8)
}
# 3
type1_error = c()
for (s in shapes) {
  p_values = c()
  rate =  sqrt(s/population_var)
  for (i in 1:10000){
    sample = rgamma(sample_size, rate=rate, shape=s)
    curr_p = t.test(sample, mu = (s/rate), alternative = "two.sided")$p.value
    p_values = c(p_values, curr_p)
  }
  # calculate type 1 error for current s. because H0 is true, each time p_value 
  # is smaller than 0.05 we get type 1 error.
  p_values = p_values[p_values < 0.05]
  cur_error = (length(p_values) / 10000)*100
  type1_error = c(type1_error, cur_error)
}

# 4
plot(shapes, type1_error, main="Type 1 error as function of shape parameter (for gamma distribution)", xlab="shape", 
     ylab="error percentage", cex.main=0.8)

# 5 
type2_error_g = c()
type2_error_n = c()
for (s in shapes) {
  p_values_g = c()
  p_values_n = c()
  rate =  sqrt(s/population_var)
  for (i in 1:10000){
    g_sample = rgamma(sample_size, rate=rate, shape=s)
    curr_p_g = t.test(g_sample, mu = ((s/rate)*1.2), alternative = "two.sided")$p.value
    p_values_g = c(p_values_g, curr_p_g)
    n_sample = rnorm(sample_size, mean=(s/rate), sd=sqrt(population_var))
    curr_p_n = t.test(n_sample, mu = (s/rate)*1.2, alternative = "two.sided")$p.value
    p_values_n = c(p_values_n, curr_p_n)
  }
  # calculate type 2 error for current s. because H0 is false, each time p_value 
  # is bigger than 0.05 (and we don't reject H0) we get type 2 error.
  p_values_g = p_values[p_values_g >= 0.05]
  cur_error = (length(p_values_g) / 10000) * 100
  type2_error_g = c(type2_error_g, cur_error)
  
  p_values_n = p_values[p_values_n >= 0.05]
  cur_error = (length(p_values_n) / 10000)*100
  type2_error_n = c(type2_error_n, cur_error)
}

# 6
plot(shapes, type2_error_g, main="Type 2 error as function of shape parameter (for gamma distribution)", xlab="shape", 
     ylab="error precentage", cex.main=0.8)
plot(shapes, type2_error_n, main="Type 2 error as function of shape for normal distribution 
     N(shape/rate,√2)", xlab="shape", 
     ylab="error precentage", cex.main=0.8)

df = data.frame(shapes, type2_error_g, type2_error_n)
ggplot(data = df) +
  geom_point(aes(x = shapes, y = type2_error_g, color = "Type 2 Error G")) + 
  geom_point(aes(x = shapes, y = type2_error_n, color = "Type 2 Error N")) +
  scale_color_manual(name = "Legend", 
                     values = c("red", "blue"), 
                     labels = c("Gamma distribution", "Normal distribution")) +  
  ggtitle("Percentage of type 2 error when H0 is wrong 
                    as function of shape") +
  labs(x = "shape", y = "error precentage")

