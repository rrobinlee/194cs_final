# load the data
x <- read.csv("Levels_Fyi_Salary_Data.csv")

# subset the data
data <- subset(x, select=c("totalyearlycompensation", "yearsofexperience", "Education"))

# keep only bachelor's, master's, and doctor's
data <- subset(data, Education == "Bachelor's Degree" | Education == "Master's Degree")

# turn Education into a factor
data$Education <- as.factor(data$Education)

# omit NA values
data <- na.omit(data)

plot(totalyearlycompensation ~ yearsofexperience,
     data=data,
     xlab="Years of experience",
     ylab="Total yearly compensation")

boxplot(totalyearlycompensation ~ Education,
        data=data,
        outline=FALSE,
        ylab="Total yearly compensation")

par(mfrow=c(1, 3)) # 1 row, 3 plots

bachelors <- subset(data, Education == "Bachelor's Degree") # people w/ bachelor's
masters <- subset(data, Education == "Master's Degree")     # people w/o master's

hist(bachelors$totalyearlycompensation, # histogram of bachelors' compensations
     main="Bachelor's compensations",
     xlab="")
hist(masters$totalyearlycompensation,   # histogram of masters' compensations
     main="Master's compensations",
     xlab="")
hist(data$yearsofexperience,
     main="Years of experience",
     xlab="")

# Obtain more samples from the population using bootstrapping
bootstrap_cor <- function(){
  re_sample <- data[sample(nrow(data), replace = TRUE),]
  cor <- cor.test(re_sample$yearsofexperience, re_sample$totalyearlycompensation,
                  alternative = 'two.sided', method = 'pearson')$estimate
  return (cor)
}

# Do 1000 repetitions
set.seed(1)
n <- 1000
sample_cors <- c()
for (i in 1:n){
  sample_cors[i] <- bootstrap_cor()
}

# Plot a histogram to visualize
hist(sample_cors, main = "Histogram of Sample Correlations from Bootstrap Samples")

## Appendix Code:

# load the data
x <- read.csv("Levels_Fyi_Salary_Data.csv")

# subset the data
data <- subset(x, select=c("totalyearlycompensation", "yearsofexperience", "Education"))

# keep only bachelor's, master's, and doctor's
data <- subset(data, Education == "Bachelor's Degree" | Education == "Master's Degree")

# turn Education into a factor
data$Education <- as.factor(data$Education)

# omit NA values
data <- na.omit(data)

# scatterplot of compensation against experience
plot(totalyearlycompensation ~ yearsofexperience,
     data=data,
     xlab="Years of experience",
     ylab="Total yearly compensation")
# boxplot of compensation against education
boxplot(totalyearlycompensation ~ Education,
        data=data,
        outline=FALSE,
        ylab="Total yearly compensation")

par(mfrow=c(1, 2)) # 1 row, 2 plots

bachelors <- subset(data, Education == "Bachelor's Degree") # people w/ bachelor's
masters <- subset(data, Education == "Master's Degree")     # people w/o master's

hist(bachelors$totalyearlycompensation, # histogram of bachelors' compensations
     main="Bachelor's compensations")
hist(masters$totalyearlycompensation,   # histogram of masters' compensations
     main="Master's compensations")


library(car)
leveneTest(data$totalyearlycompensation ~ data$Education)

hist(data$yearsofexperience,
     main="Years of experience")

perm_test <- function(n, df, response, group) {
  # get the observed difference (the test statistic) and simulated differences
  obs_diff <- abs_mean_difference(df, response, group)
  sim_diffs <- n_mean_difference_sims(n, df, response, group)
  
  # calculate the observed mean difference and the two-sided p-value
  obs_mean <- tapply(df[, response], df[, group], mean)
  p.value <- sum(obs_diff <= sim_diffs) / n
  
  # print results
  cat(
    # name of the group
    "\nName of factor:", deparse(substitute(group)),
    
    # 1st and 2nd levels of the group
    "\n1st level:", levels(df[, group])[1], "| mean =", obs_mean[1],
    "\n2nd level:", levels(df[, group])[2], "| mean =", obs_mean[2],
    
    # observed absolute mean difference and p-value from simulation
    "\n\nObserved absolute difference between the means of the levels:", obs_diff,
    "\np-value (two-sided) from simulating", n, "mean differences:", p.value)
  
  # print whether there is a significant difference (i.e. whether null hyp. is rejected)
  if (p.value <= 0.05) {
    cat("\n\nThere is a significant difference between the levels.")
  } else {
    print("\n\nThere is no significant difference between the levels.")
  }
}

# returns the difference of the mean response between the 2 groups
abs_mean_difference <- function(df, response, group) {
  obs_mean <- tapply(df[, response], df[, group], mean)  # mean response for each group
  obs_diff <- abs(as.numeric(obs_mean[1] - obs_mean[2])) # absolute difference between means
  return (obs_diff)                                      # return absolute mean difference
}

# shuffles the order of values in a column
shuffle_labels <- function(df, column) {
  labels <- df[, column]    # subset the group from the dataframe
  num_obs <- length(labels) # number of observations
  
  shuffled_groups <- sample(labels, num_obs, replace=TRUE) # shuffle groups w/ replacement
  df$shuffled_groups <- shuffled_groups # add a new column to the dataframe w/ new groups
  
  return(df) # return updated dataframe
}

# simulates mean difference 1 time
mean_difference_sim <- function(df, response, group) {
  df <- shuffle_labels(df, group) # shuffle the groups
  
  # calculate the mean difference between the two new groups
  diff <- abs_mean_difference(df, response, "shuffled_groups")
  
  return (diff) # return mean difference
}

# simulates mean difference n times
n_mean_difference_sims <- function(n, df, response, group) {
  all_diffs <- c()                                   # holds all simulated differences
  
  for (i in 1:n) {                                   # for each iteration
    diff <- mean_difference_sim(df, response, group) # simulate mean difference
    all_diffs[i] <- diff                             # store in all_diffs
  }
  
  hist(all_diffs, breaks=20, # histogram of simulated differences
       main="Histogram of simulated mean differences",
       xlab="Simulated mean differences")
  return (all_diffs)         # return the simulated differences
}

perm_test(1000, data, "totalyearlycompensation", "Education")

# Find the correlation between years at company and total annual compensation for our sample
cor.test(data$yearsofexperience,       # correlation between years of experience
         data$totalyearlycompensation, # and yearly compensation
         alternative = 'two.sided',    # two-sided test
         exact=FALSE,                  # avoid ties in data
         method = 'spearman')$estimate # get the Spearman's correlation estimate

# Obtain more samples from the population using bootstrapping
bootstrap_cor <- function() {
  re_sample <- data[sample(nrow(data), replace = TRUE),]
  cor <- cor.test(re_sample$yearsofexperience, 
                  re_sample$totalyearlycompensation,
                  alternative = 'two.sided', 
                  exact=FALSE,
                  method = 'spearman')$estimate
  return (cor)
}

# Do 1000 repetitions
set.seed(1)
n <- 1000
sample_cors <- c()
for (i in 1:n){
  sample_cors[i] <- bootstrap_cor()
}

# Plot a histogram to visualize
hist(sample_cors, main = "Histogram of Correlations from Bootstrap Samples")

# Construct a confidence interval for all sample correlations
CI_cor <- c(quantile(sample_cors, probs = 0.025), quantile(sample_cors, probs = 0.975))
CI_cor

# education level as the only predictor
fit1 <- lm(totalyearlycompensation ~ Education, data=data)
summary(fit1)

# years of experience as the only predictors
fit2 <- lm(totalyearlycompensation ~ yearsofexperience, data=data)
summary(fit2)

# education and years of experience as predictors
fit3 <- lm(totalyearlycompensation ~ Education + yearsofexperience, data=data)
summary(fit3)

# education, years of experience, and their interaction as predictors
fit4 <- lm(totalyearlycompensation ~ Education * yearsofexperience, data=data)
summary(fit4)

cbind(
  AIC=AIC(fit1, fit2, fit3, fit4)$AIC,
  BIC=BIC(fit1, fit2, fit3, fit4)$BIC
)


