# Building simulations

# Linear model with random variation.
# Categorical predictor, continuous response.

# Set seed for reproducibility
set.seed(37)

# Parameters of the simulation
ngroup <- 5 # The number of groups in the factor predictor
nrep <- 12 # Number of times the 'experiment' was repeated/measurements made

# Group effects are defined, but could be drawn from a random distribution if desired
b0 <- 15 # Intercept (the 'true' response for group 1, against which other groups are compared)
b1 <- 1.2 # b1 -> b4 are the differences between those groups and group 1 (the intercept)
b2 <- 2.7
b3 <- -1.8
b4 <- 0.2

sd <- 2.5 # Overall standard deviation

group <- rep(c("group1", "group2", "group3", "group4", "group5"), each = nrep) # A vector of group names, repeated
# for each of the replicates

experr <- rnorm(nrep, 0, sd) # Experimental observation variation

response <- b0 + b1*(group == "group2") + b2*(group == "group3") + b3*(group == "group4") + 
  b4*(group == "group5") + experr

responsedf <- data.frame(response, group)
head(responsedf)

m1 <- lm(response ~ group, data = responsedf)
summary(m1)

