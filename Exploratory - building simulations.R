##############################
#### Building simulations ####
##############################

# Linear model with random variation

# Categorical predictor, continuous response

# Set seed for reproducibility
set.seed(37)

# Parameters of the simulation
ngroup <- 5 # The number of groups in the factor predictor
nrep <- 12 # Number of times the 'experiment' is repeated/measurements made (increase/decrease replication)

# Group effects are defined, but could be drawn from a random distribution if desired
b0 <- 15 # Intercept (the 'true' response for group 1, against which other groups are compared)
b1 <- 1.2 # b1 -> b4 are the differences between those groups and group 1 (the intercept)
b2 <- 2.7
b3 <- -1.8
b4 <- 0.2

sd <- 2.5 # Overall standard deviation

group <- as.factor(rep(c("group1", "group2", "group3", "group4", "group5"), each = nrep)) # A vector of group names, repeated
# for each of the replicates

experr <- rnorm(nrep*ngroup, 0, sd) # Experimental observation variation

response <- b0 + b1*(group == "group2") + b2*(group == "group3") + b3*(group == "group4") + 
  b4*(group == "group5") + experr # Create a list of responses as the sum of the effects of each factor added to the
# intercept, plus error (stochasticity) centred on zero, with the specified standard deviation

responsedf <- data.frame(response, group) # Dataframe combining the response and group names

# The model
m1 <- lm(response ~ group, data = responsedf) # Linear model to recapture the effect sizes and residual error
summary(m1) # Effect estimates improve with increased replication, nrep

# Function to run the simulation multiple times with different seeds (random), in order to investigate the impact of
# replicate number on estimates. The parameters can be modified in the function. 

# The following packages make parameter extraction easier:
library(broom)
library(dplyr)
library(purrr)
library(ggplot2)

sim_fun_1 <- function(ngroup = 5, nrep = 12, b0 = 15, b1 = 1.2, b2 = 2.7, b3 = -1.8, b4 = 0.2, sd = 2.5){
  group <- as.factor(rep(c("group1", "group2", "group3", "group4", "group5"), each = nrep))
  experr <- rnorm(nrep*ngroup, 0, sd)
  response <- b0 + b1*(group == "group2") + b2*(group == "group3") + b3*(group == "group4") + 
  b4*(group == "group5") + experr
  responsedf <- data.frame(response, group)
  ms1 <- lm(response ~ group, data = responsedf)
  print(ms1)
}
  
sim_fun_1()

simulations_1 <- replicate(1e3, sim_fun_1(), simplify = F)

simulations_1 %>% 
  map_df(tidy) %>% 
  filter(term == "group2") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = -0.05)




