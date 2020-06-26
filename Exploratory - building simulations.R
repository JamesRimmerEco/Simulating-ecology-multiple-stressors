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

################
################
################

# Function to run the simulation multiple times with different seeds (random), in order to investigate the impact of
# replicate number on estimates. The parameters can be modified in the function. 

# The following packages make parameter extraction easier:
library(broom)
library(dplyr)
library(purrr)
library(ggplot2)

#########
#########
#########

sim_fun_1 <- function(ngroup = 5, nrep = 12, b0 = 15, b1 = 1.2, b2 = 2.7, b3 = -1.8, b4 = 0.2, sd = 2.5){
  group <- as.factor(rep(c("group1", "group2", "group3", "group4", "group5"), each = nrep))
  experr <- rnorm(nrep*ngroup, 0, sd)
  response <- b0 + b1*(group == "group2") + b2*(group == "group3") + b3*(group == "group4") + 
  b4*(group == "group5") + experr
  responsedf <- data.frame(response, group)
  ms1 <- lm(response ~ group, data = responsedf)
  print(ms1)
}
  
sim_fun_1() # Test the function is generating random numbers each time

simulations_1 <- replicate(1e3, sim_fun_1(), simplify = F) # Run the simulation 1x10^x times

simulations_1 %>% # Extract the frequency of sigma and plot a line where the true value lies
    map_dbl(~summary(.x)$sigma) %>% 
    data.frame(sigma=.) %>% 
    ggplot( aes(sigma) ) +
    geom_density(fill = "blue", alpha = .5) + 
    geom_vline( xintercept = 2.5)

simulations_1 %>% # Calculate the number of times sigma was found within a certain range of values
    map_dbl(~summary(.x)$sigma) %>%
    {. < 2.5} %>%
    mean()

# Thought; from this distribution, extract the percentage of occurances within a certain range of the 'True' sigma as
# an indication of power?

####                                    ####
####            Estimating power        ####
####                                    ####

simulations_1 %>% # Extract the frequency of sigma and plot a line where the true value lies
  map_dbl(~summary(.x)$sigma) 

sim1sigmas <- (simulations_1 %>% 
                           map_dbl(~summary(.x)$sigma))

# Plotting these resulting densities
par(mar = c(3,4,2,2))
plot(density(sim1sigmas), xlim = c(0, 4), main = "", xlab = "")
abline(v = 2.5, lty = 2, col = "red", lwd = 2)
# Now, we want to find density intervals - the HDPI (highest posterior density interval) should be suitable
library(coda)
HPDinterval(as.mcmc(sim1sigmas), prob = 0.89)
sim1interval <- HPDinterval(as.mcmc(sim1sigmas), prob = 0.89)
sim1interval[1,1]
# Add these lines to the plot as intervals
abline(v = sim1interval[1,1], lty = 1, col = "black", lwd = 2)
abline(v = sim1interval[1,2], lty = 1, col = "black", lwd = 2)

# So 89% probability that sigma lies between 2.0898 and 2.8393 when the true sigma was 2.5 and nrep replicates were
# used

