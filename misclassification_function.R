# Chrystelle Kiang
# this code creates a function that builds on the example by Flegal et al. 
# then runs examples from paper
library(here)
library(tidyverse)

#######################################
# function inputs: 
# N = sample size of population
# distrib = distribution of exposure. Options: "uniform" "normal" or "even"
# Eparam1 and Eparam2 are based on distrib:
# if "uniform" or "even" then Eparam1 = min, Eparam2 = max
# if "normal" then Eparam1 = mean, Eparam2 = SD
# a and b = parameters for disease model ln(p/1-p)=a + b*E
# MEsigma = SD of error; should be vector of ME that will be cycled through for constant E,D
# Nsims = number of simulations

simulate_misclass <- function(N, distrib, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims){
  # initialize variables
  E <- c()
  D <- c()
  Ehigh <- c()
  Eerror <- c()
  Eprime <- c()
  Eprimehigh <- c()
  values <- matrix(nrow = length(MEsigma), 
                   ncol = 13, 
                   dimnames = list(c(),c("MEsd", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", 
                                         "RRtrue", "RRobs", "ORtrue", "ORobs", "Eprev", "Eprimeprev")))
  results <- list()
  all_means <- matrix(nrow = length(MEsigma), ncol=13)
  output <- matrix(nrow = length(MEsigma), 
                   ncol = 21, 
                   dimnames = list(c(),c("N", "Distrib", "Eparam1", "Eparam2", "cutoff", "a", "b", "Nsims", 
                                         "MEsd", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", 
                                         "RRtrue","RRobs", "ORtrue", "ORobs", "Eprev", "Eprimeprev")))
  
  for (j in 1:Nsims){
    # true exposure E, categorized Ehigh, and disease status for single simulation 
    if (distrib == "uniform"){E <- as.integer(runif(N, min = Eparam1, max = Eparam2))}  
    if (distrib == "normal"){E <- as.integer(rnorm(N, mean = Eparam1, sd = Eparam2))}
    if (distrib == "even"){E <- as.integer(seq(from = Eparam1, to = Eparam2, length.out = N))}
    
    D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) 
    Ehigh <- ifelse(E > cutoff, 1, 0)
    
    # for each simulation, cycle through values of given vector of ME SD values to generate measured exposure variables
    # then get sensitivity and specificity for each 
    for (i in seq_along(MEsigma)){
      Eerror <- rnorm(N, mean = 0, sd = MEsigma[i]) # error term
      Eprime <- E + Eerror # measured continuous exposure       
      Eprimehigh <- ifelse(Eprime > cutoff, 1, 0) # measured categorized exposure
      
      values[i,1] <- MEsigma[i] # this will be constant but want it to pass through
      values[i,2] <- sum(Eprimehigh[Ehigh==1]==1)/sum(Ehigh==1) #Se all
      values[i,3] <- sum(Eprimehigh[D==1]==1 & Ehigh[D==1]==1)/sum(Ehigh[D==1]==1) #Se D=1
      values[i,4] <- sum(Eprimehigh[D==0]==1 & Ehigh[D==0]==1)/sum(Ehigh[D==0]==1) #Se D=0
      values[i,5] <- sum(Eprimehigh[Ehigh==0]==0)/sum(Ehigh==0) #Sp all
      values[i,6] <- sum(Eprimehigh[D==1]==0 & Ehigh[D==1]==0)/sum(Ehigh[D==1]==0) #Sp D=1
      values[i,7] <- sum(Eprimehigh[D==0]==0 & Ehigh[D==0]==0)/sum(Ehigh[D==0]==0) #Sp D=0
      values[i,8] <- (sum(D[Ehigh==1]==1)/sum(Ehigh==1))/(sum(D[Ehigh==0]==1)/sum(Ehigh==0)) #RR true
      values[i,9] <- (sum(D[Eprimehigh==1]==1)/sum(Eprimehigh==1))/(sum(D[Eprimehigh==0]==1)/sum(Eprimehigh==0)) #RR measured
      values[i,10] <- (sum(D[Ehigh==1]==1)/sum(D[Ehigh==1]==0))/(sum(D[Ehigh==0]==1)/sum(D[Ehigh==0]==0)) #OR true
      values[i,11] <- (sum(D[Eprimehigh==1]==1)/sum(D[Eprimehigh==1]==0))/(sum(D[Eprimehigh==0]==1)/sum(D[Eprimehigh==0]==0)) #OR measured 
      values[i,12] <- sum(Ehigh == 1)/N # true exposure prev
      values[i,13] <- sum(Eprimehigh == 1)/N # measured exposure prevalence
    }
    results[[j]] <- values # store in j number of matrices 
  }
  all_means <- Reduce("+", results)/ length(results) # mean of each matrix for all simulations 
  output <- as.data.frame(cbind(N, distrib, Eparam1, Eparam2, cutoff, a, b, Nsims, all_means)) # retain info 
  return(output)
}

# Flegal et al originally varied:  
# standard deviation of measurement error: 100, 300, 500  
# three sets of (a, b) parameters for disease: (-4.5, 0.0019), (-8.0, 0.0035), (-16.0, 0.0072)  
# 200 simulations of each on the nine combinations  

# function arguments: N, distrib, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims
# if distribution is UNIF or EVEN, Eparam1 = E min, Eparam2 = E max
# if distribution is NORM, Eparam1 = E mean, Eparam2 = standard deviation of exposure

# These are all the scenarios we considered in the paper
# note that for normal exposure, a, b differ so can have approx same expected RRs 
simulation_parameters <- list(
  # repeating the simulation by Flegal et al.
  list(N = 100, distrib = "uniform", Eparam1 = 1600, Eparam2 = 2499, cutoff = 2200, a = -4.5, b = 0.0019, MEsigma = c(100, 300, 500), Nsims = 200),
  list(N = 100, distrib = "uniform", Eparam1 = 1600, Eparam2 = 2499, cutoff = 2200, a = -8.0, b = 0.0035, MEsigma = c(100, 300, 500), Nsims = 200),
  list(N = 100, distrib = "uniform", Eparam1 = 1600, Eparam2 = 2499, cutoff = 2200, a = -16.0, b = 0.0072, MEsigma = c(100, 300, 500), Nsims = 200),
 
   # repeating again, but with increased N and # simulations
  list(N = 10000, distrib = "uniform", Eparam1 = 1600, Eparam2 = 2499, cutoff = 2200, a = -4.5, b = 0.0019, MEsigma = c(100, 300, 500), Nsims = 1000),
  list(N = 10000, distrib = "uniform", Eparam1 = 1600, Eparam2 = 2499, cutoff = 2200, a = -8.0, b = 0.0035, MEsigma = c(100, 300, 500), Nsims = 1000),
  list(N = 10000, distrib = "uniform", Eparam1 = 1600, Eparam2 = 2499, cutoff = 2200, a = -16.0, b = 0.0072, MEsigma = c(100, 300, 500), Nsims = 1000),
  
  # Normal centered about high/low cut point
  list(N = 10000, distrib = "normal", Eparam1 = 2200, Eparam2 = 150, cutoff = 2200, a = -9.5, b = 0.0019, MEsigma = c(100, 300, 500), Nsims = 1000),
  list(N = 10000, distrib = "normal", Eparam1 = 2200, Eparam2 = 150, cutoff = 2200, a = -13, b = 0.0035, MEsigma = c(100, 300, 500), Nsims = 1000),
  list(N = 10000, distrib = "normal", Eparam1 = 2200, Eparam2 = 150, cutoff = 2200, a = -18.75, b = 0.0072, MEsigma = c(100, 300, 500), Nsims = 1000),
  
  # Normal centered about midpoint 
  list(N = 10000, distrib = "normal", Eparam1 = 2050, Eparam2 = 150, cutoff = 2200, a = -8.5, b = 0.0019, MEsigma = c(100, 300, 500), Nsims = 1000),
  list(N = 10000, distrib = "normal", Eparam1 = 2050, Eparam2 = 150, cutoff = 2200, a = -11.5, b = 0.0035, MEsigma = c(100, 300, 500), Nsims = 1000),
  list(N = 10000, distrib = "normal", Eparam1 = 2050, Eparam2 = 150, cutoff = 2200, a = -18.2, b = 0.0072, MEsigma = c(100, 300, 500), Nsims = 1000),
  
  # Normal(1900, 150) mimic rare exposure 
  list(N = 10000, distrib = "normal", Eparam1 = 1900, Eparam2 = 150, cutoff = 2200, a = -5.1, b = 0.0019, MEsigma = c(100, 300, 500), Nsims = 1000),
  list(N = 10000, distrib = "normal", Eparam1 = 1900, Eparam2 = 150, cutoff = 2200, a = -8.5, b = 0.0035, MEsigma = c(100, 300, 500), Nsims = 1000),
  list(N = 10000, distrib = "normal", Eparam1 = 1900, Eparam2 = 150, cutoff = 2200, a = -16.1, b = 0.0072, MEsigma = c(100, 300, 500), Nsims = 1000)
)
# one could do this differently; but this is how I chose to run all scenarios discussed in paper
# can create a new list and/or just run one at a time

# running the desired scenarios and compiling results from each scenario
set.seed(404) 
simulation_results <- lapply(simulation_parameters, function(param){
  result_matrix <- simulate_misclass(param$N, param$distrib, param$Eparam1, param$Eparam2, param$cutoff, 
                                     param$a, param$b, param$MEsigma, param$Nsims)
  result_matrix <- result_matrix %>% 
    mutate_at(c('SeD1', 'SeD0', 'SpD1', 'SpD0'), as.numeric) %>%
    mutate(SeDiff = SeD1 - SeD0, SpDiff = SpD1 - SpD0)
})
# if simulation_results is ran as-is, should get exact same results 
# expect to still get pretty close results anyway if N and Nsims are large


# code below is a clunky way to re-organize columns and prepare for export
# TODO clean up
wanted_order <- c("N", "distrib", "Eparam1", "Eparam2", "a",
                  "b", "Nsims", "MEsd", "SeAll", "SeD1", "SeD0", "SeDiff", 
                  "SpAll", "SpD1", "SpD0", "SpDiff", "RRtrue", "RRobs",
                  "ORtrue", "ORobs", "Eprev", "Eprimeprev")

reordered_simulation_results <- map(simulation_results, ~ .x %>% 
                                 select(all_of(wanted_order)) %>%
  unite(description, c("N", "distrib", "Eparam1", "Eparam2", "a", "b", "Nsims"), sep = "_"))

simulation_results_transposed <- lapply(reordered_simulation_results, t)

stacked_results <- do.call(rbind, simulation_results_transposed)

write.csv(stacked_results, file = "Aug11output.csv", row.names = TRUE)

all_simulation_results <- do.call(rbind, reordered_simulation_results)
write.csv(all_simulation_results, file = "AUg15output.csv")
View(all_simulation_results)

# ideal plan: import csv to quarto file and create table and etc there

#####################
# example code to run one scenario: 
simulate_misclass(N = 100, distrib = "even", Eparam1 = 1600, Eparam2 = 2499, cutoff = 2200, a = -4.5, b = 0.0019, MEsigma = c(100, 300, 500), Nsims = 200) 
simulate_misclass(N = 100, distrib = "uniform", Eparam1 = 1600, Eparam2 = 2499, cutoff = 2200, a = -4.5, b = 0.0019, MEsigma = c(100, 300, 500), Nsims = 200) 
simulate_misclass(N = 10000, distrib = "normal", Eparam1 = 2050, Eparam2 = 150, cutoff = 2200, a = -18.75, b = 0.0072, MEsigma = c(100, 300, 500), Nsims = 1000)

