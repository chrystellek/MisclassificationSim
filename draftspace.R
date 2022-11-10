# Space to test functions out
library(dplyr)
library(ggplot2)


# improving on this.. repeating E,D,E',classifciation over
simulate_flegal_test <- function(N, distrib, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims){
  E <- c()
  D <- c()
  Ehigh <- c()
  Eerror <- c()
  Eprime <- c()
  Eprimehigh <- c()
  
  values <- matrix(nrow = length(MEsigma), ncol = 11, dimnames = list(c(),c("MEsd", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs", "E prev", "Eprime prev")))
  results <- list()
  all_means <- matrix(nrow = length(MEsigma), ncol=11)
  output <- matrix(nrow = length(MEsigma), ncol = 19, dimnames = list(c(),c("N", "Distrib", "Eparam1", "Eparam2", "cutoff", "a", "b", "Nsims", "ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue","RRobs", "Eprev", "Eprimeprev")))
  
  for (j in 1:Nsims){
    if (distrib == "UNIF"){E <- as.integer(runif(N, min = Eparam1, max = Eparam2))}  
    if (distrib == "NORM"){E <- as.integer(rnorm(N, mean = Eparam1, sd = Eparam2))}
    if (distrib == "EVEN"){E <- as.integer(seq(from = Eparam1, to = Eparam2, length.out = N))}
    D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) 
    Ehigh <- ifelse(E > cutoff, 1, 0)
    # Ehigh_prev <- sum(Ehigh == 1)/N
    
    for (i in seq_along(MEsigma)){
      Eerror <- rnorm(N, mean = 0, sd = MEsigma)
      Eprime <- E + Eerror
      Eprimehigh <- ifelse(Eprime > cutoff, 1, 0)
      
      values[i,1] <- MEsigma[[i]] #this will be constant but want it to pass through
      values[i,2] <- sum(Eprimehigh[Ehigh==1]==1)/sum(Ehigh==1) #Se all
      values[i,3] <- sum(Eprimehigh[D==1]==1 & Ehigh[D==1]==1)/sum(Ehigh[D==1]==1) #Se D=1
      values[i,4] <- sum(Eprimehigh[D==0]==1 & Ehigh[D==0]==1)/sum(Ehigh[D==0]==1) #Se D=0
      values[i,5] <- sum(Eprimehigh[Ehigh==0]==0)/sum(Ehigh==0) #Sp all
      values[i,6] <- sum(Eprimehigh[D==1]==0 & Ehigh[D==1]==0)/sum(Ehigh[D==1]==0) #Sp D=1
      values[i,7] <- sum(Eprimehigh[D==0]==0 & Ehigh[D==0]==0)/sum(Ehigh[D==0]==0) #Sp D=0
      values[i,8] <- (sum(D[Ehigh==1]==1)/sum(Ehigh==1))/(sum(D[Ehigh==0]==1)/sum(Ehigh==0)) #RR
      values[i,9] <- (sum(D[Eprimehigh==1]==1)/sum(Eprimehigh==1))/(sum(D[Eprimehigh==0]==1)/sum(Eprimehigh==0)) #RR measured
      values[i,10] <- sum(Ehigh == 1)/N
      values[i,11] <- sum(Eprimehigh == 1)/N #measured exposure prevalence
    }
    results[[j]] <- values # list of 200 result matrices
  }
  all_means <- Reduce('+', results)/ length(results)
  output <- as.data.frame(cbind(N, distrib, Eparam1, Eparam2, cutoff, a, b, Nsims, all_means))
  return(output)
}
 # each_run <- cbind(mean(values[,9]), mean(values[,1]), mean(values[,2]), mean(values[,3]), mean(values[,4]), mean(values[,5]), mean(values[,6]), mean(values[,7]), mean(values[,8]))

newvar <- c(100, 300, 500)
#Function input: N, distrib, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims
 
set.seed(303)
simulate_flegal(100, "UNIF", 1600, 2499, 2200, -4.5, 0.0019, newvar, 200)

simulate_flegal_test(100, "EVEN", 1600, 2499, 2200, -4.5, 0.0019, newvar, 200)

testcase <- simulate_flegal(1000, "NORM", 2050, 150, 2200, -4.5, 0.0019, newvar, 100)


# Still getting different values... 
# is it the formulas? 
# this attempt is creating a tibble to ensure I am selecting correct people
sim_formulas_flegal <- function(N, distrib, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims){
  E <- c()
  D <- c()
  Ehigh <- c()
  Eerror <- c()
  Eprime <- c()
  Eprimehigh <- c()
  # results <- matrix(nrow = Nsims, ncol = 9, dimnames = list(c(), c()))
  
  values <- matrix(nrow = length(MEsigma), ncol = 11, dimnames = list(c(),c("MEsd", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs", "E prev", "Eprime prev")))
  results <- list()
  all_means <- matrix(nrow = length(MEsigma), ncol=11)
  output <- matrix(nrow = length(MEsigma), ncol = 19, dimnames = list(c(),c("N", "Distrib", "Eparam1", "Eparam2", "cutoff", "a", "b", "Nsims", "ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue","RRobs", "Eprev", "Eprimeprev")))
  
  for (j in 1:Nsims){
    if (distrib == "UNIF"){E <- as.integer(runif(N, min = Eparam1, max = Eparam2))}  
    if (distrib == "NORM"){E <- as.integer(rnorm(N, mean = Eparam1, sd = Eparam2))}
    if (distrib == "EVEN"){E <- as.integer(seq(from = Eparam1, to = Eparam2, length.out = N))}
    D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) 
    Ehigh <- ifelse(E > cutoff, 1, 0)
    # Ehigh_prev <- sum(Ehigh == 1)/N
    
    for (i in seq_along(MEsigma)){
      Eerror <- rnorm(N, mean = 0, sd = MEsigma)
      Eprime <- E + Eerror
      Eprimehigh <- ifelse(Eprime > cutoff, 1, 0)
      
      values[i,1] <- MEsigma[[i]] #this will be constant but want it to pass through
      values[i,2] <- sum(Eprimehigh==1)/sum(Ehigh==1) #Se all
      values[i,3] <- sum(Eprimehigh[D==1]==1)/sum(Ehigh[D==1]==1) #Se D=1
      values[i,4] <- sum(Eprimehigh[D==0]==1)/sum(Ehigh[D==0]==1) #Se D=0
      values[i,5] <- sum(Eprimehigh[Ehigh==0]==0)/sum(Ehigh==0) #Sp all
      values[i,6] <- sum(Eprimehigh[D==1]==0)/sum(Ehigh[D==1]==0) #Sp D=1
      values[i,7] <- sum(Eprimehigh[D==0]==0)/sum(Ehigh[D==0]==0) #Sp D=0
      values[i,8] <- (sum(D[Ehigh==1]==1)/sum(Ehigh==1))/(sum(D[Ehigh==0]==1)/sum(Ehigh==0)) #RR
      values[i,9] <- (sum(D[Eprimehigh==1]==1)/sum(Eprimehigh==1))/(sum(D[Eprimehigh==0]==1)/sum(Eprimehigh==0)) #RR measured
      values[i,10] <- sum(Ehigh == 1)/N
      values[i,11] <- sum(Eprimehigh == 1)/N #measured exposure prevalence
    }
    # how to make each loop results in 3 outputs
    results[[j]] <- values
  }
  all_means <- Reduce("+", results)/ length(results)
  output <- as.data.frame(cbind(N, distrib, Eparam1, Eparam2, cutoff, a, b, Nsims, all_means))
  return(output)
}
# each_run <- cbind(mean(values[,9]), mean(values[,1]), mean(values[,2]), mean(values[,3]), mean(values[,4]), mean(values[,5]), mean(values[,6]), mean(values[,7]), mean(values[,8]))
sim_formulas_flegal(100, "EVEN", 1600, 2499, 2200, -4.5, 0.0019, newvar, 1000)


sim_testfunc <- function(N, distrib, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims){
  E <- c()
  D <- c()
  Ehigh <- c()
  Eerror <- c()
  Eprime <- c()
  Eprimehigh <- c()
  
  values <- matrix(nrow = length(MEsigma), ncol = 11, dimnames = list(c(),c("MEsd", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs", "E prev", "Eprime prev")))
  fakepop <- tibble()
  popsum <- tibble()
  results <- list()
  all_means <- matrix(nrow = length(MEsigma), ncol=11)
  output <- matrix(nrow = length(MEsigma), ncol = 19, dimnames = list(c(),c("N", "Distrib", "Eparam1", "Eparam2", "cutoff", "a", "b", "Nsims", "ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue","RRobs", "Eprev", "Eprimeprev")))
  
  for (j in 1:Nsims){
    if (distrib == "UNIF"){E <- as.integer(runif(N, min = Eparam1, max = Eparam2))}  
    if (distrib == "NORM"){E <- as.integer(rnorm(N, mean = Eparam1, sd = Eparam2))}
    if (distrib == "EVEN"){E <- as.integer(seq(from = Eparam1, to = Eparam2, length.out = N))}
    D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) 
    Ehigh <- ifelse(E > cutoff, 1, 0)
    
    for (i in seq_along(MEsigma)){
      fakepop <- tibble(E = E, Ehigh = Ehigh, D = D, 
                             Eerror = rnorm(N, mean = 0, sd = MEsigma), Eprime = E + Eerror,
                             Eprimehigh = ifelse(Eprime > cutoff, 1, 0))
      #popsum[[i]] <- fakepop[[i]] %>% group_by(D, Ehigh, Eprimehigh) %>% summarise(n=n(), .groups = "keep") 
      
      values[i,1] <- MEsigma[[i]] #this will be constant but want it to pass through
      values[i,2] <- sum(fakepop$Eprimehigh[Ehigh==1]==1)/sum(fakepop$Ehigh==1) #Se all
      values[i,3] <- sum(fakepop$Eprimehigh[D==1]==1 & fakepop$Ehigh[D==1]==1)/sum(fakepop$Ehigh[D==1]==1) #Se D=1
      values[i,4] <- sum(fakepop$Eprimehigh[D==0]==1 & fakepop$Ehigh[D==0]==1)/sum(fakepop$Ehigh[D==0]==1) #Se D=0
      values[i,5] <- sum(fakepop$Eprimehigh[Ehigh==0]==0)/sum(fakepop$Ehigh==0) #Sp all
      values[i,6] <- sum(fakepop$Eprimehigh[D==1]==0 & fakepop$Ehigh[D==1]==0)/sum(fakepop$Ehigh[D==1]==0) #Sp D=1
      values[i,7] <- sum(Eprimehigh[D==0]==0 & fakepop$Ehigh[D==0]==0)/sum(fakepop$Ehigh[D==0]==0) #Sp D=0
      values[i,8] <- (sum(fakepop$D[Ehigh==1]==1)/sum(fakepop$Ehigh==1))/(sum(fakepop$D[Ehigh==0]==1)/sum(fakepop$Ehigh==0)) #RR
      values[i,9] <- (sum(fakepop$D[Eprimehigh==1]==1)/sum(fakepop$Eprimehigh==1))/(sum(fakepop$D[Eprimehigh==0]==1)/sum(fakepop$Eprimehigh==0)) #RR measured
      values[i,10] <- sum(fakepop$Ehigh == 1)/N
      values[i,11] <- sum(fakepop$Eprimehigh == 1)/N #measured exposure prevalence
      
    }
    results[[j]] <- values
  }
  all_means <- Reduce("+", results)/ length(results)
  output <- as.data.frame(cbind(N, distrib, Eparam1, Eparam2, cutoff, a, b, Nsims, all_means))
  return(output)
}

sim_testfunc(100, "EVEN", 1600, 2499, 2200, -4.5, 0.0019, newvar, 2000)

# another way to call variables - didn't quite work
test_pop <- tibble(E = seq(from = 1600, to = 2499, length.out = 100), Ehigh = ifelse(E > 2200, 1, 0), 
                   D = rbinom(n = 100, size = 1, p = plogis(-4.5 + 0.0019*E)), 
                   Eerror = rnorm(100, mean = 0, sd = 100), 
                   Eprime = E + Eerror, Eprimehigh = ifelse(Eprime > 2200, 1, 0))

test_pop %>% group_by(D) %>% summarise(sum(Ehigh), sum(Eprimehigh))
test_pop %>% summarise(sum(Ehigh), sum(Eprimehigh), sum(D))
test_d <- test_pop %>% group_by(D, Ehigh, Eprimehigh) %>% summarise(n=n(), .groups = "keep") 
test_over <- test_pop %>% group_by(Ehigh, Eprimehigh) %>% summarise(n=n()) 
(test_d[4,4] + test_d[8,4])/ (test_d[4,4] + test_d[3,4] + test_d[7,4] + test_d[8,4])  
(test_d[1,4] + test_d[5,4])/ (test_d[1,4] + test_d[2,4] + test_d[5,4] + test_d[6,4])  
test_d[1,4]


#####################
# flegal_sim is what I used to re-create simulation. was able to get similar results to Flegal. 
f_means <- vector()
f_one <- f_two <- f_three <- list()
#matrix(nrow=10, ncol=14, dimnames = list(c(), c("N", "cutoff", "a", "b", "Nsim", "SD", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs")))

fall <- data.frame()

for (i in 1:10){
  f_one[[i]] <- flegal_sim(200, 1600, 2499, 2200, -4.5, 0.0019, 100, 200)
  f_two[[i]] <- flegal_sim(200, 1600, 2499, 2200, -4.5, 0.0019, 300, 200)
  f_three[[i]] <- flegal_sim(200, 1600, 2499, 2200, -4.5, 0.0019, 500, 200)
 # fall <- rbind(f_one[i,], f_two[i,], f_three[i,])
}

one_means <- Reduce("+", f_one)/ length(f_one)
two_means <- Reduce("+", f_two)/ length(f_two)
three_means <- Reduce("+", f_three)/ length(f_three)

#f_out <- as.data.frame(cbind(N, distrib, Eparam1, Eparam2, cutoff, a, b, Nsims, all_means))

assdf <- flegal_sim(200, 1600, 2499, 2200, -4.5, 0.0019, 100, 200)

flegal_sim <- function(N, min, max, cutpt, a, b, sigma, sims){
  # True exposure
  Se_all <- c()
  Se_D1 <- c()
  Se_D0 <- c()
  Sp_all <- c()
  Sp_D1 <- c()
  Sp_D0 <- c()
  RR_true <- c()
  RR_obs <- c()
  
  E <- seq(from = min, to = max, length.out = N) 
  E_high <- ifelse(E > cutpt, 1, 0)
  D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) 
  
  for (i in 1:sims){
    # set up
    Eerror <- rnorm(N, mean = 0, sd = sigma)
    Eprime <- E + Eerror
    Eprime_high <- ifelse(Eprime > cutpt,1, 0)
    
    # storing Se, Sp, RR values for each simulation
    Se_all[i] <- sum(Eprime_high==1 & E_high==1)/sum(E_high==1)
    Se_D1[i] <- sum(Eprime_high[D==1]==1 & E_high[D==1]==1)/sum(E_high[D==1]==1)
    Se_D0[i] <- sum(Eprime_high[D==0]==1 & E_high[D==0]==1)/sum(E_high[D==0]==1)
    
    Sp_all[i] <- sum(Eprime_high==0 & E_high==0)/sum(E_high==0)
    Sp_D1[i] <- sum(Eprime_high[D==1]==0 & E_high[D==1]==0)/sum(E_high[D==1]==0)
    Sp_D0[i] <- sum(Eprime_high[D==0]==0 & E_high[D==0]==0)/sum(E_high[D==0]==0)
    
    RR_true[i] <- ((sum(D==1 & E_high==1)/sum(E_high==1))/(sum(D==1 & E_high==0)/sum(E_high==0)))
    RR_obs[i] <- ((sum(D==1 & Eprime_high==1)/sum(Eprime_high==1))/(sum(D==1 & Eprime_high==0)/sum(Eprime_high==0)))
  }
  
  params <- data.frame(N, cutpt, a, b, sims, sigma, mean(Se_all), mean(Se_D1), mean(Se_D0), mean(Sp_all), mean(Sp_D1), mean(Sp_D0), mean(RR_true), mean(RR_obs))
  
  colnames(params) <- c("N", "cutoff", "a", "b", "Nsim", "SD", "Se all", "Se D=1", "Se D=0", "Sp all", "Sp D=1", "Sp D=0", "RR true", "RR obs")
  return(params)
}

