# Space to test functions out
library(dplyr)
library(ggplot2)


meas_testfunc <- function(N, E, D, cutoff, MEsigma, Nsims){
  values <- matrix(nrow = Nsims, ncol = 8, dimnames = list(c(), c()))
  # values <- data.frame()
  results <- matrix(nrow = length(MEsigma), ncol = 9, dimnames = list(c(),c("ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs")))
  

  Ehigh <- ifelse(E > cutoff, 1, 0)
  Eerror <- c()
  Eprime <- c()
  Eprimehigh <- c()

  
  for (j in seq_along(MEsigma)){
    
    for (i in 1:Nsims){  
      Eerror <- rnorm(N, mean = 0, sd = MEsigma)
      Eprime <- E + Eerror
      Eprimehigh <- ifelse(Eprime > cutoff, 1, 0)
      
      
      values[i,1] <- sum(Eprimehigh==1 & Ehigh==1)/sum(Ehigh==1) #Se all
      values[i,2] <- sum(Eprimehigh[D==1]==1 & Ehigh[D==1]==1)/sum(Ehigh[D==1]==1) #Se D=1
      values[i,3] <- sum(Eprimehigh[D==0]==1 & Ehigh[D==0]==1)/sum(Ehigh[D==0]==1) #Se D=0
      
      values[i,4] <- sum(Eprimehigh==0 & Ehigh==0)/sum(Ehigh==0) #Sp all
      values[i,5] <- sum(Eprimehigh[D==1]==0 & Ehigh[D==1]==0)/sum(Ehigh[D==1]==0) #Sp D=1
      values[i,6] <- sum(Eprimehigh[D==0]==0 & Ehigh[D==0]==0)/sum(Ehigh[D==0]==0) #Sp D=0
      
      values[i,7] <- (sum(D[Ehigh==1]==1)/sum(Ehigh==1))/(sum(D[Ehigh==0]==1)/sum(Ehigh==0)) #RR based on true exposure dichot
      values[i,8] <- (sum(D[Eprimehigh==1]==1)/sum(Eprimehigh==1))/(sum(D[Eprimehigh==0]==1)/sum(Eprimehigh==0)) #RR measured
    }
    results[j,] <- cbind(MEsigma[[j]], mean(values[,1]), mean(values[,2]), mean(values[,3]), mean(values[,4]), mean(values[,5]), mean(values[,6]), mean(values[,7]), mean(values[,8]))
  }
  return(results)
}

# if distribution is UNIF, Eparam1 = E min, Eparam2 = E max
# if distribution is NORM, Eparam1 = E mean, Eparam2 = Esd
sim_testfunc <- function(N, distrib, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims){
  output <- matrix(nrow = length(MEsigma), ncol = 17, 
                   dimnames = list(c(),c("N", "Distrib", "Eparam1", "Eparam2", "cutoff", "a", "b", "Nsims", "ME", 
                                         "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue","RRobs")))
  E <- c()
  D <- c()
  cutoff <- cutoff
  MEsigma <- MEsigma
  NumSims <- Nsims
  # initializing for each iteration of disease 
  one_d <- list()
  all_means <- matrix(nrow = length(MEsigma), ncol=9)
  
  # the separation at 900 might be overkill, but I think will help make our results comparable to theirs
  # based on description of their exposure being evenly spaced
  if (distrib == "UNIF"){
    if (N < 900){E <- as.integer(seq(from = Eparam1, to = Eparam2, length.out = N)) 
    } 
    else if (N >= 900){E <- as.integer(runif(N, min = Eparam1, max = Eparam2)) }  
  }
  if (distrib == "NORM"){E <- as.integer(rnorm(N, mean = Eparam1, sd = Eparam2))
  }  
  
  for (i in 1:Nsims){
    E <- E
    D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) # this will be created Nsims times
    one_d[[i]] <- meas_testfunc(N, E, D, cutoff, MEsigma, NumSims)
    # each run outputs a matrix with row = length(MEsigma), col = 9
  }
  
  all_means <- Reduce("+", one_d)/ length(one_d)
  output <- as.data.frame(cbind(N, distrib, Eparam1, Eparam2, cutoff, a, b, Nsims, all_means))
  return(output)
}

orange <- c(100, 300, 500)
test_normal <- sim_testfunc(100, "NORM", 2050, 150, 2200, -4.5, 0.0019, orange, 500)
test_combo4 <- sim_testfunc(100, "NORM", 2200, 100, 2200, -4.5, 0.0019, orange, 1000)


meas_testfunc <- function(N, E, D, cutoff, MEsigma, Nsims){
  values <- matrix(nrow = Nsims, ncol = 8, dimnames = list(c(), c()))
 # temp_mat <- matrix(nrow = N, ncol = 14, dimnames = list(c(), c("E", "Ehigh", "D", "Eerror", "Eprime", "Eprimehigh")))
  temp_pop <- tibble()
  results <- matrix(nrow = length(MEsigma), ncol = 9, dimnames = list(c(),c("ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs")))
  
  
  Ehigh <- ifelse(E > cutoff, 1, 0)
  Eerror <- c()
  Eprime <- c()
  Eprimehigh <- c()
  
  
  for (j in seq_along(MEsigma)){
    
    for (i in 1:Nsims){  
      temp_mat[,1] <- E
      temp_mat[,2] <- Ehigh
      temp_mat[,3] <- D
      temp_mat[,4] <- rnorm(N, mean = 0, sd = MEsigma) # Eerror
      temp_mat[,5] <- E + Eerror #Eprime
      temp_mat[,6] <- ifelse(Eprime > cutoff, 1, 0) #Eprimehigh
      #tibble and mutate? 
      attach(temp_mat)
      values[i,1] <- sum(Eprimehigh==1 & Ehigh==1)/sum(Ehigh==1) #Se all
      values[i,2] <- sum(Eprimehigh[D==1]==1 & Ehigh[D==1]==1)/sum(Ehigh[D==1]==1) #Se D=1
      values[i,3] <- sum(Eprimehigh[D==0]==1 & Ehigh[D==0]==1)/sum(Ehigh[D==0]==1) #Se D=0
      
      values[i,4] <- sum(Eprimehigh==0 & Ehigh==0)/sum(Ehigh==0) #Sp all
      values[i,5] <- sum(Eprimehigh[D==1]==0 & Ehigh[D==1]==0)/sum(Ehigh[D==1]==0) #Sp D=1
      values[i,6] <- sum(Eprimehigh[D==0]==0 & Ehigh[D==0]==0)/sum(Ehigh[D==0]==0) #Sp D=0
      
      values[i,7] <- (sum(D[Ehigh==1]==1)/sum(Ehigh==1))/(sum(D[Ehigh==0]==1)/sum(Ehigh==0)) #RR based on true exposure dichot
      values[i,8] <- (sum(D[Eprimehigh==1]==1)/sum(Eprimehigh==1))/(sum(D[Eprimehigh==0]==1)/sum(Eprimehigh==0)) #RR measured
    }
    results[j,] <- cbind(MEsigma[[j]], mean(values[,1]), mean(values[,2]), mean(values[,3]), mean(values[,4]), mean(values[,5]), mean(values[,6]), mean(values[,7]), mean(values[,8]))
  }
  return(results)
}


meas_testfunc <- function(N, E, D, cutoff, MEsigma, Nsims){
  values <- matrix(nrow = Nsims, ncol = 8, dimnames = list(c(), c()))
  # temp_mat <- matrix(nrow = N, ncol = 14, dimnames = list(c(), c("E", "Ehigh", "D", "Eerror", "Eprime", "Eprimehigh")))
  temp_pop <- tibble()
  results <- matrix(nrow = length(MEsigma), ncol = 9, dimnames = list(c(),c("ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs")))
  
  
  for (j in seq_along(MEsigma)){
    
    for (i in 1:Nsims){  
      temp_pop(E = E, Ehigh = ifelse(E > cutoff, 1, 0), D = D, 
               Eerror = rnorm(N, mean = 0, sd = MEsigma), 
               Eprime = E + Eerror, Eprimehigh = ifelse(Eprime > cutoff, 1, 0)
      )
      values <- temp_pop %>% group_by(D, Ehigh, Eprimehigh) %>% summarise(n=n())
      %>% summarise()
      values[i,1] <- sum(Eprimehigh==1 & Ehigh==1)/sum(Ehigh==1) #Se all
      values[i,2] <- sum(Eprimehigh[D==1]==1 & Ehigh[D==1]==1)/sum(Ehigh[D==1]==1) #Se D=1
      values[i,3] <- sum(Eprimehigh[D==0]==1 & Ehigh[D==0]==1)/sum(Ehigh[D==0]==1) #Se D=0
      
      values[i,4] <- sum(Eprimehigh==0 & Ehigh==0)/sum(Ehigh==0) #Sp all
      values[i,5] <- sum(Eprimehigh[D==1]==0 & Ehigh[D==1]==0)/sum(Ehigh[D==1]==0) #Sp D=1
      values[i,6] <- sum(Eprimehigh[D==0]==0 & Ehigh[D==0]==0)/sum(Ehigh[D==0]==0) #Sp D=0
      
      values[i,7] <- (sum(D[Ehigh==1]==1)/sum(Ehigh==1))/(sum(D[Ehigh==0]==1)/sum(Ehigh==0)) #RR based on true exposure dichot
      values[i,8] <- (sum(D[Eprimehigh==1]==1)/sum(Eprimehigh==1))/(sum(D[Eprimehigh==0]==1)/sum(Eprimehigh==0)) #RR measured
    }
    results[j,] <- cbind(MEsigma[[j]], mean(values[,1]), mean(values[,2]), mean(values[,3]), mean(values[,4]), mean(values[,5]), mean(values[,6]), mean(values[,7]), mean(values[,8]))
  }
  return(results)
}

test_pop <- tibble(E = rnorm(1000,2200,300), Ehigh = ifelse(E > 2200, 1, 0), 
                   D = rbinom(n = 1000, size = 1, p = plogis(-4.5 + 0.0019*E)), 
                   Eerror = rnorm(1000, mean = 0, sd = 100), 
                   Eprime = E + Eerror, Eprimehigh = ifelse(Eprime > 2200, 1, 0))

test_pop %>% group_by(D) %>% summarise(sum(Ehigh), sum(Eprimehigh))
test_pop %>% summarise(sum(Ehigh), sum(Eprimehigh), sum(D))
test_count <- test_pop %>% group_by(D, Ehigh, Eprimehigh) %>% summarise(n=n()) 
test_over <- test_pop %>% group_by(Ehigh, Eprimehigh) %>% summarise(n=n()) 
Se_all = (test_over[2,3] + test_over[4,3])/ (test_over[1,3] + test_over[3,3])  
#### TODO

# if distribution is UNIF, Eparam1 = E min, Eparam2 = E max
# if distribution is NORM, Eparam1 = E mean, Eparam2 = Esd
sim_testfunc <- function(N, distrib, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims){
  output <- matrix(nrow = length(MEsigma), ncol = 17, 
                   dimnames = list(c(),c("N", "Distrib", "Eparam1", "Eparam2", "cutoff", "a", "b", "Nsims", "ME", 
                                         "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue","RRobs")))
  E <- c()
  D <- c()
  cutoff <- cutoff
  MEsigma <- MEsigma
  NumSims <- Nsims
  # initializing for each iteration of disease 
  one_d <- list()
  all_means <- matrix(nrow = length(MEsigma), ncol=9)
  
  # the separation at 900 might be overkill, but I think will help make our results comparable to theirs
  # based on description of their exposure being evenly spaced
  if (distrib == "UNIF"){
    if (N < 900){E <- as.integer(seq(from = Eparam1, to = Eparam2, length.out = N)) 
    } 
    else if (N >= 900){E <- as.integer(runif(N, min = Eparam1, max = Eparam2)) }  
  }
  if (distrib == "NORM"){E <- as.integer(rnorm(N, mean = Eparam1, sd = Eparam2))
  }  
  
  for (i in 1:Nsims){
    E <- E
    D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) # this will be created Nsims times
    one_d[[i]] <- meas_testfunc(N, E, D, cutoff, MEsigma, NumSims)
    # each run outputs a matrix with row = length(MEsigma), col = 9
  }
  
  all_means <- Reduce("+", one_d)/ length(one_d)
  output <- as.data.frame(cbind(N, distrib, Eparam1, Eparam2, cutoff, a, b, Nsims, all_means))
  return(output)
}


##################

unif_testfunc <- function(N, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims){
  output <- matrix(nrow = length(MEsigma), ncol = 17, 
                   dimnames = list(c(),c("N", "distribution", "Eparam1", "Eparam2", "cutoff", "a", "b", "Nsims", "ME", 
                                         "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue","RRobs")))
  E <- c()
  D <- c()
  one_d <- list()
  all_means <- matrix(nrow = length(MEsigma), ncol=9)
  
  # the separation at 900 might be overkill, but I think will help make our results comparable to theirs
  # based on description of their exposure being evenly spaced
    if (N < 900){E <- as.integer(seq(from = Eparam1, to = Eparam2, length.out = N)) 
    } else if (N >= 900){E <- as.integer(runif(N, min = Eparam1, max = Eparam2)) }  
  
  NumSims <- Nsims
  
  for (i in 1:Nsims){
    E <- E
    D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) # this will be created Nsims times
    one_d[[i]] <- fleabag(N, E, D, cutoff, MEsigma, NumSims)
    # each run outputs a matrix with row = length(MEsigma), col = 9
  }
  
  all_means <- Reduce("+", one_d)/ length(one_d)
  output <- as.data.frame(cbind(N, "UNIF", Eparam1, Eparam2, cutoff, a, b, Nsims, all_means))
  return(output)
}


norm_testfunc <- function(N, Eparam1, Eparam2, cutoff, a, b, MEsigma, Nsims){
  output <- matrix(nrow = length(MEsigma), ncol = 17, 
                   dimnames = list(c(),c("N", "distribution", "Eparam1", "Eparam2", "cutoff", "a", "b", "Nsims", "ME", 
                                         "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue","RRobs")))
  E <- c()
  D <- c()
  one_d <- list()
  all_means <- matrix(nrow = length(MEsigma), ncol=9)
  
  # the separation at 900 might be overkill, but I think will help make our results comparable to theirs
  # based on description of their exposure being evenly spaced
  if (N < 900){E <- as.integer(seq(from = Eparam1, to = Eparam2, length.out = N)) 
  } else if (N >= 900){E <- as.integer(runif(N, min = Eparam1, max = Eparam2)) }  
  
  NumSims <- Nsims
  
  for (i in 1:Nsims){
    E <- E
    D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) # this will be created Nsims times
    one_d[[i]] <- fleabag(N, E, D, cutoff, MEsigma, NumSims)
    # each run outputs a matrix with row = length(MEsigma), col = 9
  }
  
  all_means <- Reduce("+", one_d)/ length(one_d)
  output <- as.data.frame(cbind(N, "UNIF", Eparam1, Eparam2, cutoff, a, b, Nsims, all_means))
  return(output)
}



############### new attempt - function that calls the next function
#flegal_exp
tester2 <- tibble(E = as.integer(seq(from = 1600, to = 2500, length.out = 200)), 
                                D = rbinom(n = 200, size = 1, p = plogis(-4.5 + 0.0019*E)))
test_input2 <- measured_flegal(100, tester2$E, tester2$D, 2200, orange, 200)
test_input2[1,]

measured_flegal <- function(N, E, D, cutoff, MEsigma, Nsims){
  
  values <- matrix(nrow = Nsims, ncol = 8, dimnames = list(c(), c("Se", "SeD1", "SeD0", "Sp", "SpD1", "SpD0", "RRtrue", "RRobs")))
  results <- matrix(nrow = length(MEsigma), ncol = 9, 
                    dimnames = list(c(),c("ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs")))
 
  for (j in seq_along(MEsigma)){
    
    for (i in 1:Nsims){ # do I need to explicitly make these a matrix or assume it runs through these vectors? 
      Ehigh <- ifelse(E > cutoff, 1, 0)
      Eerror <- rnorm(N, mean = 0, sd = MEsigma)
      Eprime <- E + Eerror
      Eprimehigh <- ifelse(Eprime > cutoff, 1, 0)
      
      values[i,1] <- sum(Eprimehigh==1 & Ehigh==1)/ sum(Ehigh==1) #Se All
      values[i,2] <- sum(Eprimehigh[D==1]==1)/ sum(Ehigh[D==1]==1) #Se D = 1
      values[i,3] <- sum(Eprimehigh[D==0]==1)/ sum(Ehigh[D==0]==1)#Se D = 0
      
      values[i,4] <- sum(Eprimehigh==0 & Ehigh==0)/ sum(Ehigh==0) #Sp All
      values[i,5] <- sum(Eprimehigh[D==1]==0)/ sum(Ehigh[D==1]==0) #Sp D = 1
      values[i,6] <- sum(Eprimehigh[D==0]==0)/ sum(Ehigh[D==0]==0) #Sp D = 0
      
      values[i,7] <- (sum(Ehigh[D==1]==1)/sum(Ehigh==1))/(sum(Ehigh[D==1]==0)/sum(Ehigh==0)) #RR true
      values[i,8] <- (sum(Eprimehigh[D==1]==1)/sum(Eprimehigh==1))/(sum(Eprimehigh[D==1]==0)/sum(Eprimehigh==0)) #RR obs
    }
    results[j,] <- cbind(MEsigma[[j]], mean(values[,1]), mean(values[,2]), mean(values[,3]), mean(values[,4]), mean(values[,5]), mean(values[,6]), mean(values[,7]), mean(values[,8]))
  }
  return(results)
}

# alternate plan is to create E and use replicate??

simulate_flegal <- function(N, Emin, Emax, cutoff, a, b, MEsigma, Nsims){
    output <- matrix(nrow = length(MEsigma), ncol = 16, 
                         dimnames = list(c(),c("N", "Emin", "Emax", "cutoff", "a", "b", "Nsims", "ME", 
                                               "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs")))
    
    # initializing for each iteration of disease 
    one_d <- list()
    one_means <- matrix(nrow = length(MEsigma), ncol=9)
   # first <- matrix(nrow = length(Nsims), ncol = 9, 
                   #  dimnames = list(c(),c("ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs")))
    # second <- matrix(nrow = length(Nsims), ncol = 9, 
                    # dimnames = list(c(),c("ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs")))
    # third <- matrix(nrow = length(Nsims), ncol = 9, 
                  #  dimnames = list(c(),c("ME", "SeAll", "SeD1", "SeD0", "SpAll", "SpD1", "SpD0", "RRtrue", "RRobs"))))
    E <- as.integer(seq(from = Emin, to = Emax, length.out = N))
    
    for (i in 1:Nsims){
      D <- rbinom(n = N, size = 1, p = plogis(a + b*E)) # this will be created Nsims times
      one_d[[i]] <- measured_flegal(N, E, D, cutoff, MEsigma, Nsims)
    #  first[i,] <- measured_flegal(N, E, D, cutoff, MEsigma, Nsims)[1,]
     # se[i,] <- measured_flegal(N, E, D, cutoff, MEsigma, Nsims)[2,]
      # first[i,] <- measured_flegal(N, E, D, cutoff, MEsigma, Nsims)[3,]
      # this outputs a matrix with row = length(MEsigma), col = 9
    }
    
   one_means <- Reduce("+", one_d)/ length(one_d)
  #  first <- one_d[1] 
    # does my empty need to be Nsims x Nsims? 
   # for (j in seq_along(length(MEsigma))){
    output <-  cbind(N, Emin, Emax, cutoff, a, b, Nsims, one_means)
  #  }
    
    return(output)
}


testmat <- matrix(test_input)
testlist <- list(test_input, test_input2)
testmean <- Reduce("+", testlist)/length(testlist)

# function input: N, Emin, Emax, cutoff, a, b, MEsigma, Nsims
orange <- c(100, 300, 500)
test1 <- simulate_flegal(100, 1600, 2499, 2200, -4.5, 0.0019, orange, 200)

#  output[j,] <- cbind(N, Emin, Emax, cutoff, a, b, Nsims, mean(one_d[1,1]), mean(one_d[1,2]), mean(one_d[1,3]), 
# mean(one_d[1,4]), mean(one_d[1,5]), mean(one_d[1,6]), mean(one_d[1,7]), mean(one_d[1,8]), mean(one_d[1,9]))
for (j in seq_along(length(MEsigma))){
  output[j,] <- cbind(N, Emin, Emax, cutoff, a, b, Nsims, mean(one_d[j,1]), mean(one_d[j,2]), mean(one_d[j,3]), 
                      mean(one_d[j,4]), mean(one_d[j,5]), mean(one_d[j,6]), mean(one_d[j,7]), mean(one_d[j,8]), 
                      mean(one_d[j,9]))
}
return(output)


output[1,] <- cbind(N, Emin, Emax, cutoff, a, b, Nsims, mean(one_d[1,1]), mean(one_d[1,2]), mean(one_d[1,3]), 
                    mean(one_d[1,4]), mean(one_d[1,5]), mean(one_d[1,6]), mean(one_d[1,7]), mean(one_d[1,8]), 
                    mean(one_d[1,9]))




#### 
