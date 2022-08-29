# Scatterplots of one simulation for each scenario we ran through
# may convert this to Rmd for the sake of chunks
# could also make a function to plot these but that can be at a later time
library(dplyr)
library(ggplot2)
library(gridExtra)

set.seed(101)
pop1 <- tibble(
  E = as.integer(runif(1000, 1600, 2499)),  
  Eerror100 = rnorm(1000, mean = 0, sd = 100), Eprime100 = as.integer(E + Eerror100), 
  Eerror300 = rnorm(1000, mean = 0, sd = 300), Eprime300 = as.integer(E + Eerror300), 
  Eerror500 = rnorm(1000, mean = 0, sd = 500), Eprime500 = as.integer(E + Eerror500), 
  E_high = ifelse(E > 2200, 1, 0), Eprime100_high = ifelse(Eprime100 > 2200, 1, 0), 
  Eprime300_high = ifelse(Eprime300 > 2200, 1, 0), Eprime500_high = ifelse(Eprime500 > 2200, 1, 0), 
  DA = as.factor(rbinom(n = 1000, size = 1, p = plogis(-4.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = 1000, size = 1, p = plogis(-8.0 + 0.0035*E))),
  DC = as.factor(rbinom(n = 1000, size = 1, p = plogis(-16.0 + 0.0072*E)))
)

range(pop1$Eprime500)
# These are exposures ~Uniform, ME ~(0, 100), RR= 1.66
Scatter1_100 <- ggplot(pop1, aes(x = E, y = Eprime100, color = DA)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(2400, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "A", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 
  
Scatter1_300 <- ggplot(pop1, aes(x = E, y = Eprime300, color = DA)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(1200, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "B", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 

Scatter1_500 <- ggplot(pop1, aes(x = E, y = Eprime500, color = DA)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(1200, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "C", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 

  
Scatter1B <- ggplot(pop1, aes(x = E, y = Eprime, color = DB)) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(expand = c(0, 0), limits = c(1300, 2900), breaks = seq(1400,3000, 200)) + theme_bw() + labs(title = "B", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed()

Scatter1C <- ggplot(pop1, aes(x = E, y = Eprime, color = DC)) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(expand = c(0, 0), limits = c(1300, 2900), breaks = seq(1400,3000, 200)) + theme_bw() + labs(title = "C", x = "True Value", y = "Measured Value ") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed()


# For now I am creating one plot at a time. It is probably possible to do this more efficiently (maybe long format vs. wide) but that is a problem for a later time...



# A. a = -4.5, b = 0.0019 
# B. a = -8.0, b = 0.0035
# 3. a = -16.0, b = 0.0072
set.seed(101)
pop5 <- tibble(
  E = rnorm(1000, mean = 2050, sd = 150), 
  Eerror100 = rnorm(1000, mean = 0, sd = 100), Eprime100 = E + Eerror100, 
  Eerror300 = rnorm(1000, mean = 0, sd = 300), Eprime300 = E + Eerror300, 
  Eerror500 = rnorm(1000, mean = 0, sd = 500), Eprime500 = E + Eerror500, 
  E_high = ifelse(E > 2200, 1, 0), Eprime100_high = ifelse(Eprime100 > 2200, 1, 0), 
  Eprime300_high = ifelse(Eprime300 > 2200, 1, 0), Eprime500_high = ifelse(Eprime500 > 2200, 1, 0), 
  DA = as.factor(rbinom(n = 1000, size = 1, p = plogis(-4.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = 1000, size = 1, p = plogis(-8.0 + 0.0035*E))),
  DC = as.factor(rbinom(n = 1000, size = 1, p = plogis(-16.0 + 0.0072*E)))
)

maths <- pop5 %>% summarise(
  RRtrue5 = (sum(DC==1 & E_high==1)/sum(E_high==1))/(sum(DC==1 & E_high==0)/sum(E_high==0)),
  RRobs500 = (sum(DC==1 & Eprime500_high==1)/sum(Eprime500_high==1))/(sum(DC==1 & Eprime500_high==0)/sum(Eprime500_high==0))
)
# These are exposures ~E(2050, 150), ME ~(0, 100), RR= 1.66
Scatter5A_100 <- ggplot(pop5, aes(x = E, y = Eprime100, color = DA)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(1200, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "D", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + theme_bw() + coord_fixed()

Scatter5A_300 <- ggplot(pop5, aes(x = E, y = Eprime300, color = DA)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(1200, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "E", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + theme_bw() + coord_fixed()

Scatter5A_500 <- ggplot(pop5, aes(x = E, y = Eprime500, color = DA)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(1200, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "F", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + theme_bw() + coord_fixed()

combo1 <- grid.arrange(Scatter1_100, Scatter1_300, Scatter1_500, Scatter5A_100, Scatter5A_300, Scatter5A_500, ncol = 3)

ggsave("plots1.png", combo1,  width = 13, height = 8, units = "in")
### 






# case 6: E <- rnorm(n, mean = 2200, sd = 150)
pop6 <- data.frame(
  E = rnorm(1000, mean = 2200, sd = 150), 
  Eerror = rnorm(1000, mean = 0, sd = 100), 
  Eprime = E + Eerror, 
  E_high = ifelse(E > 2200, 1, 0), 
  Eprime_high = ifelse(Eprime > 2200, 1, 0), 
  E_mis = ifelse(E_high != Eprime_high, 1, 0),
  DA = as.factor(rbinom(n = 1000, size = 1, p = plogis(-4.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = 1000, size = 1, p = plogis(-8.0 + 0.0035*E))),
  DC = as.factor(rbinom(n = 1000, size = 1, p = plogis(-16.0 + 0.0072*E)))
)

Scatter6A <- ggplot(pop_6, aes(x = E, y = Eprime, color = DA)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400,2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400,2800, 200)) + theme_bw() + labs(title = "E ~Normal(2200, 150), RR = 1.66", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")

Scatter6B <- ggplot(pop_6, aes(x = E, y = Eprime, color = DB)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400,2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400,2800, 200)) + theme_bw() + labs(title = "E ~Normal(2200, 150), RR = 2.49", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")

Scatter6C <- ggplot(pop_6, aes(x = E, y = Eprime, color = DC)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400,2800, 200)) + theme_bw() + labs(title = "E ~Normal(2200, 150), RR = 5.00", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")


# case7: E ~N(mean = 1900, sd = 150) rare exposure
pop7 <- data.frame(
  E = rnorm(1000, mean = 1900, sd = 150), 
  Eerror = rnorm(1000, mean = 0, sd = 100), 
  Eprime = E + Eerror, 
  E_high = ifelse(E > 2200, 1, 0), 
  Eprime_high = ifelse(Eprime > 2200, 1, 0), 
  E_mis = ifelse(E_high != Eprime_high, 1, 0),
  DA = as.factor(rbinom(n = 1000, size = 1, p = plogis(-4.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = 1000, size = 1, p = plogis(-8.0 + 0.0035*E))),
  DC = as.factor(rbinom(n = 1000, size = 1, p = plogis(-16.0 + 0.0072*E)))
)

Scatter7A <- ggplot(pop_7, aes(x = E, y = Eprime, color = DA)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + theme_bw() + labs(title = "E ~Normal(1900, 150), RR = 1.66", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")

Scatter7B <- ggplot(pop_7, aes(x = E, y = Eprime, color = DB)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + theme_bw() + labs(title = "E ~Normal(1900, 150), RR = 2.49", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")

Scatter7C <- ggplot(pop_7, aes(x = E, y = Eprime, color = DC)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400,2800, 200)) + theme_bw() + labs(title = "E ~Normal(1900, 150), RR = 5.00", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")


# uniform but n = 1000
pop8 <- data.frame(
  E = runif(1000, min = 1600, max = 2499),
  Eerror = rnorm(1000, mean = 0, sd = 100), 
  Eprime = E + Eerror, 
  E_high = ifelse(E > 2200, 1, 0), 
  Eprime_high = ifelse(Eprime > 2200, 1, 0), 
  E_mis = ifelse(E_high != Eprime_high, 1, 0),
  DA = as.factor(rbinom(n = 1000, size = 1, p = plogis(-4.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = 1000, size = 1, p = plogis(-8.0 + 0.0035*E))),
  DC = as.factor(rbinom(n = 1000, size = 1, p = plogis(-16.0 + 0.0072*E)))
)

Scatter8A <- ggplot(pop_8, aes(x = E, y = Eprime, color = DA)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400,2800, 200)) + theme_bw() + labs(title = "Uniform with n = 1,000, RR = 1.66", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")

Scatter8B <- ggplot(pop_8, aes(x = E, y = Eprime, color = DB)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + theme_bw() + labs(title = "Uniform with n = 1,000, RR = 2.49", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")

Scatter8C <- ggplot(pop_8, aes(x = E, y = Eprime, color = DC)) + geom_point() + scale_x_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + scale_y_continuous(limits = c(1400, 2800), breaks = seq(1400, 2800, 200)) + theme_bw() + labs(title = "Uniform with n = 1,000, RR = 5.00", x = "True Value", y = "Measured Value", color = "Disease" ) + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed")


Scatter1C_100 <- ggplot(pop1, aes(x = E, y = Eprime100, color = DC)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(2400, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "A", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 

Scatter1C_300 <- ggplot(pop1, aes(x = E, y = Eprime300, color = DC)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(1200, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "B", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 

Scatter1C_500 <- ggplot(pop1, aes(x = E, y = Eprime500, color = DC)) + geom_point(alpha = 0.5) + scale_x_continuous(expand = c(0, 0), limits = c(1200, 2800), breaks = seq(1200, 2800, 400)) + scale_y_continuous(expand = c(0, 0), limits = c(200, 3800), breaks = seq(200, 4000, 400)) + labs(title = "C", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 

Scatter5C_100 <- ggplot(pop5, aes(x = E, y = Eprime100, color = DC)) + geom_point(alpha = 0.5) + scale_x_continuous(limits = c(1200, 2800), breaks = seq(1200, 3800, 400)) + scale_y_continuous(limits = c(200, 3800), breaks = seq(200, 4000, 200)) + theme_bw() + labs(title = "D", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 

Scatter5C_300 <- ggplot(pop5, aes(x = E, y = Eprime300, color = DC)) + geom_point(alpha = 0.5) + scale_x_continuous(limits = c(1200, 2800), breaks = seq(1200, 3800, 400)) + scale_y_continuous(limits = c(200, 3800), breaks = seq(200, 4000, 200)) + theme_bw() + labs(title = "E", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 

Scatter5C_500 <- ggplot(pop5, aes(x = E, y = Eprime500, color = DC)) + geom_point(alpha = 0.5) + scale_x_continuous(limits = c(1200, 2800), breaks = seq(1200, 3800, 400)) + scale_y_continuous(limits = c(200, 3800), breaks = seq(200, 4000, 200)) + theme_bw() + labs(title = "F", x = "True Value", y = "Measured Value", color = "Disease") + geom_vline(xintercept = 2200) + geom_hline(yintercept = 2200, linetype = "dashed") + coord_fixed() + theme_bw() 
  
combo1 <- grid.arrange(Scatter1C_100, Scatter1C_300, Scatter1C_500, Scatter5C_100, Scatter5C_300, Scatter5C_500, ncol = 3)

ggsave("plots3.png", combo1,  width = 11, height = 8, units = "in")
### 
  


poptest <- tibble(
  E = rnorm(1000, mean = 2050, sd = 150), 
  Eerror100 = rnorm(1000, mean = 0, sd = 100), Eprime100 = E + Eerror100, 
  Eerror300 = rnorm(1000, mean = 0, sd = 300), Eprime300 = E + Eerror300, 
  Eerror500 = rnorm(1000, mean = 0, sd = 500), Eprime500 = E + Eerror500, 
  E_high = ifelse(E > 2200, 1, 0), Eprime100_high = ifelse(Eprime100 > 2200, 1, 0), 
  Eprime300_high = ifelse(Eprime300 > 2200, 1, 0), Eprime500_high = ifelse(Eprime500 > 2200, 1, 0), 
  DA = as.factor(rbinom(n = 1000, size = 1, p = plogis(-4.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = 1000, size = 1, p = plogis(-8.0 + 0.0035*E))),
  DC = as.factor(rbinom(n = 1000, size = 1, p = plogis(-16.0 + 0.0072*E)))
)

mathtests <- poptest %>% summarise(
  RRtrue5 = (sum(DC==1 & E_high==1)/sum(E_high==1))/(sum(DC==1 & E_high==0)/sum(E_high==0)),
  RRobs500 = (sum(DC==1 & Eprime500_high==1)/sum(Eprime500_high==1))/(sum(DC==1 & Eprime500_high==0)/sum(Eprime500_high==0))
)


math1 <- pop1 %>% summarise(
  RRtrue5 = (sum(DC==1 & E_high==1)/sum(E_high==1))/(sum(DC==1 & E_high==0)/sum(E_high==0)),
  RRobs500 = (sum(DC==1 & Eprime500_high==1)/sum(Eprime500_high==1))/(sum(DC==1 & Eprime500_high==0)/sum(Eprime500_high==0))
)

math2 <- pop5 %>% summarise(
  RRtrue5 = (sum(DC==1 & E_high==1)/sum(E_high==1))/(sum(DC==1 & E_high==0)/sum(E_high==0)),
  RRobs500 = (sum(DC==1 & Eprime500_high==1)/sum(Eprime500_high==1))/(sum(DC==1 & Eprime500_high==0)/sum(Eprime500_high==0))
)

math1A <- pop1 %>% summarise(
  RRtrue5 = (sum(DA==1 & E_high==1)/sum(E_high==1))/(sum(DC==1 & E_high==0)/sum(E_high==0)),
)

math5A <- pop5 %>% summarise(
  RRtrueA = (sum(DA==1 & E_high==1)/sum(E_high==1))/(sum(DA==1 & E_high==0)/sum(E_high==0)),
)
