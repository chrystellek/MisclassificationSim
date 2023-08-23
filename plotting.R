# Scatterplots of one simulation for each scenario we ran through
# may convert this to Rmd for the sake of chunks
# could also make a function to plot these but that can be at a later time
library(tidyverse)
library(ggplot2)
library(patchwork)


pop_size <- 100000
# one instance/dataset of uniform distribution
set.seed(3)
population_uniform <- tibble(
  E = as.integer(runif(pop_size, 1600, 2500)),  
  # EerrorX is error term for X SD; EprimeX is 'measured' exposure for X SD
  Eerror100 = rnorm(pop_size, mean = 0, sd = 100), Eprime100 = as.integer(E + Eerror100), 
  Eerror300 = rnorm(pop_size, mean = 0, sd = 300), Eprime300 = as.integer(E + Eerror300), 
  Eerror500 = rnorm(pop_size, mean = 0, sd = 500), Eprime500 = as.integer(E + Eerror500), 
  # high/low categories
  E_high = ifelse(E > 2200, 1, 0), Eprime100_high = ifelse(Eprime100 > 2200, 1, 0), 
  Eprime300_high = ifelse(Eprime300 > 2200, 1, 0), Eprime500_high = ifelse(Eprime500 > 2200, 1, 0), 
  # disease status for each RR: A=1.66, B=2.49, C=1.66
  DA = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-4.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-8.0 + 0.0035*E))),
  DC = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-16.0 + 0.0072*E)))
)

# creating one instance of normal distribution 
set.seed(3)
population_normal <- tibble(
  E = as.integer(rnorm(pop_size, 2050, 150)),  
  Eerror100 = rnorm(pop_size, mean = 0, sd = 100), Eprime100 = as.integer(E + Eerror100), 
  Eerror300 = rnorm(pop_size, mean = 0, sd = 300), Eprime300 = as.integer(E + Eerror300), 
  Eerror500 = rnorm(pop_size, mean = 0, sd = 500), Eprime500 = as.integer(E + Eerror500), 
  E_high = ifelse(E > 2200, 1, 0), Eprime100_high = ifelse(Eprime100 > 2200, 1, 0), 
  Eprime300_high = ifelse(Eprime300 > 2200, 1, 0), Eprime500_high = ifelse(Eprime500 > 2200, 1, 0), 
  DA = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-8.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-11.5 + 0.0035*E))),
  DC = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-18.2 + 0.0072*E)))
)

#getting the Se and Sp overall, difference, etc for the example cases; 
biasparams_norm <- population_normal %>% 
  summarize(
se_all = sum(Eprime100_high[E_high==1]==1)/sum(E_high==1), #Se all
se_d1 = sum(Eprime100_high[DC==1]==1 & E_high[DC==1]==1)/sum(E_high[DC==1]==1), #Se D=1
se_d0 = sum(Eprime100_high[DC==0]==1 & E_high[DC==0]==1)/sum(E_high[DC==0]==1), #Se D=0
sp_all = sum(Eprime100_high[E_high==0]==0)/sum(E_high==0), #Sp all
sp_d1 = sum(Eprime100_high[DC==1]==0 & E_high[DC==1]==0)/sum(E_high[DC==1]==0), #Sp D=1
sp_d0 = sum(Eprime100_high[DC==0]==0 & E_high[DC==0]==0)/sum(E_high[DC==0]==0), #Sp D=0
#count of those who are truly low but categorized as high:
mis_high_d1 = sum(Eprime100_high[DC==1]==1 & E_high[DC==1]==0),
primehigh_d1 = sum(Eprime100_high[DC==1]==1),
mis_high_d1_prop = mis_high_d1/primehigh_d1,
mis_high_d0 = sum(Eprime100_high[DC==0]==1 & E_high[DC==0]==0),
primehigh_d0 = sum(Eprime100_high[DC==0]==1),
mis_high_d0_prop = mis_high_d0/primehigh_d0,
mis_low_d1 = sum(Eprime100_high[DC==1]==0 & E_high[DC==1]==1), 
primelow_d1 = sum(Eprime100_high[DC==1]==0),
mis_low_d1_prop = mis_low_d1/primelow_d1,
mis_low_d0 = sum(Eprime100_high[DC==0]==0 & E_high[DC==0]==1),
primelow_d0 = sum(Eprime100_high[DC==0]==0),
mis_low_d0_prop = mis_low_d0/primelow_d0,
rr_true = (sum(DC[E_high==1]==1)/sum(E_high==1))/(sum(DC[E_high==0]==1)/sum(E_high==0)), #RR true
rr_obs = (sum(DC[Eprime100_high==1]==1)/sum(Eprime100_high==1))/(sum(DC[Eprime100_high==0]==1)/sum(Eprime100_high==0)), #RR measured
or_true = (sum(DC[E_high==1]==1)/sum(DC[E_high==1]==0))/(sum(DC[E_high==0]==1)/sum(DC[E_high==0]==0)), #OR true
or_obs = (sum(DC[Eprime100_high==1]==1)/sum(DC[Eprime100_high==1]==0))/(sum(DC[Eprime100_high==0]==1)/sum(DC[Eprime100_high==0]==0)) #OR measured 
) %>% mutate(distrib = "norm") 

biasparams_unif <- population_uniform  %>% 
  summarize(
    se_all = sum(Eprime100_high[E_high==1]==1)/sum(E_high==1), #Se all
    se_d1 = sum(Eprime100_high[DC==1]==1 & E_high[DC==1]==1)/sum(E_high[DC==1]==1), #Se D=1
    se_d0 = sum(Eprime100_high[DC==0]==1 & E_high[DC==0]==1)/sum(E_high[DC==0]==1), #Se D=0
    sp_all = sum(Eprime100_high[E_high==0]==0)/sum(E_high==0), #Sp all
    sp_d1 = sum(Eprime100_high[DC==1]==0 & E_high[DC==1]==0)/sum(E_high[DC==1]==0), #Sp D=1
    sp_d0 = sum(Eprime100_high[DC==0]==0 & E_high[DC==0]==0)/sum(E_high[DC==0]==0), #Sp D=0
    #count of those who are truly high/low but wrongly categorized as low/high:
    #proportions for the total number in each plot
    mis_high_d1 = sum(Eprime100_high[DC==1]==1 & E_high[DC==1]==0),
    primehigh_d1 = sum(Eprime100_high[DC==1]==1),
    mis_high_d1_prop = mis_high_d1/primehigh_d1,
    mis_high_d0 = sum(Eprime100_high[DC==0]==1 & E_high[DC==0]==0),
    primehigh_d0 = sum(Eprime100_high[DC==0]==1),
    mis_high_d0_prop = mis_high_d0/primehigh_d0,
    mis_low_d1 = sum(Eprime100_high[DC==1]==0 & E_high[DC==1]==1), 
    primelow_d1 = sum(Eprime100_high[DC==1]==0),
    mis_low_d1_prop = mis_low_d1/primelow_d1,
    mis_low_d0 = sum(Eprime100_high[DC==0]==0 & E_high[DC==0]==1),
    primelow_d0 = sum(Eprime100_high[DC==0]==0),
    mis_low_d0_prop = mis_low_d0/primelow_d0,
    rr_true = (sum(DC[E_high==1]==1)/sum(E_high==1))/(sum(DC[E_high==0]==1)/sum(E_high==0)), #RR true
    rr_obs = (sum(DC[Eprime100_high==1]==1)/sum(Eprime100_high==1))/(sum(DC[Eprime100_high==0]==1)/sum(Eprime100_high==0)), #RR measured
    or_true = (sum(DC[E_high==1]==1)/sum(DC[E_high==1]==0))/(sum(DC[E_high==0]==1)/sum(DC[E_high==0]==0)), #OR true
    or_obs = (sum(DC[Eprime100_high==1]==1)/sum(DC[Eprime100_high==1]==0))/(sum(DC[Eprime100_high==0]==1)/sum(DC[Eprime100_high==0]==0)) #OR measured 
  ) %>% 
  mutate(distrib = "unif") 

biasparams <- rbind(biasparams_norm, biasparams_unif)

biasparams <- biasparams %>%
  mutate(se_diff = se_d1 - se_d0, 
            sp_diff = sp_d1 - sp_d0,
         mis_high_d1_totalprop = mis_high_d1/pop_size,
         mis_high_d0_totalprop = mis_high_d0/pop_size,
         mis_low_d1_totalprop = mis_low_d1/pop_size,
         mis_low_d0_totalprop = mis_low_d0/pop_size,
         )
View(biasparams)

############
# creating a better way to do this?
biasparams_norm2_300 <- population_normal %>% 
  summarize(
    se_all = sum(Eprime300_high[E_high==1]==1)/sum(E_high==1), #Se all
    se_d1 = sum(Eprime300_high[DB==1]==1 & E_high[DB==1]==1)/sum(E_high[DB==1]==1), #Se D=1
    se_d0 = sum(Eprime300_high[DB==0]==1 & E_high[DB==0]==1)/sum(E_high[DB==0]==1), #Se D=0
    sp_all = sum(Eprime300_high[E_high==0]==0)/sum(E_high==0), #Sp all
    sp_d1 = sum(Eprime300_high[DB==1]==0 & E_high[DB==1]==0)/sum(E_high[DB==1]==0), #Sp D=1
    sp_d0 = sum(Eprime300_high[DB==0]==0 & E_high[DB==0]==0)/sum(E_high[DB==0]==0), #Sp D=0
    #count of those who are truly low but categorized as high:
    mis_high_d1 = sum(Eprime300_high[DB==1]==1 & E_high[DB==1]==0),
    primehigh_d1 = sum(Eprime300_high[DB==1]==1),
    mis_high_d1_prop = mis_high_d1/primehigh_d1,
    mis_high_d0 = sum(Eprime300_high[DB==0]==1 & E_high[DB==0]==0),
    primehigh_d0 = sum(Eprime300_high[DB==0]==1),
    mis_high_d0_prop = mis_high_d0/primehigh_d0,
    mis_low_d1 = sum(Eprime300_high[DB==1]==0 & E_high[DB==1]==1), 
    primelow_d1 = sum(Eprime300_high[DB==1]==0),
    mis_low_d1_prop = mis_low_d1/primelow_d1,
    mis_low_d0 = sum(Eprime300_high[DB==0]==0 & E_high[DB==0]==1),
    primelow_d0 = sum(Eprime300_high[DB==0]==0),
    mis_low_d0_prop = mis_low_d0/primelow_d0,
    rr_true = (sum(DB[E_high==1]==1)/sum(E_high==1))/(sum(DB[E_high==0]==1)/sum(E_high==0)), #RR true
    rr_obs = (sum(DB[Eprime300_high==1]==1)/sum(Eprime300_high==1))/(sum(DB[Eprime300_high==0]==1)/sum(Eprime100_high==0)), #RR measured
    or_true = (sum(DB[E_high==1]==1)/sum(DB[E_high==1]==0))/(sum(DB[E_high==0]==1)/sum(DB[E_high==0]==0)), #OR true
    or_obs = (sum(DB[Eprime300_high==1]==1)/sum(DB[Eprime300_high==1]==0))/(sum(DB[Eprime300_high==0]==1)/sum(DB[Eprime100_high==0]==0)) #OR measured 
  ) %>% mutate(distrib = "norm") 

biasparams_unif2_300 <- population_uniform %>% 
  summarize(
    se_all = sum(Eprime300_high[E_high==1]==1)/sum(E_high==1), #Se all
    se_d1 = sum(Eprime300_high[DB==1]==1 & E_high[DB==1]==1)/sum(E_high[DB==1]==1), #Se D=1
    se_d0 = sum(Eprime300_high[DB==0]==1 & E_high[DB==0]==1)/sum(E_high[DB==0]==1), #Se D=0
    sp_all = sum(Eprime300_high[E_high==0]==0)/sum(E_high==0), #Sp all
    sp_d1 = sum(Eprime300_high[DB==1]==0 & E_high[DB==1]==0)/sum(E_high[DB==1]==0), #Sp D=1
    sp_d0 = sum(Eprime300_high[DB==0]==0 & E_high[DB==0]==0)/sum(E_high[DB==0]==0), #Sp D=0
    #count of those who are truly low but categorized as high:
    mis_high_d1 = sum(Eprime300_high[DB==1]==1 & E_high[DB==1]==0),
    primehigh_d1 = sum(Eprime300_high[DB==1]==1),
    mis_high_d1_prop = mis_high_d1/primehigh_d1,
    mis_high_d0 = sum(Eprime300_high[DB==0]==1 & E_high[DB==0]==0),
    primehigh_d0 = sum(Eprime300_high[DB==0]==1),
    mis_high_d0_prop = mis_high_d0/primehigh_d0,
    mis_low_d1 = sum(Eprime300_high[DB==1]==0 & E_high[DB==1]==1), 
    primelow_d1 = sum(Eprime300_high[DB==1]==0),
    mis_low_d1_prop = mis_low_d1/primelow_d1,
    mis_low_d0 = sum(Eprime300_high[DB==0]==0 & E_high[DB==0]==1),
    primelow_d0 = sum(Eprime300_high[DB==0]==0),
    mis_low_d0_prop = mis_low_d0/primelow_d0,
    rr_true = (sum(DB[E_high==1]==1)/sum(E_high==1))/(sum(DB[E_high==0]==1)/sum(E_high==0)), #RR true
    rr_obs = (sum(DB[Eprime300_high==1]==1)/sum(Eprime300_high==1))/(sum(DB[Eprime300_high==0]==1)/sum(Eprime100_high==0)), #RR measured
    or_true = (sum(DB[E_high==1]==1)/sum(DB[E_high==1]==0))/(sum(DB[E_high==0]==1)/sum(DB[E_high==0]==0)), #OR true
    or_obs = (sum(DB[Eprime300_high==1]==1)/sum(DB[Eprime300_high==1]==0))/(sum(DB[Eprime300_high==0]==1)/sum(DB[Eprime100_high==0]==0)) #OR measured 
  ) %>% mutate(distrib = "unif") 

biasparams_2_300<- rbind(biasparams_norm2_300, biasparams_unif2_300)

biasparams_2_300 <- biasparams_2_300 %>%
  mutate(se_diff = se_d1 - se_d0, 
         sp_diff = sp_d1 - sp_d0,
         mis_high_d1_totalprop = mis_high_d1/pop_size,
         mis_high_d0_totalprop = mis_high_d0/pop_size,
         mis_low_d1_totalprop = mis_low_d1/pop_size,
         mis_low_d0_totalprop = mis_low_d0/pop_size,
  )
View(biasparams_2_300)

########################### 
# Plotting 

# getting ranges to help inform axes
summary(population_normal$E)
summary(population_uniform$E)
# true exposure - basic plot
ggplot() +
  geom_histogram(data = population_normal, aes(x=E), colour = "black", alpha = 1) +
  geom_histogram(data = population_uniform, aes(x=E), colour = "#333333", fill = "#CCCCCC", alpha = 0.7) +
  theme_classic() +
  labs(x = "True Exposure", y = "Count", title = "True exposure, no bin width set") 

# true exposure 
true_e <-
  ggplot() +
  geom_histogram(data = population_normal, aes(x=E), colour = "black", fill = "#999999", alpha = 1, binwidth = 26) +
  geom_histogram(data = population_uniform, aes(x=E), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  # 26 might be best width, followed by 35 or 41
  geom_vline(xintercept = 2200, colour = "#990000") +
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7000)) +
  theme_classic() +
  labs(x = "Exposure", y = "Count") + #, title = "True exposure distribution") +
  theme(plot.margin=unit(c(.1,1,.1,.1),"cm")) # text=element_text(size=28)) 
# all 4
# I think a grid is better where the misclassifieds can be on same side


##############
# may be easier to plot separately?
# question of which example?
# normal: colour = "black", fill = "#333333", alpha = 1
# uniform: colour = "#333333", fill = "#FFFFFF", alpha = 0.5
plot_E0D0 <- 
ggplot() +
  geom_histogram(data = population_normal %>% filter(DC == 0, Eprime300_high == 0), 
                 aes(x = E, y = after_stat(count)), colour = "black", fill = "#333333", alpha = 0.7, binwidth = 26) +
  geom_histogram(data = population_uniform %>% filter(DC == 0, Eprime300_high == 0), 
                 aes(x = E, y = after_stat(count)), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6500)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  theme_classic() +
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"cm"))+ #, text=element_text(size=18)) +
  labs(x = "True Exposure", y = "Count")

plot_E0D1 <- ggplot() +
  geom_histogram(data = population_uniform %>% filter(DC == 1, Eprime300_high == 0), 
                 aes(x = E, y = after_stat(count)), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  geom_histogram(data = population_normal %>% filter(DC == 1, Eprime300_high == 0), 
                 aes(x = E, y = after_stat(count)), colour = "black", fill = "#333333", alpha = 0.7, binwidth = 26) +
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6500)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  theme_classic() +
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"cm"))+ #, text=element_text(size=18)) +
  labs(x = "True Exposure", y = "Count") # title = "Categorized as Low Exposure, Cases")

plot_E1D1 <- ggplot() +
  geom_histogram(data = population_uniform %>% filter(DC == 1, Eprime300_high == 1), 
                 aes(x = E, y = after_stat(count)), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  geom_histogram(data = population_normal %>% filter(DC == 1, Eprime300_high == 1), 
                 aes(x = E, y = after_stat(count)), colour = "#000000", fill = "#333333", alpha = 0.7, binwidth = 26) +
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6500)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  theme_classic() +
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"cm"))+ #, text=element_text(size=18)) +
  labs(x = "True Exposure", y = "Count") #, title = "Categorized as High Exposure, Cases")

plot_E1D0 <- ggplot() +
  geom_histogram(data = population_normal %>% filter(DC == 0, Eprime300_high == 1), 
                 aes(x = E, y = after_stat(count)), colour = "#000000", fill = "#333333", alpha = 0.7, binwidth = 26) +
  geom_histogram(data = population_uniform %>% filter(DC == 0, Eprime300_high == 1), 
                 aes(x = E, y = after_stat(count)), colour = "#333333", fill = "white", alpha = 0.5, binwidth = 26) +
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6500)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  theme_classic() +
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"cm")) + #, text=element_text(size=18)) +
  labs(x = "True Exposure", y = "Count") # title = "Categorized as High Exposure, Non-cases")

(true_e + plot_spacer()) / (plot_E0D1 + plot_E1D1 + plot_E0D0 + plot_E1D0) + 
  plot_annotation(tag_levels = 'A')

# other alternative is to crop these pics and then stick together 
(true_e | (plot_E0D1 + plot_E1D1 + plot_E0D0 + plot_E1D0)) + plot_annotation(tag_levels = 'A') 

# all 4 at once 2x2
ggplot() +
  geom_histogram(data = population_normal, aes(x=E), fill = "black", alpha = 0.8, binwidth = 26) +
  geom_histogram(data = population_uniform, aes(x=E), fill = "gray", alpha = 0.6, binwidth = 26) +
  facet_wrap(DC ~ Eprime100_high, scales = "free") +
  scale_x_continuous(expand = c(0, 0), limits = c(1400, 2800)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6500)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  theme_classic() + 
  theme(plot.margin=unit(c(.2,.5,.2,.2),"cm")) +
#  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.2,.5,.2,.2),"cm")) +
  labs(x = "True Exposure", y = "Frequency") 

##### more attempts
pop_normal <- population_normal %>%
  mutate(category = factor(paste("E",Eprime100_high, "_D", DC))) %>%
  mutate(distribution = "normal")

pop_uniform <- population_uniform %>%
  mutate(category = factor(paste("E",Eprime100_high, "_D", DC))) %>%
  mutate(distribution = "uniform")

pop_combined <- rbind(pop_normal, pop_uniform)

ggplot() +
  geom_histogram(data = pop_normal, aes(x = E, fill = category), colour = "black", alpha = 0.8, binwidth = 26) +
  facet_grid(rows=vars(category))
theme_classic() 

# theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.2,.5,.2,.2),"cm")) +
#  labs(x = "True Exposure", y = "Frequency") 
ggplot() +
    geom_histogram(data = pop_combined, aes(x = E, fill = distribution, colour = distribution), 
                   alpha = 0.7, binwidth = 26, position = "dodge") +
    facet_grid(rows=vars(category)) +
    scale_fill_manual(values = c("black", "#CCCCCC")) + 
    scale_color_manual(values = c("black", "#CCCCCC")) +
    theme_classic()  
  # this looks good but the overlap is hard to see for the cases 
#may need to filter by uniform and normal so they will overlap? let's try it
ggplot() +
  geom_histogram(data = pop_combined %>% filter(distribution == "uniform"), aes(x = E), 
                 fill = "#CCCCCC", alpha = 0.7, binwidth = 26) +
  geom_histogram(data = pop_combined %>% filter(distribution == "normal"), aes(x = E), 
                 fill = "#333333", alpha = 0.9, binwidth = 26) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  facet_grid(rows=vars(category))  + 
  theme_classic()  

ggplot() +
  geom_histogram(data = pop_combined %>% filter(distribution == "uniform"), aes(x = E), 
                alpha = 0.7, binwidth = 26) +
  geom_histogram(data = pop_combined %>% filter(distribution == "normal"), aes(x = E), 
                 alpha = 0.9, binwidth = 26) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  facet_grid(rows=vars(category))  + 
  theme_classic()  

ggplot() +
  geom_histogram(data = pop_combined %>% filter(distribution == "normal"), aes(x = E), 
                 colour = "black", fill = "#333333", alpha = 1, binwidth = 26) +
  geom_histogram(data = pop_combined %>% filter(distribution == "uniform"), aes(x = E), 
                 colour = "black", fill = "#CCCCCC", alpha = 0.5, binwidth = 26) +
  geom_vline(xintercept = 2200, colour = "#990000") +
 # facet_grid(rows=vars(distribution))  + 
  theme_classic()  
  


ggplot() +
  geom_density(data = pop_combined %>% filter(distribution == "normal"), aes(x = E, y = after_stat(count)), 
                  fill = "#333333", alpha = 1) +
  geom_density(data = pop_combined %>% filter(distribution == "uniform"), aes(x = E, y = after_stat(count)), 
                fill = "#CCCCCC", alpha = 0.7) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  facet_wrap(vars(category))  + 
  labs(x = "True Exposure", y = "Density") +
  theme_classic()  


ggplot() +
  geom_histogram(data = pop_combined %>% filter(distribution == "normal"), aes(x = E), 
                 colour = "black", fill = "#333333", alpha = 1, binwidth = 26) +
  geom_histogram(data = pop_combined %>% filter(distribution == "uniform"), aes(x = E), 
                 colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  facet_grid(DB ~ Eprime300_high)  + # (a~b) where a is columns, b is rows 
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"in"))+ #, text=element_text(size=18)) +
  theme_classic()  

# other option is to have TWO facets, one among DB = 1, DB = 0 and stack those? 
# update: can't remove the exposure labels and also... the y labels don't repeat. may not be better than 4
ggplot() +
  geom_histogram(data = pop_combined %>% filter(distribution == "normal", DB == 0), aes(x = E), 
                 colour = "black", fill = "#333333", alpha = 1, binwidth = 26) +
  geom_histogram(data = pop_combined %>% filter(distribution == "uniform", DB == 0), aes(x = E), 
                 colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  facet_grid(~ Eprime300_high)  + 
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2800)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7000)) +
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"in")) + #, text=element_text(size=18)) +
  theme_classic() +
  labs(x = "Exposure", y = "Count") 




# Density plots
ggplot() +
  geom_density(data = pop_combined %>% filter(distribution == "normal"), aes(x = E, y = after_stat(count)), 
                 fill = "#333333", colour = "#333333", alpha = 1) +
  geom_density(data = pop_combined %>% filter(distribution == "uniform"), aes(x = E, y = after_stat(count)), 
               fill = "#CCCCCC", colour = "#CCCCCC", alpha = 0.7) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  # facet_grid(rows=vars(distribution))  + 
  theme_classic()  

ggplot() +
  geom_histogram(data = pop_combined %>% filter(distribution == "uniform"), aes(x = E, y = after_stat(count)), 
               fill = "#333333", alpha = 0.7) +
  geom_histogram(data = pop_combined %>% filter(distribution == "normal"), aes(x = E, y = after_stat(count)), 
                fill = "#999999", alpha = 0.9) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  facet_grid(rows=vars(category)) + 
  theme_classic()  

ggplot(data = pop_combined, aes(x = E, y = ..density..)) +
  # geom_histogram(data = pop_combined %>% filter(distribution == "uniform")) +
  geom_density(data = pop_combined %>% filter(distribution == "uniform"), colour = "red") +
  # geom_histogram(data = pop_combined %>% filter(distribution == "normal")) +
  geom_density(data = pop_combined %>% filter(distribution == "normal"), colour = "blue") +
  facet_grid(rows=vars(category)) + 
  theme_classic()


#################
# other plots/ just to get a sense of how these look
# plotting measured exposures
  ggplot() +
    geom_histogram(data = pop_unif, aes(x=Eprime300), fill = "#CCCCCC",  alpha = 1, binwidth = 26) +
    geom_histogram(data = pop_norm, aes(x=Eprime300), fill = "black", alpha = 0.8, binwidth = 26) +
    scale_x_continuous(expand = c(0, 0), limits = c(1400,2800)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(0,15000)) +
    theme_classic() +
    labs(x = "Measured Exposure", y = "Count") +
    theme(plot.margin=unit(c(.2,.5,.2,.2),"cm")) #add space because x axis label was being cut off
  
# plot measured exposure by case/ non case
  ggplot() +
    geom_histogram(data = pop_norm, aes(x=Eprime300), colour = "black", alpha = 0.8, binwidth = 15) +
    geom_histogram(data = pop_unif, aes(x=Eprime300), colour = "#333333", fill = "#CCCCCC",  alpha = 0.4, binwidth = 15) +
    facet_wrap(~DC)+
    scale_x_continuous(expand = c(0, 0), limits = c(1400,2800)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(0,15000)) +
    theme_classic() +
    labs(x = "Measured Exposure", y = "Count") +
    theme(plot.margin=unit(c(.2,.5,.2,.2),"cm")) 
  
# plotting true vs. measured exposures for Normal
  ggplot() +
    geom_histogram(data = pop_norm, aes(x=E), colour = "black", alpha = 1, binwidth = 15) +
    geom_histogram(data = pop_norm, aes(x=Eprime100), colour = "black", fill = "#CCCCCC", alpha = 0.3, binwidth = 15) +
  #  geom_histogram(data = pop_norm, aes(x=Eprime300), colour = "black", fill = "#CCCCCC", alpha = 0.5, binwidth = 15) +
   # geom_histogram(data = pop_norm, aes(x=Eprime500), colour = "black", fill = "#CCCCCC", alpha = 0.8, binwidth = 15) +
    scale_x_continuous(expand = c(0, 0), limits = c(1000,3000)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(0,15000)) +
    theme_classic() +
    labs(title = "True vs. Measured Exposure with Normal Distribution", x = "Exposure", y = "Count") +
    theme(plot.margin=unit(c(.2,.5,.2,.2),"cm")) 
  
# true and measured exposure for uniform distribution
  ggplot() +
    geom_histogram(data = pop_unif, aes(x=E), colour = "black", alpha = 1, binwidth = 10) +
    geom_histogram(data = pop_unif, aes(x=Eprime100), colour = "black", fill = "#CCCCCC",  alpha = 0.3, binwidth = 10) +
   # geom_histogram(data = pop_unif, aes(x=Eprime300), colour = "black", fill = "#CCCCCC",  alpha = 0.5, binwidth = 10) +
  #  geom_histogram(data = pop_unif, aes(x=Eprime500), colour = "black", fill = "#CCCCCC",  alpha = 0.8, binwidth = 10) +
        scale_x_continuous(expand = c(0, 0), limits = c(1400,2800)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(0,15000)) +
    theme_classic() +
    labs(title = "True vs. Measured Exposure with Uniform Distribution", x = "Exposure", y = "Count") +
    theme(plot.margin=unit(c(.2,.5,.2,.2),"cm"))
  
  
# true and measured uniform exposure; no bin width set 
  ggplot() +
    geom_histogram(data = pop_unif, aes(x=E), colour = "black", alpha = 0.8) +
    geom_histogram(data = pop_unif, aes(x=Eprime300), colour = "black", fill = "#CCCCCC",  alpha = 0.4) +
    scale_x_continuous(expand = c(0, 0), limits = c(1400,2800)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(0,15000)) +
    theme_classic() +
    labs(title = "True vs. Measured Exposure with Normal Distribution", x = "Exposure", y = "Count") +
    theme(plot.margin=unit(c(.2,.5,.2,.2),"cm"))
  
  
# densities of true vs measured by high/low and case/non-case  
  ggplot() +
    geom_density(data = pop_norm, aes(x=E), colour = "black", alpha = 0.8) +
    geom_density(data = pop_unif, aes(x=E), colour = "black", fill = "gray",  alpha = 0.4) +
    scale_x_continuous(expand = c(0, 0), limits = c(1400,2800), breaks= c(1600,2000,2500)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(0,15000)) +
    theme_classic() +
    labs(x = "True Exposure", y = "Density") +
    theme(plot.margin=unit(c(.2,.5,.2,.2),"cm")) 
  #add space because x axis label was being cut off
  

# 4 way plot of exposures by measured high/low and case/non-case
ggplot() +
  geom_histogram(data = pop_norm, aes(x=E), colour = "black", alpha = 0.8, binwidth = 15) +
  geom_histogram(data = pop_unif, aes(x=E), fill = "gray", alpha = 0.6, binwidth = 15) +
  facet_wrap(DC ~ Eprime100_high, scales = "free") +
  scale_x_continuous(expand = c(0, 0), limits = c(1400,2800)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2100)) +
  geom_vline(xintercept = 2200) +
  theme_classic() + 
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.2,.5,.2,.2),"cm")) +
  labs(x = "True Exposure", y = "Frequency") 

# above this I am working on still
#####################
# below is newer
# test space
# attempting to make the y axis a proportion
bin_width <- 35
ggplot() +
  geom_histogram(data = population_normal, aes(x=E, y=after_stat(count)/sum(after_stat(count))), colour = "black", alpha = 1, binwidth = bin_width) +
  geom_histogram(data = population_uniform, aes(x=E, y=after_stat(count)/sum(after_stat(count))), colour = "#333333", fill = "#CCCCCC", alpha = 0.7, binwidth = bin_width) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic() +
  labs(x = "True Exposure", y = "Proportion") +
  theme(plot.margin=unit(c(.1,1,.1,.1),"cm"), text=element_text(size=28)) 

