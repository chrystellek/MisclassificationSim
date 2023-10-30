# Chrystelle Kiang
# Code for illustrative example 
library(tidyverse)
library(ggplot2)
library(patchwork) # package for plot arranging

# For motivating example, choosing SD = 300, expected RR = 5 for plots
# could look at other SD and RR using this code by modifying plot codes (EerrorX and DX, where X are variable)
# Same steps taken for function in main analyses
# Can't use the misclassified function because it summarizes all the simulations
# This code is only for 1 simulation/instance, but same steps 


# set population size 
pop_size <- 100000

# one run of uniform distribution
set.seed(3)
population_uniform <- tibble(
  # exposure
  E = as.integer(runif(pop_size, 1600, 2500)),  
  # EerrorX is error term for X SD, EprimeX is measured exposure with measurement error
  Eerror100 = rnorm(pop_size, mean = 0, sd = 100), Eprime100 = as.integer(E + Eerror100), 
  Eerror300 = rnorm(pop_size, mean = 0, sd = 300), Eprime300 = as.integer(E + Eerror300), 
  Eerror500 = rnorm(pop_size, mean = 0, sd = 500), Eprime500 = as.integer(E + Eerror500), 
  # high/low categories
  E_high = ifelse(E > 2200, 1, 0), # truth
  # EprimeX is 'measured' exposure for X SD
  Eprime100_high = ifelse(Eprime100 > 2200, 1, 0), 
  Eprime300_high = ifelse(Eprime300 > 2200, 1, 0), 
  Eprime500_high = ifelse(Eprime500 > 2200, 1, 0), 
  # disease status for each expected RR: A=1.66, B=2.49, C=5
  DA = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-4.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-8.0 + 0.0035*E))),
  DC = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-16.0 + 0.0072*E)))
)

# creating one example of normal distribution 
set.seed(3)
population_normal <- tibble(
  E = as.integer(rnorm(pop_size, 2050, 150)),  
  # EerrorX is error term for X SD, EprimeX is measured exposure with measurement error
  Eerror100 = rnorm(pop_size, mean = 0, sd = 100), Eprime100 = as.integer(E + Eerror100), 
  Eerror300 = rnorm(pop_size, mean = 0, sd = 300), Eprime300 = as.integer(E + Eerror300), 
  Eerror500 = rnorm(pop_size, mean = 0, sd = 500), Eprime500 = as.integer(E + Eerror500), 
  # high/low categories
  E_high = ifelse(E > 2200, 1, 0), 
  # EprimeX is 'measured' exposure for X SD
  Eprime100_high = ifelse(Eprime100 > 2200, 1, 0), 
  Eprime300_high = ifelse(Eprime300 > 2200, 1, 0), 
  Eprime500_high = ifelse(Eprime500 > 2200, 1, 0), 
  # disease status for each expected RR: A=1.66, B=2.49, C=5
  DA = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-8.5 + 0.0019*E))),
  DB = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-11.5 + 0.0035*E))),
  DC = as.factor(rbinom(n = pop_size, size = 1, p = plogis(-18.2 + 0.0072*E)))
)

# true exposure 

# extended x axis to match measured e
true_e <-
  ggplot() +
  geom_histogram(data = population_normal, aes(x=E), colour = "black", fill = "#999999", alpha = 1, binwidth = 26) +
  geom_histogram(data = population_uniform, aes(x=E), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  scale_x_continuous(expand = c(0, 0), limits = c(600, 3500)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7000)) +
  theme_classic() +
  labs(x = "True Exposure", y = "Count") +
  theme(plot.margin=unit(c(.1,1,.1,.1),"cm")) 

measured_e <-
  ggplot() +
  geom_histogram(data = population_normal, aes(x=Eprime300), colour = "black", fill = "#999999", alpha = 1, binwidth = 26) +
  geom_histogram(data = population_uniform, aes(x=Eprime300), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  scale_x_continuous(expand = c(0, 0), limits = c(600, 3500)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7000)) +
  theme_classic() +
  labs(x = "Measured Exposure", y = "Count") +
  theme(plot.margin=unit(c(.1,1,.1,.1),"cm")) 

true_e + measured_e 

############
# alternative is looking by distribution, true vs. measured
# but run out of colors/ don't think it's a good idea to overlap SAME distribution 
# given Figure 2 is overlapping to compare distributions

# uniform 
unif_true <-  ggplot() +
    geom_histogram(data = population_uniform, aes(x=E), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
     geom_vline(xintercept = 2200, colour = "#990000") +
    scale_x_continuous(expand = c(0, 0), limits = c(600, 3500)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 7000)) +
    theme_classic() +
    labs(x = "True Exposure", y = "Count", title = "Uniform") +
    theme(plot.margin=unit(c(.1,1,.1,.1),"cm"))
  
unif_measured <-  ggplot() +
    geom_histogram(data = population_uniform, aes(x=Eprime300), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
    geom_vline(xintercept = 2200, colour = "#990000") +
    scale_x_continuous(expand = c(0, 0), limits = c(600, 3500)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 7000)) +
    theme_classic() +
    labs(x = "Measured Exposure", y = "Count", title = "Uniform") +
    theme(plot.margin=unit(c(.1,1,.1,.1),"cm"))

#normals   
normal_true <- ggplot() +
    geom_histogram(data = population_normal, aes(x=E), colour = "black", fill = "#999999", alpha = 1, binwidth = 26) +
    #geom_histogram(data = population_normal, aes(x=Eprime300), colour = "black", fill = "#999999", alpha = 0.7, binwidth = 26) +
    geom_vline(xintercept = 2200, colour = "#990000") +
    scale_x_continuous(expand = c(0, 0), limits = c(600, 3500)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 7000)) +
    theme_classic() +
    labs(x = "True Exposure", y = "Count", title = "Normal") +
    theme(plot.margin=unit(c(.1,1,.1,.1),"cm")) 
  
normal_measured <- ggplot() +
#  geom_histogram(data = population_normal, aes(x=E), colour = "black", fill = "black", alpha = 1, binwidth = 26) +
  geom_histogram(data = population_normal, aes(x=Eprime300), colour = "black", fill = "#999999", alpha = 1, binwidth = 26) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  scale_x_continuous(expand = c(0, 0), limits = c(600, 3500)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7000)) +
  theme_classic() +
  labs(x = "Measured Exposure", y = "Count", title = "Normal") +
  theme(plot.margin=unit(c(.1,1,.1,.1),"cm")) 

unif_true + normal_true + unif_measured + normal_measured + plot_annotation(tag_levels = 'A', tag_suffix = ')')

# uniform
scat_unif <- 
  ggplot(data = population_uniform, aes(x = E, y = Eprime300)) +
  geom_point(colour = "#333333", fill = "#FFFFFF", alpha = 0.3) + 
  scale_x_continuous(expand = c(0, 0), limits = c(600, 3500)) +
  scale_y_continuous(expand = c(0, 0), limits = c(500, 3850)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  geom_hline(yintercept = 2200, colour = "#990000") +
  labs(x = "True Exposure", y = "Measured Exposure", title = "Uniform") + 
  theme_classic() + theme(plot.margin=unit(c(.1,1,.1,.1),"cm")) 
# normal
scat_norm <- 
  ggplot(data = population_normal, aes(x = E, y = Eprime300)) +
  geom_point(alpha = 0.7, colour = "black", fill = "#999999") + 
  scale_x_continuous(expand = c(0, 0), limits = c(600, 3500)) +
  scale_y_continuous(expand = c(0, 0), limits = c(500, 3850)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  geom_hline(yintercept = 2200, colour = "#990000") +
  labs(x = "True Exposure", y = "Measured Exposure", title = "Normal") +
  theme_classic() + theme(plot.margin=unit(c(.1,1,.1,.1),"cm")) 

# varied layouts 
(unif_true + normal_true) / (unif_measured + normal_measured) / (scat_unif + scat_norm) + plot_annotation(tag_levels = 'A', tag_suffix = ')')

(unif_true + unif_measured + scat_unif) / (normal_true + normal_measured + scat_norm) + plot_annotation(tag_levels = 'A', tag_suffix = ')')


true_e / scat_unif / scat_norm + plot_annotation(tag_levels = 'A', tag_suffix = ')')

true_e / scat_unif / scat_norm + plot_annotation(tag_levels = 'A', tag_suffix = ')')



# plotting each subset of case/non-case and measured high/low exposure:
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
  labs(x = "True Exposure", y = "Count", title = "Non-cases classified as low exposure")

plot_E0D1 <- 
  ggplot() +
  geom_histogram(data = population_uniform %>% filter(DC == 1, Eprime300_high == 0), 
                 aes(x = E, y = after_stat(count)), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  geom_histogram(data = population_normal %>% filter(DC == 1, Eprime300_high == 0), 
                 aes(x = E, y = after_stat(count)), colour = "black", fill = "#333333", alpha = 0.7, binwidth = 26) +
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6500)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  theme_classic() +
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"cm"))+ #, text=element_text(size=18)) +
  labs(x = "True Exposure", y = "Count", title = "Cases classified as low exposure")

plot_E1D1 <- 
  ggplot() +
  geom_histogram(data = population_uniform %>% filter(DC == 1, Eprime300_high == 1), 
                 aes(x = E, y = after_stat(count)), colour = "#333333", fill = "#FFFFFF", alpha = 0.5, binwidth = 26) +
  geom_histogram(data = population_normal %>% filter(DC == 1, Eprime300_high == 1), 
                 aes(x = E, y = after_stat(count)), colour = "#000000", fill = "#333333", alpha = 0.7, binwidth = 26) +
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6500)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  theme_classic() +
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"cm"))+ #, text=element_text(size=18)) +
  labs(x = "True Exposure", y = "Count", title = "Cases classified as high exposure")

plot_E1D0 <- 
  ggplot() +
  geom_histogram(data = population_normal %>% filter(DC == 0, Eprime300_high == 1), 
                 aes(x = E, y = after_stat(count)), colour = "#000000", fill = "#333333", alpha = 0.7, binwidth = 26) +
  geom_histogram(data = population_uniform %>% filter(DC == 0, Eprime300_high == 1), 
                 aes(x = E, y = after_stat(count)), colour = "#333333", fill = "white", alpha = 0.5, binwidth = 26) +
  scale_x_continuous(expand = c(0, 0), limits = c(1300, 2750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6500)) +
  geom_vline(xintercept = 2200, colour = "#990000") +
  theme_classic() +
  theme(legend.position="none", strip.text = element_blank(), plot.margin=unit(c(.5,.5,.5,.5),"cm")) + #, text=element_text(size=18)) +
  labs(x = "True Exposure", y = "Count", title = "Non-cases classified as high exposure")

### patchwork package allows for plot arranging
Fig1_5 <-  true_e / (plot_E0D1 + plot_E1D1 + plot_E0D0 + plot_E1D0) + plot_annotation(tag_levels = 'A', tag_suffix = ')')

# alternative options:

Fig1 <- true_e2 / measured_e + plot_annotation(tag_levels = 'A', tag_suffix = ')')

Fig2 <- plot_E0D1 + plot_E1D1 + plot_E0D0 + plot_E1D0 + plot_annotation(tag_levels = 'A', tag_suffix = ')')


