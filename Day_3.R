Day_3.R
#generating a Cullen and Frey graph


# Load libraries ----------------------------------------------------------

library(fitdistrplus)
library(logspline)

# Generate log-normal data
y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)
r_norm

 plt1 <-hist(r_norm)
barplot(r_norm)

plt2 <- descdist(r_norm,  discrete = FALSE, boot = 100)
 

 y <- runif(100) 
 par(mfrow = c(2, 2)) 
 plot( x = c(1:100), y = y) 
 hist(y) 
 descdist(y, discrete = FALSE)
 

# T TEST WITH ROB ---------------------------------------------------------
#April 17th, 2018
# load libraries
 library(tidyverse)
library(plotly)
 #pretty basic hypothesis? use t-test
 #t-test = 2 thing ANOVA = multiple things
 #Biologists typically define the probability of one in twenty (0.05) 
 #as the cutoff level to reject the null hypothesis - certain that the data is not normal
 
 #One-sample t-tests: only one sample set of data that we wish to compare 
 #against a known population mean
 #Two-sample t-tests: the means of two groups are compared against each other
 

# create r data -----------------------------------------------------------

 r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                             rnorm(n = 1000, mean = 8, sd = 2)),
                     sample = c(rep("A", 1000), rep("B", 1000)))
 
 h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
   geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
   geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
   labs(x = "value")
 h
 # Make it interactive
 ggplotly(h)

# check assumptions -------------------------------------------------------
 
 #the dependent variable must be continuous (i.e. it is measured at the interval or ratio level),
 #the observations in the groups being compared are independent of each other,
 #the data are normally distributed, and
 #that the data are homoscedastic, and in particular, that there are no outliers.

 
 #Normality

#for this we may use the Shapiro-Wilk test
 shapiro.test(r_dat$dat)

  #but that is testing all the data together
 #we must be a bit more clever about how we make this test
 r_dat %>% 
   group_by(sample) %>%
   summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))
#remember, the data are normal when p >0.05
#the data are non-normal when p <- 0.05
 

# Check homoscedaticity ---------------------------------------------------

#There are amny ways to check for homoscedasity
 #which is the similarity of variance bewteen sample sets
 #for now we will simply say that this assumptiond is met when
 #the variance of the samples are not more than 2-4 times greater
 #than ome another
 
 #check everything a once ..
 #WRONG TO DO SO
 var(r_dat$dat)
 #or do it the tidy way
 r_dat %>% 
   group_by(sample) %>%
   summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
             r_norm_var = var(dat))
 
 
 
 
 
 

# A one sample t-test -----------------------------------------------------

 r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                     sample = "A")
 
#Test normality of distribution
 shapiro.test(r_one$dat)
 
 
 
 
#a visualsation? 
#run the test
 t.test(r_one$dat, mu = 20)
#mu = population mean 
 
#run a test we know will produce a significant result
 t.test(r_one$dat, mu = 30)

 
 ggplot(data = r_one, aes(y = dat, x = sample)) +
   geom_boxplot(fill = "grey") +
   # population  mean (mu) = 20
   geom_hline(yintercept = 20, colour = "green", 
              size = 3, linetype = "dashed") +
   # population  mean (mu) = 30
   geom_hline(yintercept = 30, colour = "purple", 
              size = 3, linetype = "dashed") +
   labs(y = "Value", x = NULL) +
   coord_flip() 
 
 
 

# Pick a side -------------------------------------------------------------

#are these data SMLLER/LESS that the popilation mean
 t.test(r_one$dat, mu = 20, alternative = "less")
#Or GREATER
 
 t.test(r_one$dat, mu = 20, alternative = "greater")

 #But what about for the larger population mean?
 #are the samples less than the population of 30?
 t.test(r_one$dat, mu = 30, alternative = "less")
 #what about greater?
 t.test(r_one$dat, mu = 30, alternative = "greater")
 
 

# Create two sample t-tests -----------------------------------------------

#CREATE ANOTER DATA FRAME
 
 r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                             rnorm(n = 20, mean = 5, sd = 1)),
                     sample = c(rep("A", 20), rep("B", 20)))

 
 hist(r_two$dat)
 #run a default/basic test
 t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# pick a side 
 #is A less than B 
 t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
 t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")
 
 

# Load libraries ----------------------------------------------------------

library(tidyverse) 
 library(ggpubr)

 
 

ecklonia <- read_csv("ecklonia.csv") %>% 
gather(key = "variable", value = "value", -species, -site, -ID)   



ggplot(data = ecklonia, aes(x = variable, y = value, fill =  site)) +
  geom_boxplot() +
  coord_flip() 

ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_diameter, fill =  site)) +
  geom_boxplot() +
  coord_flip() 

ecklonia1 <- read_csv("ecklonia.csv")
# By using the gatehr function it alters ecklonia to ecklonia1


#formulating an hypothesis
#filter the data
 ecklonia_sub <- ecklonia %>% 
   filter(variable == "stipe_mass")  

#create a new figure 
 ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
   geom_boxplot() +
   coord_flip() +
   labs(y = "stipe_mass (kg)", x = "") +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank())

 
 ecklonia_sub %>% 
   group_by(site) %>% 
   summarise(stipe_mass_var = var(value)[1],
             stipe_mass_norm = as.numeric(shapiro.test(value)[2]))
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

 

# Exercise 1 --------------------------------------------------------------
#setup
library(tidyverse)
library(fitdistrplus)
library(logspline)
library(plotly)
library(ggpubr)

# I hypothesise that sunflowers grown under natural conditions have more petals than sunflowers grown in a nursary
# H0 : Sunflowers grown naturally have more petals
# H1 : Sunflowers grown naturally do not have more petals


# creating data -----------------------------------------------------------

#Randomized normal data for sunflower petals

data.frame(n_petals = c(rnorm(n = 201, mean = 12, sd = 5),
                        rnorm(n = 201, mean = 5, sd = 2)),
           petals = c(rep("natural", 201), rep("nursary", 201)))
# assigning a name to data set, this allows us to view it in the global environment

sunflower <- data.frame(n_petals = c(rnorm(n = 201, mean = 12, sd = 5),
                                     rnorm(n = 201, mean = 5, sd = 2)),
                        petals = c(rep("natural", 201), rep("nursary", 201)))


#visualising datasets normality
hist(sunflower$n_petals)


#visualising dataset using a box plot
ggplot(data = sunflower, aes(y = n_petals, x = petals, fill = petals)) +
  geom_boxplot() +
  coord_flip() +
  labs( x = "", y = "Number of petals")

# testing normality of dataset
shapiro.test(sunflower$n_petals)
# p- value = 4.317e-12 (significant difference)

sunflower %>%
  group_by(petals) %>%
  summarise(r_norm_dist = as.numeric(shapiro.test(n_petals)[2]))

sunflower %>%
  group_by(petals) %>%
  summarise(r_norm_dist = as.numeric(shapiro.test(n_petals)[2]),
            r_norm_var = var(n_petals))

#run a basic test
t.test(n_petals ~ petals, data = sunflower, var.equal = TRUE )

# now we wwant to know if sample A is less than sample B
t.test(n_petals ~ petals, data = sunflower, var.equal = TRUE, alternative = "less")

#What if sample A is more?
t.test(n_petals ~ petals, data = sunflower, var.equal = TRUE, alternative = "greater")


#the basic t-test reflects a p-value od 2.2e-16 (p < 0.05),
#thus H0 is rejected

