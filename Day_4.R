# Day_4.R
# 19 April 2018
#ANOVA

#Asssumptions
# 1 Normally distributed data
# 2 Homogeneity of variances
# 3 Independence of data
# 4 In our case, we will encourage also that the data are balanced 
# ANOVA simultaneously looks at all the differences between different pairs of samples


# load libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)

#load the data
chicks <- as_tibble(ChickWeight)
# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)
#data belongs to chickens fed diet 1,2 at time 21 to see if the size differs

t.test(weight ~ Diet, data = chicks_sub)
#we do not reject the null hypothesis that there is not a significant difference 
#between  the weight of the chickens at time 21 based on the diets p = 0.2176


# Single factor ANOVA -----------------------------------------------------
#DIET IS THE ONLY FACTOR ACTING ON THE CHICKEN DATA

# Research question : is theere a difference in chicken mass attained after
# 21 days after the chickens having been fed four dfferent diets

#h0 = there is no difference in  chicken mass at day 21 aftr
# having been fed one of four diets

chicks_21 <- chicks %>% 
  filter(Time == 21)


chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# we do not accept the null hypothesis, therefore we accept the alternative hypothesis 
#that there is a signifivant difference in chicken mass at day 21 based on the diets

ggplot(chicks_21, aes(x = as.factor(Time), y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)
#suggestions of where the significant differenec might be

#overlap = no sig differece
#dont overlap = significant differnce 
#of 1 relative to the other one. so if pink doesnt overlap with purple for eg.

# box wider than quartile will add line so that length from median to quartile is equal

# #TukeyHSD test ----------------------------------------------------------

TukeyHSD(chicks.aov1)
#camparing diets to eachother 
#if lower confidance interval is positive we know there is a significant difference

ggplot(chicks_21, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE, colour = "black") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight + 2 ))



# Segments showing confidence intervals
chicks_tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_tukey$pairs <- as.factor(row.names(chicks_tukey))
  chicks_tukey   
  
  ggplot(data = chicks_tukey) +
    geom_errorbar(aes(x = pairs, ymin = lwr , ymax = upr)) +
    coord_flip() 
  
?TukeyHSD
plot(TukeyHSD(chicks.aov1, "Diet"))
 

# Multiple factors --------------------------------------------------------

# H0  : there is no cahnage in chicken mass (kg) from day 0 to day 21

#create data frame
chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 2, 21))
# See the data
ggplot(data = chicks_0_21, aes(x = Time, y = weight)) +
  geom_boxplot( notch = T, aes(fill = as.factor(Time)))

# Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))
# Perform and plot a Tukey post-hoc test
plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))

 #Look only at day 0 and 21 for both Time and Diet
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))
 #or Look at all the time , which is not the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight)))

#instead of adding, multiply diet and time
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

TUK <- TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0,21))))

plot(TUK)
 chicks_mean <- ChickWeight %>% 
   group_by(Diet, Time) %>%
   summarise(weight_mean = mean(weight, na.rm = T))

 
 ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet)) +
   geom_point(shape = 15, size = 5) +
   geom_line(size = 2) +
   labs( x = "Time (days)", y = "Mean Weight (kg)")
 
 

# Non-parametic tests -----------------------------------------------------
# no normal data ?
 
#rather use a wilcox test - fill in the same as for t.test 
 wilcox.test()
 
 #Kruskal.test?
 
kruskal.test(weight ~ Diet, data = chicks_0_21)

library(pgirmess) 
kruskalmc(weight ~ Diet, data = chicks_0_21) 
 
 
 
