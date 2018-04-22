#Day_5.R
#20 April 2018
#Taryn Jacobs


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(Rmisc)

# load data ---------------------------------------------------------------


snakes <- read_csv("snakes.csv")
snakes$day = as.factor(snakes$day)
 
#snakes <- read_csv("snakes.csv") %>%
  #mutate(day = as.factor(day))


# Summarise the data ------------------------------------------------------

snakes_summary1 <- snakes %>%
  group_by(day, snake) %>%
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))
  
snakes_summary <- snakes %>%
  group_by(day) %>%
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))
snakes_summary 

snakes.summary2 <- summarySE(data = snakes, 
                             measurevar = "openings", 
                             groupvars = c("day"))
snakes.summary2


ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)
# formulate an hypothesis -------------------------------------------------
# H0 = There is NO difference from day to day across openings 
# H1 = There is a differnce in the nnumber of openings from day to day


#two factors? - another H0 needed

#H0 = There is no difference between snakes with respect to the
 #number of openings at which they habituate
#H0 =  There is no difference between days in term of 
#the number of openings at which the snakes hibituate


#testing just the days hypothesis
snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)
# ANOVA = df, sum sq / mn sq, f-value, probability(>F) is less than 0.05

#To TEST both hypothesus
snakes.all.aov <- aov(openings ~ day + snake , data = snakes)
summary(snakes.all.aov)


# testing assumptions afterwards ------------------------------------------

#First test normality of data
snake.residuals <- residuals(snakes.all.aov)
hist(snake.residuals)

#Visualise Homoscedadcity of results
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

# Check Tukey results
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)


#every snake is campared with other individual snake. = similar
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "snake")
plot(snakes.tukey)

#interaction - day - opening line = snake.
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)


# Exercise ----------------------------------------------------------------

# Get the moth data from GitHub
#Run a two-way ANOVA on them

moths <- read_csv("Moths.csv") %>%
  gather(key = "trap", value = "count", - Location)

moths_summary <- moths %>%
  group_by(Location, trap) %>%
  summarise(moths_mean = mean(count),
            snakes_sd = sd(count))

moths_summary

moths.summary2 <- summarySE(data = moths, 
                             measurevar = "count", 
                             groupvars = c("trap"))

moths.cnt.aov <- aov(count ~ Location, data = moths)
summary(moths.loc.aov)

moths.all.aov <- aov(count ~ Location + trap , data = moths)
summary(moths.all.aov)


plot(fitted(moths.all.aov), residuals(moths.all.aov))

# Check Tukey results
moths.tukey1 <- TukeyHSD(moths.all.aov, which = "Location")
plot(moths.tukey1)


moths.tukey <- TukeyHSD(moths.all.aov, which = "trap")
plot(moths.tukey)


ggplot(data = moths, aes(x = trap, y =as.numeric(count))) +
  geom_segment(data = moths.summary2, aes(x = trap, xend = trap, y = count - ci, yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

 plt1 <- ggplot(data = moths, aes(x = trap, y =as.numeric(count))) +
  geom_segment(data = moths.summary2, aes(x = trap, xend = trap, y = count - ci, yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

 plt2 <- ggplot(moths, aes(x = Location, y = count)) +
   geom_boxplot() +
   geom_jitter(width = 0.05, shape = 21)

plt2

plt3 <- ggplot(moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap)) +
  geom_jitter(width = 0.05, shape = 21)
plt3

library(ggpubr)
ggarrange(plt1, plt2, plt3, nrow = 2, ncol = 2, labels = "AUTO")



ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)




# REGRESSION --------------------------------------------------------------

#WHAT IS THE RELATIONSHIP BETWEEN DIFFERENT LEVELS IN OUR DATA
#Regressions test the statistical significance of the dependence of one continuous variable 
#on one or many independent continuous variables

#For this explanation of this statistical analysis
#we are going to use eruption data from Faithful

head(faithful)

#plot a quick scatter plot

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth( method = "lm", se = F, colour = "orange")

#the longer you wait the longer the eruptions are 


# form a hypothsis --------------------------------------------------------

 #H0 : Waiting time does not influence the duration of an eruption
 #H1 : Waiting time has an influence the duration of an eruption



# test a hypothesis -------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)



# Correlation test --------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(corrplot)

#continuous = pearson
#ordinal = spearmans corr
# cont/ ord = kendall 

# Load data
ecklonia <- read_csv("ecklonia.csv")

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# as they are the defaut settings.
# They are only shown here to illustrate that they exist.
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

corrplot(ecklonia_pearson, method = "circle")
 


#----------------------------------------------
#H0 =  There is no relationship  between stipe length and stipe mass 
#for kelp ecklonia maxima
# H1 = There is relationship between stipe length and stipe mass
#for the kelp ecklonia maxima


# Test a hypothesis -------------------------------------------------------
cor.test(ecklonia$stipe_length, ecklonia$stipe_mass)

#visualise the data

ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_mass)) +
         geom_point()


# Run hecka tests at once -------------------------------------------------

ecklonia_sub1 <- ecklonia %>%
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub1)



# Spearmans rank order  -------------------------------------------------
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), 3))

#Then run a Spearman test
cor.test(ecklonia$length, ecklonia$stipe_diameter,method = "spearman")


#Kendall rank test
cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")


# Visualise all the things! -----------------------------------------------
ecklonia_pearson <- cor(ecklonia_sub)

corrplot(ecklonia_pearson, method = "circle")
 #blue = positively correlated
#size pf ot = strength 

library(reshape2)
melted_eck <- melt(ecklonia_pearson)

ggplot(melted_eck, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "salmon")
  
       