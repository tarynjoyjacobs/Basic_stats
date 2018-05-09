#Day_6.R
#26 April 2018


# Confidence intervals ----------------------------------------------------

Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)
data
library(rcompanion)

# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)
#steps = calculate mean steps taken by students across all levels - linear regression and anova


# one-way data
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3) #males and females separately
#trad = traditional way of calculating mean.

# two-way data
graph <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

#exercise 1
#plot mean as a point ans plot around the pont the confidence interval and 
#display the effect of teacher and sex on the means

library(tidyverse)

ggplot(data = graph, aes(x = Sex, y = Mean)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes( ymin = Mean - Trad.lower, 
                     ymax = Mean + Trad.upper)) +
  facet_wrap(~ Teacher) 


# Testing Assunptions -----------------------------------------------------
#The dependent variable must be continuous.
#The data must be independent of each other.
#The data most be normally distributed.
#The data must be homoscedastic.

#log transform the coloumn of data called steps
#cube root square root 
#frequency histogram


data %>%
  mutate(Natlog = log(Steps))

data %>%
  mutate(log10 = log10(Steps))

data %>%
  mutate(sqrt = sqrt(Steps))

data %>%
  mutate(cubrt = ((Steps)^(1/3)))

# AJ WAY ------

library(tidyverse)
dat2 <- data %>% 
  mutate(ln.step = log(Steps),
         log10.step = log(Steps),
         cube.step = Steps^(1/3),
         sqrt.step = sqrt(Steps)) %>% 
  select(-Student, -Rating) %>% 
  gather(key = "data.type", value = "trans.data",
         -Sex, -Teacher)

ggplot(data = dat2, aes(x = trans.data)) +
  geom_histogram() +
  facet_grid(Sex ~ Teacher)


# ACTUALLY ----------------------------------------------------------------

 plt1 <- ggplot(data = filter(dat2, data.type == "cube.step"), aes( x = trans.data)) +
  geom_histogram(aes(fill = Sex), position = "dodge")
  
plt2 <- ggplot(data = filter(dat2, data.type == "ln.step"), aes( x = trans.data)) +
  geom_histogram(aes(fill = Sex), position = "dodge")

 plt3 <- ggplot(data = filter(dat2, data.type == "log10.step"), aes( x = trans.data)) +
  geom_histogram(aes(fill = Sex), position = "dodge")

plt4 <- ggplot(data = filter(dat2, data.type == "sqrt.step"), aes( x = trans.data)) +
  geom_histogram(aes(fill = Sex), position = "dodge")

library(ggpubr)  
ggarrange(plt1, plt2, plt3, plt4, nrow = 2, ncol = 2, labels = "AUTO")  



# iris ANOVA data ---------------------------------------------------------

iris.dat <- as.tibble(iris)   
iris.dat 

#H0: There is no significat difference in petal width between the 3 iris species

iris.dat %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))
#WE FIND that some of tge species have non- normal data

#do a kruskal test instead of an ANOVA

kruskal.test(Petal.Width ~ Species, data = iris)
