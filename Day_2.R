#Day_2.R
#13 April 2018
#the day in which we discuss data visualisations and distributions

# Load libraries ----------------------------------------------------------

library(tidyverse)

#mean, median, variance, std


# Manual calculations -----------------------------------------------------

#The mean
#this data was generated randomly as explain in line 22
r_dat <- data.frame (dat = rnorm(n = 601, mean = 372, sd = 50) ,
                    sample = "A")




#r + distribution gives you the data 

r_dat
# Quick visualisations ----------------------------------------------------

ggplot(data = r_dat, aes( x = dat)) + 
  geom_density()


#back to the mean
#the sum of all the points 
#divided by 
#the number of all the points
#think programatically, always avoid using static numbers. use the data. eg dont just take 600 as the number of obsv
# r_n = # of samples
r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))
#The median
#subset the data at the position that is (n + 1 / 2)
#slice shows you speific row of data and you can say what you want to slice it by
#if cant do something in the tidyverse then you can root force it $
order(r_dat$dat)[(length(r_dat$dat)+1)/2]

r_dat %>%
  arrange(dat) %>% 
  slice(n()/2)
#slice doesnt like to be told to slice a specific coloumn
#if you want to specify tyou must use the square bracket
#or the tidy automagic way
r_dat %>% 
  summarise(r_median = median(dat))


# Finding the variane -----------------------------------------------------

#we need ariance to find the std

#the sum of
#each value minus the mean
#square
#divided by
 #the count of samples minus one

#nutate adds a coloumn onto the existing frame which gives you values minus the mean
#numerator = sum of errors squared
r_dat %>% 
  mutate(r_error = dat-mean(dat)) %>% 
  summarise(r_squared_sum = sum(r_error * r_error))

r_dat %>% 
  mutate(r_error = dat-mean(dat) ,
         r_error_square = r_error * r_error) %>% 
  summarise( r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n() -1),
            
            #OR USE THE BUILT IN FUNCTION
r_var_func = var(dat))


r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_funct = sd(dat))


# Exercise 1 --------------------------------------------------------------



summary(ChickWeight$weight)

ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))





# Visualizations ----------------------------------------------------------


#first load our libraries
#these few packages contain most functions necessary 
#to make publication ready figures
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)
#load our SA time data

sa_time <-read.csv("SA_time.csv")

#edit our data
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                 rep("Joburg", 2)))


sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human, -geo)


# qualitative -------------------------------------------------------------

#create a count of qualitative values

sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n/sum(n))

#stacked bar graphs
#ggplot(data = sa_long, aes(x = "", y = time_type)) +
  #geom_bar


 plt1 <- ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

#this is too check what the graph wll run with and what it will run without.
 #reading bar chart from count data thus needs stat identity
 ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
   geom_bar( stat = "identity") +
   labs(title = "Stacked bar graph", subtitle = "cumulative sum",
        x = NULL, y = "Count") +
   theme_minimal()
 
 # stacked proportion bar graph
 #how to represent graphically the proprtion of data points in the 3 categories we have

 
 
 plt2 <- ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
   geom_bar(width = 1, stat = "identity") +
   scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
   labs(title = "Stacked bar graph", subtitle = "relative proportions",
        x = NULL, y = "Proportion") +
   theme_minimal()

 #a pie chart
 
 plt3 <- ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
   geom_bar(width = 1, stat = "identity") +
   scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
   labs(title = "Pie chart", subtitle = "but why though?",
        x = NULL, y = NULL) +
   coord_polar( "y", start = 0) +
   theme_minimal()
 
 plt3
 ggarrange(plt1, plt2, plt3, nrow = 2, ncol = 2, labels = "AUTO")

 
 
 
 
 

# Continuous data ---------------------------------------------------------

#Histograms
 ggplot(data = sa_long, aes(x = minutes)) +
   geom_histogram()

 #Rob said oh no, we have to remove that one value causing problems
 
 sa_clean <- sa_long %>% 
   filter(minutes < 100)
 
#here we go again!
 ggplot(data = sa_clean, aes(x= minutes)) +
   geom_histogram(aes(fill = time_type), position = "dodge") +
   facet_wrap(~time_type, ncol = 1, scales = "free_x")
 
 
#relative proportion histogram
 ggplot(data = sa_clean, aes(x = minutes)) +
   geom_histogram(aes(y = ..density.. ,fill = time_type),
                  postion = "dodge", binwidth = 1) +
   facet_wrap(~time_type, ncol = 1, scales = "free_x")
   
   
   

# Box Plots ---------------------------------------------------------------

 ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
   geom_boxplot(aes(fill = time_type))
 
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
   geom_boxplot(aes(fill = time_type), notch = TRUE)

  #middle of box is median therefore top and bottom lines of box is q3 & q2 of box plot
 #NB for central tendency of data = interquartile range
 #tails are not same length shows all data that exist within 1.5 x the value (distance of median?)
#dot = max values
 
 
#notched boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)

#Calculate summary stats fro plottkng over the boxplots
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>%
  summarise(time_type_mean = mean(minutes))

 
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
geom_point(data = sa_summary_stats, size = 6, shape = 18,
           aes(y = time_type_mean), colour = "goldenrod")
#shows data is very skewd and non normal



# Relationships ----------------------------------------------------------
#basic scatterplot
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))
#each row is anser of 1 human rep of nownow & just now 
#one point= relationship of one humans opinion  

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))

#grey overlaps = things are not different
  
