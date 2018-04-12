#Day_1.R
#first day of stats class
#Purpose is to practice some of the concepts that we will encounter
#12 April 2018

# Integers ----------------------------------------------------------------

integer_r <- as.integer(seq(5,14, by = 1))

#this shows me what is under this name
integer_r

#this shows min max etc
summary(integer_r)

#integers are discrete data


# continuous --------------------------------------------------------------

# Generate a sequence f numeric value
numeric_r <- seq(23, 43, length.out = 10)
# continuous data has an integer part and a fractional part ("on avearge"- sometimes)
#length is a shotened version of length.out - smallest number of unique digists to acurately identify arguemnets


# Dates in r --------------------------------------------------------------

#dates is an example of time
#one may perform arithmetic with dates
#this function 
as.Date("2005-12-31") - as.Date("2005-12-12")
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")

summary(dates_r)
#format "yyyy-mm-dd" important
#as.Date("2005-12-16")) date and must be treated like date and can undergo arithmetic that are writeen specifically for dates



# load libraries ----------------------------------------------------------

library(tidyverse)


# Dataframes --------------------------------------------------------------
#create yhe base data frame
df_r <- data.frame(integers = integer_r,
                   numeric = numeric_r,
                   dates = dates_r)
#create a tibble, each coloummn will be named as above
#now upgrade above to tibble
df_r <- as_tibble(df_r)

df_r


# Qualitative data --------------------------------------------------------
#Qualitative data may be well-defined categories or they may be subjective, and generally include descriptive words for classes


# Electronics
elec_r <- as.factor(c("laptops", 
                      "desktops",
                      "cellphones"))
#people
people_r <- as.factor(c("funny hair",
                        "beautiful",
                        "beanies"))
#colours
colour_r <- as.factor(c("blue",
                        "red"))


# Ordinal data ------------------------------------------------------------
#classes are somewhat subjective
#factoral data with preferential order
# bc here we still have qualitative data

colour_qual <- ordered(c("blue", "green",
                            "yellow", "orange","red"),
                          levels = c("blue", "green",
                                     "yellow", "orange",
                                     "red"))

colour_qual
#ordeering colours from coldest to warmest- could be used to order density
#this tells the computer that one is better than other

# BINARY DATA -------------------------------------------------------------


#binary data takes one of only two outcome- on off, truefalse, absent present
#generally represented as T/F
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)


# Characters --------------------------------------------------------------

sites_r <- c("Yztervarkpunt", "Betty's Bay",
             "Gaansbaai", "Sea Point")

# Missing value -----------------------------------------------------------

#in R it is specified as N/A
chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
summary(chicks_nest)
#there is a differnce between N/A and 0



# VIEWING OUR DATA --------------------------------------------------------

summary(ChickWeight)
chicks <- ChickWeight

head(chicks, 7)
tail(chicks, 7)

chicks[c(1,54,61,12,2),2]






# Descriptive statistics --------------------------------------------------

#first create a data frame

chicks <- as_tibble(ChickWeight)
#pipe allows us to vhain commands together into logical order
 chicks %>%
   summarise(n())
 
#or
 nrow(chicks)
 #Measures of central tendency
 #mean =¯x
# calculate mean weight
 
 chicks %>% 
   summarise(mean_wt = mean(weight))
 
# be more specific 
 chicks %>% 
   filter(Time == 21) %>% 
   group_by(Diet) %>% 
   summarise(mean_wt = mean(weight),
             median_wt = median(weight))
#similar data = normally distributed and no skewness involved

 #visualize the density of the data
 
 ggplot(data = filter (chicks, Time == 21),
        aes(x = weight, fill = Diet)) +
   geom_density(alpha = 0.6)
 

# skewness ----------------------------------------------------------------

 
 #calculate the numeric value
 library(e1071) 

#compare difference in mean and median against skewness
 chicks %>% 
   filter(Time == 21) %>% 
   group_by(Diet) %>% 
   summarise(mean_wt = mean(weight),
             median_wt = median(weight),
             skew_wt = skewness(weight))
#centre of gravity of graph leaning left or right?
#negatively skew  means that the mean is behind the median
#skew =is the mean to the right or too the left of the median



# Kurtosis ----------------------------------------------------------------

#bell curve = normal distribution
#Kurtosis describes the tail shape of the data’s distribution.
#A normal distribution has zero kurtosis and thus the standard tail shape (mesokurtic).
#Negative kurtosis indicates data with a thin-tailed (platykurtic) distribution.
#Positive kurtosis indicates a fat-tailed distribution (leptokurtic).
 
#calculate the kurtosis of the tails of a distribution
 chicks %>% 
   filter(Time == 21) %>% 
   group_by(Diet) %>% 
   summarise(mean_wt = mean(weight),
             median_wt = median(weight),
             skew_wt = skewness(weight),
             kurtosis_wt = kurtosis(weight))

 exp_r <- data.frame(dat = rexp(n = 500),
                     sample = "A") 

 ggplot(data = exp_r, aes(x = dat)) +
   geom_density()

kurtosis(exp_r$dat)



# Variance and std --------------------------------------------------------

#variance first finds the mean = weight scaled
#std find the variation = weight (when talking about chicken weight)


# Variability ----------------------------------------------------------------

#below is a summary of many different statistical properties
wt_summary <- chicks %>% 
  filter( Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean (weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight))



