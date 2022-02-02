# Carson Payne - Stat 250 - Sec-002
# File: Project 1.R
# Date: February, 14, 2022
# R version 4.1.2 "Bird Hippie"
# Working directory on my laptop: "/Users/carson/Desktop/Stat 250 Files/carson_payne_proj_1/"
setwd("/Users/carson/Desktop/Stat 250 Files/carson_payne_proj_1/")

# Packages ----------------------------------------------------------------
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidytext) # You may need to install this one
library(ggpubr)
library(ggthemes)



# Data Import -------------------------------------------------------------

# This data set was taken from kaggle as contributed by the user rgriffin 
# He said that he scraped the data from sports-reference.com in 2018
# It contains data on every athlete that competed in the Olympic games from 1896 to 2016.
# Here is the link to where I downloaded the data :
# https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results

all_athletes  <- read_csv("athlete_events.csv") # This file was in my working directory

view(all_athletes)


# Medal Counts ------------------------------------------------------------

# Creates a tibble of each country's total medals won each year

year_NOC_medal <- all_athletes %>% 
  mutate(Has_Medal = # This adds a column and inserts a 1 for every medal
           case_when(Medal == "Gold" |
                       Medal == "Silver" |
                       Medal == "Bronze" ~ 1)) %>% 
  select(Year, NOC, Has_Medal) %>% # This selects only the year, NOC, and recently created variable to count medals
  group_by(Year, NOC) %>% 
  summarise(total_medals = sum(Has_Medal, na.rm = TRUE)) # This will then sum up the medals won by each country each year

view(year_NOC_medal)


# Creates a tibble of each country's total medals won each decade

decade_NOC_medal <- year_NOC_medal %>% 
  filter(Year >= 1900,
         Year <= 2009) %>% #Removes data from incomplete decades
  mutate(decade = (Year %/% 10) * 10) %>%  # Using Modular division we can transform a year to its decade
  group_by(decade) %>% 
  count(NOC, wt = total_medals, sort = TRUE) %>% #This sorts the medal count from least to greatest
  arrange(n) %>% 
  mutate(total_medals = n) %>% #Combines the values from each year to decade
  select(decade, NOC, total_medals) # This selects only the decade, NOC, and recently altered variable to count medals in each decade

view(decade_NOC_medal)


# Creates a tibble that selects the top 10 medal winners from each decade
# and reorders the countries within the decade by medals won
# This will be used for the facet

decade_plot <- decade_NOC_medal %>%
  group_by(decade) %>%
  top_n(10) %>%
  ungroup %>% # Selects only the top 10 highest medal counts in each decade
  mutate(decade = as.factor(decade),
         NOC1 = reorder_within(NOC, total_medals, decade))# Reorders the data so that the medal counts from each country in each decade are in descending order

view(decade_plot)


# Used this tibble to create a list of the top 10 overall medal winners for the line graph

overall_top10 <- decade_NOC_medal %>% 
  group_by(NOC) %>% 
  summarise( med = sum(total_medals)) %>% # Sums up all medals won by country
  arrange(desc(med)) %>% # Sorts the medal counts in descending order
  slice(1:10) #Removes all but the 10 largest medal counts

view(overall_top10)


# Creates a tibble of the top 10 countries by medals won
# and finds their cumulative total medals won
# Will be used for the line graph

decade10_line <- decade_NOC_medal %>% 
  filter(NOC == "USA" # Removes all countries except those in the top 10 medal count
         | NOC =="URS"
         | NOC =="GER"
         | NOC =="GBR"
         | NOC =="FRA"
         | NOC =="ITA"
         | NOC =="SWE"
         | NOC =="CAN"
         | NOC =="AUS"
         | NOC =="RUS") %>% 
  arrange(decade) %>% # Sorts the graph by decade
  group_by(NOC) %>% 
  mutate(cum_medals = cumsum(total_medals)) # Creates a new variable of the cumulative sum of medals over time

view(decade10_line)



# Plotting ----------------------------------------------------------------

#Creates a facet of bar charts of the top 10 countries by total Olympic medals won from each decade

decade_facet <- ggplot(data = decade_plot, 
                       mapping = aes(x = NOC1, y=total_medals, fill = NOC)) + # Sets x value as NOC, y value as medals in decade, and then uses color for each country
  geom_col() + # Creates a bar graph
  facet_wrap(~decade, scales = "free_y") + # Creates a facet for each decade
  coord_flip()+ # Changes the y and x axis
  scale_x_reordered()+ # Used to keep the order of the bars from largest to smallest
  labs(y = "Total Medals Won In Decade",
       x = "Country",
       title = "Top 10 Countries By Total Olympic Medals Won From Each Decade",
       fill = "Country",
       caption = "Source: Kaggle user 'rgriffin'")+
  theme_few()# Cleaner theme

decade_facet


#Creates a line graph of the top 10 countries by cumulative Olympic medals won over time 

decade_line <- ggplot(data = decade10_line , 
                      mapping = aes(x = decade, y = cum_medals, color = NOC))+ # Sets x value as decade, y value as medals in decade, and then uses color for each country
  geom_line()+ #Creates a line graph
  labs(y = "Total Medals Won",
       x = "Year",
       title = "Top 10 Countries With Most Olympic Medals Historical Growth",
       color = "Country",
       caption = "Source: Kaggle user 'rgriffin'")+
  theme_few() # Cleaner theme

decade_line


# Creates a single page with both plots

final_plot <- ggarrange(decade_facet, 
                        decade_line,
                        nrow=2) 

final_plot # The proportions are much nicer on the saved file


# Saved Files --------------------------------------------------------------

write_csv(decade_plot, file = "decade_plot.csv")

write_csv(decade10_line, file = "decade10_line.csv")

ggsave(filename = "final_plot.pdf", 
       plot = final_plot,
       height = 12,
       width = 8)



