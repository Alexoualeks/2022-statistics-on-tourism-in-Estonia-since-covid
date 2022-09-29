# Tourist in Estonia comparison between 2019 and 2022 broken down by countries

# Source: https://www.visitestonia.com/en/forthetrade/statistics#/

set.seed(1991)

# Load packages
library(pacman)
p_load(readxl, tidyverse, treemap, wesanderson)

# Packages from bbc_visual_tutorial_0109_2022.R
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

# Import the dataset
arrivals_tourist <- read_excel("arrivals_tourist_2022Q1.xlsx", 
                               sheet = "Q1", skip = 3, n_max = 25)

#################
# Cleaning data #
#################

# Make a dataset with the arrival numbers
arrivals_tourist_numbers <- arrivals_tourist %>%
  
  # Keeping only the names, Q1 numbers and changes from 2019 to 2022
  select(11:13, 16) %>%
  
  # Remove the rows with domestic and total numbers
  slice(4:24) %>%
  
  #Rename the years instead of "2019...11" it's "2019"
  rename('2019' = "2019...11",
         '2021' = "2021...12",
         '2022' = "2022...13",
         'country' = "...16") %>%
  
  # Pivot longer which puts the year is a single column
  pivot_longer('2019':'2022', 
               names_to = 'year',
               values_to = 'arrivals') %>%
  mutate(year = as.numeric(year))



# Make another dataset with only the changes between 2019 and 2022

arrivals_changes_2019vs2022 <- arrivals_tourist %>%
  
  # Keeping number and percentage changes from 2019 to 2022
  select(14, 15, 16) %>%
  
  # Remove the rows with domestic and total numbers
  slice(4:24) %>%
  
  #Rename the years instead of "2019...11" it's "2019"
  rename('arrival_difference' = "I kv /Q1",
         'percentage' = "...15",
         'country' = "...16")


############
# Plotting #
############

# Building a treemap to display the biggest lost in numbers between 2019 and 2022

# Create a canvas for the treemap
png(filename="arrival_loses_2019vs2022_treemap.png",width=800, height=400)

arrivals_changes_2019vs2022 %>%
  
  # Only select negative values
  filter(arrival_difference <= 0) %>%
  
  # Make the negative values into positives to get number of loses
  mutate(arrival_difference = -arrival_difference) %>%
  
  # Filter out countries with small number of loses if you want so the plot isn't crowded
  #filter(arrival_difference > 3000) %>%
  
  treemap(., index = 'country',
          vSize = 'arrival_difference',
          palette = wes_palette("Royal2"),
          type = 'index',
          fontsize.title = 0)

# Print the treemap on png
dev.off()

