# Tourism in Estonia

# Visualising the changes in tourism in Estonia

set.seed(1991)

# Load packages
library(pacman)
p_load(readr, tidyverse)

# Packages from bbc_visual_tutorial_0109_2022.R
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

# Import the dataset
estonian_tourist_by_months <- read_csv("TU121_estonian_tourist_by_month_yea.csv", 
                                                skip = 1)

#################
# Cleaning data #
#################

# Cleaning
estonian_tourist_by_months <- estonian_tourist_by_months %>%
  
  # replace the .. with NA
  replace(. == '..', NA) %>%
  
  # Convert the 2022 character into numeric
  mutate_at(c('2022'), as.numeric) %>% 
  
  # change variables and column names to snake case
  mutate(across(where(is.character), tolower)) %>%
  mutate(across(where(is.character), ~ str_replace_all(., ' ', '_'))) %>%
  rename_all(., .funs = tolower)

# Pivot the years into a single column called 'years'
estonian_tourist_by_months <- estonian_tourist_by_months %>%
  pivot_longer('1996':'2022', 
               names_to = 'year',
               values_to = 'tourist') %>%
  mutate(year = as.numeric(year))


############
# Plotting #
############

# Basic ggplot dot plot
estonian_tourist_by_months %>%
  filter(indicator == "accommodated_tourists_total",
         month == 'ascending_total') %>%
  ggplot(aes(x = year, y = tourist)) + 
    geom_point() +
    theme_classic()

# BBC style multiline comparing foreign and domestic tourist
bbc_style_estonian_tourist_by_months <- estonian_tourist_by_months %>%
  
  # Remove the total overall tourist number (Domestic and international)
  # Keep only the yearly tourist numbers
  filter(indicator != "accommodated_tourists_total",
         month == 'ascending_total') %>%
  
  # ggplot draws the graph with bbc styling from bbplot
  ggplot(., aes(x = year, y = tourist, colour = indicator)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    bbc_style() +
  
  # Adding title and changing legend names to 'Foreign'and 'Domestic'
    labs(title = 'Visitors accommodated in Estonia') +
    scale_colour_discrete(labels = c('Foreign', 'Domestic')) +
  
  # Scaling to 1996 to 2021 only
    xlim (1996, 2021) +
  # adding a comma in the large numbers. 2,000,000 instead of 2000000
    scale_y_continuous(labels = scales::comma)

# Display the plot in R
bbc_style_estonian_tourist_by_months

# Export the plot 
finalise_plot(plot_name = bbc_style_estonian_tourist_by_months, 
              source = 'Source: EESTI STATISTIKA',
              save_filepath = 'Estonia_yearly_accommendated_bbc_style.png')








# Have the foreign tourist numbers recovered in 2022?
# How does 2018 and 2022 monthly foreign visitors compare? 
foreign_visitor <- estonian_tourist_by_months %>%
  
  # Keep only foreign tourist numbers
  # Keep only 2018 and 2022
  # Remove the total yearly numbers
  filter(indicator == 'accommodated_foreign_visitors',
         year == 2018 | year == 2022 | year == 2021,
         month != 'ascending_total') %>%
  
  # Make a new month column as a factor instead of character
  # Make the year as discrete variable by converting it to a character, for plotting
  mutate(factor_month = factor(month, levels = c(tolower(month.name))),
         year = as.character(year)) %>%
  
  ggplot(., aes(x = factor_month, y = tourist, group = year)) +
    geom_line(aes(colour = year)) +
    geom_hline(yintercept = 0, size = 1) +
  #bbc style formate function
    bbc_style() +
  # Make the labels on x axis side ways
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  
  # Adding title
  labs(title = 'Foreign visitor accommodated in Estonia') +
  
  # adding a comma in the large numbers. 2,000,000 instead of 2000000
  scale_y_continuous(labels = scales::comma)


# Export the plot 
finalise_plot(plot_name = foreign_visitor, 
              source = 'Source: EESTI STATISTIKA',
              save_filepath = 'Foreign_visitor_accommodated_Estonia.png')
