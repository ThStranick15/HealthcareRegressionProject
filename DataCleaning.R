library(tidyverse)

#Data Cleaning & Wrangling

survey_data <- read.csv("Data/adult24.csv")
survey_tibble <- tibble(survey_data)

nrow(survey_tibble)

#Getting rows with premium data, 11931 matches codebook
premium_data <- survey_tibble %>% filter(!is.na( HICOSTR1_A ) & HICOSTR1_A != 99997
                                      & HICOSTR1_A != 99998  & HICOSTR1_A != 99999)



