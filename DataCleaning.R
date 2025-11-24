library(tidyverse)

#Data Tidying

survey_data <- read.csv("Data/adult24.csv")
survey_tibble <- tibble(survey_data)

nrow(survey_tibble)

#Getting rows with premium data, and all covariates chosen have filled data
premium_data <- survey_tibble %>% 
  filter(!is.na( HICOSTR1_A ) & HICOSTR1_A != 99997 & HICOSTR1_A != 99998  & HICOSTR1_A != 99999) %>% 
  select(URBRRL23, REGION, SEX_A, AGEP_A, HISPALLP_A, EDUCP_A, PHSTAT_A, LSATIS4_A, HICOSTR1_A, 
         ACCSSHOM_A, SHTFLU12M_A, SHTCVD191_A, TRAVEL_A, ANXFREQ_A, DEPFREQ_A, ORIENT_A, MARITAL_A , 
         AFVET_A, NATUSBORN_A, EMDINDSTN1_A, EMDOCCUPN1_A, INCWRKO_A, INCINTER_A) %>% 
  drop_na()

nrow(premium_data)





