library(tidyverse)

#Data Tidying

survey_data <- read.csv("Data/adult24.csv")
survey_tibble <- tibble(survey_data)

nrow(survey_tibble)

#Getting rows with premium data, and all covariates chosen have filled data
#Not using HICOSTR2_A, only taking into account primary plans
premium_data <- survey_tibble %>% 
  filter(!is.na( HICOSTR1_A ) & HICOSTR1_A != 99997 & HICOSTR1_A != 99998  & HICOSTR1_A != 99999) %>% 
  mutate(across(c(HISPALLP_A,SEX_A),as.factor)) %>% #need to factor() all categorical variables, do not want to treat them as continuous
  select(URBRRL23, REGION, SEX_A, AGEP_A, HISPALLP_A, EDUCP_A, HICOSTR1_A, #to be updated based on todays discussion
         ACCSSHOM_A, SHTFLU12M_A, SHTCVD191_A, TRAVEL_A, ANXFREQ_A, DEPFREQ_A, ORIENT_A, MARITAL_A , 
         AFVET_A, NATUSBORN_A, EMDINDSTN1_A, EMDOCCUPN1_A) %>% 
  drop_na()

nrow(premium_data)

#maybe rename, want to easily know what covariate is which

#linear model

premium_model <- lm(HICOSTR1_A ~ ., data=premium_data)

summary(premium_model)



