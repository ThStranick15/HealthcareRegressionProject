library(tidyverse)

#Read in Excel, convert to tibble for ease of use

survey_data <- read.csv("Data/adult24.csv")
survey_tibble <- tibble(survey_data)

total_surveyed = nrow(survey_tibble)

#Select only SAs with no dependents
survey_singles <- survey_tibble %>% filter(PRPLCOV1_A == 2)

#Getting rows with premium data, do not want to include NA, or other non-values
invalid_premium <- c(99999, 99998, 99997)

premium_data <- survey_tibble %>% 
  filter(!is.na( HICOSTR1_A ) & !HICOSTR1_A %in% invalid_premium) 

#selecting all covariates
premium_data <- premium_data %>% select(AGEP_A, SEX_A, RACEALLP_A, HISPALLP_A, MARITAL_A, NATUSBORN_A, ORIENT_A, #demographic
                                        POVRATTC_A, RATCAT_A, EDUCP_A, EMDOCCUPN2_A, EMDOCCUPN1_A, ACCSSHOM_A, EMDINDSTN1_A, #socio-economic
                                        DIBEV_A, PREDIB_A, GESDIB_A, HYPEV_A, ASEV_A, LONGCOVD2_A, EVERCOVD_A, ANXFREQ_A,DEPFREQ_A, #health conditions
                                        SMOKELSEV1_A, SMKCIGST_A, DRKLIFE_A, DRK12MWK_A, DRKSTAT_A, MODFREQW_A, #health related behaviors
                                        REGION, URBRRL23, #geography
                                        MCPART_A, MCCHOICE_A, EXCHANGE_A, PLNWRKR1_A, #access to care
                                        PPSU, PSTRAT, WTFA_A)#survey design

# premium_data <- premium_data %>% drop_na()
# 
# premium_data <- premium_data %>% mutate(across(c(HISPALLP_A,SEX_A),as.factor))

nrow(premium_data)

#create linear model

premium_model <- lm(HICOSTR1_A ~ ., data=premium_data)

summary(premium_model)

#Numerical & Graphical Summary

#Hypothesis Test

#Confidence Interval

#F-Test/Anova

#Residual Standard Error/R^2 vs Num of Predictors

#Hw6 Things

#Residuals vs Fitted

#Q-Q - Plot

#Shapiro Test

#Partial Regression Plot

#Leverage Points

#Jackknife Residuals & Bonferonni Correction

#Influential Points w/ Cooks Distance - Half Normal Plot

#Condition Number

#Forward Stepwise Selection

#Backward Stepwise Selection


