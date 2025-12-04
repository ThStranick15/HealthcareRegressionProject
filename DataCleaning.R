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
premium_data <- premium_data %>% select(HICOSTR1_A, #response y
                                        AGEP_A, SEX_A, RACEALLP_A, HISPALLP_A, MARITAL_A, NATUSBORN_A, ORIENT_A, #demographic
                                        POVRATTC_A, RATCAT_A, EDUCP_A, EMDOCCUPN2_A, EMDOCCUPN1_A, ACCSSINT_A, EMDINDSTN1_A, #socio-economic
                                        DIBEV_A, PREDIB_A, GESDIB_A, HYPEV_A, ASEV_A, LONGCOVD2_A, EVERCOVD_A, ANXFREQ_A,DEPFREQ_A, #health conditions
                                        SMOKELSEV1_A, SMKCIGST_A, DRKLIFE_A, DRK12MWK_A, DRKSTAT_A, MODFREQW_A, #health related behaviors
                                        REGION, URBRRL23, #geography
                                        MCPART_A, MCCHOICE_A, EXCHANGE_A, PLNWRKR1_A, #access to care
                                        PPSU, PSTRAT, WTFA_A)#survey design

#easy interactions

#GESDIB_A
#on SEX_A=2

#LONGCOVD2_A
#on EVERCOVD_A=1

#DRK12MWK_A
#on DRKLIFE_A=1

#hard interactions?

#EMDOCCUPN2_A,EMDOCCUPN1_A,EMDINDSTN1_A
#on EMPLASTWK_A=1 or EMPNOWRK_A=1 or EMPWHYNOT_A=7 or EMPWHENWRK_A=1

#MCPART_A
#on MEDICARE_A in (1,2)

#MCCHOICE_A
#on MEDICARE_A in (1,2) and MCPART_A in (2,3,7,8,9)

#EXCHANGE_A
#on PRIVATE_A = 1

#PLNWRKR1_A
#on PRIVATE_A in (1,2)

#making invalid values NA (refused, not ascertained, don't know)
premium_data$AGEP_A[premium_data$AGEP_A %in% c(97, 98, 99)] <- NA
premium_data$SEX_A[premium_data$SEX_A %in% c(7,8,9)] <- NA
premium_data$RACEALLP_A[premium_data$RACEALLP_A %in% c(7,8,9)] <- NA
premium_data$HISPALLP_A[premium_data$HISPALLP_A %in% c(97,98,99)] <- NA
premium_data$MARITAL_A[premium_data$MARITAL_A %in% c(7,8,9)] <- NA
premium_data$NATUSBORN_A[premium_data$NATUSBORN_A %in% c(7,8,9)] <- NA
premium_data$ORIENT_A[premium_data$ORIENT_A %in% c(7,8)] <- NA
premium_data$RATCAT_A[premium_data$RATCAT_A %in% c(98)] <- NA
premium_data$EDUCP_A[premium_data$EDUCP_A %in% c(97,98,99)] <- NA
premium_data$EMDOCCUPN2_A[premium_data$EMDOCCUPN2_A %in% c(97,98,99)] <- NA
premium_data$EMDOCCUPN1_A[premium_data$EMDOCCUPN1_A %in% c(97,98,99)] <- NA
premium_data$ACCSSINT_A[premium_data$ACCSSINT_A %in% c(7,8,9)] <- NA
premium_data$EMDINDSTN1_A[premium_data$EMDINDSTN1_A %in% c(97,98,99)] <- NA
premium_data$NATUSBORN_A[premium_data$NATUSBORN_A %in% c(7,8,9)] <- NA
premium_data$DIBEV_A[premium_data$DIBEV_A %in% c(7,8,9)] <- NA
premium_data$PREDIB_A [premium_data$PREDIB_A %in% c(7,8,9)] <- NA
premium_data$GESDIB_A[premium_data$GESDIB_A %in% c(7,8,9)] <- NA
premium_data$ASEV_A[premium_data$ASEV_A %in% c(7,8,9)] <- NA
premium_data$LONGCOVD2_A [premium_data$LONGCOVD2_A %in% c(7,8,9)] <- NA
premium_data$EVERCOVD_A[premium_data$EVERCOVD_A %in% c(7,8,9)] <- NA
premium_data$ANXFREQ_A[premium_data$ANXFREQ_A %in% c(7,8,9)] <- NA
premium_data$DEPFREQ_A[premium_data$DEPFREQ_A %in% c(7,8,9)] <- NA
premium_data$SMOKELSEV1_A[premium_data$SMOKELSEV1_A %in% c(7,8,9)] <- NA
premium_data$SMKCIGST_A[premium_data$SMKCIGST_A %in% c(9)] <- NA
premium_data$DRKLIFE_A[premium_data$DRKLIFE_A %in% c(7,8,9)] <- NA
premium_data$DRK12MWK_A[premium_data$DRK12MWK_A %in% c(97,98,99)] <- NA
premium_data$DRKSTAT_A[premium_data$DRKSTAT_A %in% c(10)] <- NA
premium_data$MODFREQW_A[premium_data$MODFREQW_A %in% c(97,98,99)] <- NA
premium_data$MCPART_A[premium_data$MCPART_A %in% c(7,8,9)] <- NA
premium_data$MCCHOICE_A[premium_data$MCCHOICE_A %in% c(7)] <- NA
premium_data$EXCHANGE_A[premium_data$EXCHANGE_A %in% c(8)] <- NA
premium_data$PLNWRKR1_A[premium_data$PLNWRKR1_A %in% c(97,98,99)] <- NA

#non-categorical: AGEP_A,POVRATTC_A,PPSU, PSTRAT, WTFA_A

premium_data <- premium_data %>% mutate(across(c(SEX_A, RACEALLP_A, HISPALLP_A, MARITAL_A, NATUSBORN_A, ORIENT_A,
                                                 RATCAT_A, EDUCP_A, EMDOCCUPN2_A, EMDOCCUPN1_A, ACCSSINT_A, EMDINDSTN1_A,
                                                 DIBEV_A, PREDIB_A, GESDIB_A, HYPEV_A, ASEV_A, LONGCOVD2_A, EVERCOVD_A, ANXFREQ_A,DEPFREQ_A,
                                                 SMOKELSEV1_A, SMKCIGST_A, DRKLIFE_A, DRK12MWK_A, DRKSTAT_A, MODFREQW_A,
                                                 REGION, URBRRL23,
                                                 MCPART_A, MCCHOICE_A, EXCHANGE_A, PLNWRKR1_A),as.factor))

nrow(premium_data)

#create linear model

premium_model <- lm(HICOSTR1_A ~ AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                    POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + EMDINDSTN1_A + #socio-economic
                    DIBEV_A + PREDIB_A + GESDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                    SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                    REGION + URBRRL23 + #geography
                    MCPART_A + MCCHOICE_A + EXCHANGE_A + PLNWRKR1_A + #access to care
                    PPSU + PSTRAT + WTFA_A, 
                    #SEX_A * GESDIB_A + LONGCOVD2_A * EVERCOVD_A + DRKLIFE_A * DRK12MWK_A, #interactions: EMDOCCUPN2_A,EMDOCCUPN1_A,EMDINDSTN1_A,MCPART_A,MCCHOICE_A,EXCHANGE_A,PLNWRKR1_A
                    data=premium_data)

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


