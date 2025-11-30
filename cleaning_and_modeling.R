
library(data.table)
library(tidyverse)
library(dplyr)
data <- read.csv("adult24.csv", stringsAsFactors=FALSE) # reading all variables as factors
data <- data.table(data)

invalid_values <- c(99999, 99998, 99997)
nrow(data)

# Filtering for valid premium values
premium_data <- data[(!is.na(data$HICOSTR1_A) & !data$HICOSTR1_A %in% invalid_values), ]
nrow(premium_data)

premium_data <- premium_data[premium_data$POLHLD1_A == 1]

table(premium_data$EMDINDSTN1_A)

# Selecting the required variables
premium_data_filtered <- premium_data %>% select("AGEP_A", "SEX_A","RACEALLP_A", "HISPALLP_A", "MARITAL_A",
                                                 "POVRATTC_A","RATCAT_A","EDUCP_A","EMDOCCUPN2_A",
                                                 "PREDIB_A","GESDIB_A","HYPEV_A","ASEV_A","LONGCOVD2_A",
                                                 "SMOKELSEV1_A","SMKCIGST_A",
                                                 "DRKLIFE_A","DRK12MWK_A","DRKSTAT_A","MODFREQW_A","REGION",
                                                 "URBRRL23","MCPART_A","MCCHOICE_A",
                                                 "EXCHANGE_A","PLNWRKR1_A","PRPLCOV1_A","HICOSTR1_A","PPSU",
                                                 "PSTRAT")

# Replacing invalid values with NA

table(premium_data_filtered$AGEP_A)  # Age of the sample adult
premium_data_filtered[['AGEP_A']][premium_data_filtered[['AGEP_A']] >= 97] <- NA

table(premium_data_filtered$SEX_A) # Sexual orientation of the sample adult
premium_data_filtered[['SEX_A']][premium_data_filtered[['SEX_A']] > 2] <- NA

table(premium_data_filtered$RACEALLP_A) # Race of the sample adult
premium_data_filtered[['RACEALLP_A']][premium_data_filtered[['RACEALLP_A']] >= 7] <- NA

table(premium_data_filtered$HISPALLP_A) # Race of the sample adult
premium_data_filtered[['RACEALLP_A']][premium_data_filtered[['RACEALLP_A']] >= 97] <- NA

table(premium_data_filtered$MARITAL_A) # Marital status of the sample adult
premium_data_filtered[['MARITAL_A']][premium_data_filtered[['MARITAL_A']] >= 7] <- NA

table(premium_data_filtered$POVRATTC_A) # family poverty ratio for sample adult

table(premium_data_filtered$RATCAT_A) # Ratio of family-income to poverty threshold for sample adult's family
premium_data_filtered[['RATCAT_A']][premium_data_filtered[['RATCAT_A']] == 98] <- NA

table(premium_data_filtered$EDUCP_A) # Educational level of sample adult
premium_data_filtered[['EDUCP_A']][premium_data_filtered[['EDUCP_A']] >= 97] <- NA

table(premium_data_filtered$EMDOCCUPN2_A) # Employement industry of sample adult
premium_data_filtered[['EMDOCCUPN2_A']][premium_data_filtered[['EMDOCCUPN2_A']] >= 97] <- NA

table(premium_data_filtered$PREDIB_A) # Employement industry of sample adult
premium_data_filtered[['PREDIB_A']][premium_data_filtered[['PREDIB_A']] >= 7] <- NA

table(premium_data_filtered$PREDIB_A) # Whether sample adult had prediabetes
premium_data_filtered[['PREDIB_A']][premium_data_filtered[['PREDIB_A']] >= 7] <- NA

table(premium_data_filtered$GESDIB_A) # Whether sample adult had gestational diabetes
premium_data_filtered[['GESDIB_A']][premium_data_filtered[['GESDIB_A']] >= 7] <- NA

table(premium_data_filtered$HYPEV_A) # Whether sample adult had hypertension
premium_data_filtered[['HYPEV_A']][premium_data_filtered[['HYPEV_A']] >= 7] <- NA

table(premium_data_filtered$ASEV_A) # Whether sample adult had Asthma
premium_data_filtered[['ASEV_A']][premium_data_filtered[['ASEV_A']] >= 7] <- NA

table(premium_data_filtered$LONGCOVD2_A) # Whether sample adult had COVID symptoms longer than 3 or more months
premium_data_filtered[['LONGCOVD2_A']][premium_data_filtered[['LONGCOVD2_A']] >= 7] <- NA

table(premium_data_filtered$SMOKELSEV1_A) # Whether sample adult had used tobacco products (even one time)
premium_data_filtered[['SMOKELSEV1_A']][premium_data_filtered[['SMOKELSEV1_A']] >= 7] <- NA

table(premium_data_filtered$SMKCIGST_A) # sample adult current smoking status
premium_data_filtered[['SMKCIGST_A']][premium_data_filtered[['SMKCIGST_A']] == 9] <- NA

table(premium_data_filtered$DRKLIFE_A) # Whether sample adult had atleast one alcohol drink
premium_data_filtered[['DRKLIFE_A']][premium_data_filtered[['DRKLIFE_A']] >= 7] <- NA

table(premium_data_filtered$DRK12MWK_A) # sample adult alcohol drinks per week in l12m
premium_data_filtered[['DRK12MWK_A']][premium_data_filtered[['DRK12MWK_A']] >= 97] <- NA

table(premium_data_filtered$DRKSTAT_A) # sample adult current alcohol status
premium_data_filtered[['DRKSTAT_A']][premium_data_filtered[['DRKSTAT_A']] == 10] <- NA

table(premium_data_filtered$MODFREQW_A) # sample adult frequency of moderate physical activity (times per week)
premium_data_filtered[['MODFREQW_A']][premium_data_filtered[['MODFREQW_A']] >= 97] <- NA

table(premium_data_filtered$REGION) # sample adult region



# creating the survey design object
library(survey)

svy_design <- svydesign(
  id = ~PPSU,           
  strata = ~PSTRATA,    
  weights = ~WTFA_A,    
  data = premium_data_filtered,    # Final filtered and cleaned data
  nest = TRUE
)

# building the regression model
model <- svyglm(HICOSTR1_A ~ ., 
                design = svy_design)  # This is default
summary(model)






