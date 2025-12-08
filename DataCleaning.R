library(tidyverse)
library(ggplot2)
library(stringr)

##################Data Cleaning######################

#Read in Excel, convert to tibble for ease of use

survey_data <- read.csv("Data/adult24.csv")
survey_tibble <- tibble(survey_data)

total_surveyed = nrow(survey_tibble)

#Select only SAs with no dependents
survey_singles <- survey_tibble %>% filter(PRPLCOV1_A == 2) #%>% filter(MARITAL_A == 3)

#Getting rows with premium data, do not want to include NA, or other non-values
invalid_premium <- c(99999, 99998, 99997)

premium_data <- survey_singles %>% 
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

####################### Numerical & Graphical Summary of Data #########################

str(premium_data)
summary(premium_data)

num_total <- nrow(survey_data)

num_sample_adults <- nrow(premium_data) #number of SA with premium value

num_obs <- nobs(premium_model) #number of observations in model

num_NAs <- num_sample_adults - num_obs #number of people who did not answer some of the questions

sample_table <- data.frame(
  total_adults = num_total,
  premium_adults = num_sample_adults,
  invalid_responses = num_NAs
)

sample_table

##Premiums

p <- ggplot(premium_data, aes(x = HICOSTR1_A)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black")

# Extract histogram data
hist_data <- ggplot_build(p)$data[[1]]

# Find tallest bin
top_bin <- hist_data %>%
  slice_max(ncount, n = 1)   # or 'count' if you prefer raw counts

# Add label to the tallest bin
p + 
  annotate("text",
           x = top_bin$x,          # bin center
           y = top_bin$count,      # height of bar
           label = paste0("Peak: ", round(top_bin$x, 2)),
           vjust = -0.5,
           color = "black",
           fontface = "bold")

#Demographics
#Age Histogram
ggplot(premium_data, aes(x = AGEP_A)) +
  geom_histogram(
    binwidth = 5,
    fill = "lightblue",
    color = "black",
    size = 1,          # thicker bar borders
    alpha = 0.7        # more solid bars
  ) +
  theme_minimal() +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # centered title
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

nrow(premium_data)
mean(premium_data$HICOSTR1_A)
p <- ggplot(premium_data, aes(x = log(HICOSTR1_A))) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black")

# Extract histogram data
hist_data <- ggplot_build(p)$data[[1]]

# Find tallest bin
top_bin <- hist_data %>%
  slice_max(ncount, n = 1)   # or 'count' if you prefer raw counts

# Add label to the tallest bin
p + 
  annotate("text",
           x = top_bin$x,          # bin center
           y = top_bin$count,      # height of bar
           label = paste0("Peak bin: ", round(exp(top_bin$x), 2)),
           vjust = -0.5,
           color = "red",
           fontface = "bold")

#Gender Table
gender_table <- table(premium_data$SEX_A)
names(gender_table) <- c("Male", "Female")
gender_table

#Race Histogram

freq_table <- table(premium_data$HISPALLP_A)

df <- as.data.frame(freq_table)
names(df) <- c("Code", "Frequency")

labels <- c(
  "1" = "Hispanic",
  "2" = "Non-Hispanic White only",
  "3" = "Non-Hispanic Black/African American only",
  "4" = "Non-Hispanic Asian only",
  "5" = "Non-Hispanic AIAN only",
  "6" = "Non-Hispanic AIAN and any other group",
  "7" = "Other single and multiple races")

df$Description <- str_wrap(labels[as.character(df$Code)], width = 20)

ggplot(df, aes(x = reorder(Description, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label=Frequency), hjust=-0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal() +
  labs(title = "Frequency of Race/Ethnicity Groups",
       x = "Race/Ethnicity Group",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#Socio-Economic
#Poverty Ratio Density Plot

ggplot(premium_data, aes(x = POVRATTC_A)) +
  geom_density(
    fill = "lightgreen",
    alpha = 0.6,
    color = "darkgreen",
    size = 1.1          # thicker, more visible curve
  ) +
  theme_minimal() +
  labs(
    title = "Family Poverty Ratio",
    x = "Poverty Ratio",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

#Health Totals
#DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions

dibev_yes <- sum(premium_data$DIBEV_A == 1, na.rm = TRUE)
dibev_no  <- sum(premium_data$DIBEV_A == 2, na.rm = TRUE)

predib_yes <- sum(premium_data$PREDIB_A == 1, na.rm = TRUE)
predib_no  <- sum(premium_data$PREDIB_A == 2, na.rm = TRUE)

hypev_yes <- sum(premium_data$HYPEV_A == 1, na.rm = TRUE)
hypev_no  <- sum(premium_data$HYPEV_A == 2, na.rm = TRUE)

asev_yes <- sum(premium_data$ASEV_A == 1, na.rm = TRUE)
asev_no  <- sum(premium_data$ASEV_A == 2, na.rm = TRUE)

health_conditions <- data.frame(
  Condition = c("Diabetes", "Prediabetes", "Hypertension", "Asthma"),
  Has = c(dibev_yes, predib_yes, hypev_yes, asev_yes),
  Does_Not  = c(dibev_no,  predib_no,  hypev_no,  asev_no)
)

health_conditions

########## Linear Regression ###############

#creating the survey design object
library(survey)

svy_design <- svydesign(
  id = ~PPSU,           
  strata = ~PSTRAT,    
  weights = ~WTFA_A,    
  data = premium_data,    # Final filtered and cleaned data
  nest = TRUE
)

#MCPART_A + MCCHOICE_A + EXCHANGE_A + PLNWRKR1_A,

premium_model <- svyglm(HICOSTR1_A ~ AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                          POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + #socio-economic
                          DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                          SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                          REGION + URBRRL23 + #geography
                          SEX_A + EVERCOVD_A + DRKLIFE_A,
                          #dummy * GESDIB_A, #+ LONGCOVD2_A * EVERCOVD_A + DRKLIFE_A * DRK12MWK_A, #interactions: EMDOCCUPN2_A,EMDOCCUPN1_A,EMDINDSTN1_A,MCPART_A,MCCHOICE_A,EXCHANGE_A,PLNWRKR1_A
                          design = svy_design)


summary(premium_model)

####### Assumptions ##########

#Linearity Check - relationship between response and independant variables, scatter plot - consider transforms

#Continuous AGEP_A Linearity Check
ggplot(svy_design$variables, aes(x = AGEP_A, y = HICOSTR1_A)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_minimal()

#Continuous POVRATTC_A Linearity Check
ggplot(svy_design$variables, aes(x = POVRATTC_A, y = HICOSTR1_A)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_minimal()

#Variance of residuals, should be constant - plot residuals against predicted, should be randomly scattered no pattern
#Residuals vs Fitted

resid_df <- data.frame(
  Fitted = fitted(premium_model),
  Residuals = residuals(premium_model, type = "deviance")
)

ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  )

#Histogram of Residuals
ggplot(resid_df, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Histogram of Residuals")

#Collinearity - independant variablse should no be correlated - correlation matrix, compute variance inflation factor for each predictor
#high IF values indicate high collinearity

alias(premium_model)$Complete #DRKSTAT is collinear?

library(car)
vif(lm(HICOSTR1_A ~ AGEP_A + POVRATTC_A,
       data = svy_design$variables))

# Correlation matrix for continuous predictors
cor(svy_design$variables[, cont_vars], use = "complete.obs")

#observe outlier points in scatter plots, look at continuous covariates, possible replace/remove, influential points Cooks distance

# Fit lm for diagnostics
lm_model <- lm(HICOSTR1_A ~ ., data = svy_design$variables %>% select(HICOSTR1_A, all_of(all_vars)))

# Cook's distance
cooks <- cooks.distance(lm_model)
plot(cooks, type="h", main="Cook's Distance")
abline(h = 4/(nrow(svy_design$variables)), col="red", lty=2)

# Hat values
hat_vals <- hatvalues(lm_model)
plot(hat_vals, type="h")
abline(h = 2*mean(hat_vals), col="red", lty=2)

#mean of residuals should be 0

mean(residuals(premium_model))

#Normality of errors, residuals should be normally distributed -histogram for residuals (bell curve if normal)
#Q-Q - Plot

ggplot(resid_df, aes(sample = Residuals)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "QQ Plot of Deviance Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

#Leverage Points

#Jackknife Residuals & Bonferonni Correction

#Influential Points w/ Cooks Distance - Half Normal Plot

#Condition Number


############# Covariate Selection & Analysis ######################
#Hypothesis Test

#Confidence Interval

#F-Test/Anova
#remove each category from model
#Do one of health conditions affect health insurance premium?

premium_model_nodemo <- svyglm(HICOSTR1_A ~ #SEX_A + AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                                   POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + #socio-economic
                                   DIBEV_A + PREDIB_A + EVERCOVD_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                                   SMOKELSEV1_A + SMKCIGST_A + DRKLIFE_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                                   REGION + URBRRL23, #geography
                                 design = svy_design)

anova(premium_model_nodemo, premium_model)

premium_model_noecon<- svyglm(HICOSTR1_A ~ SEX_A + AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                                   #POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + #socio-economic
                                   DIBEV_A + PREDIB_A + EVERCOVD_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                                   SMOKELSEV1_A + SMKCIGST_A + DRKLIFE_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                                   REGION + URBRRL23, #geography
                                 design = svy_design)

anova(premium_model_noecon, premium_model)

premium_model_nohealth <- svyglm(HICOSTR1_A ~ SEX_A + AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                                   POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + #socio-economic
                                   #DIBEV_A + PREDIB_A + EVERCOVD_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                                   SMOKELSEV1_A + SMKCIGST_A + DRKLIFE_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                                   REGION + URBRRL23, #geography
                                 design = svy_design)

anova(premium_model_nohealth, premium_model)

premium_model_nobehav <- svyglm(HICOSTR1_A ~ SEX_A + AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                                   POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + #socio-economic
                                   DIBEV_A + PREDIB_A + EVERCOVD_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                                   #SMOKELSEV1_A + SMKCIGST_A + DRKLIFE_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                                   REGION + URBRRL23, #geography
                                 design = svy_design)

anova(premium_model_nobehav, premium_model)

premium_model_nogeo <- svyglm(HICOSTR1_A ~ SEX_A + AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                                  POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + #socio-economic
                                  DIBEV_A + PREDIB_A + EVERCOVD_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                                  SMOKELSEV1_A + SMKCIGST_A + DRKLIFE_A + DRKSTAT_A + MODFREQW_A, #health related behaviors
                                  #REGION + URBRRL23, #geography
                                design = svy_design)

anova(premium_model_nogeo, premium_model)

#Residual Standard Error/R^2 vs Num of Predictors
covariates <- c(
  "AGEP_A", "RACEALLP_A", "HISPALLP_A", "MARITAL_A", "NATUSBORN_A",
  "ORIENT_A", "POVRATTC_A", "RATCAT_A", "EDUCP_A", "ACCSSINT_A",
  "DIBEV_A", "PREDIB_A", "HYPEV_A", "ASEV_A", "ANXFREQ_A", "DEPFREQ_A",
  "SMOKELSEV1_A", "SMKCIGST_A", "DRKSTAT_A", "MODFREQW_A",
  "REGION", "URBRRL23", "SEX_A", "EVERCOVD_A", "DRKLIFE_A"
)


rse_values <- numeric(length(covariates))

for(i in 1:length(covariates)) {
  # Create formula with first i covariates
  formula_i <- as.formula(
    paste("HICOSTR1_A ~", paste(covariates[1:i], collapse = "+"))
  )
  
  # Fit survey-weighted model
  model_i <- svyglm(formula_i, design = svy_design)
  
  # Extract residual standard error from dispersion
  rse_values[i] <- sqrt(summary(model_i)$dispersion)
}

# Combine into a data frame for plotting
rse_df <- data.frame(
  NumCovariates = 1:length(covariates),
  RSE = rse_values
)

ggplot(rse_df, aes(x = NumCovariates, y = RSE)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  theme_minimal() +
  labs(
    title = "Residual Standard Error vs. Number of Covariates (Survey-Weighted)",
    x = "Number of Covariates",
    y = "Residual Standard Error"
  ) +
  scale_x_continuous(breaks = 1:length(covariates))





#Partial Regression Plot for POVRATTC_A
# 1. Residuals of Y after all other predictors
resid_y <- residuals(svyglm(HICOSTR1_A ~ AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                                               RATCAT_A + EDUCP_A + ACCSSINT_A + #socio-economic
                                               DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                                               SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                                               REGION + URBRRL23 + #geography
                                               SEX_A + EVERCOVD_A + DRKLIFE_A,
                                             #SEX_A * GESDIB_A + LONGCOVD2_A * EVERCOVD_A + DRKLIFE_A * DRK12MWK_A, #interactions: EMDOCCUPN2_A,EMDOCCUPN1_A,EMDINDSTN1_A,MCPART_A,MCCHOICE_A,EXCHANGE_A,PLNWRKR1_A
                                             design = svy_design))

# 2. Residuals of X_j after all other predictors
resid_x <- residuals(svyglm(POVRATTC_A ~ AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                              RATCAT_A + EDUCP_A + ACCSSINT_A + #socio-economic
                              DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                              SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                              REGION + URBRRL23 + #geography
                              SEX_A + EVERCOVD_A + DRKLIFE_A,
                            #SEX_A * GESDIB_A + LONGCOVD2_A * EVERCOVD_A + DRKLIFE_A * DRK12MWK_A, #interactions: EMDOCCUPN2_A,EMDOCCUPN1_A,EMDINDSTN1_A,MCPART_A,MCCHOICE_A,EXCHANGE_A,PLNWRKR1_A
                            design = svy_design))

partial_df <- data.frame(
  Residual_Y = resid_y,
  Residual_X = resid_x
)

ggplot(partial_df, aes(x = Residual_X, y = Residual_Y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  theme_minimal() +
  labs(title = "Partial Regression Plot for POVRATTC_A",
       x = "Residuals of POVRATTC_A ~ other predictors",
       y = "Residuals of HICOSTR1_A ~ other predictors")


#Forward Stepwise Selection

#Backward Stepwise Selection

#Hybrid/Both Selction


