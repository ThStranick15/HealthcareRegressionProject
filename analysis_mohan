library(qqplotr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(dplyr)
library(car)
data <- read.csv("adult24.csv", stringsAsFactors=FALSE)
data <- data.table(data)

invalid_values <- c(99999, 99998, 99997)
nrow(data)

# Filtering for valid premium values
premium_data <- data[(!is.na(data$HICOSTR1_A) & !data$HICOSTR1_A %in% invalid_values), ]
nrow(premium_data)

# filtering for plans that cover just single person (policyholder)
premium_data <- premium_data[premium_data$PRPLCOV1_A == 2]
nrow(premium_data)

"URBRRL23" %in% names(premium_data)

#selecting all covariates
premium_data <- premium_data %>% dplyr::select(HICOSTR1_A, #response y
                                        AGEP_A, SEX_A, RACEALLP_A, HISPALLP_A, MARITAL_A, NATUSBORN_A, ORIENT_A, #demographic
                                        POVRATTC_A, RATCAT_A, EDUCP_A, ACCSSINT_A, #socio-economic
                                        DIBEV_A, PREDIB_A, HYPEV_A, ASEV_A, EVERCOVD_A, ANXFREQ_A, DEPFREQ_A, #health conditions
                                        SMOKELSEV1_A, SMKCIGST_A, DRKLIFE_A, DRKSTAT_A, MODFREQW_A, #health related behaviors
                                        REGION, URBRRL23, #geography
                                        PPSU, PSTRAT, WTFA_A)#survey design
nrow(premium_data)

# Replacing invalid values with NA
premium_data$AGEP_A[premium_data$AGEP_A %in% c(97, 98, 99)] <- NA
premium_data$SEX_A[premium_data$SEX_A %in% c(7,8,9)] <- NA
premium_data$RACEALLP_A[premium_data$RACEALLP_A %in% c(7,8,9)] <- NA
premium_data$HISPALLP_A[premium_data$HISPALLP_A %in% c(97,98,99)] <- NA
premium_data$MARITAL_A[premium_data$MARITAL_A %in% c(7,8,9)] <- NA
premium_data$NATUSBORN_A[premium_data$NATUSBORN_A %in% c(7,8,9)] <- NA
premium_data$ORIENT_A[premium_data$ORIENT_A %in% c(7,8)] <- NA
premium_data$RATCAT_A[premium_data$RATCAT_A %in% c(98)] <- NA
premium_data$EDUCP_A[premium_data$EDUCP_A %in% c(97,98,99)] <- NA
premium_data$ACCSSINT_A[premium_data$ACCSSINT_A %in% c(7,8,9)] <- NA
premium_data$DIBEV_A[premium_data$DIBEV_A %in% c(7,8,9)] <- NA
premium_data$PREDIB_A [premium_data$PREDIB_A %in% c(7,8,9)] <- NA
premium_data$ASEV_A[premium_data$ASEV_A %in% c(7,8,9)] <- NA
premium_data$EVERCOVD_A[premium_data$EVERCOVD_A %in% c(7,8,9)] <- NA
premium_data$ANXFREQ_A[premium_data$ANXFREQ_A %in% c(7,8,9)] <- NA
premium_data$DEPFREQ_A[premium_data$DEPFREQ_A %in% c(7,8,9)] <- NA
premium_data$SMOKELSEV1_A[premium_data$SMOKELSEV1_A %in% c(7,8,9)] <- NA
premium_data$SMKCIGST_A[premium_data$SMKCIGST_A %in% c(9)] <- NA
premium_data$DRKLIFE_A[premium_data$DRKLIFE_A %in% c(7,8,9)] <- NA
premium_data$DRKSTAT_A[premium_data$DRKSTAT_A %in% c(10)] <- NA
premium_data$MODFREQW_A[premium_data$MODFREQW_A %in% c(97,98,99)] <- NA

premium_data <- premium_data %>% mutate(across(c(SEX_A, RACEALLP_A, HISPALLP_A, MARITAL_A, NATUSBORN_A, ORIENT_A,
                                                 RATCAT_A, EDUCP_A, ACCSSINT_A,
                                                 DIBEV_A, PREDIB_A, HYPEV_A, ASEV_A, EVERCOVD_A, ANXFREQ_A, DEPFREQ_A,
                                                 SMOKELSEV1_A, SMKCIGST_A, DRKLIFE_A, DRKSTAT_A, MODFREQW_A,
                                                 REGION, URBRRL23),as.factor))

nrow(premium_data)

summary(premium_data)

num_obs <- nobs(premium_model)

# Initially considering linear regression
model_lm <- lm(HICOSTR1_A ~ AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                  POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + 
                  DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                  SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                  REGION + URBRRL23 + #geography
                  SEX_A + EVERCOVD_A,
                data = premium_data)
# Extract residuals and fitted values manually
residuals_OLS <- resid(model_lm)
fitted_vals_OLS <- fitted(model_lm)

# Create data frame for plotting
plot_data <- data.frame(
  fitted = fitted_vals_OLS,
  residuals = residuals_OLS
)

# Creating residual vs fitted values plot plot
ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point(
    alpha = 0.6,           # Transparency for overplotting
    size = 2,              # Slightly larger points
    color = "#2E86AB",     # Attractive blue color
    shape = 19             # Filled circles
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#E15554",     # Contrasting red color
    linewidth = 0.8
  ) +
  # Add smoothing line to see patterns
  geom_smooth(
    method = "loess",
    se = TRUE,             # Confidence band
    color = "#3A2E39",     # Dark color for smooth line
    fill = "#E1CE7A",      # Light yellow for confidence band
    alpha = 0.2,
    linewidth = 1
  ) +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs. Fitted Values",
    subtitle = "OLS Model Diagnostic Plot"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      color = "gray40",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    # plot.background = element_rect(fill = "white", color = NA)
  ) +
  # Add informative caption
  labs(caption = "Dashed red line at y=0 | Gray band shows LOESS smooth with 95% CI")

## QQ plot to check normality assumption of response variable
qqnorm(residuals_OLS, main = "Normal Q-Q Plot of LM Residuals")
# Add a reference line (essential for visual assessment)
qqline(residuals_OLS)
# Create a data frame
data_for_qq <- data.frame(residuals = residuals_OLS)

ggplot(data_for_qq, aes(sample = residuals)) +
  # Adds the points (quantiles of the sample vs. theoretical normal)
  stat_qq_point(
    size = 2,           # Slightly larger points
    color = "#1F78B4"   # A nice blue color
  ) +
  # Adds the confidence band (95% is standard)
  stat_qq_band(
    fill = "grey80",    # Light grey band
    alpha = 0.5         # Semi-transparent
  ) +
  # Adds the diagonal reference line
  stat_qq_line(
    color = "red",
    linetype = "dashed" # Use a dashed line for contrast
  ) +
  # Labels and Title
  labs(
    title = "Normal Q-Q Plot of OLS Residuals",
    # subtitle = "Assessing the Normality Assumption",
    x = "Theoretical Quantiles (Normal Distribution)",
    y = "Sample Quantiles (Residuals)"
  ) +
  # Use a clean theme
  theme_classic() +
  # Further theme adjustments for a professional look
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


# creating the system design object
library(survey)

svy_design <- svydesign(
  id = ~PPSU,           
  strata = ~PSTRAT,    
  weights = ~WTFA_A,    
  data = premium_data,    # Final filtered and cleaned data
  nest = TRUE
)

premium_data[premium_data$HICOSTR1_A == 0]

# building the regression model
model_GLM <- svyglm(HICOSTR1_A ~ AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                  POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + 
                  DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                  SMOKELSEV1_A + SMKCIGST_A + DRKLIFE_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                  REGION + URBRRL23 + #geography
                  SEX_A + EVERCOVD_A,
                  family = Gamma(link = "log"),
                design = svy_design)

# Extract residuals and fitted values manually
residuals_gamma <- residuals(model_GLM, type = "deviance")
fitted_vals_gamma <- fitted(model_GLM)
plot(fitted_vals_gamma, residuals_gamma, xlab = "Fitted Values (Gamma link)", ylab = "Residuals (Gamma link)")
abline(h = 0, lty = 2)

# Create data frame for plotting
plot_data <- data.frame(
  fitted = fitted_vals_gamma,
  residuals = residuals_gamma
)

# Creating residual vs fitted values plot plot
ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point(
    alpha = 0.6,           # Transparency for overplotting
    size = 2,              # Slightly larger points
    color = "#2E86AB",     # Attractive blue color
    shape = 19             # Filled circles
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#E15554",     # Contrasting red color
    linewidth = 0.8
  ) +
  # Add smoothing line to see patterns
  geom_smooth(
    method = "loess",
    se = TRUE,             # Confidence band
    color = "#3A2E39",     # Dark color for smooth line
    fill = "#E1CE7A",      # Light yellow for confidence band
    alpha = 0.2,
    linewidth = 1
  ) +
  labs(
    x = "Fitted Values",
    y = "Deviance Residuals",
    title = "Deviance Residuals vs. Fitted Values",
    subtitle = "GLM (Gamma - Log link) Model Diagnostic Plot"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      color = "gray40",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    # plot.background = element_rect(fill = "white", color = NA)
  ) +
  # Add informative caption
  labs(caption = "Dashed red line at y=0 | Gray band shows LOESS smooth with 95% CI")

# Heteroscedasticity (Non-constant variance): Clear funnel/fan shape — residuals spread increases with fitted values. 
# Variance is not constant, violating a key regression assumption.

# Large positive residuals (up to 50,000+) but negative residuals are bounded (around -5,000). 
# Suggests the response variable is right-skewed with outliers.

# Several extreme points in the upper region (30,000-60,000) are likely influential observations.

# Checking Multi-Collinearity
summary(model_GLM)$aliased          # TRUE for aliased coefficients
alias(model_GLM)                    # shows exact linear dependencies
with(premium_data, table(RACEALLP_A, HISPALLP_A))

data_for_qq <- data.frame(residuals = residuals_gamma)

ggplot(data_for_qq, aes(sample = residuals)) +
  # Adds the points (quantiles of the sample vs. theoretical normal)
  stat_qq_point(
    size = 2,           # Slightly larger points
    color = "#1F78B4"   # A nice blue color
  ) +
  # Adds the confidence band (95% is standard)
  stat_qq_band(
    fill = "grey80",    # Light grey band
    alpha = 0.5         # Semi-transparent
  ) +
  # Adds the diagonal reference line
  stat_qq_line(
    color = "red",
    linetype = "dashed" # Use a dashed line for contrast
  ) +
  # Labels and Title
  labs(
    title = "Normal Q-Q Plot of GLM (Gamma - Log) Residuals",
    # subtitle = "Assessing the Normality Assumption",
    x = "Theoretical Quantiles (Normal Distribution)",
    y = "Sample Quantiles (Residuals)"
  ) +
  # Use a clean theme
  theme_classic() +
  # Further theme adjustments for a professional look
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


library(mgcv)      # for gam()
library(gratia) 

# GAM smooth curves
gam_diag <- gam(
  HICOSTR1_A ~ s(AGEP_A, k = 6) + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
    s(POVRATTC_A, k = 6) + RATCAT_A + EDUCP_A + ACCSSINT_A + 
    DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
    SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
    REGION + URBRRL23 + #geography
    SEX_A + EVERCOVD_A,
  family = Gamma(link = "log"),
  data   = premium_data,
  weights = WTFA_A     # optional: include survey weights as case-weights
)
draw(gam_diag)

library(car)
library(ggplot2)

model_GLM_v2 <- glm(HICOSTR1_A ~ AGEP_A + RACEALLP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                      POVRATTC_A + RATCAT_A + EDUCP_A + ACCSSINT_A + 
                      DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                      SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                      REGION + URBRRL23 + #geography
                      SEX_A + EVERCOVD_A,
                    family = Gamma(link = "log"),
                    data = premium_data)
# Calculating VIFs
v <- vif(model_GLM_v2)

library(DescTools)
library(dplyr)
library(ggplot2)

cat_vars <- c("RACEALLP_A", "HISPALLP_A", "SEX_A", "MARITAL_A", "NATUSBORN_A", "ORIENT_A",
              "POVRATTC_A", "RATCAT_A", "EDUCP_A", "ACCSSINT_A", "DIBEV_A", "PREDIB_A", "HYPEV_A",
              "ASEV_A", "ANXFREQ_A", "DEPFREQ_A", "SMOKELSEV1_A", "SMKCIGST_A", "MODFREQW_A", "EVERCOVD_A",
              "DRKSTAT_A", "DRKLIFE_A", "REGION", "URBRRL23")

# compute Cramér's V for each pair of categorical vars
pairs <- expand.grid(cat_vars, cat_vars, stringsAsFactors = FALSE)
cramer_mat <- matrix(NA, nrow = length(cat_vars), ncol = length(cat_vars),
                     dimnames = list(cat_vars, cat_vars))

for (i in seq_along(cat_vars)) {
  for (j in seq_along(cat_vars)) {
    tab <- table(premium_data[[cat_vars[i]]], premium_data[[cat_vars[j]]])
    cramer_mat[i, j] <- CramerV(tab, bias.correct = TRUE)
  }
}

# heatmap of correlation
melted <- reshape2::melt(cramer_mat, na.rm = TRUE)

ggplot(melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#ffffff", high = "#264653", na.value = "white") +
  coord_equal() +
  labs(x = "", y = "", fill = "Cramér's V",
       title = "Association between categorical predictors") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Cook's distance (influential points)
cooks_d <- cooks.distance(model_GLM)
plot(cooks_d, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
# abline(h = 4/length(cooks_d), col = "red", lty = 2)  # Common threshold
abline(h = 4/length(cooks_d), col="blue", lwd=2, lty=2)   # conservative threshold

# Get top 10 influential points
top_idx <- order(cooks_d, decreasing = TRUE)[1:10]

# Create a summary table
top_influential <- data.frame(
  index = top_idx,
  cooks_distance = cooks_d[top_idx],
  fitted_values = fitted(model_GLM)[top_idx],
  residuals = residuals(model_GLM, type = "deviance")[top_idx]
)

print(top_influential)

length(cooks_d)
# Identify influential observations
influential <- which(cooks_d > 4/length(cooks_d))
print(paste("Number of influential points:", length(influential)))


# excluding outliers
premium_data_clean <- premium_data[-top_idx, ]

svy_design_2 <- svydesign(
  id = ~PPSU,           
  strata = ~PSTRAT,    
  weights = ~WTFA_A,    
  data = premium_data_clean,    # Final filtered and cleaned data
  nest = TRUE
)
# building the regression model w/o outliers and collinear factors
model_GLM_wo_outliers <- svyglm(HICOSTR1_A ~ AGEP_A + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                      POVRATTC_A + EDUCP_A + ACCSSINT_A + 
                      DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                      SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                      REGION + URBRRL23 + #geography
                      SEX_A + EVERCOVD_A,
                    family = Gamma(link = "log"),
                    design = svy_design_2)

d <- summary(model_GLM_wo_outliers)$dispersion
print(paste("Dispersion parameter:", round(d, 4)))

# Extract residuals and fitted values manually
residuals_gamma <- residuals(model_GLM_wo_outliers, type = "deviance")
fitted_vals_gamma <- fitted(model_GLM_wo_outliers)
plot(fitted_vals_gamma, residuals_gamma, xlab = "Fitted Values (Gamma link)", ylab = "Residuals (Gamma link)")
abline(h = 0, lty = 2)

# Create data frame for plotting
plot_data <- data.frame(
  fitted = fitted_vals_gamma,
  residuals = residuals_gamma
)

# Creating residual vs fitted values plot plot
ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point(
    alpha = 0.6,           # Transparency for overplotting
    size = 2,              # Slightly larger points
    color = "#2E86AB",     # Attractive blue color
    shape = 19             # Filled circles
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#E15554",     # Contrasting red color
    linewidth = 0.8
  ) +
  # Add smoothing line to see patterns
  geom_smooth(
    method = "loess",
    se = TRUE,             # Confidence band
    color = "#3A2E39",     # Dark color for smooth line
    fill = "#E1CE7A",      # Light yellow for confidence band
    alpha = 0.2,
    linewidth = 1
  ) +
  labs(
    x = "Fitted Values",
    y = "Deviance Residuals",
    title = "Deviance Residuals vs. Fitted Values",
    subtitle = "GLM (Gamma - Log link) Model Diagnostic Plot"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      color = "gray40",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.5),
    # plot.background = element_rect(fill = "white", color = NA)
  ) +
  # Add informative caption
  labs(caption = "Dashed red line at y=0 | Gray band shows LOESS smooth with 95% CI")


# Dispersion parameter
dispersion <- summary(model_GLM)$dispersion
print(paste("Dispersion parameter:", round(dispersion, 4)))

# Influential points in the raw data
mf <- model.frame(model_GLM)

# Add Cook's distance and keep only those rows
inf_tbl <- cbind(
  mf[top_idx, ],
  cooks_distance = cooks_d[top_idx]
)

inf_tbl
write.csv(inf_tbl, "top_10_influential.csv")


# Partial Residual Plots (Component + Residual Plots)
library(car)

mf <- model.frame(model_gamma)  # data actually used

library(mgcv)
gam_model <- gam(HICOSTR1_A ~ s(AGEP_A) + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
                   s(POVRATTC_A) + EDUCP_A + ACCSSINT_A + 
                   DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
                   SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
                   REGION + URBRRL23 + #geography
                   SEX_A + EVERCOVD_A,
                 family = Gamma(link="log"),
                 data = mf)
plot(gam_model)


# observed non-linear relationship between Age and log(premium), so decided to use splines for Age
library(splines)

model_gamma_splines <- svyglm(
    HICOSTR1_A ~ ns(AGEP_A, df = 4) + HISPALLP_A + MARITAL_A + NATUSBORN_A + ORIENT_A + #demographic
    POVRATTC_A + EDUCP_A + ACCSSINT_A + 
    DIBEV_A + PREDIB_A + HYPEV_A + ASEV_A + ANXFREQ_A + DEPFREQ_A + #health conditions
    SMOKELSEV1_A + SMKCIGST_A + DRKSTAT_A + MODFREQW_A + #health related behaviors
    REGION + URBRRL23 + #geography
    SEX_A + EVERCOVD_A,
  family = Gamma(link = "log"),
  design = svy_design
)

# Extract residuals
dev_resid_2 <- residuals(model_gamma_splines, type = "deviance")
fitted_vals_2 <- fitted(model_gamma_splines)

plot(fitted_vals_2, dev_resid_2, xlab = "Fitted Values (Gamma link)", ylab = "Residuals (Gamma link)")
abline(h = 0, lty = 2)

qqnorm(dev_resid_2, main = "Q-Q Plot of Deviance Residuals")
qqline(dev_resid_2, col = "red", lwd = 2)












