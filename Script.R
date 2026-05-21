install.packages("readxl")
install.packages("dplyr")
install.packages("tseries")
install.packages("ARDL")
library(readxl)
library(dplyr)
library(tseries)
library(tidyverse)
library(ARDL)
data <- read_excel("ETW2510_A3 dataset change.xlsx", sheet = "Time-Series Data")
head(data)
str(data)
data_clean <- data %>%
  select(date, DI, GDP, UNEMP, PCE, TRANSFER)
head(data_clean)
summary(data_clean)

#Histogram for variables
hist(data$DI)
hist(data$GDP)
hist(data$UNEMP)
hist(data$PCE)
hist(data$TRANSFER)

colSums(is.na(data_clean))
DI_ts <- ts(data_clean$DI, frequency = 4, start = c(1951, 4))
DI_ts <- log(DI_ts)
GDP_ts <- ts(data_clean$GDP, frequency = 4, start = c(1951, 4))
UNEMP_ts <- ts(data_clean$UNEMP, frequency = 4, start = c(1951, 4))
UNEMP_ts <-log(UNEMP_ts)
PCE_ts <- ts(data_clean$PCE, frequency = 4, start = c(1951, 4))
PCE_ts <- log(PCE_ts)
TRANSFER_ts <- ts(data_clean$TRANSFER, frequency = 4, start = c(1951, 4))
TRANSFER_ts <- log(TRANSFER_ts)


# ADF tests at level
adf.test(DI_ts)
adf.test(GDP_ts)
adf.test(UNEMP_ts)
adf.test(PCE_ts)
adf.test(TRANSFER_ts)

# First differences
d_DI <- diff(DI_ts)
d_GDP <- diff(GDP_ts)
d_PCE <- diff(PCE_ts)
d_TRANSFER <- diff(TRANSFER_ts)

# ADF tests at first difference
adf.test(d_DI)
adf.test(d_GDP)
adf.test(d_PCE)
adf.test(d_TRANSFER)

hist(data$DI)
hist(data$GDP)
hist(data$UNEMP)
hist(data$PCE)
hist(data$TRANSFER)

# --- Automatic ARDL lag selection (max 4 lags per variable) ---
cat("========= ARDL MODEL ESTIMATION =========\n")
cat("Running auto_ardl() with AIC criterion (max order = 4)...\n\n")

auto_result <- auto_ardl(
  DI ~ GDP + UNEMP + PCE + TRANSFER,
  data      = data_clean,
  max_order = c(4, 4, 4, 4, 4),
  selection = "AIC"
)

selected_order <- auto_result$best_order
best_model     <- auto_result$best_model

cat(sprintf("Selected model: ARDL(%s)\n\n",
            paste(selected_order, collapse = ",")))

# Print ARDL regression summary
cat("--- ARDL Model Summary ---\n")
print(summary(best_model))

# --- Long-run coefficients ---
cat("\n--- Long-Run (Level) Coefficients ---\n")
print(multipliers(best_model))


# --- Bounds F-test ---
cat("\n========= BOUNDS F-TEST FOR COINTEGRATION (Case 3) =========\n")
cat("H0: No long-run relationship  (pi_1 = pi_2 = pi_3 = 0 in the UECM)\n")
cat("H1: Long-run relationship exists\n\n")

bounds_result <- bounds_f_test(best_model, case = 3)
bounds_result$tab
bounds_result

# Pesaran et al. (2001) critical values at the 5% significance level (k = 4):
# Lower bound: 4.38
# Upper bound: 5.71

cat("\nInterpretation guide:\n")
cat("  F > Upper bound (I(1)) --> Reject H0: cointegration EXISTS\n")
cat("  F < Lower bound (I(0)) --> Fail to reject H0: no cointegration\n")
cat("  F between bounds       --> Inconclusive\n\n")
cat("  F between bounds       --> Inconclusive\n\n")