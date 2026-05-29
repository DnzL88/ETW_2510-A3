install.packages("readxl")
install.packages("dplyr")
install.packages("tseries")
install.packages("ARDL")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("strucchange")

library(readxl)
library(dplyr)
library(tseries)
library(urca)
library(ggplot2)
library(ARDL)
library(tidyverse)
library(lmtest)
library(strucchange)

data <- read_excel("Dataset.xlsx", sheet = "Time-Series Data")
head(data)
str(data)

# Histogram for each variable to decide whether to apply log-transform
ggplot(data, aes(x = DI)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8 ) +
  labs(title = "Distribution of Disposable Income", x = "Disposable Income (US$)", y = "Count") +
  theme_minimal()

ggplot(data, aes(x = GDP)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8 ) +
  labs(title = "Distribution of GDP", x = "GDP (US$ bil)", y = "Count") +
  theme_minimal()

ggplot(data, aes(x = UNEMP)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8 ) +
  labs(title = "Distribution of Unemployment rate", x = "Unemployment rate (%)", y = "Count") +
  theme_minimal()

ggplot(data, aes(x = PCE)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8 ) +
  labs(title = "Distribution of Personal Consumption Expenditure", x = "PCE (US$ bil)", y = "Count") +
  theme_minimal()

ggplot(data, aes(x = TRANSFER)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8 ) +
  labs(title = "Distribution of Federal Government Transfer", x = "TRANSFER (US$ bil)", y = "Count") +
  theme_minimal()

#---Clean Columns---
data_clean <- data %>%
  select(date, DI, GDP, UNEMP, PCE, TRANSFER) %>%
  na.omit() %>%
  mutate(
    DI       = log(DI),
    GDP      = log(GDP),
    PCE      = log(PCE),
    TRANSFER = log(TRANSFER)
    # UNEMP is NOT logged — rates are never log-transformed
  )

head(data_clean)
summary(data_clean)
colSums(is.na(data_clean))

# Histogram for each variable after log-transform
ggplot(data_clean, aes(x = DI)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8) +
  labs(title = "Distribution of Disposable Income (log-transform)", x = "log(Disposable Income)", y = "Count") +
  theme_minimal()

ggplot(data_clean, aes(x = GDP)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8 ) +
  labs(title = "Distribution of GDP (log-transform)", x = "log(GDP)", y = "Count") +
  theme_minimal()

ggplot(data_clean, aes(x = PCE)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8 ) +
  labs(title = "Distribution of Personal Consumption Expenditure (log-transform)", x = "log(PCE)", y = "Count") +
  theme_minimal()

ggplot(data_clean, aes(x = TRANSFER)) +
  geom_histogram(fill = "steelblue",   color = "white",     alpha = 0.8 ) +
  labs(title = "Distribution of Federal Government Transfer (log-transform)", x = "log(TRANSFER)", y = "Count") +
  theme_minimal()

#---Convert to Time Series Object---
DI_ts <- ts(data_clean$DI, frequency = 4, start = c(1951, 4))
GDP_ts <- ts(data_clean$GDP, frequency = 4, start = c(1951, 4))
UNEMP_ts <- ts(data_clean$UNEMP, frequency = 4, start = c(1951, 4))
PCE_ts <- ts(data_clean$PCE, frequency = 4, start = c(1951, 4))
TRANSFER_ts <- ts(data_clean$TRANSFER, frequency = 4, start = c(1951, 4))

#---ADF tests at level---
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

# PP tests at level as robustness check
pp.test(DI_ts)
pp.test(GDP_ts)
pp.test(UNEMP_ts)
pp.test(PCE_ts)
pp.test(TRANSFER_ts)

# PP tests at first difference as robustness check
pp.test(d_DI)
pp.test(d_GDP)
pp.test(d_PCE)
pp.test(d_TRANSFER)

# Plot selected variables over time
ggplot(data_clean, aes(x = date, y = DI)) +
  geom_line() +
  labs(title = "Disposable Income over Time", x = "Date", y = "DI")

ggplot(data_clean, aes(x = date, y = GDP)) +
  geom_line() +
  labs(title = "GDP over Time", x = "Date", y = "GDP")

ggplot(data_clean, aes(x = date, y = UNEMP)) +
  geom_line() +
  labs(title = "Unemployment Rate over Time", x = "Date", y = "UNEMP")

ggplot(data_clean, aes(x = date, y = PCE)) +
  geom_line() +
  labs(title = "Personal Consumption Expenditure over Time", x = "Date", y = "PCE")

ggplot(data_clean, aes(x = date, y = TRANSFER)) +
  geom_line() +
  labs(title = "Federal Government Transfer over Time", x = "Date", y = "TRANSFER")

# Bound test
# --- Automatic ARDL lag selection (max 4 lags per variable) ---
cat("========= ARDL MODEL ESTIMATION =========\n")
cat("Running auto_ardl() with AIC criterion (max order = 4)...\n\n")

auto_result <- auto_ardl(
  DI ~ GDP + UNEMP + PCE + TRANSFER,
  data      = data_clean,
  max_order = c(4, 4, 4, 4, 4),
  # Using BIC yields higher F-test, yet AIC is already significant.
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
print(bounds_result)

# Pesaran et al. (2001) critical values (k = 4):
#   - Lower bound (I(0)): 3.79-4.05
#   - Upper bound (I(1)): 4.85-5.20  

cat("\nInterpretation guide:\n")
cat("  F > Upper bound (I(1)) --> Reject H0: cointegration EXISTS\n")
cat("  F < Lower bound (I(0)) --> Fail to reject H0: no cointegration\n")
cat("  F between bounds       --> Inconclusive\n\n")

# --- Extract UECM (Unrestricted ECM) ---
uecm_model <- uecm(best_model)
summary(uecm_model)

cat("\n--- Long-run Coefficients with HAC standard errors ---\n")
print(multipliers(best_model,vcov = vcovHAC(uecm_model)))

# --- Extract Restricted ECM ---
recm_model <- recm(best_model, case = 3)
summary(recm_model)

# ---Error Correction Term---
cat("\nInterpretation guide:\n")
cat("\nECT must be: negative + statistically significant\n")
cat("\nIf positive or insignificant → no valid adjustment mechanism\n")

#-----------------------------------------------------------------------------------------
#Convert ARDL to lm for diagnostic testing
model_lm <- to_lm(best_model, fix_names = TRUE)
summary(model_lm)

#Correlograms of residuals
par(mfrow = c(1, 2))
acf(na.omit(resid(model_lm)), lag.max = 20, main = "ACF of residuals")
pacf(resid(model_lm), lag.max = 20, main = "PACF of residuals")
par(mfrow = c(1, 1))

#--Breusch Godfrey serial correlation lm test
#Breusch-Godfrey test (order = 3, Chi-sq)
bg3 <- bgtest(model_lm, order = 3)
cat("--- (a) Breusch-Godfrey Serial Correlation Test (3 lags) ---\n")
cat("H0: No serial correlation in residuals up to lag 3\n")
print(bg3)

#Breusch-Godfrey test (order = 4, Chi-sq)
bg4 <- bgtest(model_lm, order = 4, type = "Chisq")
cat("--- (a) Breusch-Godfrey Serial Correlation Test (4 lags) ---\n")
cat("H0: No serial correlation in residuals up to lag 4\n")
print(bg4)

#Breusch-Godfrey test (order = 4, F)
bg4 <- bgtest(model_lm, order = 4, type = "F")
cat("--- (a) Breusch-Godfrey Serial Correlation Test (4 lags) ---\n")
cat("H0: No serial correlation in residuals up to lag 4\n")
print(bg4)

cat("\n--- OLS with HAC (Newey-West) standard errors ---\n")
coeftest(model_lm, vcov. = vcovHAC)


se_ols  <- sqrt(diag(vcov(model_lm)))
se_hac  <- sqrt(diag(vcovHAC(model_lm)))
round(cbind(OLS = se_ols, HAC = se_hac), 4)


#--Breusch Pagan Test for heteroskedasticity
bp <- bptest(model_lm)
cat("--- (b) Breusch-Pagan Heteroskedasticity Test ---\n")
cat("H0: Homoskedasticity (constant error variance)\n")
print(bp)
cat("Decision:", ifelse(bp$p.value > 0.05,
    "p > 0.05 -- PASS: No evidence of heteroskedasticity.\n\n",
    "p < 0.05 -- FAIL: Heteroskedasticity detected. Consider HC-robust SEs.\n\n"))

#Visualisaing the residuals to check if there exists heteroskedasticity.
ggplot(data = data.frame(fitted = fitted(model_lm), resid = resid(model_lm)),
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

# Jarque-Bera test for residual normality
jarque.bera.test(resid(model_lm))

# Ramsey RESET test for functional form
resettest(model_lm)

# CUSUM stability test
cusum_test <- efp(formula(model_lm), data = model_lm$model, type = "Rec-CUSUM")
plot(cusum_test)
sctest(cusum_test)

# MOSUM stability test as an additional stability check
mosum_test <- efp(formula(model_lm), data = model_lm$model, type = "OLS-MOSUM")
plot(mosum_test)
sctest(mosum_test)

# CUSUMSQ-style stability plot based on recursive residuals
# Get recursive residuals
rec_resid <- recresid(model_lm)

# Cumulative sum of squared recursive residuals
cusumsq_process <- cumsum(rec_resid^2) / sum(rec_resid^2)

# Time index
time_index <- seq_along(cusumsq_process) / length(cusumsq_process)

# Plot CUSUMSQ process
plot(
  time_index, cusumsq_process,
  type = "l",
  lwd = 2,
  main = "CUSUMSQ Plot Based on Recursive Residuals",
  xlab = "Time",
  ylab = "Cumulative Sum of Squared Recursive Residuals"
)

# Reference line
abline(0, 1, col = "red", lty = 2)
