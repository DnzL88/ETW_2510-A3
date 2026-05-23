install.packages("readxl")
install.packages("dplyr")
install.packages("tseries")
install.packages("ARDL")
install.packages("ggplot2")

library(readxl)
library(dplyr)
library(tseries)
library(urca)
library(ggplot2)
library(ARDL)

data <- read_excel("Dataset.xlsx", sheet = "Time-Series Data")
head(data)
str(data)

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

par(mfrow = c(1, 2))
acf(na.omit(resid(model_lm)), lag.max = 20, main = "ACF of residuals")
pacf(resid(model_lm), lag.max = 20, main = "PACF of residuals")

#--Breusch Godfrey serial correlation lm test
#Breusch-Godfrey test (order = 1, Chi-sq)
bg1 <- bgtest(model_lm, order = 1)
cat("--- (a) Breusch-Godfrey Serial Correlation Test (1 lags) ---\n")
cat("H0: No serial correlation in residuals up to lag 1\n")
print(bg1)

bg2 <- bgtest(model_lm, order = 2)
cat("--- (a) Breusch-Godfrey Serial Correlation Test (2 lags) ---\n")
cat("H0: No serial correlation in residuals up to lag 2\n")
print(bg2)

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

#Ljung box test
Box.test(resid(model_lm), lag = 4, type = "Ljung-Box")

#--Breusch Pagan Test for heteroskedasticity
bp <- bptest(model_lm)
cat("--- (b) Breusch-Pagan Heteroskedasticity Test ---\n")
cat("H0: Homoskedasticity (constant error variance)\n")
print(bp)
cat("Decision:", ifelse(bp$p.value > 0.05,
    "p > 0.05 -- PASS: No evidence of heteroskedasticity.\n\n",
    "p < 0.05 -- FAIL: Heteroskedasticity detected. Consider HC-robust SEs.\n\n"))
