install.packages("readxl")
install.packages("dplyr")
install.packages("tseries")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(tseries)
library(ggplot2)
data <- read_excel("ETW2510_A3 dataset.xlsx", sheet = "Time-Series Data")
head(data)
str(data)
data_clean <- data %>%
  select(date, DI, GDP, UNEMP, PCE, TRANSFER)
head(data_clean)
summary(data_clean)
colSums(is.na(data_clean))
DI_ts <- ts(data_clean$DI, frequency = 4, start = c(1951, 4))
GDP_ts <- ts(data_clean$GDP, frequency = 4, start = c(1951, 4))
UNEMP_ts <- ts(data_clean$UNEMP, frequency = 4, start = c(1951, 4))
PCE_ts <- ts(data_clean$PCE, frequency = 4, start = c(1951, 4))
TRANSFER_ts <- ts(data_clean$TRANSFER, frequency = 4, start = c(1951, 4))

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
