install.packages("readxl")
install.packages("dplyr")
install.packages("tseries")
library(readxl)
library(dplyr)
library(tseries)
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