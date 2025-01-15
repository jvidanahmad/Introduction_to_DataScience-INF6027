#INSTALL AND LOAD ALL PACKAGES USED IN THE ANALYSIS
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("stats")
install.packages("TTR")
install.packages("zoo")
install.packages("corrplot")
library(dplyr)
library(caret)
library(ggplot2)
library(tidyverse)
library(stats)
library(TTR)
library(zoo)
library(corrplot)

# Set the path as the project folder path

#LOADING TECHNOLOGY INDUSTRY STOCKS - APPLE, MICROSOFT, NVIDIA
apple_data <- read.csv("Datasets/AAPL.csv", header = TRUE, stringsAsFactors = FALSE)
microsoft_data <- read.csv("Datasets/MSFT.csv", header = TRUE, stringsAsFactors = FALSE)
nvidia_data <- read.csv("Datasets/NVDA.csv", header = TRUE, stringsAsFactors = FALSE)

#LOADING HEALTHCARE INDUSTRY STOCKS - PFIZER, JOHNSON&JOHNSON, UNITEDHEALTH GROUP
pfizer_data <- read.csv("Datasets/PFE.csv", header = TRUE, stringsAsFactors = FALSE)
johnson_data <- read.csv("Datasets/JNJ.csv", header = TRUE, stringsAsFactors = FALSE)
unitedhealth_data <- read.csv("Datasets/UNH.csv", header = TRUE, stringsAsFactors = FALSE)

#LOADING ENERGY INDUSTRY STOCKS - NEXTERA ENERGY, CONOCOPHILIPS, EXXONMOBIL
nextera_data <- read.csv("Datasets/NEE.csv", header = TRUE, stringsAsFactors = FALSE)
conoco_data <- read.csv("Datasets/COP.csv", header = TRUE, stringsAsFactors = FALSE)
exxon_data <- read.csv("Datasets/XOM.csv", header = TRUE, stringsAsFactors = FALSE)
#DATA EXPLOARTION AND CLEANING

#1.Inspecting loaded data
str(apple_data)
head(apple_data)

str(microsoft_data)
head(microsoft_data)

str(nvidia_data)
head(nvidia_data)

str(pfizer_data)
head(pfizer_data)

str(johnson_data)
head(johnson_data)

str(unitedhealth_data)
head(unitedhealth_data)

str(nextera_data)
head(nextera_data)

str(conoco_data)
head(conoco_data)

str(exxon_data)
head(exxon_data)

#2.checking for missing values
colSums(is.na(apple_data))
colSums(is.na(microsoft_data))
colSums(is.na(nvidia_data))
colSums(is.na(pfizer_data))
colSums(is.na(johnson_data))
colSums(is.na(unitedhealth_data))
colSums(is.na(nextera_data))
colSums(is.na(conoco_data))
colSums(is.na(exxon_data))

#3.Converting Date column to Date format
apple_data$Date <- as.Date(apple_data$Date, format = "%d-%m-%Y")
microsoft_data$Date <- as.Date(microsoft_data$Date, format = "%d-%m-%Y")
nvidia_data$Date <- as.Date(nvidia_data$Date, format = "%d-%m-%Y")
pfizer_data$Date <- as.Date(pfizer_data$Date, format = "%d-%m-%Y")
johnson_data$Date <- as.Date(johnson_data$Date, format = "%d-%m-%Y")
unitedhealth_data$Date <- as.Date(unitedhealth_data$Date, format = "%d-%m-%Y")
nextera_data$Date <- as.Date(nextera_data$Date, format = "%d-%m-%Y")
conoco_data$Date <- as.Date(conoco_data$Date, format = "%d-%m-%Y")
exxon_data$Date <- as.Date(exxon_data$Date, format = "%d-%m-%Y")
 
#Keeping only necessary columns
apple_data <- apple_data %>% select(Date, Open, High, Low, Close)
microsoft_data <- microsoft_data %>% select(Date, Open, High, Low, Close)
nvidia_data <- nvidia_data %>% select(Date, Open, High, Low, Close)
pfizer_data <- pfizer_data %>% select(Date, Open, High, Low, Close)
johnson_data <- johnson_data %>% select(Date, Open, High, Low, Close)
unitedhealth_data <- unitedhealth_data %>% select(Date, Open, High, Low, Close)
nextera_data <- nextera_data %>% select(Date, Open, High, Low, Close)
conoco_data <- conoco_data %>% select(Date, Open, High, Low, Close)
exxon_data <- exxon_data %>% select(Date, Open, High, Low, Close)

#DATA EXPLORATION AND VISUALIZATION

#1.Combining and plotting companies' close prices within each industry for comparison
#1.1 technology
technology_data <- bind_rows(
  apple_data %>% mutate(Stock = "Apple"),
  microsoft_data %>% mutate(Stock = "Microsoft"),
  nvidia_data %>% mutate(Stock = "NVIDIA")
)

ggplot(technology_data, aes(x = Date, y = Close, color = Stock)) +
  geom_line() +
  labs(title = "Technology Stocks Closing Prices Over Time", x = "Date", y = "Closing Price")

#1.2 Healthcare
healthcare_data <- bind_rows(
  pfizer_data %>% mutate(Stock = "Pfizer"),
  johnson_data %>% mutate(Stock = "Johnson&Johnson"),
  unitedhealth_data %>% mutate(Stock = "UnitedHealth Group")
)

ggplot(healthcare_data, aes(x = Date, y = Close, color = Stock)) +
  geom_line() +
  labs(title = "Healthcare Stocks Closing Prices Over Time", x = "Date", y = "Closing Price")

#1.3 Energy
energy_data <- bind_rows(
  nextera_data %>% mutate(Stock = "NextEra Energy"),
  conoco_data %>% mutate(Stock = "ConocoPhillips"),
  exxon_data %>% mutate(Stock = "Exxon Mobil")
)

ggplot(energy_data, aes(x = Date, y = Close, color = Stock)) +
  geom_line() +
  labs(title = "Energy Stocks Closing Prices Over Time", x = "Date", y = "Closing Price")






#FEATURE ENGINEERING - Deriving Intraday Return variable for all 9 stocks

#Calculate Intraday Return for Apple (tech)
apple_data <- apple_data %>%
  mutate(Intraday_Return = Close - Open)
summary(apple_data$Intraday_Return)

#Calculate Intraday Return for Microsoft (tech)
microsoft_data <- microsoft_data %>%
  mutate(Intraday_Return = Close - Open)
summary(microsoft_data$Intraday_Return)

#Calculate Intraday Return for NVIDIA (tech)
nvidia_data <- nvidia_data %>%
  mutate(Intraday_Return = Close - Open)
summary(nvidia_data$Intraday_Return)

#Calculate Intraday Return for Pfizer (healthcare)
pfizer_data <- pfizer_data %>%
  mutate(Intraday_Return = Close - Open)
summary(pfizer_data$Intraday_Return)

#Calculate Intraday Return for Johnson&Johnson (healthcare)
johnson_data <- johnson_data %>%
  mutate(Intraday_Return = Close - Open)
summary(johnson_data$Intraday_Return)

#Calculate Intraday Return for UnitedHealth Group (healthcare)
unitedhealth_data <- unitedhealth_data %>%
  mutate(Intraday_Return = Close - Open)
summary(unitedhealth_data$Intraday_Return)

#Calculate Intraday Return for NextEra Energy (energy)
nextera_data <- nextera_data %>%
  mutate(Intraday_Return = Close - Open)
summary(nextera_data$Intraday_Return)

#Calculate Intraday Return for ConocoPhillips (energy)
conoco_data <- conoco_data %>%
  mutate(Intraday_Return = Close - Open)
summary(conoco_data$Intraday_Return)

#Calculate Intraday Return for ExxonMobil (energy)
exxon_data <- exxon_data %>%
  mutate(Intraday_Return = Close - Open)
summary(exxon_data$Intraday_Return)




#FEATURE ENGINEERING - Deriving Daily Return variable for all 9 stocks

#Calculate Daily Return for Apple (tech)
apple_data <- apple_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))
summary(apple_data$Daily_Return)

#Calculate Daily Return for Microsoft (tech)
microsoft_data <- microsoft_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))

summary(microsoft_data$Daily_Return)

#Calculate Daily Return for NVIDIA (tech)
nvidia_data <- nvidia_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))
summary(nvidia_data$Daily_Return)

#Calculate Daily Return for Pfizer (healthcare)
pfizer_data <- pfizer_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))
summary(pfizer_data$Daily_Return)

#Calculate Daily Return for Johnson&Johnson (healthcare)
johnson_data <- johnson_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))
summary(johnson_data$Daily_Return)

#Calculate Daily Return for UnitedHealth Group (healthcare)
unitedhealth_data <- unitedhealth_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))
summary(unitedhealth_data$Daily_Return)

#Calculate Daily Return for NextEra Energy (energy)
nextera_data <- nextera_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))
summary(nextera_data$Daily_Return)

#Calculate Daily Return for ConocoPhillips (energy)
conoco_data <- conoco_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))
summary(conoco_data$Daily_Return)

#Calculate Daily Return for ExxonMobil (energy)
exxon_data <- exxon_data %>%
  mutate(Daily_Return = (Close - lag(Close)) / lag(Close))
summary(exxon_data$Daily_Return)




#FEATURE ENGINEERING - Deriving 14-Day Weigthed Moving Average (WMA) variable for all 9 stocks

#using "TTR" library (built-in function for WMA)

 #Calculate 14-day WMA for Apple (tech)
apple_data <- apple_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(apple_data$WMA_14)

#Calculate 14-day WMA for Microsoft (tech)
microsoft_data <- microsoft_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(microsoft_data$WMA_14)

#Calculate 14-day WMA for NVIDIA (tech)
nvidia_data <- nvidia_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(nvidia_data$WMA_14)


#Calculate 14-day WMA for Pfizer (healthcare)
pfizer_data <- pfizer_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(pfizer_data$WMA_14)


#Calculate 14-day WMA for Johnson&Johnson (healthcare)
johnson_data <- johnson_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(johnson_data$WMA_14)


#Calculate 14-day WMA for UnitedHealth Gorup (healthcare)
unitedhealth_data <- unitedhealth_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(unitedhealth_data$WMA_14)


#Calculate 14-day WMA for NextEra Energy (energy)
nextera_data <- nextera_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(nextera_data$WMA_14)


#Calculate 14-day WMA for ConocoPhillips (energy)
conoco_data <- conoco_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(conoco_data$WMA_14)


#Calculate 14-day WMA for ExxonMobil (energy)
exxon_data <- exxon_data %>%
  mutate(WMA_14 = WMA(Close, n = 14))
summary(exxon_data$WMA_14)

#Plotting 14-day WMA Close Prices vs  Raw Close Prices to track trends (only one company selected for simplicity)

ggplot(apple_data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Closing Price"), size = 1.2) +  # Make the line thicker
  geom_line(aes(y = WMA_14, color = "14-Day WMA"), size = 1.2) +    # Make the line thicker
  labs(title = "Apple: Raw Close Prices vs 14-Day WMA Close Prices",
       x = "Date",
       y = "Price",
       color = NULL) +  # Remove the word "Legend"
  theme_minimal()



#FEATURE ENGINEERING - Deriving 14-Day Rolling Standard Deviation variable for all 9 stocks

#using"ZOO" library (built-in function for rolling SD)

#Calculate 14-day Rolling SD for Apple (tech)
apple_data <- apple_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(apple_data$Rolling_SD_14)

#Calculate 14-day Rolling SD for Microsoft (tech)
microsoft_data <- microsoft_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(microsoft_data$Rolling_SD_14)

#Calculate 14-day Rolling SD for NVIDIA (tech)
nvidia_data <- nvidia_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(nvidia_data$Rolling_SD_14)

#Calculate 14-day Rolling SD for Pfizer (healthcare)
pfizer_data <- pfizer_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(pfizer_data$Rolling_SD_14)

#Calculate 14-day Rolling SD for Johnson&Johnson (healthcare)
johnson_data <- johnson_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(johnson_data$Rolling_SD_14)

#Calculate 14-day Rolling SD for UnitedHealth Group (healthcare)
unitedhealth_data <- unitedhealth_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(unitedhealth_data$Rolling_SD_14)

#Calculate 14-day Rolling SD for NextEra Energy (energy)
nextera_data <- nextera_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(nextera_data$Rolling_SD_14)

#Calculate 14-day Rolling SD for ConocoPhillips (energy)
conoco_data <- conoco_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(conoco_data$Rolling_SD_14)

#Calculate 14-day Rolling SD for ExxonMobil (energy)
exxon_data <- exxon_data %>%
  mutate(Rolling_SD_14 = rollapply(Close, width = 14, FUN = sd, fill = NA, align = "right"))
summary(exxon_data$Rolling_SD_14)



#FEATURE ENGINEERING - Deriving Stochastic Oscillator (14-Day) variable for all 9 stocks

#using "TTR"library (built-in function for Stochastic Oscillator)

#Calculate 14-day Stochastic Oscillator for Apple (tech)
apple_data <- apple_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(apple_data$Stochastic_Oscillator_14)

#Calculate 14-day Stochastic Oscillator for Microsoft (tech)
microsoft_data <- microsoft_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(microsoft_data$Stochastic_Oscillator_14)

#Calculate 14-day Stochastic Oscillator for NVIDIA (tech)
nvidia_data <- nvidia_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(nvidia_data$Stochastic_Oscillator_14)

#Calculate 14-day Stochastic Oscillator for Pfizer (healthcare)
pfizer_data <- pfizer_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(pfizer_data$Stochastic_Oscillator_14)

#Calculate 14-day Stochastic Oscillator for Johnson&Johnson (healthcare)
johnson_data <- johnson_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(johnson_data$Stochastic_Oscillator_14)

#Calculate 14-day Stochastic Oscillator for UnitedHealth Group (healthcare)
unitedhealth_data <- unitedhealth_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(unitedhealth_data$Stochastic_Oscillator_14)

#Calculate 14-day Stochastic Oscillator for NextEra Energy (energy)
nextera_data <- nextera_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(nextera_data$Stochastic_Oscillator_14)

#Calculate 14-day Stochastic Oscillator for ConocoPhillips (energy)
conoco_data <- conoco_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(conoco_data$Stochastic_Oscillator_14)

#Calculate 14-day Stochastic Oscillator for ExxonMobil (energy)
exxon_data <- exxon_data %>%
  mutate(Stochastic_Oscillator_14 = (Close - runMin(Low, n = 14)) / 
           (runMax(High, n = 14) - runMin(Low, n = 14)) * 100)
summary(exxon_data$Stochastic_Oscillator_14)



#CLEAN THE DATA - keeping only rows where all variables have valid (non-NA) values
apple_data <- apple_data %>% drop_na()
microsoft_data <- microsoft_data %>% drop_na()
nvidia_data <- nvidia_data %>% drop_na()
pfizer_data <- pfizer_data %>% drop_na()
johnson_data <- johnson_data %>% drop_na()
unitedhealth_data <- unitedhealth_data %>% drop_na()
nextera_data <- nextera_data %>% drop_na()
conoco_data <- conoco_data %>% drop_na()
exxon_data <- exxon_data %>% drop_na()



#EXPLORATORY DATA ANALYSIS (EDA) - correlation matrix to evaluate features relevancy and check for multicollinearity

#using "corrplot" library

engineered_data <- select(apple_data, Close,Intraday_Return, Daily_Return, WMA_14, Rolling_SD_14, Stochastic_Oscillator_14)
corrplot(cor(engineered_data, use = "complete.obs"), method = "circle")
cor(apple_data$Close, apple_data$WMA_14, use = "complete.obs")




#BUILDING LINEAR REGRESSION MODEL - APPLE (tech)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(apple_data)), size = 0.8 * nrow(apple_data))
apple_train <- apple_data[train_index, ]
apple_test <- apple_data[-train_index, ]

#fitting the model
apple_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = apple_train)
summary(apple_model)

#evaluating the model with test data
apple_predictions <- predict(apple_model, newdata = apple_test)

mae_apple <- mean(abs(apple_predictions - apple_test$Close))  # Mean Absolute Error
mse_apple <- mean((apple_predictions - apple_test$Close)^2)   # Mean Squared Error
r_squared_apple <- 1 - sum((apple_predictions - apple_test$Close)^2) / sum((apple_test$Close - mean(apple_test$Close))^2)

cat("Apple Model Performance:\n")
cat("Mean Absolute Error (MAE):", mae_apple, "\n")
cat("Mean Squared Error (MSE):", mse_apple, "\n")
cat("R-squared:", r_squared_apple, "\n")

#Create a plot for predicted vs actual values for Apple
ggplot(data = apple_test, aes(x = Close, y = apple_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Apple: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()



#BUILDING LINEAR REGRESSION MODEL - NVIDIA (tech)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(nvidia_data)), size = 0.8 * nrow(nvidia_data))
nvidia_train <- nvidia_data[train_index, ]
nvidia_test <- nvidia_data[-train_index, ]

#fitting the model
nvidia_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = nvidia_train)
summary(nvidia_model)

#evaluating the model with test data
nvidia_predictions <- predict(nvidia_model, newdata = nvidia_test)
mae_nvidia <- mean(abs(nvidia_predictions - nvidia_test$Close))  # Mean Absolute Error
mse_nvidia <- mean((nvidia_predictions - nvidia_test$Close)^2)   # Mean Squared Error
r_squared_nvidia <- 1 - sum((nvidia_predictions - nvidia_test$Close)^2) / sum((nvidia_test$Close - mean(nvidia_test$Close))^2)

cat("Mean Absolute Error (MAE):", mae_nvidia, "\n")
cat("Mean Squared Error (MSE):", mse_nvidia, "\n")
cat("R-squared:", r_squared_nvidia, "\n")

#Create a plot for predicted vs actual values for NVIDIA
ggplot(data = nvidia_test, aes(x = Close, y = nvidia_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "NVIDIA: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()


#BUILDING LINEAR REGRESSION MODEL - MICROSOFT (tech)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(microsoft_data)), size = 0.8 * nrow(microsoft_data))
microsoft_train <- microsoft_data[train_index, ]
microsoft_test <- microsoft_data[-train_index, ]

#fitting the model
microsoft_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = microsoft_train)
summary(microsoft_model)

#evaluating the model with test data
microsoft_predictions <- predict(microsoft_model, newdata = microsoft_test)

mae_microsoft <- mean(abs(microsoft_predictions - microsoft_test$Close))  # Mean Absolute Error
mse_microsoft <- mean((microsoft_predictions - microsoft_test$Close)^2)   # Mean Squared Error
r_squared_microsoft <- 1 - sum((microsoft_predictions - microsoft_test$Close)^2) / sum((microsoft_test$Close - mean(microsoft_test$Close))^2)

cat("Mean Absolute Error (MAE):", mae_microsoft, "\n")
cat("Mean Squared Error (MSE):", mse_microsoft, "\n")
cat("R-squared:", r_squared_microsoft, "\n")

#Create a plot for predicted vs actual values for Microsoft
ggplot(data = microsoft_test, aes(x = Close, y = microsoft_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Microsoft: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()


#BUILDING LINEAR REGRESSION MODEL - PFIZER (healthcare)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(pfizer_data)), size = 0.8 * nrow(pfizer_data))
pfizer_train <- pfizer_data[train_index, ]
pfizer_test <- pfizer_data[-train_index, ]

#fitting the model
pfizer_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = pfizer_train)
summary(pfizer_model)

#evaluating the model with test data
pfizer_predictions <- predict(pfizer_model, newdata = pfizer_test)

mae_pfizer <- mean(abs(pfizer_predictions - pfizer_test$Close))  # Mean Absolute Error
mse_pfizer <- mean((pfizer_predictions - pfizer_test$Close)^2)   # Mean Squared Error
r_squared_pfizer <- 1 - sum((pfizer_predictions - pfizer_test$Close)^2) / sum((pfizer_test$Close - mean(pfizer_test$Close))^2)

cat("Mean Absolute Error (MAE):", mae_pfizer, "\n")
cat("Mean Squared Error (MSE):", mse_pfizer, "\n")
cat("R-squared:", r_squared_pfizer, "\n")

#Create a plot for predicted vs actual values for Pfizer
ggplot(data = pfizer_test, aes(x = Close, y = pfizer_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Pfizer: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()



#BUILDING LINEAR REGRESSION MODEL - Johnson&Johnson (healthcare)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(johnson_data)), size = 0.8 * nrow(johnson_data))
johnson_train <- johnson_data[train_index, ]
johnson_test <- johnson_data[-train_index, ]

#fitting the model
johnson_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = johnson_train)
summary(johnson_model)

#evaluating the model with test data
johnson_predictions <- predict(johnson_model, newdata = johnson_test)
mae_johnson <- mean(abs(johnson_predictions - johnson_test$Close))  # Mean Absolute Error
mse_johnson <- mean((johnson_predictions - johnson_test$Close)^2)   # Mean Squared Error
r_squared_johnson <- 1 - sum((johnson_predictions - johnson_test$Close)^2) / sum((johnson_test$Close - mean(johnson_test$Close))^2)

cat("Mean Absolute Error (MAE):", mae_johnson, "\n")
cat("Mean Squared Error (MSE):", mse_johnson, "\n")
cat("R-squared:", r_squared_johnson, "\n")

#Create a plot for predicted vs actual values for Johnson&Johnson
ggplot(data = johnson_test, aes(x = Close, y = johnson_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Johnson & Johnson: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()


#BUILDING LINEAR REGRESSION MODEL - UnitedHealth Group (healthcare)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(unitedhealth_data)), size = 0.8 * nrow(unitedhealth_data))
unitedhealth_train <- unitedhealth_data[train_index, ]
unitedhealth_test <- unitedhealth_data[-train_index, ]

#fitting the model
unitedhealth_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = unitedhealth_train)
summary(unitedhealth_model)

#evaluating the model with test data
unitedhealth_predictions <- predict(unitedhealth_model, newdata = unitedhealth_test)
mae_unitedhealth <- mean(abs(unitedhealth_predictions - unitedhealth_test$Close))  # Mean Absolute Error
mse_unitedhealth <- mean((unitedhealth_predictions - unitedhealth_test$Close)^2)   # Mean Squared Error
r_squared_unitedhealth <- 1 - sum((unitedhealth_predictions - unitedhealth_test$Close)^2) / sum((unitedhealth_test$Close - mean(unitedhealth_test$Close))^2)
cat("Mean Absolute Error (MAE):", mae_unitedhealth, "\n")
cat("Mean Squared Error (MSE):", mse_unitedhealth, "\n")
cat("R-squared:", r_squared_unitedhealth, "\n")

#Create a plot for predicted vs actual values for UnitedHealth Group
ggplot(data = unitedhealth_test, aes(x = Close, y = unitedhealth_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "UnitedHealth: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()



#BUILDING LINEAR REGRESSION MODEL - NextEra Energy (energy)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(nextera_data)), size = 0.8 * nrow(nextera_data))
nextera_train <- nextera_data[train_index, ]
nextera_test <- nextera_data[-train_index, ]

#fitting the model
nextera_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = nextera_train)
summary(nextera_model)

#evaluating the model with test data
nextera_predictions <- predict(nextera_model, newdata = nextera_test)

mae_nextera <- mean(abs(nextera_predictions - nextera_test$Close))  # Mean Absolute Error
mse_nextera <- mean((nextera_predictions - nextera_test$Close)^2)   # Mean Squared Error
r_squared_nextera <- 1 - sum((nextera_predictions - nextera_test$Close)^2) / sum((nextera_test$Close - mean(nextera_test$Close))^2)

cat("Mean Absolute Error (MAE):", mae_nextera, "\n")
cat("Mean Squared Error (MSE):", mse_nextera, "\n")
cat("R-squared:", r_squared_nextera, "\n")

#Create a plot for predicted vs actual values for NextEra

ggplot(data = nextera_test, aes(x = Close, y = nextera_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "NextEra: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()


#BUILDING LINEAR REGRESSION MODEL - ConocoPhillips (energy)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(conoco_data)), size = 0.8 * nrow(conoco_data))
conoco_train <- conoco_data[train_index, ]
conoco_test <- conoco_data[-train_index, ]

#fitting the model
conoco_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = conoco_train)
summary(conoco_model)

#evaluating the model with test data
conoco_predictions <- predict(conoco_model, newdata = conoco_test)

mae_conoco <- mean(abs(conoco_predictions - conoco_test$Close))  # Mean Absolute Error
mse_conoco <- mean((conoco_predictions - conoco_test$Close)^2)   # Mean Squared Error
r_squared_conoco <- 1 - sum((conoco_predictions - conoco_test$Close)^2) / sum((conoco_test$Close - mean(conoco_test$Close))^2)

cat("Mean Absolute Error (MAE):", mae_conoco, "\n")
cat("Mean Squared Error (MSE):", mse_conoco, "\n")
cat("R-squared:", r_squared_conoco, "\n")

#Create a plot for predicted vs actual values for ConocoPhillips
ggplot(data = conoco_test, aes(x = Close, y = conoco_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "ConocoPhillips: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()


#BUILDING LINEAR REGRESSION MODEL - ExxonMobil (energy)

#train-test split
set.seed(123)

train_index <- sample(seq_len(nrow(exxon_data)), size = 0.8 * nrow(exxon_data))
exxon_train <- exxon_data[train_index, ]
exxon_test <- exxon_data[-train_index, ]

#fitting the model
exxon_model <- lm(Close ~ Intraday_Return + Daily_Return + Rolling_SD_14 + Stochastic_Oscillator_14, data = exxon_train)
summary(exxon_model)

#evaluating the model with test data
exxon_predictions <- predict(exxon_model, newdata = exxon_test)

mae_exxon <- mean(abs(exxon_predictions - exxon_test$Close))  # Mean Absolute Error
mse_exxon <- mean((exxon_predictions - exxon_test$Close)^2)   # Mean Squared Error
r_squared_exxon <- 1 - sum((exxon_predictions - exxon_test$Close)^2) / sum((exxon_test$Close - mean(exxon_test$Close))^2)

cat("Mean Absolute Error (MAE):", mae_exxon, "\n")
cat("Mean Squared Error (MSE):", mse_exxon, "\n")
cat("R-squared:", r_squared_exxon, "\n")

#Create a plot for predicted vs actual values for ExxonMobil
ggplot(data = exxon_test, aes(x = Close, y = exxon_predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "ExxonMobil: Predicted vs Actual Closing Prices",
       x = "Actual Closing Prices",
       y = "Predicted Closing Prices") +
  theme_minimal()

