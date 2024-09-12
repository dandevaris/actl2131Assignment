# Install Required Packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("e1071")
install.packages("gridExtra")
library(ggplot2)
library(dplyr)
library(tidyr)
library(e1071)
library(gridExtra)

#Import Required Data
spyData <- read.csv(file = "SPY.csv", header = TRUE)
etsyData <- read.csv(file = "ETSY.csv")

  ## Question 1 ##

#Calculate log returns for both SPY and ETSY
spyData <- spyData %>% 
  mutate(logReturns = log(Adj.Close) - log(lag(Adj.Close)))
etsyData <- etsyData %>%
  mutate(logReturns = log(Adj.Close) - log(lag(Adj.Close)))

#Create a new data frame for the log returns
logReturns <- data.frame(Date = spyData$Date, 
                      spy = spyData$logReturns,
                      etsy = etsyData$logReturns
)

# Ensure the date column is in a date format
logReturns$Date <- as.Date(logReturns$Date)

#Plot line graph of log returns vs time
ggplot(logReturns, aes(x = Date)) +
  geom_line(aes(y = spy, color = "SPY"), alpha = 0.7) + 
  geom_line(aes(y = etsy, color = "ETSY"), alpha = 0.7) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Date", y = "Log Returns", colour = "Asset") + 
  scale_color_manual(values = c(SPY = 'violet', ETSY = 'lightblue')) +
  ggtitle("Log Returns of SPY vs ETSY") +
  theme_minimal()

# Save file to wd
ggsave("Log Returns vs Time.png", height = 10, width = 13, units = 'cm', dpi = 1080)
  
  ##Question 2##

# Create a vector of summary statistics for SPY and ETSY
spySummary <- c(
  Mean = mean(logReturns$spy, na.rm = TRUE), 
  Variance = var(logReturns$spy, na.rm = TRUE), 
  Skewness = skewness(logReturns$spy, na.rm = TRUE),
  Kurtosis = kurtosis(logReturns$spy, na.rm = TRUE) + 3
)

etsySummary <- c(
  Mean = mean(logReturns$etsy, na.rm = TRUE), 
  Variance = var(logReturns$etsy, na.rm = TRUE), 
  Skewness = skewness(logReturns$etsy, na.rm = TRUE),
  Kurtosis = kurtosis(logReturns$etsy, na.rm = TRUE) + 3
)
                
# Create a data frame of both summary statistics
summaryStatistics <- data.frame(
  SPY = spySummary,
  ETSY = etsySummary
)


  ## Question 3 ##

# Create histograms for SPY and ETSY log returns and assign them a variable name
spyHist <- ggplot(logReturns, aes(x = spy)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = 'violet', colour = 'black') +
  stat_function(fun = dnorm, 
                args = list(mean = summaryStatistics["Mean", "SPY"], 
                            sd = sqrt(summaryStatistics["Variance", "SPY"])), 
                            color = "black", size = 0.8) +
  labs(title = "SPY Log Returns Density", x = "SPY Log Returns", y = "Density") +
  theme_minimal() + 
  xlim(-0.2, 0.2) + 
  ylim(0, 50)


etsyHist <- ggplot(logReturns, aes(x = etsy)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = 'lightblue', colour = 'black') +
  stat_function(fun = dnorm, 
                args = list(mean = summaryStatistics["Mean", "ETSY"], 
                            sd = sqrt(summaryStatistics["Variance", "ETSY"])), 
                            color = "black", size = 0.8) +
  labs(title = "ETSY Log Returns Density", x = "ETSY Log Returns", y = "Density") +
  theme_minimal() + 
  xlim(-0.2, 0.2) + 
  ylim(0, 50)

# Save file to wd of both histograms
png("Histograms vs Normal.png", height = 10, width = 15, units = 'cm', res = 1080)
grid.arrange(spyHist, etsyHist, ncol = 2)
dev.off()


  ## Question 4 ##
# Create a Q-Q plot of each stock compared to the Q-Q plot of a normal 
# distribution with same parameters.

# Generate SPY quantiles
spyQuants <- qnorm(ppoints(length(logReturns$spy)), 
                               mean = summaryStatistics["Mean", "SPY"], 
                               sd = sqrt(summaryStatistics["Variance", "SPY"]))

# Generate ETSY quantiles
etsyQuants <- qnorm(ppoints(length(logReturns$etsy)), 
                    mean = summaryStatistics["Mean", "ETSY"], 
                    sd = sqrt(summaryStatistics["Variance", "ETSY"]))

# Plot and save graphs
png("Q-Q Plot vs Normal.png", height = 10, width = 20.5, units = 'cm', res = 1080)
par(mfrow = c(1, 2))
qqplot(spyQuants, logReturns$spy, 
       main = "Q-Q Plot of SPY vs Normal Q-Q Line", 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles")
abline(0, 1, col = 'red')

qqplot(etsyQuants, logReturns$etsy, 
       main = "Q-Q Plot of ETSY vs Normal Q-Q Line", 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles")
abline(0, 1, col = 'red')
dev.off()

  ## Question 5 ##

# Calculate correlation between SPY and ETSY log returns
corSpyEtsy <- cor(na.omit(logReturns)$spy, na.omit(logReturns)$etsy)
print(corSpyEtsy)

# Scatter plot between SPY and ETSY
ggplot(logReturns, aes(x = spy, y = etsy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = 'red') +
  labs(title = "SPY vs ETSY Scatter Plot", x = "SPY", y = "ETSY")
ggsave("SPY vs ETSY Scatter Plot.png", height = 10, width = 13, units = 'cm', dpi = 1080)


 ## Question 6 ##

# Create annualised versions of mean and variance for SPY and ETSy
spyAnnualisedMean <- summaryStatistics["Mean", "SPY"]*250
etsyAnnualisedMean <- summaryStatistics["Mean", "ETSY"]*250
spyAnnualisedVar <- summaryStatistics["Variance", "SPY"]*250^2
etsyAnnualisedVar <- summaryStatistics["Variance", "ETSY"]*250^2
print(paste("Annualised SPY Daily Log returns:", spyAnnualisedMean))
print(paste("Annualised ETSY Daily Log Returns:", etsyAnnualisedMean))
print(paste("Annualised SPY Variance:", spyAnnualisedVar))
print(paste("Annualised ETSY Variance:", etsyAnnualisedVar))

# Calculate the test statistics under H_0
testStatistic <- 
  (spyAnnualisedMean - etsyAnnualisedMean)/
  sqrt(spyAnnualisedVar/length(na.omit(logReturns)$spy) + 
          etsyAnnualisedVar/length(na.omit(logReturns)$etsy))
print(testStatistic)

# Calculate the p-value from the test statistic value
pValue <- 2 * pnorm(testStatistic, mean = 0, sd = 1, lower.tail = FALSE)
print(pValue)
