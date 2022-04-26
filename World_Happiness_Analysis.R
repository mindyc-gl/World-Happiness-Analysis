
setwd("~/Desktop ")

#Packages downloading
install.packages("todyverse")
install.packages("countrycode")
install.packages("PerformanceAnalytics")
install.packages("Metrics")
install.packages("leaps")
#install.packages("rcompanion")
install.packages('DescTools', dependencies = TRUE)

library(tidyverse)
library(countrycode)
library(PerformanceAnalytics)
library(Metrics)
library(leaps)
#library(rcompanion)

-----------------------------
#Data cleaning
dataset <- read.csv("2018.csv", header = T)

#Removing rank column
hd <- subset(dataset, select = -Overall.rank)

#Adding Continent column
hd$Continent <- NA
hd$Continent <- countrycode(sourcevar = hd[, "Country.or.region"], origin = "country.name", destination = "continent")

#Check for NA values
sapply(hd, function(x) sum(is.na(x)))
#checking NA in continent
which(is.na(hd$Continent) == T)
#Updating NA
hd[66,]
hd$Continent[66] <- "Europe"

#Renaming columns and convert to tibble to see head
hd %>%
  rename(
    Country = Country.or.region,
    GDP = GDP.per.capita,
    Social = Social.support,
    Health = Healthy.life.expectancy,
    Freedom = Freedom.to.make.life.choices,
    Corruption = Perceptions.of.corruption
  ) -> hd
(hd_tibble <- as_tibble(hd))
hd_tibble

-------------------------------
#Data Explore and Visualization
attach(hd_tibble)

#Descriptive stats for all variables
summary(hd_tibble)

#Histogram and boxplot
hd_tibble %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins = 13, fill = "orange") +
  labs(title = "Histograms of all numerical attributes")
##Score seems approximately normally distributed. 
#Generosity is highly right-skewed and Social is highly left-skewed

hd_tibble %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplots of all numerical attributes")
#Boxplot shows outliers in Freedom, Generosity,and Social columns.

#Identifying 'outliers'
boxplot.stats(Freedom)$out
out_index <- which(Freedom %in% boxplot.stats(Freedom)$out)

boxplot.stats(Generosity)$out
append(out_index, which(Generosity %in% boxplot.stats(Generosity)$out)) -> out_index
boxplot.stats(Social)$out
append(out_index, which(Social %in% boxplot.stats(Social)$out))  -> out_index

clean_hd <- hd_tibble[-out_index,]
clean_hd <- as_tibble(clean_hd)
detach(hd_tibble)
attach(clean_hd)
clean_hd

hd_tibble[-c(1,2,3,5,6,7,9,14,16,34,76,152,102,145,149,155,154,135),] %>%
  group_by(Continent) %>%
  summarise(Mean_Score = mean(Score)) -> score_continent
score_continent
#It shows Oceania has the has the highest mean Scores than American and Europe
#And then followed by Asia and Africa
ggplot(data = score_continent, aes(x = Continent, y = Mean_Score, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Happiness score for different continents")

#One-way ANOVA to compare means of happiness score across Continents.
#alpha = 0.05
aov <- aov(Score ~ as.factor(Continent))
summary(aov)
#Calculate F-test, critical F and compare to see test that at least two of the groups have underlying means that are different from each other.
qf(1-0.05, df1 = 4-1, df2 = 136-4)
##35.82 > 2.673218. We have significant evidence at alpha = 0.05 level that there is a difference in Scores between Continents. 
#The p-value is less than 0.001.

#Multiple Linear Regression
#Fitting MLR model with all numeric variables (GDP, Social, Health, Freedom, Generosity, Corruption)
m <- lm(Score ~ GDP + Social + Health + Freedom + Generosity + Corruption)
#Formally testing whether the features are associated with Happiness Score at alpha = 0.05 level. 
#MLR Inference Global F test.

#1. Hypothesis
#H0: B_GDP = B_Social = B_Health = B_Freedom = B_Generosity = B_Corruption (No linear relationship)
#Ha: B_i != 0 for i = GDP, Social, Health, Freedom, Generosity, Corruption (There is a linear relationship)

#2. Test Statistic
#Calculating Critical F value
qf(1-0.05, df1 = 6, df2 = 136 - 6 - 1)

#3. Decision Rule
#Reject H0 if F >= 2.169591. Otherwise, do not reject H0

#4. Computation
#F-statistic: 58.22 on 6 and 129 DF,  p-value: < 2.2e-16
anova(m)
summary(m)

#5. Conclusion
#Reject H0 since 58.22 >= 2.169591. p-value is also less than 0.05. We have significant evidence at alpha = 0.05 level that GDP, Social, Health, Freedom, Generosity, 
#Corruption, when taken together are significant predictors of happiness score. 
#There is a linear association between Score and features.
##adjusted R2 value measures the goodness of fitness of the model (Since there are many predictors). 
#Our R2 value is 0.7178 indicates a fairly good fit of our model. 
##Model 1: Y_Score = 1.70 + 0.66X_GDP + 1.19X_Social + 1.25X_Health + 1.40X_Freedom + 0.38X_Generosity + 1.16X_Corruption

#Model Selection

#critical t value and comparison with summary
qt(1-0.05/2, df = 136 - 6 - 1)

#95% confidence interval calculation
confint(m, level = .95)
##Without formally testing, t value for Generosity and Corruption are smaller than critical t value 1.978524. 
#We would fail to reject H0, or simply, these variables are not significant predictors, as p-values indicate.


#Backward Elimination: Eliminate the least significance = Generosity
m2 <- lm(Score ~ GDP + Social + Health + Freedom + Corruption)
summary(m2)
#model 2: Y_Score = 1.76 + 0.63X_GDP + 1.19X_Social + 1.25X_Health + 1.47X_Freedom + 1.27X_Corruption

#Eliminate the next least significance = Corruption
m3 <- update(m2, .~. - Corruption)
summary(m3)

##model 3: Y_Score = 1.86 + 0.66X_GDP + 1.12X_Social +1.25X_Health + 1.65X_Freedom 
##Generosity and corruption levels have a p-value higher than 0.05, indicating that it may not be a significant predictor. 

#Model comparison
par(mfrow = c(1,1))
#residuals vs fitted plot of all features--Model 1
plot(fitted(m), c(resid(m)), axes = T, frame.plot = T, main = "Residual vs Fitted (All)", 
     xlab = "Fitted Values", ylab = "Residual")
abline(h=0)
#residuals vs fitted plot of 5 features --Model 2
plot(fitted(m2), c(resid(m2)), axes = T, frame.plot = T, main = "Residual vs Fitted (5 feat)", 
     xlab = "Fitted Values", ylab = "Residual")
abline(h=0)
#residuals vs fitted plot of 4 features--Model 3
plot(fitted(m3), c(resid(m3)), axes = T, frame.plot = T, main = "Residual vs Fitted (4 feat)", 
     xlab = "Fitted Values", ylab = "Residual")
abline(h=0)

#Visually there is small improvements in fit for m2 than m3 and then m1. 
#All models approximately within assumption bound.

#histogram of residual
hist(resid(m1), xlab = "Residuals", ylab = "Frequency", main = "Histogram of Residuals (All)")
hist(resid(m2), xlab = "Residuals", ylab = "Frequency", main = "Histogram of Residuals (5 feat)")
hist(resid(m3), xlab = "Residuals", ylab = "Frequency", main = "Histogram of Residuals (4 feat)")

##Histogram shows that residuals of m2 are more normally distributed than other models.

#boxplot of fitted values, identify the outliers
boxplot(fitted(m1), main = "Boxplot of fitted (All)")
boxplot(fitted(m2), main = "Boxplot of fitted (5 feat)")
boxplot(fitted(m3), main = "Boxplot of fitted (4 feat)")


#Calculating RMSE to get a sense of fit/Model fit comparison using AICc, the adjustment to AIC
rmse(Score, fitted(m1))
rmse(Score, fitted(m2))
rmse(Score, fitted(m3))
# Model 2(m2) is least AICc. The model from Criterion based method.

#Logistic Regression Analysis
#Model 1:
#lm1<- glm(Score ~ GDP + Social + Health + Freedom + Generosity + Corruption)
#summary(lm1)
#Model 2:
#lm2<- glm(Score ~ GDP + Social + Health + Freedom + Corruption)
#summary(lm2)
#Model 3:
#lm3<- glm(Score ~ GDP + Social + Health + Freedom )
#summary(lm3)

#From the above results of these three models. Model 2 is the least AIC. 
#The most optimal linear regression is:
#Y_Score = 1.76 + 0.63X_GDP + 1.19X_Social + 1.25X_Health + 1.47X_Freedom + 1.27X_Corruption


