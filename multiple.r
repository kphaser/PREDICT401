# Data Analysis Dealing with Multiple Linear Regression

# homes.csv is comprised of data from home sales.  It consists of 32 sales for
# which the sales price, taxes, number of bathrooms and number of fireplaces are
# recorded.  The multiple linear regression analysis is intended to determine 
# the relationship between sales price and the other three variables. 

# Sales  sales price in thousands of dollars
# Taxes  taxes in thousands of dollars
# Baths  number of bathrooms, either single(1) or bath and a half (1.5)
# Fire   number of fireplaces, either none(0) or one (1)

require(moments)
require(ggplot2)

prices <- read.csv(file.path("c:/Rdata/","homes.csv"),sep=",")
str(prices)
summary(prices)

# Baths is an ordinal variable.  Fire is an integer.  They will be expressed as 
# factors for this analysis. For notational simplicity, the other variables will
# be defined as separate vectors. 

baths <- factor(prices$Baths)
fire <- factor(prices$Fire)
S <- prices$Sales
T <- prices$Taxes

prices <- data.frame(prices, S, T, baths, fire)
str(prices)

# Overview table

ms <- aggregate(S~(baths+fire), data = prices, mean)
mt <- aggregate(T~(baths+fire), data = prices, mean)
mt <- mt[,3]
overview <- cbind(ms,mt)
colnames(overview) <- c("Baths","Fireplaces", "Sales Price", "Taxes")
overview

# EDA comes first looking at the predictors.

par(mfrow = c(1,2))
boxplot(S~fire, main = "Sales Price by Fireplaces", col = "red", ylab = "Sales Price",
        xlab = "Number of Fireplaces")
boxplot(S~baths, main = "Sales Price by Baths", col = "blue", ylab = "Sales Price",
        xlab = "Number of Baths")
par(mfrow = c(1,1))

p <- ggplot(prices, aes(x = T, y = S))+geom_point(aes(color = baths), size = 4)+
  ggtitle("Plot of Sales Price versus Taxes Colored by Number of Baths")
p
g <- ggplot(prices, aes(x = T, y = S))+geom_point(aes(color = fire), size = 4)+
  ggtitle("Plot of Sales Price versus Taxes Colored by Number of Fireplaces")
g

# The EDA indicates a relationship between sales price, taxes and number of baths.
# The role of the number of fireplaces seems unimportant.  Regression analysis
# will sort this out.  Doing so is important.  The following t-test on Sales 
# as a function of baths indicates the type of result that can be obtained when
# other predictors are not considered.

boxplot(S~baths, main = "Sales Price by Baths", col = "blue", ylab = "Sales Price",
        xlab = "Number of Baths")
s1 <- S[baths == "1"]
s15 <- S[baths == "1.5"]
t.test(s15,s1,alternative=c("two.sided"),mu=0,paired=FALSE)

# Without taking other predictors into account, this suggests baths by itself 
# increases the sales price by $15.2 thousand dollars. This result will not 
# be replicated by the regression analysis and is indicative of the challenges
# in developing a regression model based on observational data.  Have all the 
# relevant variables been considered?

rs <- lm(S~T+baths+fire, data=prices)
summary(rs)

# Evaluation of the results of a multiple linear regression analysis is informative. 

# 1) A statistically significant result was obtained overall as indicated by the 
# F-statistic which is 42.0 with a p-value = 1.694e-10.  This indicates the model  
# has produced statistically significant results to be investigated.
# 2) The multiple R-squared value of 0.8183 indicates that 81.83% of the variation
# in Sales is accounted for by the independent variables Taxes, Baths and Fireplaces.
# This indicates the model has the potential to be useful for making predictions.
# 3) The t values and p-values reported for the four line items indicate fire as
# a predictor is not statistically significant and can be excluded from a simplified
# regression model that includes T and baths as predictors.  

rs <- lm(S~T+baths, data=prices)
summary(rs)
std = 5.523

# Development of a predictive model is a search for a parsimonious model that
# fits the data, and also provides useful predictions.  By excluding fire from
# the model there is little change in the summary statistics.

# The intercept pertains to the data with one bath.  This can be used to plot one
# fitted line to the data with slope 10.669.  For the homes with "1.5" baths, it is 
# necessary to add 9.761 to the intercept to obtain the intercept for the fitted
# line that pertains to those data.  This results in the following plot.

p + geom_abline(intercept=78.709, slope=10.669)+geom_abline(intercept=84.47,slope=10.669)

# Further analysis of residuals is necessary to evaluate the goodness of fit.
# The first step is to extract the residuals and fitted values.

r <- residuals(rs)
fit <- fitted(rs)

# Next these data will be examined.  There are a variety of methods for this 
# purpose.  In this example, the distribution of residuals and their relationship
# to the fitted values and independent variables will be considered.  Based on 
# the assumptions of ordinary least squares, it is highly desirable for the residuals
# to correspond to a normal distribution with no apparent pattern when compared
# to the independent variables T and baths, or the fitted values.

x <- r
hist(r, main = "Histogram of Residuals", col = "red", freq = FALSE, ylim = c(0, 0.08))
curve(dnorm(x,0,std),add=TRUE, col="green", lwd = 2)

qqnorm(r, main = "Q-Q Plot of Residuals", col = "red", pch = 16)
qqline(r, col = "green", lty = 2, lwd = 2)
skewness(r)
kurtosis(r)

boxplot(r~baths, col = "blue", main = "Boxplot of Residuals by Number of Baths")

plot(T,r, main = "Plot of Residuals versus Taxes", xlab = "Taxes",
     ylab = "Residuals", col = "red", pch = 16, ylim = c(-15, 15))
abline(h = 0, lty = 2, lwd = 2, col = "green")
abline(h = c(1.96*std, -1.96*std), lty = 2, lwd = 2, col = "blue")

plot(fit,r, main = "Plot of Residuals versus Fitted Values", xlab = "Fitted Values",
     ylab = "Residuals", col = "red", pch = 16, ylim = c(-15, 15))
abline(h = 0, lty = 2, lwd = 2, col = "green")
abline(h = c(1.96*std, -1.96*std), lty = 2, lwd = 2, col = "blue")

# Statistically significant versus materially significant?
# Is the magnitude of the average difference due to Baths of material import?  
# It is about 104.4% of the estimated standard error from the regression analysis.
# It is also about 4.0% of the average sales price of a home (5.764/143.8=0.040).

CV <- (5.764/std)*100
CV

# All models are wrong, but some are useful.  The evaluation of just how useful
# depends on statistical considerations, but equally important are practical 
# considerations of material impact.  Who is going to use the model and why? 
# How representative are the results of the larger population of homes being sold?
# These are the types of questions which need to be answered.