# Predict 401 Example Data Analysis 
#---------------------------------------------------------------------------
#----------------------------------------------------------------------------
# One-way Analysis of Variance and Linear Regression
#----------------------------------------------------------------------------
# schools.csv contains educational expenditures over three consecutive years.
# Each year fifty schools are selected at random across the nation. The nation is
# divided into four regions.  The definition of the variables is:
# Y Per capita annual expenditure on public education 
# X Per capita monthly personal income 
# region A: Northeast, B: North Central, C: South, D: West 
# year "1", "2", "3" factor

require(moments)
require(ggplot2)
require(rockchalk)

# Check data file structure.

schools <- read.csv(file.path("c:/Rdata/","schools.csv"),sep=",")
str(schools)

# Check summary statistics.
summary(schools)

# Form an overview table.

my <- aggregate(schools$Y~schools$region, data = schools, mean)
mx <- aggregate(schools$X~schools$region, data = schools, mean)
mx <- mx[,2]
overview <- cbind(my,mx)
colnames(overview) <- c("region","expenditures", "income")
overview

# Evaluate distributions.

par(mfrow = c(2,2))
boxplot(Y~year, data = schools, col = "red", main = "Expenditures by Year")
boxplot(Y~region, data = schools, col = "red", main = "Expenditures by Region")
boxplot(X~year, data = schools, col = "blue", main = "Monthly Income")
boxplot(X~region, data = schools, col = "blue", main = "Monthly Income")
par(mfrow = c(1,1))

# Perform initial one-way analyses of variance.

aov.region <- aov(Y~region, schools)
summary(aov.region)

# Statistically significant F-test results.  Perform TukeyHSD.  Compare to the boxplots.

TukeyHSD(aov.region)

#-------------------------------------------------------------------------------------
# Two-way layout is more efficient and includes an interaction term.

result <- aov(Y~region+year+region*year,schools)
summary(result)

# To take full advantage of aov() and lm() functions, it is necessary to use various 
# "extractor" functions.  Verzani shows a number of these in Table 11.1 page 368.
# summary() is one example.  The residuals and the fitted values will be examined.

r <- residuals(result)
fitt <- fitted(result)

par(mfrow = c(1,2))
hist(r, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(r, col = "red", main = "Boxplot Residuals", ylab = "Residual")
par(mfrow = c(1,1))

qqnorm(r, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(r, col = "green", lty = 2, lwd = 2)


skewness(r)
kurtosis(r)

plot(fitt,r, main = "Plot of residuals versus fitted values", xlab = "fitted values",
     ylab = "residuals", col = "red")
abline(h = 0, lty = 2, col = "green")
abline(h = 101.96, lty = 2, col = "blue")
abline(h = -101.96, lty = 2, col = "blue")

# This evaluation suggests another factor is needed to explain the variability.
#-------------------------------------------------------------------------------------

# A bivariate plot is a useful way to visualize data.

plot(schools$X, schools$Y,main = "Expenditures versus Personal Income", 
     xlab = "Per capita monthly personal income", ylab = "Per capita annual 
     expenditure on public education", col = "red", pch = 16)
abline(v = median(schools$X), col = "green", lty = 2, lwd = 2)
abline(h = median(schools$Y), col = "green", lty = 2, lwd = 2)

# Evaluate association.

X_factor <- factor(schools$X > median(schools$X), labels = c("below", "above"))
Y_factor <- factor(schools$Y > median(schools$Y), labels = c("below", "above"))
combined <- (table(X_factor,Y_factor))
addmargins(combined)

matrix <- function(x){
  # To be used to add margins with 2x2 contingency table.  
  x13 <- x[1,1]+x[1,2]
  x23 <- x[2,1]+x[2,2]
  vc <- c(x13,x23)
  x <- cbind(x,vc)
  x31 <- x[1,1]+x[2,1]
  x32 <- x[1,2]+x[2,2]
  x33 <- x31+x32
  vr <- c(x31,x32,x33)
  x <- rbind(x,vr)
  x
}
matrix(combined)

chisq.test(combined, correct = FALSE)

count <- seq(53, 75,1)
sum(dhyper(count,75,75,75))
# Evaluate Pearson Product Moment Correlation between Y and X.

cor(schools[,1],schools[,2], method = c("p"))

# This suggests a simple linear regression analysis may be used.

result <- lm(Y~X,data=schools)
summary(result)

# Note that the correlation coefficient from cor() when squared equals
# the multiple R-squared value of 0.3435 in the simple linear regression.  
#-------------------------------------------------------------------------------------

# The AOV results point to region as an important factor for a multiple regresson model.
# Using ggplot2 it is possible to visualize the role played by region.

p <- ggplot(schools, aes(x = X, y = Y))+geom_point(aes(color = region), size = 3)+
  ggtitle("Plot of Expenditures versus Income Colored by Region")
p


result <- lm(Y~X+region,schools)
summary(result)

section <- combineLevels(schools$region, levs=c("A","B","C"),newLabel = "S")
schools <- cbind(schools,section)
str(schools)
result <- lm(Y~X+section,schools)
summary(result)

p + geom_abline(intercept=92.29, slope=0.0305)+geom_abline(intercept=35.38,slope=0.0305)
 
# A multiple regression model needs to be evaluated.  Using the residuals is one way.
# It is highly desirable for the residuals to conform to a normal distribution with 
# few to no outliers.  Other examinations are also useful.

r <- residuals(result)
fitt <- fitted(result)

par(mfrow = c(1,2))
hist(r, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(r, col = "red", main = "Boxplot Residuals", ylab = "Residual")
par(mfrow = c(1,1))

qqnorm(r, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(r, col = "green", lty = 2, lwd = 2)

skewness(r)
kurtosis(r)

plot(fitt,r, main = "Plot of residuals versus fitted values", xlab = "fitted values",
     ylab = "residuals", col = "red")
abline(h = 0, lty = 2, col = "green")
abline(h = 87.65, lty = 2, col = "blue")
abline(h = -87.65, lty = 2, col = "blue")

# Note that the sum of residuals in ordinary least squares must be zero.  There 
# should be no trend departing from the horizontal line shown.The residuals indicate 
# the regression model is a reasonable fit to the data despite a few outliers.  

# All models are wrong, some are useful.  This model can be improved with more 
# data in the form of a larger sample size and additional predictive variables.