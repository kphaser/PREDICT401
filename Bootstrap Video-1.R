# Bootstrap T for Predict 401 
#-----------------------------------------------------------------------------------

require (moments)

w <- seq(-4,4,0.1)
plot(w,dnorm(w,0,1), xlab="Standard Normal Variable", ylab="Density",
     main="Standard Normal Density Function", col="red", type = "l", lwd = 2)
abline(v=0, col="green", lty=2, lwd=2)

#--------------------------------------------------------------------------------
# This next portion draws random samples and produces plots.  The sample size must be set.
# This illustrates the impact of asymmetry and outliers.
#--------------------------------------------------------------------------------
n <- 40  # sample size
set.seed(124)

# Define vectors for storage purposes.
t.m <- numeric(0)
mu <-  0
std <- 1

N <- 10000  # Number of iterations used in establishing the sampling distribution.

for (i in 1:N)
{
  x <- rnorm(n,0,1)
  stder <- sd(x)/sqrt(n)
  t.m[i] <- (mean(x)-mu)/(stder)
}

#-------------------------------------------------------------------------------
# Evaluate performance of t-statistic n = 40.
# Determine how the sampling distribution compares to theoretical results.
#-------------------------------------------------------------------------------
skewness(t.m)
kurtosis(t.m)

x <- seq(-4, 4,0.1)
par(mfrow = c(2,1))
hist(t.m, main = "Sampling Distribution of t-statistic n=40", breaks = "Sturges",
     ylim = c(0, 0.45), prob = TRUE, col = "green", xlab = "t-statistic values")
curve(dt(x, df=39),add=TRUE, col= "darkred", lwd = 2)
legend("topright", legend = c("skewness = 0.01", "kurtosis = 3.16"))
abline(v= quantile(t.m, probs = 0.025), col = "blue", lwd = 2, lty = 2)
abline(v= quantile(t.m, probs = 0.975), col = "blue", lwd = 2, lty = 2)

par(mfrow = c(1,1))

quantile(t.m, probs = c(0.025, 0.975))
qt(c(0.025,0.975),39,lower.tail=TRUE)
#-------------------------------------------------------------------------------
# Now we will do bootstrapping with a single sample of size 40.
#-------------------------------------------------------------------------------
n <- 40  # sample size
set.seed(124)

rx <- rnorm(n,0,1)
skewness(rx)
kurtosis(rx)

x <- seq(-4, 4,0.1)
hist(rx, col="red", main = "Histogram of Random Sample from a Normal Distribution", prob = TRUE)
curve(dnorm(x,0,1), add=TRUE, col= "green", xlab = "Normal Variable", ylab = "Density", lwd = 2)

# Define vectors for storage purposes.

t.m <- numeric(0)
mu <-  mean(rx)
std <- sd(rx)

N <- 10000  # Number of iterations used in establishing the sampling distribution.

for (i in 1:N)
{
  z <- sample(rx, n, replace = TRUE)
  stder <- sd(z)/sqrt(n)
  t.m[i] <- (mean(z)-mu)/(stder)
}

#----------------------------------------------------------------------
# Evaluate performance of t-statistic n = 40.
x <- seq(-4, 4,0.1)

hist(t.m, main = "Sampling Distribution of t-statistic n=40", breaks = "Sturges",
     ylim = c(0, 0.45), prob = TRUE, col = "green", xlab = "t-statistic values")
curve(dt(x, df=39),add=TRUE, col= "darkred", lwd = 2)
legend("topleft", legend = c("skewness = -0.075", "kurtosis = 3.209"))
abline(v= quantile(t.m, probs = 0.025), col = "blue", lwd = 2, lty = 2)
abline(v= quantile(t.m, probs = 0.975), col = "blue", lwd = 2, lty = 2)

skewness(t.m)
kurtosis(t.m)
quantile(t.m, probs = c(0.025, 0.975))
qt(c(0.025,0.975),39,lower.tail=TRUE)
#----------------------------------------------------------------------

# Traditional confidence interval for the mean using srs and t-statistic.
t.test(rx, conf.level=0.95, alternative = c("two.sided"))
# We will compare bootstrapping to this confidence interval.
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Determine a two-sided confidence interval using bootstrap t distribution.
Q1 <- quantile(t.m, prob = c(0.025), names = FALSE)
Q2 <- quantile(t.m, prob = c(0.975), names = FALSE)
round(mu - Q2*(std/sqrt(n)), digits = 3)
round(mu - Q1*(std/sqrt(n)), digits = 3)
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Now for something non-normal--the exponential
#----------------------------------------------------------------------
#----------------------------------------------------------------------

w <- seq(0,5,0.1)
plot(w,dexp(w,1), main="Exponential with Rate = 1", ylab="Density",
     xlab="Waiting Times", col="red", type = "l", lwd = 2)
abline(v=1, col="green", lty=2, lwd=2)

#----------------------------------------------------------------------
# This next portion draws random samples and produces plots.  The sample size must be set.
# This illustrates the impact of asymmetry and outliers.
#----------------------------------------------------------------------
n <- 40  # sample size
set.seed(124)

# Define vectors for storage purposes.
t.m <- numeric(0)
mu <-  1
std <- 1

N <- 10000  # Number of iterations used in establishing the sampling distribution.

for (i in 1:N)
{
  x <- rexp(n,1)
  stder <- sd(x)/sqrt(n)
  t.m[i] <- (mean(x)-mu)/(stder)
}

#----------------------------------------------------------------------
# Evaluate performance of t-statistic n = 40.
skewness(t.m)
kurtosis(t.m)

hist(t.m, main = "Sampling Distribution of t-statistic n=40", breaks = "Sturges",
     ylim = c(0, 0.45), prob = TRUE, col = "green", xlab = "t-statistic values")
curve(dt(x, df=39),add=TRUE, col= "darkred", lwd = 2)
legend("topleft", legend = c("skewness = 0.01", "kurtosis = 3.16"))
abline(v= quantile(t.m, probs = 0.025), col = "blue", lwd = 2, lty = 2)
abline(v= quantile(t.m, probs = 0.975), col = "blue", lwd = 2, lty = 2)

quantile(t.m, probs = 0.025)
quantile(t.m, probs = 0.975)
qt(c(0.025,0.975),39,lower.tail=TRUE)
#-----------------------------------------------------------------------
# Bootstrapping demonstration
#-----------------------------------------------------------------------

n <- 40  # sample size
set.seed(124)

results_tt <- numeric(0)
results_bt <- numeric(0)
L <- 500
N <- 1000  # Number of iterations used in establishing the sampling distribution.

for (k in 1:L)
{
  rx <- rexp(n,1)

# Define vectors for storage purposes.

  t.m <- numeric(0)
  mu <-  mean(rx)
  std <- sd(rx)

for (i in 1:N)
{
  z <- sample(rx, n, replace = TRUE)
  stder <- sd(z)/sqrt(n)
  t.m[i] <- (mean(z)-mu)/(stder)
}

Q1 <- quantile(t.m, prob = c(0.025), names = FALSE)
Q2 <- quantile(t.m, prob = c(0.975), names = FALSE)
bt1 <- round(mu - Q2*(std/sqrt(n)), digits = 3)
bt2 <- round(mu - Q1*(std/sqrt(n)), digits = 3)
results_bt[k] <- (bt1 <= 1  && 1 <= bt2) 
tt1 <- mu - qt(0.975, n-1, lower.tail = TRUE)*std/sqrt(n)
tt2 <- mu - qt(0.025, n-1, lower.tail = TRUE)*std/sqrt(n)
results_tt[k] <- (tt1 <= 1 && 1 <= tt2)

}
sum(results_bt)/L
sum(results_tt)/L
#----------------------------------------------------------------------
#----------------------------------------------------------------------







