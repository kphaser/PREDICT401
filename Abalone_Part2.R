# Data Analysis Assignment 2

# Question 1
# read in and examine data
mydata <- read.csv("./Data Analysis 2/mydata.csv", sep="")
str(mydata)
summary(mydata)

plot(mydata[,2:8], main = "Matrix of Bivariate Plots")

# create table of correlation coefficient values
allCorrPearson <- cor(mydata[,2:8], method = "pearson")
allCorrSpearman <- cor(mydata[,2:8], method = "spearman")

library(gridExtra)
frame() # start new plot frame
grid.table(allCorr, rows=NULL) # for pretty print
frame() # start new plot frame
grid.table(allCorrSpearman, rows=NULL) # for pretty print


# Question 2
library(ggplot2)
ggplot(mydata, aes(x=SEX, y=SHUCK)) + geom_boxplot(aes(color=CLASS)) + ggtitle("Boxplot of SHUCK by CLASS and SEX") 
ggplot(mydata, aes(x=CLASS, y=SHUCK)) + geom_boxplot(aes(color=SEX)) + ggtitle("Boxplot of SHUCK by CLASS and SEX") # or is this better plot?

# Question 3
# define pearson chi square statistic function
chisquared <- function(x) {
    e11 <- x[3,1]*x[1,3]/x[3,3] # expected value of x[1,1]
    e12 <- x[3,2]*x[1,3]/x[3,3] # expected value of x[1,2]
    e21 <- x[3,1]*x[2,3]/x[3,3] # expected value of x[2,1]
    e22 <- x[3,2]*x[2,3]/x[3,3] # expected value of x[2,2]
    
    chisqStat <- (x[1,1]-e11)^2/e11 + (x[1,2]-e12)^2/e12 + (x[2,1]-e21)^2/e21 + (x[2,2]-e22)^2/e22
        
    return(list("chi-squared" = chisqStat, "p-value" = pchisq(chisqStat, 1, lower.tail = F)))
}

# dichotomize SHUCK and VOLUME
shuck <- factor(mydata$SHUCK > median(mydata$SHUCK), labels = c("below", "above"))
volume <- factor(mydata$VOLUME > median(mydata$VOLUME), labels = c("below", "above"))

# generate table 
shuck_volume <- addmargins(table(shuck, volume))
shuck_volume
frame()
grid.table(shuck_volume) # pretty print of table

# apply user-defined function on table
frame()
grid.table(data.frame(chisquared(shuck_volume)),rows=NULL)
# compare p-value with built-in chi square test in R (should match)
chisq.test(shuck_volume[1:2, 1:2], correct = F) 
pchisq(323.213, 1, lower.tail = FALSE)

# Question 4
aov.shuck <- aov(SHUCK ~ CLASS+SEX+CLASS*SEX, mydata)
summary(aov.shuck)
aov.shuck2 <- aov(SHUCK ~ CLASS+SEX, mydata)
summary(aov.shuck2)
TukeyHSD(aov.shuck2)

# Question 5
library(ggplot2)
ggplot(mydata, aes(x=VOLUME, y=SHUCK)) + geom_point(aes(color=CLASS)) + ggtitle("SHUCK vs VOLUME by CLASS")
L_SHUCK <- log(mydata$SHUCK)
L_VOLUME <- log(mydata$VOLUME)
ggplot(mydata, aes(x=L_VOLUME, y=L_SHUCK)) + geom_point(aes(color=CLASS)) + ggtitle("L_SHUCK vs L_VOLUME by CLASS")

# Question 6
out <- lm(L_SHUCK~L_VOLUME+CLASS+SEX, mydata)
summary(out)

# Question 7
library(moments)
hist(out$residuals) # NAME THESE GRAPHS
qqnorm(out$residuals, main="Q-Q Plot of Residuals")
qqline(out$residuals)
skewness(out$residuals)
kurtosis(out$residuals)

ggplot(out, aes(x=L_VOLUME, y=out$residuals)) + geom_point(aes(color=CLASS)) + labs(x="L_VOLUME",y="Residual") + ggtitle("Residuals vs L_VOLUME by CLASS")
ggplot(out, aes(x=L_VOLUME, y=out$residuals)) + geom_point(aes(color=SEX)) + labs(x="L_VOLUME",y="Residual") + ggtitle("Residuals vs L_VOLUME by SEX")
ggplot(out, aes(x=SEX, y=out$residuals)) + geom_boxplot(aes(color=CLASS)) + labs(x="SEX",y="Residual") + ggtitle("Residuals differentiated by SEX and CLASS")
ggplot(out, aes(x=L_VOLUME, y=out$residuals)) + geom_boxplot(aes(color=SEX)) + labs(x="L_VOLUME",y="Residual") + ggtitle("Residuals vs L_VOLUME by CLASS")

# Question 8
idxi <- mydata[,1] == "I"
idxf <- mydata[,1] == "F"
idxm <- mydata[,1] == "M"

max.v <- max(mydata$VOLUME)
min.v <- min(mydata$VOLUME)
delta <- (max.v - min.v)/100
prop.infants <- numeric(0)
prop.adults <- numeric(0)
volume.value <- numeric(0)

total.infants <- length(mydata[idxi,1])
total.adults <- length(mydata[idxf,1]) + length(mydata[idxm,1])

for (k in 1:100) {
    value <- min.v + k*delta
    volume.value[k] <- value
    prop.infants[k] <- sum(mydata$VOLUME[idxi] <= value)/total.infants
    prop.adults[k] <- (sum(mydata$VOLUME[idxf] <= value) + sum(mydata$VOLUME[idxm] <= value))/total.adults
}

# infants
n.infants <- sum(prop.infants <= 0.5)
split.infants <- min.v + (n.infants + 0.5)*delta
plot(volume.value, prop.infants, col = "blue", main = "Infant Harvest Proportions", type = "l", lwd = 2, ylim = c(0,1))
abline(h=0.5)
abline(v=split.infants)
lines(volume.value,1-prop.infants,col="red",lwd=2)
legend(0.07,0.8,c("Not Harvested","Harvested"),lty=c(1,1),lwd=c(2,2),col=c("blue","red"))

# adults
n.adults <- sum(prop.adults <= 0.5)
split.adults <- min.v + (n.adults + 0.5)*delta 
plot(volume.value, prop.adults, col = "blue", main = "Adult Harvest Proportions", type = "l", lwd = 2)
abline(h=0.5)
abline(v=split.adults)
lines(volume.value,1-prop.adults,col="red",lwd=2)
legend(0.07,0.8,c("Not Harvested","Harvested"),lty=c(1,1),lwd=c(2,2),col=c("blue","red"))

# Question 9
plot(volume.value,1-prop.adults,col="red",main="Proportion of Adults Harvested",type="l",lwd=2)
plot(volume.value,1-prop.infants,col="blue",main="Proportion of Infants Harvested",type="l",lwd=2)
plot(volume.value,prop.infants-prop.adults,col="orange",main="Difference of Adults vs Infants Harvested",type="l",lwd=2)

# roc curve of infant and adult harvest proportions
plot(1-prop.infants, 1-prop.adults, col = "blue", main="ROC Curve of Adult & Infant Harvest Proportions", type="l",lwd=2, xlab="Infant Harvest Proportion", ylab="Adult Harvest Proportion")
abline(a=0,b=1,lty=2,lwd=2)

# find harvest threshold volume that protects infants and gives largest harvest of adults
# identify largest infant
max(mydata$VOLUME[mydata$SEX == "I"])
# find smallest volume value that corresponds to 0 harvested infants
min(volume.value[(1-prop.infants) == 0])
max(1-prop.adults[(1-prop.infants) == 0])

# Question 10
cutoff <- 0.0347

index.A1 <- (mydata$CLASS == "A1")
indexi <- index.A1 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A1)

index.A2 <- (mydata$CLASS == "A2")
indexi <- index.A2 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A2)

index.A3 <- (mydata$CLASS == "A3")
indexi <- index.A3 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A3)

index.A4 <- (mydata$CLASS == "A4")
indexi <- index.A4 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A4)

index.A5 <- (mydata$CLASS == "A5")
indexi <- index.A5 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A5)

index.A6 <- (mydata$CLASS == "A6")
indexi <- index.A6 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A6)
