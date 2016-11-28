# Data Analysis Assignment 2, PREDICT 401
# R. L. Sanford Ph.D.

# For this assignment, EDA, analysis of variance and simple linear 
# regression will be performed.

library(ggplot2)
library(gridExtra)
library(moments)
library(reshape)

# 1) Construct a plot matrix using plot(mydata[,2:8]). Determine for
# which pairs of variables a Pearson Correlation Coefficient or a
# Spearman Correlation Coefficent is appropriate.

# Read-in mydata.csv and store as data frame "mydata"
# mydata.csv assumed to be in working directory
mydata <- read.csv("mydata.csv", sep = "")
str(mydata) # 'data.frame':   500 obs. of  12 variables

# Construct plot matrix of variables 2 - 8
plot(mydata[,2:8])

# Compute correlations as appropriate.  

spatial <- mydata[, c("LENGTH", "DIAM", "HEIGHT")]
weight <- mydata[, c("WHOLE", "SHUCK", "VISCERA", "SHELL")]

completeCorr <- rbind(
melt(cor(spatial, spatial, method = "pearson")),
melt(cor(weight, weight, method = "pearson")),
melt(cor(spatial, weight, method = "spearman")))

completeCorr$method <- rep(c("Pearson", "Spearman"),
	each = length(spatial)^2 + length(weight)^2,
	length.out = nrow(completeCorr))

completeCorr <- completeCorr[!completeCorr$X1 == completeCorr$X2 &
	!duplicated(completeCorr$value), ]
completeCorr <- completeCorr[order(-abs(completeCorr$value)), ]

completeCorr$value <- round(completeCorr$value, 3)

colnames(completeCorr) <- c("Variable", "Variable", "Coefficient", "Method")
grid.table(completeCorr, rows = NULL)


#2) SHUCK represents harvestable meant.  Form a matrix of boxplots showing SHUCK differentiated 
# by CLASS and SEX.  There will be 18 boxplots in this matrix.  Each age CLASS is a separate
# population due to factors persent during the life of the population.  What does this reveal
# about the variability in abalone growth and aging, particularly in view of unknown factors?

# a) A convenient way to do this is with indices and the par() function with mfrow = c(3,1)

ggplot(mydata, aes(x = SEX, y = SHUCK)) + geom_boxplot(aes(color = CLASS))


# 3)  Write a function in R to calculate the Pearson chi square statistic on 2x2 contingency 
# tables which have the marginal totals.  Test for independence using this function on SHUCK 
# and VOLUME.   Show the chi square value and p-value in a table. Discuss the results.  

chi <- function(x){
	# To be used with 2x2 contingency tables that have margins added.
	# Expected values are calculated.
	e11 <- x[3,1]*x[1,3]/x[3,3]
	e12 <- x[3,2]*x[1,3]/x[3,3]
	e21 <- x[3,1]*x[2,3]/x[3,3]
	e22 <- x[3,2]*x[2,3]/x[3,3]
	# Value of chi square statistic is calculated.
	chisqStat <- (x[1,1]-e11)^2/e11 + (x[1,2]-e12)^2/e12 + (x[2,1]-e21)^2/e21 + 
		(x[2,2]-e22)^2/e22
	return(list("chi-squared" = chisqStat, "p-value" = pchisq(chisqStat, 1, lower.tail = F)))
}

# a) To dichotomize SHUCK and VOLUME use statements similar to this:
# shuck <- factor(mydata$SHUCK > median(mydata$SHUCK, labels = c("below","above")))

shuck <- factor(mydata$SHUCK > median(mydata$SHUCK), labels = c("below", "above"))
volume <- factor(mydata$VOLUME > median(mydata$VOLUME), labels = c("below", "above"))

# b) To generate a table use shuck_volume <- addmargins(table(shuck,volume))

shuck_volume <- addmargins(table(shuck,volume))

chi(shuck_volume)

chisq.test(shuck_volume[1:2,1:2], correct = F)


# 4) Perform an analysis of variance (ANOVA) on SHUCK using CLASS and SEX as the grouping
# variables. Assume equal variances. First use the model SHUCK~CLASS+SEX+CLASS*SEX.
# Then use the model SHUCK~CLASS+SEX.  Follow up on the second model using TukeyHSD
# to isolate pairwise significant differences.  Report the results and your interpretation.

anova <- aov(SHUCK~CLASS+SEX+CLASS*SEX, mydata)
summary(anova)

anova <- aov(SHUCK~CLASS+SEX, mydata)
summary(anova)

TukeyHSD(anova)


# 5) Use ggplot2 to form a scatterplot of SHUCK versus VOLUME and a scatterplot of their
# logarithms labeling the variables as L_SHUCK and the latter as L_VOLUME. Use color to 
# differentiate CLASS in the plots. Compare the two scatterplots.
# Where do the various CLASS levels appear in the plots?

mydata$L_SHUCK <- log(mydata$SHUCK)
mydata$L_VOLUME <- log(mydata$VOLUME)

# a) ggplot2 must be installed from CRAN.  Use library(ggplot2) prior to executing code.
# b) Here is an example of what should be produced using ggplot.

grid.arrange(
	ggplot(mydata, aes(x = VOLUME,y = SHUCK)) + geom_point(aes(color = CLASS)) +
		labs(x = "VOLUME", y = "SHUCK")+ theme(legend.position="none"),
	ggplot(mydata, aes(x = L_VOLUME, y = L_SHUCK)) + geom_point(aes(color = CLASS)) +
		labs(x = "LOG VOLUME", y= "LOG SHUCK"),
	nrow = 1)


# 6) Regress L_SHUCK as the dependent variable on L_VOLUME, CLASS and SEX. Follow the steps 
# shown in Section 16.1 of Lander but use the following multiple regression model:
# out <- lm(L_SHUCK~L_VOLUME+CLASS+SEX, data = mydata).  Use summary() on the object "out"
# and comment on the results.

model <- lm(L_SHUCK~L_VOLUME+CLASS+SEX, data = mydata)
summary(model)

model2 <- lm(L_SHUCK ~ L_VOLUME, data = mydata)
summary(model2)


#7) Perform an analysis of the residuals.  Use out$residuals and construct a histogram and
# QQ plot. Compute the skewness and kurtosis. The residuals should approximate a normal 
# distribution. Describe the distribution of residuals. Use ggplot and plot the residuals
# versus L_VOLUME coloring the data points by CLASS and a second time coloring the 
# data points by SEX.  Also use ggplot to present boxplots of the residuals
# differentiated by SEX and CLASS.  How well does the regression model fit the data?

summary(model$residuals)
skewness(model$residuals)
kurtosis(model$residuals)

hist(model$residuals, col = "dodgerblue4", xlab = "Residuals",
	main = "Histogram of Residuals")

qqnorm(model$residuals, col = "dodgerblue4")
qqline(model$residuals, lty = 2, lwd = 1)


grid.arrange(
	ggplot(model, aes(x = model$residuals)) + geom_histogram(aes(fill = ..count..)) +
		xlab("Residuals") + ylab("Frequency") + labs(fill = ""),
	ggplot(model, aes(sample = model$residuals)) + stat_qq(color = "darkcyan", size = 3, shape = 1) +
		geom_abline(intercept = mean(model$residuals), slope = sd(model$residuals)),
	nrow = 1)


grid.arrange(
	ggplot(model, aes(x = L_VOLUME,y = model$residuals)) + geom_point(aes(color = CLASS)) +
  		labs(x = "L_VOLUME", y = "Residuals") +
		theme(legend.direction = "horizontal", legend.position = "top"),
	ggplot(model, aes(x = L_VOLUME,y = model$residuals)) + geom_point(aes(color = SEX)) +
  		labs(x = "L_VOLUME", y = "Residuals") +
		theme(legend.direction = "horizontal", legend.position = "top"),
	nrow = 1)

grid.arrange(
	ggplot(model, aes(x = CLASS,y = model$residuals)) + geom_boxplot(aes(color = CLASS)) +
		labs(y = "Residuals") + theme(legend.position = "none"),
	ggplot(model, aes(x = SEX,y = model$residuals)) + geom_boxplot(aes(color = SEX)) + 
		labs(y = "Residuals") + theme(legend.position = "none"),
	nrow = 1)


#8) The next portion is a study of the potential use of VOLUME as a means of selecting abalone for 
# harvest.  There is a tradeoff faced in managing the abalone harvest.  The infant population must
# be protected since that represents the future harvest.  On the other hand, the harvest should be
# designed to be efficient with a sufficient yield to justify the effort.  This part will use a 
# simple decision rule.  If the VOLUME for an abalone is below a specified volume, that individual 
# will not be harvested.  If above, it will be harvested.

#a) This part of the assignment will calculate the proportion of infant abalone which fall
# beneath a specified volume or "cutoff".  A series of volumes covering the range from the
# minimum volume to the maximum volume will be used in a "for loop".  The code for doing this 
# is supplied.  This calculation will show how the harvest proportion of infants changes as
# the volume cutoff used for assessment changes. 

# The following code shows how to evaluate volume levels for the infant population. A "for loop"
# is used to progressively evaluate the proportion of infants which would not be harvested
# at each specified volume. Both the proportions and the volumes are retained.

idxi <- mydata[,1]=="I"
idxf <- mydata[,1]=="F"
idxm <- mydata[,1]=="M"

max.v <- max(mydata$VOLUME)
min.v <- min(mydata$VOLUME)
delta <- (max.v - min.v)/100
prop.infants <- numeric(0)
prop.adults <- numeric(0)
volume.value <- numeric(0)

total.infants <- length(mydata[idxi,1])  # This value must be changed if adults are being considered.
total.adults <- length(mydata[idxf,1])+length(mydata[idxm,1])  

for (k in 1:100) { 
	value <- min.v + k*delta
	volume.value[k] <- value
	prop.infants[k] <- sum(mydata$VOLUME[idxi] <= value)/total.infants
	prop.adults[k] <- (sum(mydata$VOLUME[idxf] <= value)+sum(mydata$VOLUME[idxm] <= value))/total.adults
}

n.infants <- sum(prop.infants <= 0.5)
split.infants <- min.v + (n.infants + 0.5)*delta  # This estimates the desired volume.

n.adults <- sum(prop.adults <= 0.5)
split.adults <- min.v + (n.adults + 0.5)*delta  


plot(volume.value, prop.adults, col = "dodgerblue4", lwd = 2, type = "l",
	main = "Propotion of Adults and Infants Protected",
	xlab = "Volume", ylab = "Proportion")
lines(volume.value, prop.infants, col = "coral3", lwd = 2, type = "l")
abline(h = 0.5)
abline(v = c(split.adults, split.infants))
text(split.infants, 0.475, round(split.infants, 4), pos = 4, cex = 0.8)
text(split.adults, 0.475, round(split.adults, 4), pos = 4, cex = 0.8)
legend(c(0.075, 0.10), c(0.78, 0.9), c("Adults", "Infants"), col = c("dodgerblue4", "coral3"),
	lwd = 1.5)

## Calculations for SHUCK weights and proportions
sum(mydata$SHUCK[mydata$VOLUME > 0.0164])
sum(mydata$SHUCK[mydata$VOLUME > 0.0396])
sum(mydata$SHUCK)

sum(mydata$SHUCK[mydata$VOLUME > 0.0164]) / sum(mydata$SHUCK)
sum(mydata$SHUCK[mydata$VOLUME > 0.0396]) / sum(mydata$SHUCK)

# 8 continued) Modify the code.  This time instead of counting infants, count adults.  Present
# your modified code and a plot showing the adult proportions versus volume.  Compute the 
# 50% "split" volume value for the adults and show on the plot similarly to the plot for 
# infants. Compare the "split" values for infants and adults and comment. Show the code used.

# It is essential that the males and females be combined into a single count as "adults" for
# computing the proportion for "adults".  Part #10 will require plotting of infants versus adults.
# For this plotting to be accomplished, a "for loop", similar to the one above, must be
# used to compute the adult harvest proportions.  It must use the same value for the constants
# min.v and delta.  It must also use the statement for (k in 1:100).  Otherwise, the resulting
# vector of adult proportions can not be directly compared to the infant proportions.

# Note the difference in the 50% cutoffs for infants and adults.


#-----------------------------------------------------------------------------------
# 9) This part will address determination of volume or set of volumes which maximize the
# difference in percentages of adults and infants.  To calculate this result, the proportions
# from #8) must be used.  These proportions must be converted from "not harvested" proportions
# to "harvested" proportions for the calculation  by subtracting (1-prop.infants) from 
# (1-prop.adults).  (The reason the proportion for infants drops sooner than adults is 
# that they are maturing and becoming adults with larger volumes.)  Select a range of values 
# which have the potential to maximize the difference in harvest proportions.

# a) Present three plots: 1) a plot of (1-prop.adults) versus volume.value, 
# 2) a plot of (1-prop.infants) versus volume.value, and 3) a plot of the difference 
# (prop.infants-prop.adults) versus volume.value.  (volume.value is from #8).

## 9(a)(1), (2)
par(mfrow = c(1, 2))
plot(volume.value, (1 - prop.adults),
	main = "Proportion of Adults Harvested",
	col = "darkred", lwd = 2, type = "l", ylab = "Proportion Harvested",
	xlab = "Volume")
plot(volume.value, (1 - prop.infants),
	main = "Proportion of Infants Harvested",
	col = "dodgerblue4", lwd = 2, type = "l", ylab = "",
	xlab = "Volume")
par(mfrow = c(1, 1))

## 9(a)(3)
difference <- (1-prop.adults)-(1-prop.infants)
difference <- prop.infants - prop.adults
plot(volume.value, difference, main = "Difference in Harvest Proportions", col = "darkred", 
	lwd = 2, type = "l", xlab = "Volume",
	ylab = "Difference in Proportions Harvested")
abline(v = volume.value[which.max(difference)], lty = 2, col = "dodgerblue4")
text(volume.value[which.max(difference)] + 0.003, 0.4,
	paste("volume =",
		round(volume.value[which.max(difference)], 5)), srt = 90)

# Alternatively, the harvest proportions can be plotted together.
plot(volume.value,(1 - prop.adults), col = "dodgerblue4", type = "l", lwd = 2,
	main = "Adult and Infant Harvest Proportions", xlab = "Volume", ylab = "Proportion Harvested")
lines(volume.value,(1-prop.infants), col = "coral3", type = "l", lwd=2)
abline(v = min(volume.value[(1 - prop.infants) == 0]), lty = 2)
text(0.044, 0.65, paste("volume =", round(min(volume.value[(1 - prop.infants) == 0]), 4)),
	pos = 4, cex = 0.8, srt = 90)
abline(h = 0.3256, lty = 2, col = "dodgerblue4")
text(0.0495, 0.35, paste("adult proportion =", round(max(1-prop.adults[(1 - prop.infants) == 0]), 4) ),
	pos = 4, cex = 0.8)
legend(c(0.075, 0.10), c(0.87, 1.0), c("Adults", "Infants"), col = c("dodgerblue4", "coral3"),
	lwd = 1.5)

# Calculations for "zero-infant" harvest volume and adult proportion
max(1-prop.adults[(1 - prop.infants) == 0]) # Maximum harvest proportion of adults with zero infants.
min(volume.value[(1 - prop.infants) == 0])  # Maximum volume cutoff with zero infants harvested.

table(mydata$VOLUME > min(volume.value[(1 - prop.infants) == 0]), mydata$SEX)
sum(mydata$SHUCK[mydata$VOLUME > min(volume.value[(1 - prop.infants) == 0])]) / sum(mydata$SHUCK)

table(mydata$SEX, mydata$CLASS)

# b) Now construct an ROC curve by plotting (1-prop.adults) versus (1-prop.infants). Each point
# which appears corresponds to a particular volume.value.  This curve is used often to illustrate
# the tradeoffs involved in with different decision rules.  Find the largest cutoff volume for 
# which no infant is harvested (i.e.-(1-prop.infants = 0)).  Report it and the 1-prop.adults value. 
# Comment.  Does this seem to be a reasonable choice for a decision rule?

## 9(b)
plot(1-prop.infants,1-prop.adults, col = "dodgerblue4", lwd = 2, type = "l",
	main = "ROC curve of adult and infant harvest proportions", ylab = "Adult harvest proportion",
	xlab = "Infant harvest proportion")
abline(a=0, b=1, col = "darkred", lty = 2, lwd = 2)

# The departure of the plotted point from the straight line shows the degree of difference between the 
# two abalone populations (infants and adults). The following calculation does a numerical integration
# under the ROC curve. One rule of thumb for statistical discrimination is that if this value
# exceeds 0.8, the procedure involved has good discriminating capability.


# 10) In #9 a set of volumes were identified which could be used for harvest decision making.  To
# settle on one volume, which may not be included in the volumes selected in #9), an additional 
# evaluation will be performed.  Harvesting of classes A1 and A2 must be minimized.

# a) Using the code supplied, find the smallest volume which produces a zero harvest of abalone in
# in classes A1 and A2.  This can be accomplished by substituting values into the code supplied.
# The level of precision required can be achieved with values such as 0.036, 0.035, 0.034 and so on.
# More than that is unnecessary.  Report your result and discuss how this compares to the volumes
# identified from the plot of differences in harvesting proportions in #9).  

# vectorized search approach, alternate:
round(max(mydata[mydata$CLASS == "A1" |
	mydata$CLASS == "A2" &
	mydata$SEX == "I", "VOLUME"]) + 0.0003, digits = 3)

cutoff <- 0.034 # Example volume cutoff value.

index.A1 <- (mydata$CLASS=="A1")
indexi <- index.A1 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A1)

index.A2 <- (mydata$CLASS=="A2")
indexi <- index.A2 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A2)

index.A3 <- (mydata[,10]=="A3")
indexi <- index.A3 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A3)

index.A4 <- (mydata[,10]=="A4")
indexi <- index.A4 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A4)

index.A5 <- (mydata[,10]=="A5")
indexi <- index.A5 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A5)

index.A6 <- (mydata[,10]=="A6")
indexi <- index.A6 & idxi
sum(mydata[indexi,11] >= cutoff)/sum(index.A6)


# Calculations for SEX and SHUCK proportions
table(mydata$VOLUME > 0.035, mydata$SEX)
sum(mydata$SHUCK[mydata$VOLUME > 0.035]) / sum(mydata$SHUCK)

# Density plots, by SEX and CLASS
grid.arrange(
	ggplot(mydata, aes(x = VOLUME)) +
		geom_density(aes(color = CLASS), alpha = 0.25) +
		xlab("Volume") + ylab("Density"),
	ggplot(mydata, aes(x = VOLUME)) +
		geom_density(aes(color = SEX), alpha = 0.25) +
		xlab("Volume") + ylab("Density"),
	nrow = 1)

# In your report conclusions, discuss what you have learned about the use of
# physical measurements as a basis for abalone harvesting.  Consider feasibility and 
# the tradeoffs involved.  How much reliance would you place in the volume cutoff
# determined in step #10)?  What else might be done to verify this conclusion?
# If you have specific harvesting recommendations or strategy,discuss them.  
# Also discuss what you see as difficulties in analyzing data from an observational 
# study involving different classes or cohorts of subjects.  What cautions come to mind?
# What did you learn about abalone with this analysis?

# Save the script for future use.



# R has a very powerful family of functions, called "apply", which
# includes apply(), by(), tapply(), lapply(), mapply(), rapply() and eapply()

# These functions, for the most part, serve as optimized loops over R objects
# and environments; allowing one to repeatedly apply a function to different
# aspects of an object, such as rows of data frame.

# A demonstration of apply() is included below. Type "?apply()" into the console
# for the help/documentation page.

# The abalone dataset includes four (4) distinct weight values:  whole, shuck,
# viscera and shell. The following code will create three (3) vectors with the
# proportional "shuck", "viscera" and "shell" weights as part of the whole.
# apply() is then used to identify which proportion is the largest per row.

# First, create the proportional weight vectors:
shuck.prop <- mydata$SHUCK / mydata$WHOLE
viscera.prop <- mydata$VISCERA / mydata$WHOLE
shell.prop <- mydata$SHELL / mydata$WHOLE

# Second, use apply() to identify which proportion is largest, per row
prop.comp <- apply(data.frame(shuck.prop, viscera.prop, shell.prop),
	1, function(x) which.max(x)) # "1" instructs R to execute by row; "2" by column

prop.comp <- data.frame(mydata$SEX, prop.comp)
str(prop.comp) # 'data.frame':   500 obs. of  2 variables:

# prop.comp is predominated by "1"s, meaning in most instances that the shuck weight
# is the largest proportion of the total weight.

# Factor prop.comp variable for ease of interpretation:
prop.comp$prop.comp <- factor(prop.comp$prop.comp,
	levels = c(1, 2, 3),
	labels = c("shuck", "viscera", "shell"))

table(prop.comp$prop.comp, prop.comp$mydata.SEX)
# The instances in which "shell" weight is the largest proportion are predominately
# male.

