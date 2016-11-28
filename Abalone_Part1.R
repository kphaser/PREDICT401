# Data Analysis Assignment 1

# Question 1
# Use sample to create index variable for selecting rows from abalone.csv
# Read the abalone data set into RStudio and examine the file

abalone <- read.csv("./Data Analysis 1/abalone.csv", sep="")
str(abalone)
set.seed(123)
mydata <- abalone[sample(1:4141, 500),]
write.table(mydata, file = "./Data Analysis 1/mydata.csv")


# Question 2
# Check mydata using str(). Use summary() on mydata
# Plot mydata to construct matrix of variables 2-8
# What's the relationship between the variables?

mydata <- read.csv("./Data Analysis 1/mydata.csv", sep="")
str(mydata)
summary(mydata)
plot(mydata[,2:8], main = "Matrix of Bivariate Plots")

# Question 3
# Determine the proportions of infant, female and male abalone in mydata.
# Construct 95% two-sided CI for each using prop.test() w/ argument correct=FALSE.
# Compare to the proportions in the original abalone data set.

# abalone proportions
p_infants <- sum(abalone$SEX == "I")/length(abalone$SEX)
p_females <- sum(abalone$SEX == "F")/length(abalone$SEX)
p_males <- sum(abalone$SEX == "M")/length(abalone$SEX)

# construct confidence interval to evaluate whether proportion of each SEX in mydata equals proportions in abalone dataset
prop.test(sum(mydata$SEX == "I"), nrow(mydata), p = p_infants, conf.level = 0.95, correct = FALSE)
prop.test(sum(mydata$SEX == "F"), nrow(mydata), p = p_females, conf.level = 0.95, correct = FALSE)
prop.test(sum(mydata$SEX == "M"), nrow(mydata), p = p_males, conf.level = 0.95, correct = FALSE)

# Question 4
# Calculate new variable VOLUME by multiplying LENGTH, DIAM, HEIGHT together
# Use data.frame() to include this variable in mydata. Plot WHOLE vs VOLUME
# Discuss nature of the plot. What type of relationship appears to be present?
# Compare to the bivariate plots in (3). What differences are there?

# define VOLUME column and use data.frame() to append VOLUME column
VOLUME = mydata$LENGTH * mydata$DIAM * mydata$HEIGHT
mydata <- data.frame(mydata, VOLUME)

# plot WHOLE vs VOLUME
plot(mydata$WHOLE, mydata$VOLUME, main = "Plot of WHOLE vs VOLUME", ylab = "VOLUME", xlab = "WHOLE")

# The relationship appears to be linear. There is more variability in the later pieces of data.
# This plot is very similar to the bivariate plots in question 3. Strong positive correlation and weaker at the end.

# Question 5
# Create and save new variable DENSITY by dividing WHOLE by VOLUME.
# Present matrix of histograms showing VOLUME, WHOLE, DENSITY differentiated by sex.
# Matrix should have 9 histograms. Do same thing with 9 boxplots.
# Discuss your observations.

mydata$DENSITY <- mydata$WHOLE / mydata$VOLUME
str(mydata)

par(mfrow = c(3,3))
# histograms of volume by sex
hist(mydata[mydata$SEX == "I", "VOLUME"], col = "red", main = "Infant Volume", xlab = "Volume")
hist(mydata[mydata$SEX == "F", "VOLUME"], col = "green", main = "Female Volume", xlab = "Volume")
hist(mydata[mydata$SEX == "M", "VOLUME"], col = "blue", main = "Male Volume", xlab = "Volume")
# histograms of whole by sex
hist(mydata[mydata$SEX == "I", "WHOLE"], col = "red", main = "Infant Whole", xlab = "Whole")
hist(mydata[mydata$SEX == "F", "WHOLE"], col = "green", main = "Female Whole", xlab = "Whole")
hist(mydata[mydata$SEX == "M", "WHOLE"], col = "blue", main = "Male Whole", xlab = "Whole")
# histograms of density by sex
hist(mydata[mydata$SEX == "I", "DENSITY"], col = "red", main = "Infant Density", xlab = "Density")
hist(mydata[mydata$SEX == "F", "DENSITY"], col = "green", main = "Female Density", xlab = "Density")
hist(mydata[mydata$SEX == "M", "DENSITY"], col = "blue", main = "Male Density", xlab = "Density")

par(mfrow = c(3,3))
# boxplot of volume by sex
boxplot(mydata[mydata$SEX == "I", "VOLUME"], col = "red", main = "Infant Volume", ylim = c(0,0.1))
boxplot(mydata[mydata$SEX == "F", "VOLUME"], col = "green", main = "Female Volume", ylim = c(0,0.1))
boxplot(mydata[mydata$SEX == "M", "VOLUME"], col = "blue", main = "Male Volume", ylim = c(0,0.1))
# boxplot of whole by sex
boxplot(mydata[mydata$SEX == "I", "WHOLE"], col = "red", main = "Infant Whole", ylim = c(0,2.4))
boxplot(mydata[mydata$SEX == "F", "WHOLE"], col = "green", main = "Female Whole", ylim = c(0,2.4))
boxplot(mydata[mydata$SEX == "M", "WHOLE"], col = "blue", main = "Male Whole", ylim = c(0,2.4))
# boxplot of density by sex
boxplot(mydata[mydata$SEX == "I", "DENSITY"], col = "red", main = "Infant Density", ylim = c(14,45))
boxplot(mydata[mydata$SEX == "F", "DENSITY"], col = "green", main = "Female Density", ylim = c(14,45))
boxplot(mydata[mydata$SEX == "M", "DENSITY"], col = "blue", main = "Male Density", ylim = c(14,45))
par(mfrow=c(1,1))

# Question 6
# Present matrix of QQ plots for DENSITY by sex. Use qqnorm() and qqline().
# Matrix should have 3 plots. Discuss what you observe.

par(mfrow=c(1,3))
qqnorm(mydata[mydata$SEX == "I", "DENSITY"], main = "Q-Q Plot of DENSITY for Infant")
qqline(mydata[mydata$SEX == "I", "DENSITY"])

qqnorm(mydata[mydata$SEX == "F", "DENSITY"], main = "Q-Q Plot of DENSITY for Female")
qqline(mydata[mydata$SEX == "F", "DENSITY"])

qqnorm(mydata[mydata$SEX == "M", "DENSITY"], main = "Q-Q Plot of DENSITY for Male")
qqline(mydata[mydata$SEX == "M", "DENSITY"])

par(mfrow=c(1,1))

# Question 7
# plot VOLUME, WHOLE, DENSITY vs RINGS using ggplot()
# Use sex as a facet to color the plot.
# How do variables relate to RINGS? How is SEX distributed across RINGS? Where do infants appear in these plots?

# load required libraries ggplot2 and gridExtra
require(ggplot2)
require(gridExtra)

# plot using ggplot and use grid.arrange to separate plots in one display
grid.arrange(
        ggplot(data = mydata, aes(x = RINGS, y = VOLUME)) +
                geom_point(aes(color = SEX), size = 2) +
                ggtitle("RINGS vs. VOLUME by SEX"),
        ggplot(data = mydata, aes(x = RINGS, y = WHOLE)) +
                geom_point(aes(color = SEX), size = 2) +
                ggtitle("RINGS vs. WHOLE by SEX"),
        ggplot(data = mydata, aes(x = RINGS, y = DENSITY)) +
                geom_point(aes(color = SEX), size = 2) +
                ggtitle("RINGS vs. DENSITY by SEX"),
        nrow = 1)

# Question 8
# Compute count of infants, males, females for each CLASS level
# Construct a graph showing counts of each sex for each class.
# There should be 3 lines on this plot. 

x <- data.frame(table(mydata$SEX, mydata$CLASS))
x
out <- as.data.frame(x)
colnames(out) <- c("sex", "class", "count")
ggplot(data = out, aes(x = class, y = count, group = sex, color = sex)) +
        geom_line() + 
        geom_point(size = 4) +
        ggtitle("Plot showing Sample Counts of Different Sexes vs. CLASS")


# Compute the proportions of infants, males, females for each CLASS level (proportions should sum to 1.0 for each CLASS level)
# Construct one graph showing proportions vs CLASS level. There should be three lines appearing, one for each sex.

y <- data.frame(table(mydata$CLASS))
y
x$CLASS_total <- y[match(x$Var2, y$Var1), "Freq"]
x
x$Proportion <- x$Freq / x$CLASS_total
x
out2 <- as.data.frame(x)
colnames(out2) <- c("sex", "class", "count", "class total", "proportion")
ggplot(data = out2, aes(x = class, y = proportion, group = sex, color = sex)) +
        geom_line() + 
        geom_point(size = 4) +
        ggtitle("Plot showing Sample Proportions of Different Sexes vs. CLASS")
# Discuss the implications

# Question 9
# Use ggplot to display two separate side by side boxplots for VOLUME and WHOLE by CLASS.
# These should be six boxplots for VOLUME and same for WHOLE.
# What do these reveal about the variability in VOLUME and WHOLE relative to CLASS?
# How well would these perform as predictors of CLASS membership?

grid.arrange(
        ggplot(mydata, aes(x = CLASS, y = VOLUME)) +
                geom_boxplot() + 
                ggtitle("Boxplot of VOLUME by CLASS"),
        ggplot(mydata, aes(x = CLASS, y = WHOLE)) +
                geom_boxplot() + 
                ggtitle("Boxplot of WHOLE by CLASS"),
        nrow = 2)

# Question 10
# Use aggregate() to compute mean values of WHOLE for each combination of SEX and CLASS.
# Use the resulting object with ggplot to generate plot of these mean values vs CLASS.
# One graph should be generated with three separate lines appearing, one for each sex, showing avg WHOLE vs CLASS.
# Do the same with DENSITY. Compare to prior displays and discuss.

a <- aggregate(WHOLE ~ SEX+CLASS, data = mydata, mean)
ggplot(a, aes(x = CLASS, y = WHOLE, group = SEX, color = SEX)) +
        geom_line() +
        geom_point(size = 4) + 
        ggtitle("Plot showing Mean WHOLE vs. CLASS for Three Sexes")

b <- aggregate(DENSITY ~ SEX+CLASS, data = mydata, mean)
ggplot(b, aes(x = CLASS, y = DENSITY, group = SEX, color = SEX)) +
        geom_line() +
        geom_point(size = 4) + 
        ggtitle("Plot showing Mean DENSITY vs. CLASS for Three Sexes")

# Conclusion questions to respond to:
# 1. Relative to the abalone study, to what extent may physical measurements be used
# for predicting age? Be specific in terms of the EDA you have performed.

# 2. What do hese data reveal about abalone sex vs physical measurements?

# 3. If you were presented with an overall histogram and summary stats for a population,
# what questions might you ask before accepting them as representative of that population?

# 4. What do you see as important difficulties with observational studies in general?
