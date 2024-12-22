# loading necessary library for CSV file

library(readr)

# reading the winequality-white data set and value

wine <- read.csv("winequality-white.csv", header = TRUE, sep = ";")

View(wine) # viewing data set in another window

# calculating mean to divide the alcohol into subclasess
mean_alcohol <- mean(wine$alcohol)

# division of two sub classes based on mean // "low" and "High" categories
wine$alcohol_class <- ifelse(wine$alcohol <= mean_alcohol, "Low", "High")

#showing first two rows of the dataset
head(wine,2)



# plotting the histogram

hist(wine$quality, main = "Histogram of Wine Quality", xlab = "Wine Quality(1-10)", ylab="Frequency", col = "lightblue", border = "black", freq = TRUE)

# Plotting the  normal bell curve
mean_quality <- mean(wine$quality)
sd_quality <- sd(wine$quality)
x_values <- seq(min(wine$quality), max(wine$quality), length.out = 100)
normal_curve <- dnorm(x_values, mean = mean_quality, sd = sd_quality)
lines(x_values, normal_curve * length(wine$quality) * diff(hist(wine$quality, plot = FALSE)$mids)[1], col = "red", lwd = 2, lty = 2)

#box plot of the two sub classes in relation to wine quality
boxplot(wine$quality ~ wine$alcohol_class, data = wine, xlab = "Alcohol Level", ylab = "Wine quality(1-10)", main = "Wine Quality Based on Alcohol Level")

# Performing a t-test to compare the means of the two classes
t_test_result <- t.test(wine$quality ~ wine$alcohol_class)
print(t_test_result)

#Wilcoxon test to reject or accept the null hypothesis 
wilcox_test <- wilcox.test(wine$quality ~ wine$alcohol_class, data = wine)
print(wilcox_test)
