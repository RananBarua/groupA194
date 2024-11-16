# loading necessary library for CSV file

library(readr)

# reading the winequality-white data set and value

wine <- read.csv("winequality-white.csv", header = TRUE, sep = ";")

View(wine) # viewing data set in another window

# plotting the histogram

hist(wine$quality, main = "Histogram of Wine Quality", xlab = "Quality", col = "lightblue", border = "black")

#plotting the bell curve suggesting not normally distributed

lines(density(wine$quality), col = "red", lwd = 2)

# Sharpiro test to check if the distribution is normal or not

shapiro.test(wine$quality)

# calculating mean to divide the alcohol into subclasess
mean_alcohol <- mean(wine$alcohol)

# division of two sub classes based on mean // "low" and "High" categories
wine$alcohol_class <- ifelse(wine$alcohol <= mean_alcohol, "Low", "High")

#box plot of the two sub classes in relation to wine quality
boxplot(wine$quality ~ wine$alcohol_class, data = wine, xlab = "alcohol_class", ylab = "Wine_quality", main = "Wine Quality Based on Class")

#Wilcox test to check difference in means 
wilcox.test(wine$quality ~ wine$alcohol_class)