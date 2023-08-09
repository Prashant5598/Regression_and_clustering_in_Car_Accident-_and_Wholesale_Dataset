# Starting and cleaning the R environment 
rm(list = ls())
install.packages("ggfortify")

# Importing the required library for the analysis 
library(ggplot2)
library(dplyr)
library(corrplot)
library(tsoutliers)
library(NbClust)
library(scales)
library(purrr)
library(caTools)
library(neuralnet)
library(ggfortify)

# Code for the Part 1 (Regression analysis)

# 1. Loading and studying the data and handling the missing values 

data = read.csv("dataArrests.csv", header = TRUE, sep = ";")
str(data)
missing_value <- any(is.na(data))
data = data[complete.cases(data),]
missing_value <- any(is.na(data))
str(data)

# 2. Exploratory data analysis on  four explanatory variables and the dependent variable
new_data <- select( data, Murder, Assault, UrbanPop, Traffic, CarAccidents)

# 2.1 Numerical analysis
summary(new_data)
standard_deviation_values = summarize_all(new_data, sd)
variance_values = summarize_all(new_data, var)
IQR_values = summarize_all(new_data, IQR)

# 2.2 Graphical analysis
par(mfrow = c(1,2))
boxplot(new_data$Murder, xlab = "", ylab = "Number of Murders", main = "Boxplot of Murder arrest")
hist(new_data$Murder, xlab = "Murder arrests", ylab = "Frequency", main = "Histogram of Murder arrest")

par(mfrow = c(1,2))
boxplot(new_data$UrbanPop, xlab = "", ylab = "Urban Pop %", main = "Boxplot of Urban Pop")
hist(new_data$UrbanPop, xlab = "Urban Pop", ylab = "Frequency", main = "Histogram of Urban Pop")

par(mfrow = c(1,2))
boxplot(new_data$Assault, xlab = "", ylab = "Number of Assault", main = "Boxplot of Assault arrest")
hist(new_data$Assault, xlab = "No of Assualt", ylab = "Frequency", main = "Histogram of Assault arrest")

par(mfrow = c(1,2))
boxplot(new_data$Traffic, xlab = "", ylab = "Number of Traffic Arrest", main = "Boxplot of Traffic arrest")
hist(new_data$Traffic, xlab = "No of Traffic Arrest", ylab = "Frequency", main = "Histogram of Traffic arrest")

par(mfrow = c(1,2))
boxplot(new_data$CarAccidents, xlab = "", ylab = "Number of Accidents", main = "Boxplot of Car Accidents")
hist(new_data$CarAccidents, xlab = "No of Accidents", ylab = "Frequency", main = "Histogram of Car Accidents")



# 3. Correlation analysis between all the variables
corrmat = cor(data)

# 4. Visualization of correlation for all the variables
corrplot(corrmat, 'number')

# 5. Elimination of explanatory variables with high correlation
explan_var = data[,(colnames(data)!= 'Murder')]
dep_var = data[,'Murder']
newcorr = abs(cor(explan_var))
diag(newcorr) = 0

while (max(newcorr) >= 0.8){
  maxvar = which(newcorr==max(newcorr), arr.ind = TRUE)
  maxavg =which.max(rowMeans(newcorr[maxvar[,1],]))
  print(rownames(maxvar)[maxvar[,1]==maxvar[maxavg,1]])
  explan_var = explan_var[,-maxvar[maxavg,1]]
  newcorr = newcorr[-maxvar[maxavg,1],-maxvar[maxavg,1]]
}

data_for_model = cbind('Murder'= dep_var, explan_var)

# 5.1 Removing the Potential Outliers from the data
data_for_model <- data_for_model %>% filter(Murder < 23)


# 6. Implementation of the linear regression with all explanatory variables
linear_model = lm(Murder ~ Assault + UrbanPop + Drug + Traffic + Cyber + Kidnapping + Domestic + Alcohol, data=data_for_model)
summary(linear_model)


# 7 & 8. Optimal linear regression model for the data i.e., steps towards final model
linear_model = lm(Murder ~ Assault + UrbanPop + Drug + Traffic + Cyber + Domestic + Alcohol, data = data_for_model)
summary(linear_model)

linear_model = lm(Murder ~ Assault + UrbanPop + Drug  + Cyber + Domestic + Alcohol, data = data_for_model)
summary(linear_model)

linear_model = lm(Murder ~ Assault + UrbanPop + Drug  + Cyber  + Alcohol, data = data_for_model)
summary(linear_model)
linear_model = lm(Murder ~ Assault + UrbanPop  + Cyber  + Alcohol, data = data_for_model)
summary(linear_model)

linear_model = lm(Murder ~ Assault + UrbanPop  + Cyber,  data = data_for_model)
summary(linear_model)


linear_model = lm(Murder ~ Assault + UrbanPop,  data = data_for_model)
summary(linear_model)


# 9. Checking  5 properties for linear regression using OLS
 
# 9.1 Mean of residuals is around 0
mean(residuals(linear_model))

# 9.2 correlation between residuals and the independent variables
explan_name = c('Assault', 'UrbanPop')
cor(residuals(linear_model),data_for_model[, explan_name])


# 9.3 The variance of the of the residuals is constant and 
# 9.4 The residual are linearly independent of each other 

plot(residuals(linear_model), type = "p", col = "blue", ylim = c(-20,20), pch = 16,
     ylab ="Residuals", main = "Residuals over time")
abline(a=3*sd(residuals(linear_model)), b = 0, col="red", lty =2)
abline(a=-3*sd(residuals(linear_model)), b = 0, col="red",lty =2)
abline(a=-0, b = 0, col="black",lty =2)

# 9.5 Check if the residuals are normally distributed 
JarqueBera.test(residuals(linear_model))
hist(residuals(linear_model))
boxplot(residuals(linear_model))


# Code for the Part 2 (Clustering)

# 1. Loading and studying the data

data1 = read.csv("wholesale.csv", header = TRUE, sep = ",")
str(data1)
missing_value <- any(is.na(data1))
unique(data1$Channel)
unique(data1$Region)


# 2. Exploratory data analysis for all variables

# 2.1 Numerical analysis

summary(data1)
standard_deviation_values1 = summarize_all(data1, sd)
variance_values1 = summarize_all(data1, var)
IQR_values1 = summarize_all(data1, IQR)

# 2.2 Graphical analysis

par(mfrow = c(1,2))
barplot(table(data1$Channel), xlab = "Channel", ylab = "Frequency", main = "Bar graph of Channel")
barplot(table(data1$Region),  xlab = "Regions", ylab = "Frequency", main = "Bar graph of Regions")

par(mfrow = c(1,2))
boxplot(data1$Fresh, xlab = "", ylab = "Fresh annual sale", main = "Boxplot of Fresh")
hist(data1$Fresh, xlab = "Fresh", ylab = "Frequency", main = "Histogram of Fresh")

par(mfrow = c(1,2))
boxplot(data1$Milk, xlab = "", ylab = "Milk annual sale", main = "Boxplot of Milk")
hist(data1$Milk, xlab = "Milk", ylab = "Frequency", main = "Histogram of Milk")

par(mfrow = c(1,2))
boxplot(data1$Grocery, xlab = "", ylab = "Grocery annual sale", main = "Boxplot of Grocery")
hist(data1$Grocery, xlab = "Fresh", ylab = "Frequency", main = "Histogram of Grocery")

par(mfrow = c(1,2))
boxplot(data1$Frozen, xlab = "", ylab = "Frozen annual sale", main = "Boxplot of Frozen")
hist(data1$Frozen, xlab = "Frozen", ylab = "Frequency", main = "Histogram of Frozen")

par(mfrow = c(1,2))
boxplot(data1$Detergents_Paper, xlab = "", ylab = "Detergents_Paper annual sale", main = "Boxplot of Detergents_Paper")
hist(data1$Detergents_Paper, xlab = "Detergents_Paper", ylab = "Frequency", main = "Histogram of Detergents_Paper")

par(mfrow = c(1,2))
boxplot(data1$Delicassen, xlab = "", ylab = "Delicassen annual sale", main = "Boxplot of Delicassen")
hist(data1$Delicassen, xlab = "Delicassen", ylab = "Frequency", main = "Histogram of Delicassen")


# 3. Correlation analysis And Multivariate analysis
corrmat = cor(data1)
corrplot(corrmat, 'number' )


# 4. Min-Max Normalization
new_data1 = data1
new_data1 =  (apply(new_data1, 2, rescale, to=c(0,1)))

# 5. Determination of the optimal number of clusters 
TWSS = map_dbl(1:8,function(k){
  model = kmeans(new_data1, centers = k)
  model$tot.withinss
})
plot(1:8, TWSS, type = "o", xlab = "Number of clusters", ylab = "TWSS")

Silclust = NbClust(new_data1, distance = "euclidean",
                   min.nc = 2, max.nc = 8,
                   method = "kmeans", 
                   index = "silhouette")
Gapclust = NbClust(new_data1, distance = 'euclidean',
                   min.nc = 2, max.nc = 8,
                   method = 'kmeans', 
                   index = 'gap')
CHclust = NbClust(new_data1, distance = 'euclidean',
                  min.nc = 2, max.nc = 8,
                  method = 'kmeans', 
                  index = 'ch')

par(mfrow = c(1,3))
plot(2:8, Silclust$All.index, type ='o', col ='blue',
     xlab = 'Number of clusters',ylab = 'Silhouette Value')
plot(2:8, Gapclust$All.index, type ='o', col ='blue',
     xlab = 'Number of clusters',ylab = 'Gap Statistics')
plot(2:8, CHclust$All.index, type ='o', col ='blue',
     xlab = 'Number of clusters',ylab = 'Calinski Harabasz Index')

# 6. Implementation of K means algorithm with n start at least 25

kmean_model = kmeans(new_data1, centers = 3, nstart = 25)
kmean_model$cluster
autoplot(kmean_model,new_data1,frame = TRUE)
kmean_model$centers 

# 6.1 Visualization
data_new = data1 %>% mutate(membership = factor(kmean_model$cluster))

data_new %>% group_by(membership) %>% summarise_all(list(avg =mean))

data_new %>% group_by(membership) %>% summarise_all(list(Std =sd))


ggplot(data_new, aes(x = membership, y = Fresh, fill = membership))+
  geom_boxplot() +
  xlab("Clusters")

ggplot(data_new, aes(x = membership, y = Fresh, fill = membership))+
  geom_boxplot() +
  xlab("Clusters")

ggplot(data_new, aes(x = membership, y = Milk, fill = membership))+
  geom_boxplot() +
  xlab("Clusters")

ggplot(data_new, aes(x = membership, y = Grocery, fill = membership))+
  geom_boxplot() +
  xlab("Clusters")

ggplot(data_new, aes(x = membership, y = Frozen, fill = membership))+
  geom_boxplot() +
  xlab("Clusters")

ggplot(data_new, aes(x = membership, y = Detergents_Paper, fill = membership))+
  geom_boxplot() +
  xlab("Clusters")

ggplot(data_new, aes(x = membership, y = Delicassen, fill = membership))+
  geom_boxplot() +
  xlab("Clusters")
