library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(nortest)
library(randomForest)
library(tidyverse)
require(gridExtra)
set.seed(2023)

data <- read.csv("G:/Il mio Drive/MAGISTRALE/Data Analysis/Project/Clean_Dataset.csv")
str(data)
data$X <- NULL

#Checking for rows with missing values
nrow(data)
data = na.omit(data)
nrow(data)
#no missing values

#checking for duplicates
sum(duplicated(data) == TRUE)

#Changing wrong airlines names
data$airline <- gsub("Air_India", "Air India", data$airline)
data$airline <- gsub("GO_FIRST", "Go First", data$airline)
data$airline <- gsub("AirAsia", "Air Asia", data$airline)

head(data)



#EDA

attach(data)
#Creating frequency pie plot
par(mfrow=c(1,2))
perc <- round(as.matrix(table(data$source_city))[1:length(table(data$source_city))]/nrow(data), 4)*100
name <- names(table(data$source_city))
name <- paste(name," (", perc, "%)", sep="")
pie(perc, labels = name, col=heat.colors(length(table(data$source_city))), main = "Source cities frequencies")

#Creating frequency pie plot
perc <- round(as.matrix(table(data$destination_city))[1:length(table(data$destination_city))]/nrow(data), 4)*100
name <- names(table(data$destination_city))
name <- paste(name," (", perc, "%)", sep="")
pie(perc, labels = name, col=heat.colors(length(table(data$destination_city))), main = "Destination cities frequencies")

#Creating frequency pie plot
par(mfrow=c(1,1))
perc <- round(as.matrix(table(data$airline))[1:length(table(data$airline))]/nrow(data), 2)*100
name <- names(table(data$airline))
name <- paste(name," (", perc, "%)", sep="")
pie(perc, labels = name, col=heat.colors(length(table(data$airline))), main = "Airlines frequencies")

#Creating frequency pie plot
par(mfrow=c(1,2))
perc <- round(as.matrix(table(data$departure_time))[1:length(table(data$departure_time))]/nrow(data), 2)*100
name <- names(table(data$departure_time))
name <- paste(name," (", perc, "%)", sep="")
pie(perc, labels = name, col=heat.colors(length(table(data$departure_time))), main = "Departure time frequencies")

#Creating frequency pie plot
perc <- round(as.matrix(table(data$arrival_time))[1:length(table(data$arrival_time))]/nrow(data), 2)*100
name <- names(table(data$arrival_time))
name <- paste(name," (", perc, "%)", sep="")
pie(perc, labels = name, col=heat.colors(length(table(data$arrival_time))), main = "Arrival time frequencies")

#Creating frequency pie plot
par(mfrow=c(1,1))
perc <- round(as.matrix(table(data$class))[1:length(table(data$class))]/nrow(data), 2)*100
name <- names(table(data$class))
name <- paste(name," (", perc, "%)", sep="")
pie(perc, labels = name, col=heat.colors(length(table(data$class))), main = "Class frequencies")





#visualize numerical attributes
#duration
p1 <- ggplot(data, aes(x=duration)) + 
  geom_boxplot(fill="#FF0000", alpha=0.2) +
  labs(x = "Duration", title = "Boxplot of duration variable")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  coord_flip()
p2 <- ggplot(data, aes(x=duration)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.9,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + 
  labs(x = "Duration", y = "Density", title = "Distribution of duration variable")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
grid.arrange(p2, p1, ncol=2)


ggplot(data, aes(x = duration, y = stops, fill = stops)) + 
  geom_boxplot(fill="#FF6666", alpha=0.7) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Boxplot of Duration by Number of Stops", x = "Duration", y = "Number of stops") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



#days left
p1 <- ggplot(data, aes(x=days_left)) + 
  geom_boxplot(fill="#FF0000", alpha=0.2) +
  labs(x = "Days left", title = "Boxplot of days left variable")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  coord_flip()
p2 <- ggplot(data, aes(x=days_left)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=0.9,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + 
  labs(x = "Days left", y = "Density", title = "Distribution of days left variable")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
grid.arrange(p2, p1, ncol=2)


#price (standardized because otherwise it would take too much time)
p1 <- ggplot(data, aes(x=(price-mean(price))/sd(price))) + 
  geom_boxplot(fill="#FF0000", alpha=0.2) +
  labs(x = "Price", title = "Boxplot of price variable")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  coord_flip()
p2 <- ggplot(data, aes(x=(price-mean(price))/sd(price))) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(x = "Price", y = "Density", title = "Distribution of price variable")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))
grid.arrange(p2, p1, ncol=2)


#INTERESTING RESEARCH QUESTIONS
#Does price vary with Airlines?
input <- aggregate(price ~ airline, data, mean)
ggplot(input, aes(x = price, y = airline, fill = price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FF9999", high = "#FF5555") +
  labs(x = "Average price", y = "Airline", title = "Average price of each airline") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#comment also by looking at the airlines frequency pie chart

#mean prices according to class and airline
input <- aggregate(price ~ class + airline, data, mean)
ggplot(input, aes(x = airline, y = price, fill = class)) +
  geom_bar(stat = "identity", position="dodge", width=0.7) +
  labs(x = "Class", y = "Price", fill= "Class", title = "Average price of each airline according to class") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#from this plot we note also that not all companies offer business class
#only vistara and air india offer business class


#Does the price vary according to the number of stops?
input <- aggregate(price ~ stops, data, mean)
ggplot(input, aes(x = price, y = stops, fill = price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FF9999", high = "#FF5555") +
  labs(x = "Average price", y = "Number of stops", title = "Average price of each airline") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#This is not true in fact:
p1 <- ggplot(data[data$class=="Economy",], aes(x = (price-mean(price))/sd(price) , y = stops, fill = stops)) + 
  geom_boxplot(fill="#FF0000", alpha=0.7) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Price boxplots for Economy by number of Stops", x = "Standardized price", y = "Number of stops") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  coord_flip()
p2 <- ggplot(data[data$class=="Business",], aes(x = (price-mean(price))/sd(price) , y = stops, fill = stops)) + 
  geom_boxplot(fill="#FF0000", alpha=0.7) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Price boxplots for Business by Number of Stops", x = "Standardized price", y = "Number of stops") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  coord_flip()
grid.arrange(p2, p1, ncol=2)

#How is the price affected when tickets are bought in just 1 or 2 days before departure?
input <- aggregate(price ~ days_left, data, mean)
ggplot(input, aes(x = days_left, y = price, fill = price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FF9999", high = "#FF5555") +
  labs(x = "Days left", y = "Price", title = "Average price for days left until departure") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(limits=seq(1,49))


input <- aggregate(price ~ class + days_left, data, mean)
ggplot(input, aes(x = days_left, y = price, fill = class)) + 
  geom_bar(stat="identity", position="dodge", width=0.7) +
  labs(x = "Days left", y = "Price", fill="Class", title = "Average price for days left according to class") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(limits=seq(1,49))
#we expect that the two plots have substantially the same shape but this is not the case,
#especially in the first day bar. This is due to the difference in percentages of Business 
#and Economy ticket availability between one day left until departure and the others.
#In particular the percentages for one day left are 86% Economy and 14% Business tickets, while
#the ones for two days left are 65% Economy and 25% Business tickets available.
#The percentage of Economy tickets is much higher when there is only one day left
#than when there are two or more days left until departure. This implies that if we compute the mean
#of the ticket's price without looking at the class in the first day the result is much lower than 
#if we computed it for the other days.
#The conclusion is that, in any case, buying the ticket (Economy or Business) one day left until departure
#is not convenient: the first plot is useful to get a preliminary idea but not essential to take decisions.

#in fact:
x <- aggregate(class ~ days_left, data, length)
total_tikets <- x$class #total tickets available for each day left until departure

df_business <- data[data$class == "Business",]
x_business <- aggregate(df_business$class ~ df_business$days_left, data, length)
perc_business <- x_business$`df_business$class`/ x$class

df_economy <- data[data$class == "Economy",]
x_economy <- aggregate(df_economy$class ~ df_economy$days_left, data, length)
perc_economy <- x_economy$`df_economy$class`/ x$class

df <- cbind(x,perc_business,perc_economy)
names(df)[2] <- "total tickets"

ggplot(df, aes(x = days_left, y = perc_economy)) +
  geom_bar(stat = "identity", color="#FF9999") +
  labs(x = "Days left", y = "Economy ticket percentage availability", title = "Percentages of Economy tickets available according to days left") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#Since we are working with percentages we show only the plot for the Economy class tickets
#We note that there is a different behaviour in terms of availability of tickets when you book one with
#only one day left until departure


#Does ticket price change based on the departure time and arrival time?
input <- aggregate(price ~ departure_time, data, mean)
p1 <- ggplot(input, aes(x = departure_time, y = price, fill = price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FF9999", high = "#FF5555") +
  labs(x = "Departure time", y = "Price", title = "Average price according to departure times") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

input <- aggregate(price ~ arrival_time, data, mean)
p2 <- ggplot(input, aes(x = arrival_time, y = price, fill = price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FF9999", high = "#FF5555") +
  labs(x = "Arrival time", y = "Price", title = "Average price according to arrival times") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(p1, p2)



#How the price changes with change in Source and Destination?
input <- aggregate(price ~ source_city, data, mean)
p1 <- ggplot(input, aes(x = source_city, y = price, fill = price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FF9999", high = "#FF5555") +
  labs(x = "Source city", y = "Price", title = "Average price according to source city") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

input <- aggregate(price ~ destination_city, data, mean)
p2 <- ggplot(input, aes(x = destination_city, y = price, fill = price)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#FF9999", high = "#FF5555") +
  labs(x = "Destination city", y = "Price", title = "Average price according to destination city") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(p1, p2)

#mean price from every source city to destination city
input <- aggregate(price ~ destination_city + source_city, data, mean)
ggplot(input, aes(x = source_city, y = price, fill = destination_city)) + 
  geom_bar(stat="identity", position="dodge", width=0.7) +
  labs(x = "Source city", y = "Price", fill="Destination city", title = "Average price between source city and destination cities") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



#How does the ticket price vary between Economy and Business class?
input <- aggregate(price ~ class, data, mean)
ggplot(input, aes(x = class, y = price, fill = price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FF9999", high = "#FF5555") +
  labs(x = "Class", y = "Price", title = "Average price according to class") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



#CORRELATION BETWEEN NUMERICAL VARIABLES
par(mfrow=c(1,1))
corrplot.mixed(cor(data[,9:11]))
#no interesting correlations


#PRE-PROCESSING
df = data
df$flight <- NULL
head(df)

df$airline <- as.factor(df$airline)
df$source_city <- as.factor(df$source_city)
df$departure_time <- as.factor(df$departure_time)
df$stops <- as.factor(df$stops)
df$arrival_time <- as.factor(df$arrival_time)
df$destination_city <- as.factor(df$destination_city)
df$class <- as.factor(df$class)
str(df)

#Create dummies for each nominal feature
dummy <- dummyVars(" ~ .", data=df)
models_df <- data.frame(predict(dummy, newdata=df))

#Correlations of all features
corrplot(cor(models_df))
# we expect that from the variable importance point of view, the features class and
# stops will have a big impact on the prediction of the future prices 

#Create index for train and test splittig
index <- sample(nrow(models_df), 0.8 * nrow(models_df))

#Split in train and test set for regression
df_train <- models_df[index,]
df_test <- models_df[-index,]
dim(df_train)
dim(df_test)

#Split in train and test for decision tree and random forest with same index
df_train_rf <- df[index,]
df_test_rf <- df[-index,]
dim(df_train_rf)
dim(df_test_rf)


#Scaling data function
#we use min max since the numerical features are always positive
min_max_scale <- function(x, range = c(0, 1)) {
  # Calculate min and max values of x
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  # Scale x to the specified range
  (x - x_min) / (x_max - x_min) * (range[2] - range[1]) + range[1]
}

#Scaling days_left and duration
min_duration = min(df_train$duration)
max_duration = max(df_train$duration)
min_days = min(df_train$days_left)
max_days = max(df_train$days_left)

df_train$days_left <- min_max_scale(df_train$days_left)
df_train$duration <- min_max_scale(df_train$duration)

df_test$days_left <- (df_test$days_left - min_days)/(max_days - min_days)
df_test$duration <- (df_test$duration - min_duration)/(max_duration - min_duration)


# Error measures function
error_measures <- function(y_true, y_pred, model_name = "Tree_Model") {
  r_squared <- cor(y_true, y_pred) ^ 2
  mse <- mean((y_true - y_pred) ^ 2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_true - y_pred))
  
  metrics_df <- data.frame(Model_Name = model_name,
                           R_Squared = r_squared, 
                           MSE = mse, 
                           RMSE = rmse, 
                           MAE = mae)
  return(metrics_df)
}


#MODELS

#Linear Model
m1 <- lm(price ~ ., data=df_train)
summary(m1)
#The NAs derive from the fact that we are working with dummy variables. Therefore if we consider
#one ore more attributes of a certain feature, the other\the last one is ignored since it is trivial.
anova(m1)

m2 <- lm(price ~ . -departure_time.Evening -departure_time.Late_Night -arrival_time.Late_Night, data=df_train)
summary(m2)
anova(m2)
anova(m1,m2)
#We choose m2 as our final general model after the anova for nested models test

#Residual analysis
r1 <- ggplot(data = data.frame(residuals(m2)), aes(x = residuals(m2))) + 
  geom_histogram(binwidth = 1, color = "#FF8888") + 
  xlab("Residuals") + ylab("Frequency") + 
  ggtitle("Residuals histogram for the linear model") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

r2 <- ggplot(data = data.frame(residuals(m2)), aes(sample = residuals(m2))) + 
  stat_qq() + 
  stat_qq_line(colour="#FF6666") + 
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + 
  ggtitle("QQ Plot of Residuals") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(r1, r2)
# check the tails in the qqplot. I want that they come from the same distribution.
# the tails are fatter than what you would see from a true normal. In other words
# those points are much further from the mean than you would expect if the 
# data generating process were actually a normal distribution.
# Also by looking at the histogram we would say that our residuals are more likely to follow a t-stud distribution
# rather than a normal

ad.test(residuals(m2)) #H1: log(L) ~ N
# obviously the normality of the residuals is rejected

predicted_m2 <- predict(m2, newdata = df_test)
err_m1 <- error_measures(df_test$price, predicted_m2, model_name = "Linear regression: all features ")
err_m1


# Since the Vistara airline company is the one that offers most flights among the airlines
# we chose to build 6 different models according to class and number of stops:
#   - 3 model for economy class
#   - 3 for business class

#First of all we need six different train dataframes (one for each model)
df_economy = df[df$class=="Economy", ]
df_business = df[df$class=="Business", ]
df_economy$class <- NULL
df_business$class <- NULL

df_economy_vistara = df_economy[df_economy$airline=="Vistara", ]
df_business_vistara = df_business[df_business$airline=="Vistara", ]
df_economy_vistara$airline <- NULL
df_business_vistara$airline <- NULL

df_business_vistara_zero = df_business_vistara[df_business_vistara$stops=="zero", ]
df_business_vistara_one = df_business_vistara[df_business_vistara$stops=="one", ]
df_business_vistara_two = df_business_vistara[df_business_vistara$stops=="two_or_more", ]
df_economy_vistara_zero = df_economy_vistara[df_economy_vistara$stops=="zero", ]
df_economy_vistara_one = df_economy_vistara[df_economy_vistara$stops=="one", ]
df_economy_vistara_two = df_economy_vistara[df_economy_vistara$stops=="two_or_more", ]
df_business_vistara_zero$stops <- NULL
df_business_vistara_one$stops <- NULL
df_business_vistara_two$stops <- NULL
df_economy_vistara_zero$stops <- NULL
df_economy_vistara_one$stops <- NULL
df_economy_vistara_two$stops <- NULL



###############################
#Model 1
#Economy zero_stops
#splitting in train and test set
index_train <- sample(nrow(df_economy_vistara_zero), 0.8 * nrow(df_economy_vistara_zero))

df_model <- df_economy_vistara_zero[index_train, ]
df_test_model <- df_economy_vistara_zero[-index_train, ]

#Scaling days_left and duration
min_duration = min(df_model$duration)
max_duration = max(df_model$duration)
min_days = min(df_model$days_left)
max_days = max(df_model$days_left)

df_model$days_left <- min_max_scale(df_model$days_left)
df_model$duration <- min_max_scale(df_model$duration)

df_test_model$days_left <- (df_test_model$days_left - min_days)/(max_days - min_days)
df_test_model$duration <- (df_test_model$duration - min_duration)/(max_duration - min_duration)


#Create dummies for each categorical feature
dummy <- dummyVars(" ~ .", data=df_model)
models_df <- data.frame(predict(dummy, newdata=df_model))

dummy <- dummyVars(" ~ .", data=df_test_model)
df_test_model <- data.frame(predict(dummy, newdata=df_test_model))

#Creating model
lm1 <- lm(price ~ ., data=models_df)
summary(lm1)

#Prediction on test set
prediction = predict(lm1, newdata = df_test_model)
true_data = df_test_model$price

err_m2 <- error_measures(true_data, prediction, model_name = "Linear regression: Economy class - zero stops ")
err_m2


#Model 2
#Economy one_stops
#splitting in train and test set
index_train <- sample(nrow(df_economy_vistara_one), 0.8 * nrow(df_economy_vistara_one))

df_model <- df_economy_vistara_one[index_train, ]
df_test_model <- df_economy_vistara_one[-index_train, ]

#Scaling days_left and duration
min_duration = min(df_model$duration)
max_duration = max(df_model$duration)
min_days = min(df_model$days_left)
max_days = max(df_model$days_left)
df_model$days_left <- min_max_scale(df_model$days_left)
df_model$duration <- min_max_scale(df_model$duration)

df_test_model$days_left <- (df_test_model$days_left - min_days)/(max_days - min_days)
df_test_model$duration <- (df_test_model$duration - min_duration)/(max_duration - min_duration)


#Create dummies for each categorical feature
dummy <- dummyVars(" ~ .", data=df_model)
models_df <- data.frame(predict(dummy, newdata=df_model))

dummy <- dummyVars(" ~ .", data=df_test_model)
df_test_model <- data.frame(predict(dummy, newdata=df_test_model))

#Creating model

lm2 <- lm(price ~ ., data=models_df)
summary(lm2)

#Prediction on test set
prediction = predict(lm2, newdata = df_test_model)
true_data = df_test_model$price

err_m3 <- error_measures(true_data, prediction, model_name = "Linear regression: Economy class - one stop ")
err_m3


#Model 3
#Economy two_stops
#splitting in train and test set
index_train <- sample(nrow(df_economy_vistara_two), 0.8 * nrow(df_economy_vistara_two))

df_model <- df_economy_vistara_two[index_train, ]
df_test_model <- df_economy_vistara_two[-index_train, ]

#Scaling days_left and duration
min_duration = min(df_model$duration)
max_duration = max(df_model$duration)
min_days = min(df_model$days_left)
max_days = max(df_model$days_left)
df_model$days_left <- min_max_scale(df_model$days_left)
df_model$duration <- min_max_scale(df_model$duration)

df_test_model$days_left <- (df_test_model$days_left - min_days)/(max_days - min_days)
df_test_model$duration <- (df_test_model$duration - min_duration)/(max_duration - min_duration)


#Create dummies for each categorical feature
dummy <- dummyVars(" ~ .", data=df_model)
models_df <- data.frame(predict(dummy, newdata=df_model))

dummy <- dummyVars(" ~ .", data=df_test_model)
df_test_model <- data.frame(predict(dummy, newdata=df_test_model))

#Creating model

lm3 <- lm(price ~ ., data=models_df)
summary(lm3)

#Prediction on test set
prediction = predict(lm3, newdata = df_test_model)
prediction_vector = as.data.frame(prediction)$prediction
true_data = df_test_model$price

err_m4 <- error_measures(true_data, prediction, model_name = "Linear regression: Economy class - two or more stops ")
err_m4


#Model 4
#Business zero_stops
#splitting in train and test set
index_train <- sample(nrow(df_business_vistara_zero), 0.8 * nrow(df_business_vistara_zero))

df_model <- df_business_vistara_zero[index_train, ]
df_test_model <- df_business_vistara_zero[-index_train, ]

#Scaling days_left and duration
min_duration = min(df_model$duration)
max_duration = max(df_model$duration)
min_days = min(df_model$days_left)
max_days = max(df_model$days_left)
df_model$days_left <- min_max_scale(df_model$days_left)
df_model$duration <- min_max_scale(df_model$duration)

df_test_model$days_left <- (df_test_model$days_left - min_days)/(max_days - min_days)
df_test_model$duration <- (df_test_model$duration - min_duration)/(max_duration - min_duration)


#Create dummies for each categorical feature
dummy <- dummyVars(" ~ .", data=df_model)
models_df <- data.frame(predict(dummy, newdata=df_model))

dummy <- dummyVars(" ~ .", data=df_test_model)
df_test_model <- data.frame(predict(dummy, newdata=df_test_model))

#Creating model

lm4 <- lm(price ~ ., data=models_df)
summary(lm4)

#Prediction on test set
prediction = predict(lm4, newdata = df_test_model)
true_data = df_test_model$price

err_m5 <- error_measures(true_data, prediction, model_name = "Linear regression: Business class - zero stops ")
err_m5

#Model 5
#Business one_stops
#splitting in train and test set
index_train <- sample(nrow(df_business_vistara_one), 0.8 * nrow(df_business_vistara_one))

df_model <- df_business_vistara_one[index_train, ]
df_test_model <- df_business_vistara_one[-index_train, ]

#Scaling days_left and duration
min_duration = min(df_model$duration)
max_duration = max(df_model$duration)
min_days = min(df_model$days_left)
max_days = max(df_model$days_left)
df_model$days_left <- min_max_scale(df_model$days_left)
df_model$duration <- min_max_scale(df_model$duration)

df_test_model$days_left <- (df_test_model$days_left - min_days)/(max_days - min_days)
df_test_model$duration <- (df_test_model$duration - min_duration)/(max_duration - min_duration)


#Create dummies for each categorical feature
dummy <- dummyVars(" ~ .", data=df_model)
models_df <- data.frame(predict(dummy, newdata=df_model))

dummy <- dummyVars(" ~ .", data=df_test_model)
df_test_model <- data.frame(predict(dummy, newdata=df_test_model))

#Creating model

lm5 <- lm(price ~ ., data=models_df)
summary(lm5)

#Prediction on test set
prediction = predict(lm5, newdata = df_test_model)
true_data = df_test_model$price

err_m6 <- error_measures(true_data, prediction, model_name = "Linear regression: Business class - one stop ")
err_m6

#Model 6
#Business two_stops
#splitting in train and test set
index_train <- sample(nrow(df_business_vistara_two), 0.8 * nrow(df_business_vistara_two))

df_model <- df_business_vistara_two[index_train, ]
df_test_model <- df_business_vistara_two[-index_train, ]

#Scaling days_left and duration
min_duration = min(df_model$duration)
max_duration = max(df_model$duration)
min_days = min(df_model$days_left)
max_days = max(df_model$days_left)
df_model$days_left <- min_max_scale(df_model$days_left)
df_model$duration <- min_max_scale(df_model$duration)

df_test_model$days_left <- (df_test_model$days_left - min_days)/(max_days - min_days)
df_test_model$duration <- (df_test_model$duration - min_duration)/(max_duration - min_duration)


#Create dummies for each categorical feature
dummy <- dummyVars(" ~ .", data=df_model)
models_df <- data.frame(predict(dummy, newdata=df_model))

dummy <- dummyVars(" ~ .", data=df_test_model)
df_test_model <- data.frame(predict(dummy, newdata=df_test_model))

#Creating model

lm6 <- lm(price ~ ., data=models_df)
summary(lm6)

#Prediction on test set
prediction = predict(lm6, newdata = df_test_model)
true_data = df_test_model$price

err_m7 <- error_measures(true_data, prediction, model_name = "Linear regression: Business class - two or more stops ")
err_m7



# Decision trees
# General
df_dt <- rpart(price ~ ., data = df_train_rf, method="anova")
summary(df_dt)
rpart.plot(df_dt, extra=101)
prediction_dt <- predict(df_dt, newdata = df_test_rf, method="anova")
err_m8 <- error_measures(df_test_rf$price, prediction_dt, model_name = "Decision tree: all features ")
err_m8
#negative mean error -> the predicted value is less than the true value on average

# Economy class
df_dt_eco <- rpart(price ~ ., data = df_train_rf[df_train_rf$class=="Economy",], method="anova")
summary(df_dt_eco)
rpart.plot(df_dt_eco, extra=101)
prediction_dt <- predict(df_dt_eco, newdata = df_test_rf, method="anova")
err_m9 <- error_measures(df_test_rf$price, prediction_dt, model_name = "Decision tree: Economy class ")
err_m9

# Business class
df_dt_bus <- rpart(price ~ ., data = df_train_rf[df_train_rf$class=="Business",], method="anova")
summary(df_dt_bus)
rpart.plot(df_dt_bus, extra=101)
prediction_dt <- predict(df_dt_bus, newdata = df_test_rf, method="anova")
err_m10 <- error_measures(df_test_rf$price, prediction_dt, model_name = "Decision tree: Business class ")
err_m10


#Random forests
df_rf_eco <- randomForest(price ~ ., data = df_train_rf[df_train_rf$class=="Economy",], ntree = 15)
prediction_rf <- predict(df_rf_eco, df_test_rf)
varImpPlot(df_rf_eco, main = "Feature importance of Economy class random forest")
err_m11 <- error_measures(df_test_rf$price, prediction_rf, model_name = "Random forest: Economy class ")
err_m11

df_rf_bus <- randomForest(price ~ . -class, data = df_train_rf[df_train_rf$class=="Business",], ntree = 15)
prediction_rf <- predict(df_rf_bus, df_test_rf)
varImpPlot(df_rf_bus, main = "Feature importance of Business class random forest")
err_m12 <- error_measures(df_test_rf$price, prediction_rf, model_name = "Random forest: Business class ")
err_m12


final_table <- rbind(err_m1, err_m2, err_m3, err_m4, err_m5, err_m6, err_m7, err_m8, err_m9, err_m10, err_m11, err_m12)
View(final_table)
