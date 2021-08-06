#Installing the required Packages
install.packages("randomForest")
install.packages("fastDummies")
install.packages("e1071")
install.packages("caret")
library(randomForest)
library(fastDummies)
library(caret)
library(e1071)

#Loading the data file
file <- read.csv(file.choose())
View(file)

#Data Manipulation
data <- file
colnames(data)
data$Undergrad <- ifelse(data$Undergrad=='YES',1,0)
data$Urban <- ifelse(data$Urban=='YES',1,0)
data$Taxable.Income <- as.factor(ifelse(data$Taxable.Income<=30000,"Risky","Good"))
data <- fastDummies::dummy_cols(data,select_columns = "Marital.Status")
new_data <- data[-2]

#Normalizing the dataset
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
norm_data <- as.data.frame(lapply(new_data[-2], norm))
norm_data["Taxable.Income"] <- new_data$Taxable.Income

#Splitting dataframe into training and testing dataset
ind <- sample(2,nrow(new_data),replace = T,prob = c(0.8,0.2))
train <- new_data[ind==1,]
test <- new_data[ind==2,]

#Making the Model
model <- randomForest(Taxable.Income ~ .,data = train, na.importance=TRUE ,ntree=500)

#Predicting Training Values
pred_train <- predict(model,train)
#Checking the training accuracy
mean(train$Taxable.Income==pred_train)
confusionMatrix(train$Taxable.Income,pred_train)

#Predicting Testing Values
pred_test <- predict(model,test)
#Checking the testing Accuracy
mean(test$Taxable.Income==pred_test)
confusionMatrix(test$Taxable.Income,pred_test)

#Visualizing the Feature Importance
varImpPlot(model)

#Visualizing the Model formed
plot(model,lwd=2)
legend("topright",colnames(model$err.rate),fill = 1:4,cex=0.8)

help(legend)
