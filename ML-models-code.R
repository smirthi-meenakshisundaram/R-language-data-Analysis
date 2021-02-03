#importing the dataset
dataset=read.csv('ks-projects-201612.csv')

#changing the class of required columns into numeric from chr
str(dataset)
dataset$goal <- as.numeric(dataset$goal)
dataset$backers <- as.numeric(dataset$backers)
dataset$pledged <- as.numeric(dataset$pledged)
str(dataset)



#dropping unwanted columns and take the copy the required subset of the data into the dataset used for logistic regression
lr_dataset=subset(dataset, select=-c(ID, name, category, main_category,deadline,launched,usd.pledged, X, X.1, X.2, X.3, country, currency))

#omitting rows with NA values
lr_dataset <- na.omit(lr_dataset)

# listing unique values in state column
unique(lr_dataset[c("state")])
#removing unwanted or mistaken values in the state column and taking only valid rows
lr_dataset<- subset(lr_dataset, state =="failed" | state=="canceled" | state == "successful" | state =="live" | state=="undefined" | state=="suspended")
unique(lr_dataset[c("state")])


#encoding categorical values of state column into numerical values
lr_dataset$state = factor(lr_dataset$state, levels= c('successful', 'failed', 'canceled', 'live', 'undefined', 'suspended'), labels= c(0,1,1,1,1,1))
unique(lr_dataset[c("state")])


unique(dataset[c("currency")])
dataset<- subset(dataset, currency =="GBP" | currency=="USD" | currency == "CAD" | currency =="NOK" | currency=="AUD" | currency=="EUR" | currency=="MXN" | currency=="SEK" | currency=="NZD" | currency=="CHF" | currency=="DKK" | currency=="HKD")
unique(dataset[c("currency")])
#dataset$backers=ifelse(is.na(dataset$backers),ave(dataset$backers, FUN= function(x) mean(x,na.rm=TRUE)),dataset$backers)

#intalling the package used for splitting the dataset
install.packages('caTools')
library(caTools)
set.seed(150)

#splitting the dataset into trainging 80% and remaining test dataset
split =sample.split(lr_dataset$state, SplitRatio = 0.8)
lr_training_set=subset(lr_dataset, split==TRUE)
lr_test_set=subset(lr_dataset, split=FALSE)

#loop for Feature Scaling of numeric columns in training_data
performScaling <- TRUE  # Turn it on/off for experimentation.

if (performScaling) {
  
  # Loop over each column.
  for (colName in names(lr_training_set)) {
    
    # Check if the column contains numeric data.
    if(class(lr_training_set[,colName]) == 'integer' | class(lr_training_set[,colName]) == 'numeric') {
      
      # Scale this column (scale() function applies z-scaling).
      lr_training_set[,colName] <- scale(lr_training_set[,colName])
    }
  }
}

#loop for Feature Scaling of numeric columns in test_data

if (performScaling) {
  
  # Loop over each column.
  for (colName in names(lr_test_set)) {
    
    # Check if the column contains numeric data.
    if(class(lr_test_set[,colName]) == 'integer' | class(lr_test_set[,colName]) == 'numeric') {
      
      # Scale this column (scale() function applies z-scaling).
      lr_test_set[,colName] <- scale(lr_test_set[,colName])
    }
  }
}

#lr_training_set =lr_training_set[,2:5]
#lr_test_set =lr_test_set[,2:5]

#Fitting the logistic regression to the training set
classifier =glm(formula=state ~ ., family =binomial, data=lr_training_set, na.action = na.pass )

lr_test_set
lr_test_set[3]
unique(lr_test_set[3])

#Predicting the test set results
prob_pred =predict(classifier, type='response', newdata =lr_test_set[-3] )

#probability results
prob_pred
min(prob_pred)
max(prob_pred)
#converting the propabiltiy results to 0 to 1
#y_pred =ifelse(prob_pred < 0.167, 0, ifelse (0.167<=prob_pred < 0.33, 1,ifelse(0.33<=prob_pred<0.5,2,ifelse(0.5<= prob_pred< 0.667, 3,ifelse(0.667<= prob_pred<0.83,4,5)))   )  )
y_pred <- ifelse(prob_pred<0.5,0,1)
unique(y_pred)
unique(lr_test_set[,3])
#making the confusion matrix
cm =table(y_pred,lr_test_set[,3])
cm
install.packages('e1071')
library(e1071) 


result1 <- confusionMatrix(cm)
result1

#Random forest classification

#dropping unwanted columns and take the copy the required subset of the data into the dataset used for Random Forest
RF_dataset=subset(dataset, select=-c(ID, name, category, main_category,deadline,launched,usd.pledged, X, X.1, X.2, X.3, country, currency))

#omitting rows with NA values
RF_dataset <- na.omit(RF_dataset)

#encoding categorical values of state column into numerical values
RF_dataset$state = factor(RF_dataset$state, levels= c('successful', 'failed', 'canceled', 'live', 'undefined', 'suspended'), labels= c(0,1,1,1,1,1))
unique(RF_dataset[c("state")])

#intalling the package used for splitting the dataset
library(caTools)
set.seed(150)

#splitting the dataset into trainging 80% and remaining test dataset
split =sample.split(RF_dataset$state, SplitRatio = 0.8)
RF_training_set=subset(RF_dataset, split==TRUE)
RF_test_set=subset(RF_dataset, split=FALSE)

#loop for Feature Scaling of numeric columns in training_data
performScaling <- TRUE  # Turn it on/off for experimentation.

if (performScaling) {
  
  # Loop over each column.
  for (colName in names(RF_training_set)) {
    
    # Check if the column contains numeric data.
    if(class(RF_training_set[,colName]) == 'integer' | class(RF_training_set[,colName]) == 'numeric') {
      
      # Scale this column (scale() function applies z-scaling).
      RF_training_set[,colName] <- scale(RF_training_set[,colName])
    }
  }
}

#loop for Feature Scaling of numeric columns in test_data

if (performScaling) {
  
  # Loop over each column.
  for (colName in names(RF_test_set)) {
    
    # Check if the column contains numeric data.
    if(class(RF_test_set[,colName]) == 'integer' | class(RF_test_set[,colName]) == 'numeric') {
      
      # Scale this column (scale() function applies z-scaling).
      RF_test_set[,colName] <- scale(RF_test_set[,colName])
    }
  }
}

#Fitting random forest classification into the training set
#install.packages('randomForest')
library(randomForest)

RF_classifier = randomForest(x= RF_training_set[-3], y=RF_training_set$state, ntree= 100 )

RF_prob_pred =predict(RF_classifier, type='response', newdata =RF_test_set[-3] )
unique(RF_prob_pred)

RF_cm =table(RF_prob_pred,RF_test_set[,3])
RF_cm
install.packages('caret')
library(caret) 
install.packages('e1071')
library(e1071) 

result <- confusionMatrix(RF_cm)
result

#install.packages('rpart')
library(rpart)

dt_classifier =rpart(formula =state ~ ., data=RF_training_set)
dt_ypred =predict(dt_classifier, newdata= RF_test_set[-3], type= 'class')

dt_cm =table(RF_test_set[,3], dt_ypred)
dt_cm
library(caret)
library(e1071)

result2 <- confusionMatrix(dt_cm)
result2

#Team Member: Sai Krishna Mannava (801136361)
#Team Member:Kumar Mani Chandra Yelisetty (801168244)
#Team Member:Smirthi Meenakshisundaram (801129947)

