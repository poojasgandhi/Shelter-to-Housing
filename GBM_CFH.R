setwd("C:/Users/Pooja/Desktop/Fall 16/Machine learning/Project/Oct-25")
trainuse <- read.csv("C:/Users/Pooja/Desktop/Fall 16/Machine learning/Project/Oct-25/statedata_touse.csv")
View(trainuse)

#Dividing data into train and test
train = trainuse[1:1600,]
View(train)
test = trainuse[1601:nrow(trainuse),]
View(test)

#Storing label as a seperate frame and as numbers since GBM only deals with numeric y
house_train = train$In.Permanent.Housing
house_train <- as.numeric(house_train)-1

#Removing label from the data to be trained
train1 = select(train, -In.Permanent.Housing)

#Doing similar process of removing label for test
house_test = test$In.Permanent.Housing
test1 = select(test, -In.Permanent.Housing)

#Running GBM model (this was after several iterations of tuning the parameters)
model = gbm.fit(x=train1, y = house_train, distribution = "bernoulli", n.trees = 5000, shrinkage = 0.01, interaction.depth = 1, n.minobsinnode = 20, verbose = TRUE, nTrain = end_train*0.7)

#to find optimum number of trees after which validation error starts increasing
gbm.perf(model)

#predicting the model for train and test data sets
testpred = predict(model, newdata = test1, n.trees = 1270, type = 'response')
trainpred = predict(model, newdata = train1, n.trees = 1270, type = 'response')

#any value greater than 0.5 taken as 1, No otherwise. This is done to see if 0.5 is the right cut off
# Turns out it is. With it the error rate comes to 6.1%, which is acceptable and lower than other iterations and cut offs
trainpred = round(trainpred)
1-sum(abs(house_train-trainpred))/length(trainpred)

#predictions for test are written to a file and compared to actual values. Misclassification rate obtained is 7.7%
write.csv(prediction, file='gbm.pred.csv')

#To get variable importance of the model
summary(model)



