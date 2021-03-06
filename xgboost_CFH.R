setwd("C:/Users/Pooja/Desktop/Fall 16/Machine learning/Project/Oct-25")
trainuse <- read.csv("C:/Users/Pooja/Desktop/Fall 16/Machine learning/Project/Oct-25/statedata_touse.csv")
View(trainuse)
class(trainuse$In.Permanent.Housing)
y_train <- as.numeric(trainuse$In.Permanent.Housing)
head(y_train)
nlevels(y_train)
View(y_train)
y_train <- as.numeric(trainuse$In.Permanent.Housing)-1
View(y_train)
label_trainoutcome <- data.frame(trainuse$In.Permanent.Housing, y_train)
View(label_trainoutcome)
attach(trainuse)
xgb_train <- xgb.DMatrix(model.matrix(~Domestic.violence.victim.survivor + F_Employment_Status + Highest.Level.of.Education.Attained + Covered.by.Health.Insurance + Does.the.client.have.a.disabling.condition. + If.yes.for.Domestic.violence.victim.survivor..when.experience.occurred + Length.of.Stay.in.Previous.Place + Total.number.of.months.continuously.homeless.immediately.prior.to.project.entry + Number.of.Times.the.Client.has.been.Homeless.in.the.Past.Three.Years + Primary.Reason.for.Homelessness + Is.Client.Chronically.Homeless. + Is.Juvenile.Parent. + Non.cash.benefit.from.any.source + Housing.Status.prior.to.entry + Prior.Living.Situation..Immediately.Prior.to.Entry. + NewColumn.U.S..Military.Veteran., data = trainuse), label = y_train, missing = NA)
xgb_model <- xgboost(xgb_train, y_train, nfold = 5, nrounds = 25, objective = "binary.logistic", early.stop.round = 3)
xgb_model <- xgboost(xgb_train, y_train, nfold = 5, nrounds = 25, objective = "binary.logistic", early.stop.round = 3, maximize = FALSE)
xgb_model <- xgboost(xgb_train, y_train, nfold = 5, nrounds = 25, objective = "binary.logistic")
xgb_model <- xgb.cv(data = xgb_train, label = y_train, nfold = 5, nrounds = 25, objective = "binary:logistic")
xgb_model <- xgb.cv(data = xgb_train, label = y_train, nfold = 5, nrounds = 25, objective = "binary:logistic", early.stop.round = 3, maximize = FALSE)
xgb_model <- xgb.cv(data = xgb_train, label = y_train, nfold = 4, nrounds = 20, objective = "binary:logistic", early.stop.round = 3, maximize = FALSE)
xgb_model <- xgb.cv(data = xgb_train, label = y_train, nfold = 4, nrounds = 17, objective = "binary:logistic")
xgb_model1 <- xgb.cv(data = xgb_train, label = y_train, nfold = 4, nrounds = 20, objective = "binary:logistic", early.stop.round = 3, maximize = FALSE)
names <- dimnames(xgb_train)[[2]]
importance_matrix <- xgb.importance(names, model = xgb_model1)
xgb_model1 = xgb.cv(data = xgb_train, label = y_train, nfold = 4, nrounds = 20, objective = "binary:logistic", early.stop.round = 3, maximize = FALSE)
importance_matrix <- xgb.importance(names, model = xgb_model1)
importance_matrix <- xgb.importance(xgb_train$data@Dimnames[[2]], model = xgb_model1)
xgb_model1 = xgboost(data = xgb_train, label = y_train, missing = NA, nfold = 4, nrounds = 20, objective = "binary:logistic", early.stop.round = 3, maximize = FALSE)
numberofclasses <- max(y_train)+1
param <- list("objective" = "binary:logistic", "eval_metric"="mlogloss", num_class = numberofclasses)
bst = xgboost(params = param, data = xgb_train)
bst.cv = xgb.cv(params = param, data = xgb_train, label = y_train, nfold = 5, nrounds = 25)
bst = xgboost (params = param, data = xgb_train, label = y_train, nrounds = 25)
names <- dimnames(xgb_train)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
xgb.plot.importance(importance_matrix[1:10,])
??ckmeans.1d
??Ckmeans.1d.dp
install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
xgb.plot.importance(importance_matrix[1:10,])
importance_matrix <- xgb.importance(xgb_train$data@Dimnames[[2]], model = bst)
xgb.plot.importance(importance_matrix)
> bst = xgboost (params = param, data = xgb_train, label = y_train, nrounds = 25, objective = "binary:logistic")
> bst1 = xgboost (params = param, data = xgb_train, label = y_train, max.depth = 2,
                  eta = 1, nthread = 2, nround = 25,objective = "binary:logistic")
> bst1 = xgboost (params = param, data = xgb_train, label = y_train, max.depth = 2, eta = 1, nthread = 2, nround = 25,objective = "binary:logistic")
> bst1 <- xgboost (params = param, data = xgb_train, label = y_train, max.depth = 2, eta = 1, nthread = 2, nround = 25,objective = "binary:logistic")
> bst1 <- xgboost (params = param, data = xgb_train, label = y_train, max.depth = 2, eta = 1, nthread = 2, nround = 25,objective = 'binary:logistic')
> bst1 <- xgboost (params = param, data = xgb_train, label = y_train, max.depth = 2, eta = 1, nthread = 2, nround = 25,objective = binary:logistic)
View(importance_matrix)
View(xgb_train)
View(data.frame(xgb_train))
colnames(xgb_train)
z <- model.matrix(~Domestic.violence.victim.survivor + F_Employment_Status + Highest.Level.of.Education.Attained + Covered.by.Health.Insurance + Does.the.client.have.a.disabling.condition. + If.yes.for.Domestic.violence.victim.survivor..when.experience.occurred + Length.of.Stay.in.Previous.Place + Total.number.of.months.continuously.homeless.immediately.prior.to.project.entry + Number.of.Times.the.Client.has.been.Homeless.in.the.Past.Three.Years + Primary.Reason.for.Homelessness + Is.Client.Chronically.Homeless. + Is.Juvenile.Parent. + Non.cash.benefit.from.any.source + Housing.Status.prior.to.entry + Prior.Living.Situation..Immediately.Prior.to.Entry. + NewColumn.U.S..Military.Veteran., data = trainuse)
View(z)
str(xgb_train)
summary(bst)
