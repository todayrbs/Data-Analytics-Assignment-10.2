
#------------------------Assignment 20 -----------------------------

# Perform the below given activities:
# a. Create classification model using different random forest models
# b. Verify model goodness of fit
# c. Apply all the model validation techniques
# d. Make conclusions
# e. Plot importance of variables

#--------------------------------------------------------------------

# import data set
data_set <- read.csv("E:/Data Analytics with RET/Assignment/Dataset/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv")
View(data_set)

# remove irrelevant collumns viz. name, cvtd_timestamp, new_window
data <- data_set[,-c(1,4,5)]
View(data)
str(data)

sum(is.na(data))  # there are no missing values

# spliting the data set for train and test

library(caTools)
set.seed(123)
split = sample.split(data$classe, SplitRatio = 0.7) 

train = subset(data, split == TRUE)            # train data
test = subset(data, split == FALSE)            # test data

dim(train)
dim(test)
# a. Create classification model using different random forest models

library(tree); library(rpart); library(caret); library(C50); library(randomForest)
library(adabag); library(gbm)


# 1
train_control <- trainControl(method = "cv", number = 10)
cvmodel1 <- train(classe ~ ., data = train, trControl = train_control, method = "rf") 
cvpred1 <- predict(cvmodel1, test)                        # make prediction
cvconf1 <- confusionMatrix(test$classe, cvpred1)          # confusion matrix
cvconf1$overall[1]                                        # accuracy

# default
set.seed(123)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
rf_default <- train(classe ~ ., data = train, trControl = train_control, method = "rf",
                    metric = 'Accuracy', tuneGrid = expand.grid(.mtry = sqrt(ncol(train)))) 
pred_rf_default <- predict(rf_default, test)                            # make prediction
conf_rf_default <- confusionMatrix(test$classe, pred_rf_default)        # confusion matrix
conf_rf_default$overall[1]                                              # accuracy
varImp(rf_default)                                                      # var importance - 20

# random search for parameters
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = 'random')
rf_random <- train(classe ~ ., data = train, trControl = train_control, method = "rf",
                   metric = 'Accuracy', tuneLength = 15) 
pred_rf_random <- predict(rf_random, test)                            # make prediction
conf_rf_random <- confusionMatrix(test$classe, pred_rf_random)        # confusion matrix
conf_rf_random$overall[1]                                             # accuracy
varImp(rf_random)                                                     # var importance - 20

# Grid Search
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = 'grid')
rf_grid <- train(classe ~ ., data = train, trControl = train_control, method = "rf",
                 metric = 'Accuracy', tuneGrid = expand.grid(.mtry=c(1:15))) 
pred_rf_grid <- predict(rf_grid, test)                            # make prediction
conf_rf_grid <- confusionMatrix(test$classe, pred_rf_grid)        # confusion matrix
conf_rf_grid$overall[1]                                           # accuracy
varImp(rf_grid)                                                   # var importance - 20


# Goodness of Fit
chisq.test(table(test$classe), prop.table(table(cvpred1)))            # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_rf_default)))    # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_rf_random)))     # pv = 0.2202
chisq.test(table(test$classe), prop.table(table(pred_rf_grid)))       # pv = 0.2202


# Problem was to predict how well the activity is performed
# The target variable is the 5 classe; 1 accurate and 4 type of error 
# occured during the activity

# error (target) detection was done by classifying an 
# execution to one of the mistake classes

# we could detect mistakes fairly accurately

# Gradient bossting model is most accurate with less number of predictors 
# Model is good fit and the Accuracy is 1

plot <- plot(conf_rf_grid$table, col = topo.colors(6))

# -------------------------------------------------------------------------------------



