# this script contains the analyses with kernel SVM

rm(list = ls())

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("doParallel")) install.packages("doParallel")
if (!require("e1071")) install.packages("e1071")
if (!require("modelr")) install.packages("modelr")
if (!require("kernlab")) install.packages("kernlab")
if (!require("pROC")) install.packages("pROC")
if (!require("kerndwd")) install.packages("kerndwd")
if (!require("gmodels")) install.packages("gmodels")

library(caret)
library(e1071)
library(tidyverse)
library(doParallel) 
library(kernlab)
library(kerndwd)

set.seed(42)

# read data
data <- readRDS('data/tidy_data/data_model_merged.rds')
data <- drop_na(data)

# watch unemployment rate
data$unemployed_long %>% table

# drop irrelevant vars
data_model <- data %>% select(-c(year, persnr, unemployment_duration))

# prepare data for feature selection
data_x <- model.matrix(unemployed_long ~ .,
                       data = data_model)[, -1] %>% scale(T, T)
data_x <- data_x[, -which(colnames(data_x) == 'marital_status6')]
data_y <- data_model$unemployed_long
data_y <- make.names(data_y)

# define training parameter
trainControl <- trainControl(method = 'repeatedcv',
                             number = 5,
                             repeats = 5,
                             search = 'random',
                             summaryFunction = twoClassSummary, 
                             classProbs = T,
                             savePredictions = T)

# define pre process options
preProc <-c('center', 'scale')

################################################################################
## feature selection
####################
# originally not necessary to set a seed here, only made for making it
# possible to execute the code only in this subsection 
set.seed(42)

# speed things up
# be careful if CPU is currently busy
cl <- makeCluster(parallel::detectCores() / 2)
registerDoParallel(cl)

# train model
fs_svm <- train(x = data_x,
                y = data_y,
                method = 'svmRadial',
                trControl = trainControl,
                preProc = preProc,
                tuneLength = 100)

stopCluster(cl)

# watch inmportances and save
svmImp <- varImp(fs_svm, scale = FALSE)

fs_svm$importance <- svmImp

# watch results
fs_svm
fs_svm$bestTune

# extract best tuning parameters
idx <- 
  fs_svm$pred$sigma == fs_svm$bestTune$sigma &
  fs_svm$pred$C == fs_svm$bestTune$C

roc <- roc(fs_svm$pred$obs[idx], fs_svm$pred$X0[idx])
auc(roc)

fs_svm$roc <- roc

# save feature selection model
saveRDS(fs_svm, 'data/fitted_models/svm_fs.rds')
#plot(roc)

# select relevant vars with minimum importance .52
# thresshold selected through eye-balling and educated guess
relevant_vars <- svmImp$importance %>% filter(X0 >= .52) %>% rownames() %>%
  str_remove('[0-9]*[-]?[0-9]*[+]?$') %>% unique()

relevant_vars <- relevant_vars %>% append(c('year',
                                            'persnr',
                                            'unemployment_duration',
                                            'unemployed_long'))


# read data again
data <- readRDS('data/tidy_data/data_model_merged.rds')

# watch NA count
na_count <- data.frame('n' = is.na(data) %>% apply(2, sum))

# select selected variables according to feature selection
data <- data %>% select_at(relevant_vars)
data <- drop_na(data)

data$unemployed_long %>% table()

# save data for svm final mdoel with relevant vars
saveRDS(data, 'data/tidy_data/data_fs_svm.rds')

################################################################################
## CV Model Parameters
#######################

# remove irrelevant vars
data_model <- data %>% select(-c(year, persnr, unemployment_duration))

# split into train and test data
percent <- .8
df_part <- modelr::resample_partition(data_model,
                                      c(train = percent, test = 1 - percent))
# convert list to data frames
data_train <- as.data.frame(df_part$train) # training sample
data_test <- as.data.frame(df_part$test) # test sample

# prepare feature matrices
data_train_x <- model.matrix(unemployed_long ~ ., data_train)[, -1]
data_train_y <- data_train$unemployed_long %>% as.factor()

data_test_x <- model.matrix(unemployed_long ~ ., data_test)[, -1]
data_test_y <- data_test$unemployed_long

data_train_y <- make.names(data_train_y)

# define training parameters
trainControl <- trainControl(method = 'repeatedcv',
                             number = 5,
                             repeats = 5,
                             search = 'random',
                             summaryFunction = twoClassSummary, 
                             classProbs = T,
                             savePredictions = T)

# define pre process options
preProc <-c('center', 'scale')

# set seed vor individual execution of this subsection
set.seed(42)

# speed things up
# be careful if your CPU is currently busy
cl <- makeCluster(parallel::detectCores() / 2)
registerDoParallel(cl)

param_search_svm <- train(x = data_train_x,
                          y = data_train_y,
                          method = 'svmRadial',
                          trControl = trainControl,
                          preProc = preProc,
                          tuneLength = 200)

stopCluster(cl)

# watch model outcome
param_search_svm
param_search_svm$bestTune

# extract best tune
idx <- 
  param_search_svm$pred$sigma == param_search_svm$bestTune$sigma &
  param_search_svm$pred$C == param_search_svm$bestTune$C

# create ROC
roc <- roc(param_search_svm$pred$obs[idx], param_search_svm$pred$X0[idx])
auc(roc)

param_search_svm$roc <- roc

# save cross validation results
saveRDS(param_search_svm, 'data/fitted_models/svm_cv.rds')


################################################################################
## Final Model
##############

# train final model
model_svm <- ksvm(as.factor(unemployed_long) ~ .,
                  data = data_train,
                  kernel = 'rbfdot',
                  kpar = list(sigma = param_search_svm$bestTune$sigma),
                  C = param_search_svm$bestTune$C,
                  cross = 5,
                  prob.model = T)

# watch errors / risks
model_svm@error
model_svm@cross

# create predictions
prediction_train <- data.frame(actual = data_train$unemployed_long,
                               latent = predict(model_svm, data_train,
                               type = 'probabilities')[, 2],
                               predicted = predict(model_svm, data_train,
                                                   type = 'response'))
prediction_test <- data.frame(actual = data_test$unemployed_long,
                              latent = predict(model_svm, data_test,
                              type = 'probabilities')[, 2],
                              predicted = predict(model_svm, data_test,
                                                  type = 'response'))

# calculate risks
train_error <- 1 - mean(prediction_train$actual == prediction_train$predicted)
train_error

test_error <- 1 - mean(prediction_test$actual == prediction_test$predicted)
test_error

svm_predictions <- list()
svm_predictions$prediction_train <- prediction_train
svm_predictions$prediction_test <- prediction_test

# produce ROC
roc <- roc(prediction_train$actual, prediction_train$latent)
auc(roc)

svm_predictions$roc_train <- roc
#plot(roc)

roc <- roc(prediction_test$actual, prediction_test$latent)
auc(roc)

svm_predictions$roc_test <- roc
#plot(roc)

# create confusion matrices
gmodels::CrossTable(prediction_train$actual, prediction_train$predicted,
                    prop.chisq = FALSE,
                    prop.c = FALSE,
                    prop.r = FALSE,
                    dnn = c('actual', 'predicted'))
gmodels::CrossTable(prediction_test$actual, prediction_test$predicted,
                    prop.chisq = FALSE,
                    prop.c = FALSE,
                    prop.r = FALSE,
                    dnn = c('actual', 'predicted'))

# save model and predictions
saveRDS(model_svm, 'data/fitted_models/svm.rds')
saveRDS(svm_predictions, 'data/fitted_models/svm_predictions.rds')


