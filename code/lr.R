# this script contains analyses with logistic regression

rm(list = ls())
if (!require("gmodels")) install.packages("gmodels")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("modelr")) install.packages("modelr")
if (!require("glmnetcr")) install.packages("glmnetcr")
if (!require("pROC")) install.packages("pROC")
library(tidyverse)
library(glmnetcr)

set.seed(42)

# load and clean data
data <- readRDS('data/tidy_data/data_model_merged.rds')
data <- drop_na(data)

# remove irrelevant vars
data_model <- data %>% select(-c(year, persnr, unemployment_duration))

# prepare data
data_x <- model.matrix(unemployed_long ~ .,
                       data = data_model)[, -1] %>% scale(T, T)
data_x <- data_x[, -which(colnames(data_x) == 'marital_status6')]
data_y <- data_model$unemployed_long

# train model
model_probit_cv <- cv.glmnet(x = data_x,
                             y = data_y,
                             alpha = 1,
                             family = 'binomial')
model_probit_cv
model_probit_cv$lambda.min

model_probit <- glmnet(x = data_x,
                       y = data_y,
                       alpha = 1,
                       family = 'binomial')
model_probit

# select step that lead to the best result
crit <- list()
crit$num <- which(model_probit_cv$lambda.min == model_probit$lambda)
crit$char <- 's' %>% paste(crit$num - 1, sep = '')
model_probit$crit <- crit

# save models
saveRDS(model_probit_cv, 'data/fitted_models/logit_fs1.rds')
saveRDS(model_probit, 'data/fitted_models/logit_fs2.rds')

# choose vars that have been selected at respective step
relevant_vars <- names(nonzero.glmnetcr(model_probit, s = crit$num)$beta) %>%
  str_remove('[0-9]*[-]?[0-9]*[+]?$') %>%
  unique()

# add vars that are necessary for identification
relevant_vars <- relevant_vars %>% append(c('year',
                                            'persnr',
                                            'unemployment_duration',
                                            'unemployed_long'))

# load data again because of droped NAs
data <- readRDS('data/tidy_data/data_model_merged.rds')

# watch number of NAs
na_count <- data.frame('n' = is.na(data) %>% apply(2, sum))

# select relevant vars and clean
data <- data %>% select_at(relevant_vars)
data <- drop_na(data)

data$unemployed_long %>% table()

# save data set used for final logistic regression
saveRDS(data, 'data/tidy_data/data_fs_logistic.rds')

# remove irrelevant vars
data_model <- data %>% select(-c(year, persnr, unemployment_duration))

# prepare data
dv <- data_model$unemployed_long
idx <- sapply(data_model, is.numeric)
data_model[idx] <- lapply(data_model[idx], scale)
data_model$unemployed_long <- dv

# split data in train and test set
percent <- .8
df_part <- modelr::resample_partition(data_model,
                                      c(train = percent, test = 1 - percent))
# convert list to data frames
data_train <- as.data.frame(df_part$train) # training sample
data_test <- as.data.frame(df_part$test) # test sample

# create feature matrices
data_train_x <- model.matrix(unemployed_long ~ ., data_train)[, -1]
data_train_y <- data_train$unemployed_long %>% as.factor()

data_test_x <- model.matrix(unemployed_long ~ ., data_test)[, -1]
data_test_y <- data_test$unemployed_long

data_train_y <- make.names(data_train_y)

# train final model
logit <- glm(unemployed_long ~ ., data = data_train,
             family = binomial(link = 'logit'))
summary(logit)

# create predictions
prediction_train <- data.frame(actual = data_train$unemployed_long,
                               #latent = predict(logit, data_train, 'terms'),
                               latent = predict(logit, data_train, 'response'))
prediction_train$predicted <- ifelse(prediction_train$latent >= .5,
                                     1,
                                     0)

logit$prediction_train <- prediction_train

prediction_test <- data.frame(actual = data_test$unemployed_long,
                              #latent = predict(logit, data_test, 'terms'),
                              latent = predict(logit, data_test, 'response'))
prediction_test$predicted <- ifelse(prediction_test$latent >= .5,
                                    1,
                                    0)

logit$prediction_test <- prediction_test

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

# calculate risks
train_error <- 1 - mean(prediction_train$actual == prediction_train$predicted)
train_error

test_error <- 1 - mean(prediction_test$actual == prediction_test$predicted)
test_error

# create ROCs
roc <- roc(prediction_train$actual, prediction_train$latent)
auc(roc)

logit$roc_train <- roc

roc <- roc(prediction_test$actual, prediction_test$latent)
auc(roc)

logit$roc_test <- roc

# save final model and predictions
saveRDS(logit, 'data/fitted_models/logit.rds')

# simulate cross validation error
set.seed(42)
 
idx <- data_train %>% nrow() %>% sample()

chunks <- split(idx, ceiling(seq_along(idx)/nrow(data_train) %/% 5))[1:5]
errors <- c()


for (i in 1:length(chunks)) {
  prediction <- predict(logit, data_train[chunks[[i]], ])
  # decision criterion
  prediction <- ifelse(prediction >= .5, 1, 0)
  actual <- data_train[chunks[[i]], ] %>% select(unemployed_long)
  error <- (actual == prediction) %>% sum()
  error <- 1 - (error / length(chunks[[i]]))
  errors <- errors %>% append(error)
}

mean_cv_error <- errors %>% mean()
mean_cv_error