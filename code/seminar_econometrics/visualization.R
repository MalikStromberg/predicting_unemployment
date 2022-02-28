# this script creates analysis-related figures in the thesis

rm(list = ls())

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("pROC")) install.packages("pROC")

library(ggplot2)
library(tidyverse)
library(pROC)

################################################################################
## Coefficients
###############

# load logit results
model_logit <- readRDS('data/fitted_models/logit_fs2.rds')
model_logit_cv <- readRDS('data/fitted_models/logit_fs1.rds')

# extract coefficients
betas <- model_logit$beta %>% as.matrix

# define categories for weights: most important, important, unimportant
selection <- betas[, model_logit$crit$char] %>% abs()
selection_zero <- selection[selection == 0]
selection_nonzero <- selection[selection != 0]
selection_nonzero <-
  selection_nonzero[order(selection_nonzero, decreasing = T)]
betas1 <- betas[names(selection_nonzero[1:8]), ]
betas2 <- betas[names(selection_zero), ]
betas3 <- betas[names(selection_nonzero[9:length(selection_nonzero)]), ]

# extract lambdas
colnames(betas1) <- model_logit$lambda
colnames(betas2) <- model_logit$lambda
colnames(betas3) <- model_logit$lambda

# prepare data for plot
betas_plt1 <- reshape2::melt(betas1)
betas_plt2 <- reshape2::melt(betas2)
betas_plt3 <- reshape2::melt(betas3)

# create plot
p <- ggplot() +
  geom_line(data = betas_plt1, size = 1, show.legend = T,
            aes(x = Var2, y = value, group = Var1, color = Var1)) +
  geom_point(data = betas_plt1, size = 1.5,
             aes(x = Var2, y = value, group = Var1, color = Var1)) +
  geom_line(data = betas_plt2, alpha = .25,
            aes(x = Var2, y = value, group = Var1)) +
  geom_line(data = betas_plt3, alpha = 1,
            aes(x = Var2, y = value, group = Var1)) +
  geom_vline(xintercept = model_logit_cv$lambda.min, color = 'red') +
  geom_hline(yintercept = 0, color = 'black') +
  theme_bw(base_size = 14) +
  ggtitle(expr('Lasso Logistic Regression: Weights Depending on '~lambda)) +
  labs(x = expr(lambda), y = 'Weight',
       color = element_blank()
       ) +
  scale_color_discrete(
                     labels = c(
    worried_about_crime_in_germany = 'worr_crime',
    satisfaction_with_dwelling = 'sat_dwelling',
    current_health = 'health',
    caution_towards_foreigners = 'caution_foreigners',
    find_suitable_position = 'suitable_pos',
    gainfully_employed_interest3 = 'empl_interest: both',
    `amount_of_closed_friends6-10` = 'friends: 6-10',
    marital_status4 = 'divorced',
    german_nationality1 = 'german',
    acceptance_within_two_weeks1 =
      'acceptance',
    would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests1 =
      'state_helpful',
    employment_in_the_future_intended = 'empl_intended',
    `number_of_children_in_hh4+` = 'children: > 3',
    household_net_income = 'hh_income',
    i_want_my_rivals_to_fail = 'state_rivals',
    number_of_children_in_hh3 = 'children: 3',
    i_deserve_to_be_seen_as_a_great_personality = 'state_personality'
                      )) +
theme(plot.title = element_text(hjust = .5, size = 16),
      legend.title.align = .5,
      legend.position = 'bottom') +
guides(color = guide_legend(nrow = 2, byrow = T))

# save plot
ggsave('figures/lr_coefficients_lambda.png',
       p,
       width = 8,
       height = 6,
       dpi = 600)

###############################################################################
## ROC-Curve
############

# load data
model_logit <- readRDS('data/fitted_models/logit.rds')
model_svm <- readRDS('data/fitted_models/svm_predictions.rds')

# prepare plot data
plot_data <- list(model_logit$roc_test,
                  model_svm$roc_test)

# extract AUC values
label_lr <- paste('AUC:', round(model_logit$roc_test$auc, 3), sep = ' ')
label_svm <- paste('AUC:', round(model_svm$roc_test$auc, 3), sep = ' ')


p <- ggroc(plot_data, legacy.axes = T) +
  geom_abline(intercept = 0, slope = 1,
              color = "darkgrey", linetype = "dashed") +
  annotate(geom="text", x = .5, y = .6, label = label_lr,
           color="red", size = 3.5) +
  annotate(geom="text", x = .25, y = .75, label = label_svm,
           color="blue", size = 3.5) +
  theme_bw(base_size = 14) +
  scale_color_manual(
    breaks = c("1", "2"),
    values = c('red', 'blue'),
    labels = c(`1` = 'Logistic Regression',
               `2` = 'Support Vector Machine')) +
  ggtitle('ROC Curves') +
  labs(x = '1 - Specificity', y = 'Sensitivity') +
  theme(plot.title = element_text(hjust = .5, size = 16),
        legend.title = element_blank(),
        legend.position = c(0.7, 0.4))

# save figure
ggsave('figures/roc_test.png',
       p,
       width = 5.5,
       height = 5.5,
       dpi = 600)


##########################################################################
## Variable Importance
######################

# load data
svm_fs <- readRDS('data/fitted_models/svm_fs.rds')

# prepare plot data
plot_data <- svm_fs$importance$importance

# create dictionary for labeling of vars
dict <- c("worr_crime",
          "worr_econ",
          "worr_finances",
          "sat_dwelling",
          "sat_pincome",
          "sat_hincome",
          "sat_life",
          "health",
          "caution_foreigners",
          "state_exploitive",
          "suitable_pos",
          "gender",
          "empl_interest: part-time",
          "empl_interest: both",
          "empl_interest: not sure",
          "severely_disabled",
          "friends: 6-10",
          "friends: 11-15",
          "friends: 16-20",
          "friends: > 21",
          "marital_status: married, separated",
          "marital_status: single",
          "marital_status: divorced",
          "marital_status: widowed",
          "marital_status: registered partnership, separated",
          "german",
          "age",
          "acceptance",
          "state_losers",
          "state_helpful",
          "state_trust",
          "state_special",
          "empl_intended",
          "children: 1",
          "children: 2",
          "children: 3",
          "children: > 4",
          "state_personality",
          "state_rivals",
          "state_show",
          "hh_income"
          )
names(dict) <- rownames(plot_data)

# prepare plot data
plot_data <- bind_cols(plot_data, names = dict)
plot_data <- plot_data[order(plot_data$X0, decreasing = T), ]
plot_data <- plot_data[1:30, ]

p <- ggplot(data = plot_data) +
  geom_bar(stat = 'identity', aes(x = reorder(names, X0), y = X0),
           fill = 'grey25') +
  geom_hline(yintercept = .52, color = 'red') +
  coord_flip(ylim = c(.5, .605)) +
  ggtitle('SVM: Importance of Predictor Variables') +
  labs(x = element_blank(), y = 'AUC') +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = .5, size = 16))

# save plot
ggsave('figures/svm_importance.png',
       p,
       width = 8,
       height = 8,
       dpi = 600)