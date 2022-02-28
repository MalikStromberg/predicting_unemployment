if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

source('support_scripts/load_soep_data_hh.R')

# load soep data
load('data/loaded_data/master_hh.RData')

# load variable names
dictionary <- readxl::read_excel('data/loaded_data/basket_hh.xlsx')

# convert variable names
dictionary$label <- dictionary$label %>%
  iconv(from = 'UTF-8',
        to = 'ASCII//TRANSLIT') %>%
  str_remove_all('[^a-zA-Z0-9 ]') %>%
  str_to_lower() %>%
  str_replace_all(' ', '_') %>%
  paste(dictionary$period_name, sep = '_')

# give variable working-names
master <- rename(master,
                 dictionary %>% select(label, name) %>% 
                   filter(name %in% names(master)) %>% deframe)

# extract years
relevant_years <- dictionary$period_name %>% as.factor %>% levels

# drop irrelevant cols
data <- master[,-c(1:10, 12:46)]

# convert data set from wide format to long format
# prepare base data set 
data_long <- select(data, persnr, ends_with(relevant_years[1]))
colnames(data_long) <- names(data_long) %>%
  str_remove('_' %>% paste(relevant_years[1], '$', sep = ''))
data_long <- bind_cols('year' = relevant_years[1], data_long)

for (i in relevant_years[2:length(relevant_years)]) {
  data_temp <- select(data, persnr, ends_with(i))
  colnames(data_temp) <- names(data_temp) %>%
    str_remove('_' %>% paste(i, '$', sep = ''))
  data_temp <- bind_cols('year' = i, data_temp)
  data_temp[is.na(data_temp)] <- -999
  data_long <- bind_rows(data_long, data_temp)
}

# merge same data with different varible names
data_long$household_net_income <- ifelse(is.na(data_long$household_net_income),
                                         data_long$amount_hh_net_incomemonth,
                                         data_long$household_net_income)
data_long$household_net_income <- ifelse(
  is.na(data_long$household_net_income),
  data_long$monthly_net_household_income,
  data_long$household_net_income)
data_long <- data_long %>% select(-c(monthly_net_household_income,
                                     amount_hh_net_incomemonth))

# replace NA indicator
data_long[data_long == -999] <- NA

# convert to numeric
data_long$year <- data_long$year %>% as.numeric()

saveRDS(data_long, 'data/loaded_data/data_hh.rds')

data <- readRDS('data/temp_data/data_model.rds')

# join hh data on individual data
data <- left_join(data, data_long, by = c('persnr', 'year'))

# replace NA indicator
data[data < 0] <- NA

# save full data set
saveRDS(data, 'data/tidy_data/data_model_merged.rds')








