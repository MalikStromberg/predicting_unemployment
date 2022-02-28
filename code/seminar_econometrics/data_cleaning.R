# this script loads and cleans SOEP-data and prepares it for analyses

# clear workspace
rm(list = ls())

# install required packages
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# create soep data set
source('support_scripts/load_soep_data.R')

# load soep data
load('data/loaded_data/master.RData')

# load variable names
dictionary <- readxl::read_excel('data/loaded_data/basket201229.xlsx')

# convert variable names
dictionary$label <- dictionary$label %>%
  iconv(from = 'UTF-8',
        to = 'ASCII//TRANSLIT') %>%
  str_remove_all('[^a-zA-Z0-9 ]') %>%
  str_to_lower() %>%
  str_replace_all(' ', '_') %>%
  paste(dictionary$period_name, sep = '_')

# adjust some names
dictionary$label <- ifelse(dictionary$label_de %>% str_detect('aktuelles'),
                           dictionary$label %>% str_replace('prev', 'this'),
                           dictionary$label)

# some data points in dictionary need special treatment in order to get
# unique column names

dictionary[dictionary$name == 'bhp_122_06',]$label <- 
  'registered_unemployed_jandec_prev_yr_2017'

dictionary[dictionary$name == 'bip_21',]$label <- 
  'registered_unemployed_2018'

dictionary[dictionary$name == 'bip_123_06',]$label <- 
  'registered_unemployed_jandec_prev_yr_2018'

dictionary[dictionary$name == 'bhp_05',]$label <- 
  'd_how_job_ended_2017'

dictionary[dictionary$name == 'bhp_03',]$label <- 
  'd_left_last_job_month_2016_2017'

dictionary[dictionary$name == 'bip_38_01',]$label <- 
  'd_left_last_job_month_2017_2018'

dictionary[dictionary$name == 'bep8412',]$label <- 
  'militarycomm_srvce_jandec_prv_yr_2014'

dictionary[dictionary$name == 'bdp9712',]$label <- 
  'militarycomm_srvce_jandec_prv_yr_2013'

dictionary[dictionary$name == 'bgp10411',]$label <- 
  'militarycomm_srvce_jandec_prv_yr_2016'

dictionary[dictionary$name == 'bcp7912',]$label <- 
  'militarycomm_srvce_jandec_prv_yr_2012'

dictionary[dictionary$name == 'bfp11811',]$label <- 
  'militarycomm_srvce_jandec_prv_yr_2015'

dictionary[dictionary$name == 'bipmonin',]$label <- 
  'month_of_interview_2018'

# give variable working-names
master <- rename(master,
                 dictionary %>% select(label, name) %>% 
                   filter(name %in% names(master)) %>% deframe)

# extract years
relevant_years <- dictionary$period_name %>% as.factor %>% levels

# drop irrelevant cols
data <- master[,-c(2:50)]

# convert data set from wide format to long format
# prepare base data set 
data_long <- select(data, persnr, ends_with(relevant_years[1]))
colnames(data_long) <- names(data_long) %>%
  str_remove('_' %>% paste(relevant_years[1], '$', sep = ''))
data_long <- bind_cols('year' = relevant_years[1], data_long)

for (i in relevant_years[2:length(relevant_years)]) {
  # extract cols that correspond to the year of interest
  data_temp <- select(data, persnr, ends_with(i))
  # adjust column names
  colnames(data_temp) <- names(data_temp) %>%
    str_remove('_' %>% paste(i, '$', sep = ''))
  # add year col
  data_temp <- bind_cols('year' = i, data_temp)
  # replace NAs
  data_temp[is.na(data_temp)] <- -999
  # add rows to main data set
  data_long <- bind_rows(data_long, data_temp)
}

# just an overview
data_summary <- data_long
data_summary[data_summary == -999] <- NA
summary(data_summary)

# tidy up regarding unemployment data
# unfortunately only from 2009 on the unemployment data is documented detailed
# enough to make the investigations my thesis tries to aim to

# load function
source('functions/f_create_numerics.R')

# filter for people who have been unemployed at least once in the period
people_unemployed <- data_long %>%
  filter(registered_unemployed_jandec_prev_yr %>%
           str_detect(pattern = '01') &
           year >= 2009) %>%
  select(persnr) %>%
  unique()
data_long <- data_long %>% filter(persnr %in% people_unemployed$persnr)

# sort data set
data_long <- data_long[order(data_long$persnr, data_long$year), ]

# match rows that belong together
dictionary_concept <- dictionary %>% select(label, concept_name)
dictionary_concept$label <- dictionary_concept$label %>%
  str_remove('[_][0-9][0-9][0-9][0-9]$')
dictionary_concept <- unique(dictionary_concept)
dictionary_concept <- dictionary_concept %>%
  filter(label %in% colnames(data_long))
concepts <- unique(dictionary_concept$concept_name)

# this loop matches variables that belong to the same concept
for (i in concepts) {
  var_names <- dictionary_concept %>% filter(concept_name == i) %>%
    select(label)
  # if there are at least two different variable names that belong to the
  # same concept
  if (length(var_names$label) >= 2) {
    for (j in 2:length(var_names$label)) {
      # merge rows
      data_long[, var_names$label[1]] <- ifelse(
        is.na(data_long[, var_names$label[1]]),
         data_long[, var_names$label[j]],
         data_long[, var_names$label[1]])
      # delete second col
      data_long <- data_long %>% select(-one_of(var_names$label[j]))
    }
  }
}

# replace NA-code -999
data_long[data_long == -999] <- NA

# convert to factor
data_long$year <- as.factor(data_long$year)

## watch and fill variables that did not appear every year
data_long <- data_long %>% group_by(persnr) %>% fill(
  being_a_very_special_person_gives_me_a_lot_of_strength,
  i_manage_to_be_the_center_of_attention_with_my_outstanding_contributions,
  i_react_with_annoyance_if_another_person_steals_the_show_from_me,
  i_deserve_to_be_seen_as_a_great_personality,
  i_want_my_rivals_to_fail,
  most_people_are_basically_losers,
  .direction = 'up'
)

data_long1 <- data_long %>% filter(year %in% c(2008:2010)) %>%
  group_by(persnr) %>% fill(
    impulsiveness,
    people_can_generally_be_trusted,
    nowadays_you_can_not_trust_anybody_anymore,
    caution_towards_foreigners,
    most_people_are_exploitive_fair,
    would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests,
    .direction = 'down'
  )
data_long2 <- data_long %>% filter(year %in% c(2011:2015)) %>%
  group_by(persnr) %>% fill(
    impulsiveness,
    people_can_generally_be_trusted,
    nowadays_you_can_not_trust_anybody_anymore,
    caution_towards_foreigners,
    most_people_are_exploitive_fair,
    would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests,
    .direction = 'downup'
  )
data_long3 <- data_long %>% filter(year %in% c(2016:2018)) %>%
  group_by(persnr) %>% fill(
    impulsiveness,
    people_can_generally_be_trusted,
    nowadays_you_can_not_trust_anybody_anymore,
    caution_towards_foreigners,
    most_people_are_exploitive_fair,
    would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests,
    .direction = 'up'
  )

# bind all together by rows
data_long <- bind_rows(data_long1, data_long2, data_long3)
rm(data_long1)
rm(data_long2)
rm(data_long3)

data_long <- data_long %>% group_by(persnr) %>% fill(
  amount_of_closed_friends,
  state_of_health_affects_ascending_stairs,
  state_of_health_affects_tiring_tasks,
  within_next_two_years_become_selfemployed,
  within_next_two_years_additional_qualification,
  probability_start_work_in_next_2_years,
  .direction = 'down'
)

# convert years for computational reasons
data_long$year <- create_numerics(data_long$year)

# select relevant years
data_long <- data_long %>%
  mutate(year = create_numerics(year)) %>%
  filter(year >= 2009)

# adjust coding such that all entries have the same length
data_long$registered_unemployed_jandec_prev_yr <-
  ifelse(nchar(data_long$registered_unemployed_jandec_prev_yr) == 0,
         '000000000000000000000000',
         data_long$registered_unemployed_jandec_prev_yr)

data_long$registered_unemployed_jandec_prev_yr <-
  ifelse(data_long$registered_unemployed_jandec_prev_yr == '-999',
         '999999999999999999999999',
         data_long$registered_unemployed_jandec_prev_yr)

data_long$registered_unemployed_jandec_prev_yr <-
  ifelse(data_long$registered_unemployed_jandec_prev_yr == '-5',
         '555555555555555555555555',
         data_long$registered_unemployed_jandec_prev_yr)

# load function
source('functions/f_detect_unemployment_periods.R')

# initialization
data_unemployed <- NULL
data_long$month_of_interview[is.na(data_long$month_of_interview)] <- -999
data_long$registered_unemployed[is.na(data_long$registered_unemployed)] <- -999

# filter again for people who have been unemployed at least once in the period
people_unemployed <- data_long %>%
  filter(registered_unemployed_jandec_prev_yr %>%
           str_detect(pattern = '01')) %>%
  select(persnr) %>%
  unique()
people_unemployed$persnr <- as.numeric(people_unemployed$persnr)

# reduce data set to people that have been unemployed
data_long <- data_long %>% filter(persnr %in% people_unemployed$persnr)

# convert to list to speed up the loop
data_long_list <- split(data_long, f = data_long$persnr)

# this loop produces a data set out of data_long that is reduced to people
# for which there is enough data to classify them as long-term or short-term
# unemployed
for (i in (1:length(data_long_list))) {
  # extract specific individual
  dat_temp <- data_long_list[[i]]
  # sort the extracted data set
  dat_temp <- dat_temp[order(dat_temp$year, decreasing = F), ]
  # lead the data for unemployment because data is observed in one year for
  # the previous year
  dat_temp$registered_unemployed_jandec <-
    dat_temp$registered_unemployed_jandec_prev_yr %>% lead()
  dat_temp <- dat_temp[-nrow(dat_temp), ]
  
  # initialization
  history <- NULL
  
  # this loop creates the unemployment history for the specific individual
  # from 2010 to 2017
  for (j in 1:nrow(dat_temp)) {
    history <- paste(history,
                     dat_temp$registered_unemployed_jandec[j],
                     sep = '')
  }
  
  # detect most recent relevant unemployment period in the history
  unemployed <- detect_unemployment_periods(history)
  
  # if detection was successful
  if (!is.null(unemployed)){
    # calc relevant data
    duration <- (unemployed$end + 1 - unemployed$start) / 2
    duration_to_end <- ((nchar(history) - unemployed$end) / 2)
    crit_year <- max(as.numeric(dat_temp$year)) -
      ((duration_to_end + duration - 1) %/% 12)
    crit_month <- 12 - ((duration_to_end + duration - 1) %% 12)
    
    # initialization
    actually_solved <- T
    solved <- T
    
    # check whether the individual have been interviewed and has been 
    # registered unemployed in the corresponding unemployment period,
    # i. e. do both answers regarding registered unemployment agree
    if (
      crit_month >= (filter(dat_temp, year == crit_year)$month_of_interview) |
      filter(dat_temp, year == crit_year)$registered_unemployed != 1) {
      # update solved indicator
      solved  <- F
      # initialization
      p <- 1
      while (!solved) {
        # check if there exist a subsequent year
        if (crit_year + 1 <= max(dat_temp$year)) {
          # check whether the interview took place in the subsequent year
          # during the unemployment period
          criterion <- (
            filter(dat_temp,
                   year == crit_year + 1)$month_of_interview -
              crit_month + 12 + 1 <= duration &
              filter(dat_temp,
                     year == crit_year + 1)$registered_unemployed == 1)
        } else {
          criterion <- F
        }
        
        if (criterion){
          # if there exist an interview in the subsequent year during
          # unemployment period, use the data
            crit_year <- crit_year + 1
            solved <- T
            actually_solved <- T
        } else {
          # if this is not the case, look for the next most recent unemployment
          # period of the individual
          p <- p + 1
          unemployed <- detect_unemployment_periods(history, period = p)
          if  (unemployed != 'infeasible') {
            # if period detection was successfull, calc relevant vars again
            duration <- (unemployed$end + 1 - unemployed$start) / 2
            duration_to_end <- ((nchar(history) - unemployed$end) / 2)
            crit_year <- max(as.numeric(dat_temp$year)) -
              ((duration_to_end + duration - 1) %/% 12)
            crit_month <- 12 - ((duration_to_end + duration - 1) %% 12)
            
          } else {
            # if there is no unemployment period left, the individual drops out
            
            # escape while loop
            solved <- T
            
            # indicate drop out
            actually_solved <- F
          }
          if (
            crit_month <
            (filter(dat_temp, year == crit_year)$month_of_interview) &
            filter(dat_temp, year == crit_year)$registered_unemployed == 1) {
            # check again whether all criteria are fulfilled and the answers
            # agree
            
            solved  <- T
            actually_solved <- T
          }
        }
      }
    }
    
    # add labeling
    if (actually_solved) {
      if (duration <= 12) {
        label <- 0
      } else {
        label <- 1
      }
      # add relevant vars
      dat_crit <- dat_temp %>% filter(year == crit_year)
      dat_crit$unemployed_long <- label
      dat_crit$unemployment_duration <- duration
    } else {
      dat_crit <- NULL
    }
  
  
  } else {
      dat_crit <- NULL
  }
  # add observation to the final data set
  data_unemployed <- bind_rows(data_unemployed, dat_crit)
  
  # watch loop progress
  if (i %% 1000 == 0) {
    print(paste('pogress: ',
                round(i/nrow(people_unemployed)*100, 2),
                '%',
                sep = ''))
  }
}

# save final data set with all loaded vars
saveRDS(data_unemployed, 'data/temp_data/data_unemployed.rds')

# select vars for feature selection
data_model <- data_unemployed %>% select(
   year,
   persnr,
   unemployed_long,
   unemployment_duration,
   worried_about_crime_in_germany,
   worried_about_economic_development,
   worried_about_finances,
   satisfaction_with_dwelling,
   satisfaction_with_personal_income,
   satisfaction_with_household_income,
   satisfaction_with_life_at_present,
   current_health,
   caution_towards_foreigners,
   most_people_are_exploitive_fair,
   find_suitable_position,
   gender,
   future_job_prospects,
   gainfully_employed_interest,
   why_job_terminated,
   severely_disabled,
   amount_of_closed_friends,
   marital_status,
   german_nationality,
   age_of_individual,
   acceptance_within_two_weeks,
   most_people_are_basically_losers,
   activities_in_life_are_valuable_and_useful,
   would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests,
   people_can_generally_be_trusted,
   being_a_very_special_person_gives_me_a_lot_of_strength,
   impulsiveness,
   employment_in_the_future_intended,
   number_of_children_in_hh,
   success_in_life_is_due_to_fate,
   i_deserve_to_be_seen_as_a_great_personality,
   i_want_my_rivals_to_fail,
   probability_start_work_in_next_2_years,
   within_next_two_years_additional_qualification,
   within_next_two_years_become_selfemployed,
   i_react_with_annoyance_if_another_person_steals_the_show_from_me
)

# make factor variables ready for calculations
data_model <- apply(data_model, 2, create_numerics) %>% data.frame()

# replace NA-indicators
data_model[data_model < 0] <- NA

# adjust variable codings
data_model$most_people_are_exploitive_fair <-
  2 - data_model$most_people_are_exploitive_fair

data_model$severely_disabled <- 2 - data_model$severely_disabled

data_model$german_nationality <- 2 - data_model$german_nationality

data_model$would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests <-
  2 - data_model$would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests

data_model$acceptance_within_two_weeks <-
  2 - data_model$acceptance_within_two_weeks

# na summary
na_count <- data.frame('n' = is.na(data_model) %>% apply(2, sum))
# after eye-balling choose minimum number of NAs
na_count <- na_count %>% filter(n <= 800)

# select workable vars
data_model <- data_model %>% select(one_of(rownames(na_count)))

# make factor vars
data_model$unemployed_long <- data_model$unemployed_long %>% as.factor()
data_model$german_nationality <- data_model$german_nationality %>% as.factor()
data_model$marital_status <- data_model$marital_status %>% as.factor()
data_model$severely_disabled <- data_model$severely_disabled %>% as.factor()
data_model$acceptance_within_two_weeks <-
  data_model$acceptance_within_two_weeks %>% as.factor()
data_model$would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests <-
  data_model$would_you_say_that_people_usually_try_to_helpful_or_pursue_own_interests %>%
  as.factor()
data_model$most_people_are_exploitive_fair <-
  data_model$most_people_are_exploitive_fair %>% as.factor()
data_model$gender <- data_model$gender %>% as.factor()
data_model$gainfully_employed_interest <-
  data_model$gainfully_employed_interest %>% as.factor()

# define levels
data_model$number_of_children_in_hh <- cut(data_model$number_of_children_in_hh,
                                    breaks = c(-Inf, 0.5, 1.5, 2.5, 3.5, Inf),
                                    labels = c('0', '1', '2',
                                               '3', '4+'))
data_model$amount_of_closed_friends <- cut(data_model$amount_of_closed_friends,
                                           breaks = c(-Inf, 5.5, 10.5, 15.5,
                                                      20.5, Inf),
                                           labels = c('<6', '6-10', '11-15',
                                                      '16-20', '21+'))

# save data used for analyses
saveRDS(data_model, 'data/temp_data/data_model.rds')

# merge household-data
source('support_scripts/merge_hh_data.R')