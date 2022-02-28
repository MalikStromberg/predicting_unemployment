# this functions detects unemployment periods in individual's unemployment
# history and returns the specified most recent one

detect_unemployment_periods <- function(history, period = 1) {
  
  # initialization
  unemployed <- NULL
  
  # looking for a period that is right censored by construction, but not
  # left censored
  unemployed2 <- data.frame(str_locate_all(history, '(?<=(00))(01)+$'))
  
  # if there exists a period that is right censored by construction
  if (nrow(unemployed2) == 1) {
    if (unemployed2$end[1] -
        unemployed2$start[1] + 1 > 24) {
      unemployed <- unemployed2[1, ]
    } 
  }
  
  # looking for a period that is neither left censored nor right censored,
  # neither by construction nor by NA
  unemployed1 <- data.frame(str_locate_all(history,
                                           '(?<=(00))(01)+(?=(00))'))
  # looking for a period that is not left censored but right censored
  # by NA
  unemployed3 <- data.frame(str_locate_all(history,
                                           '(?<=(00))(01)+(?=[(55)(99)])'))

  # if there are uncensored periods
  if (nrow(unemployed1) != 0) {
    unemployed <- bind_rows(unemployed, unemployed1)
  } 
  
  # if there is only data that is censored from right by NA
  if (nrow(unemployed3) != 0) {
    # if at least one NA-censored period is longer than a year
    if (any(unemployed3$end - unemployed3$start + 1 > 24)) {
      unemployed <-  bind_rows(unemployed,
                               unemployed3[which(
                                 unemployed3$end -
                                   unemployed3$start + 1 > 24), ])
    }
  }
  
  if (!is.null(unemployed)) {
    if (nrow(unemployed) >= period) {
      unemployed <- unemployed[order(unemployed$start, decreasing = T), ]
      return(unemployed[period, ])
    } else {
      return('infeasible')
    }
  } else {
    return(NULL)
  }
}