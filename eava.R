# implements the hierarchical expert algorithm for verbal autopsy described in 
# [Kalter et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4416334/) (doi: 10.7189/jogh.05.010415)

library(vida)
library(dplyr)
library(tidyr)
library(lubridate)
# data('master')

supported_formats <- c("c12_16")

#' Determine causes of death for ALL decedents in a verbal autopsy (VA) dataset that has been
#' translated into `babel` format with the `vida` package
#' 
#' @param babel_data : a data.frame of VA responses for multiple decedents (typically from one study site)
#' @param format : mapper used in babel translation
#' @return causes : causes of death determined by the hierarchical expert algorithm 
#' 
get_causes <- function(babel_data, format) {
  if( format %in% supported_formats ){
    causes <- vector(mode = "character")
    for (i in 1:nrow(babel_data)) {
      causes[i] <- cod(babel_data[i,], format)
    }
    return(causes)
  } else{
    message(paste0(format, " is not a supported babel mapper format.\nPlease check your VA data and try again."))
    return(NULL)
  }
}

#' Determine ONE decedent's cause of death from verbal autopsy (VA) data that has been
#' translated into `babel` format with the `vida` package
#' 
#' @param responses : a data.frame of VA responses for one person (one row from a larger VA dataset)
#' @return cod : the cause of death determined by the hierarchical expert algorithm 
#' 
cod <- function(responses, format) {
  
  # for later use, determine durations of conditions in _DAYS_
  days_fever <- fever_duration(responses, format) 
  days_rash <- rash_duration(responses, format) 
  days_cough <- cough_duration(responses, format) 
  days_diarrhea <- diarrhea_duration(responses, format) 
  days_fast_breathing <- fast_breathing_duration(responses, format) 
  
  if (injury(responses, format)) {
    return("Injury")
  }
  
  if (aids(responses, format)) {
    return("AIDS")
  }
  
  if (underlying_malnutrition(responses, format)) {
    return("Malnutrition (underlying)")
  }
  
  if (measles(responses, format)) {
    return("Measles")
  }
  
  if (meningitis(responses, format)) {
    return("Meningitis")
  }
  
  if (dysentery(responses, format)) {
    return("Dysentery")
  }
  
  if (diarrhea(responses, format)) {
    return("Diarrhea")
  }
  
  if (pertussis(responses, format)) {
    return("Pertussis")
  }
  
  if (pneumonia(responses, format)) {
    return("Pneumonia")
  }
  
  if (malaria(responses, format)) {
    return("Malaria")
  }
  
  if (possible_dysentery(responses, format)) {
    return("Possible dysentery")
  }
  
  if (possible_diarrhea(responses, format)) {
    return("Possible diarrhea")
  }
  
  if (possible_pneumonia(responses, format)) {
    return("Possible pneumonia")
  }
  
  if (hemorrhagic_fever(responses, format)) {
    return("Hemorrhagic fever")
  }
  
  if (other_infection(responses, format)) {
    return("Other infection")
  }
  
  if (residual_infection(responses, format)) {
    return("Residual infection")
  }
  
  if (malnutrition(responses, format)) {
    return("Malnutrition")
  }
  
  # reaching this point means that no specific cause can be determined by this algorithm
  return("Unspecified")
}

# Durations -----
fever_duration <- function(responses, format) {
  if( format == "c12_16"){
    # fever indicates fever (yes, no, dk)
    # fever_duration gives units (days, weeks, dk) 
    # days_weeks_fever provides value
    # the following computes duration of fever in DAYS
    if( !is.na( responses$fever ) & responses$fever == "yes" ){
      if( !is.na(responses$fever_duration) & (responses$fever_duration == "weeks") ){
        days_fever <- 7*as.numeric(responses$days_weeks_fever)
      } else if( !is.na(responses$fever_duration) & (responses$fever_duration == "days") ){
        days_fever <- as.numeric(responses$days_weeks_fever)
      } else{
        days_fever <- NA
      }
    } else{
      days_fever <- NA
    }
    return(days_fever)
  } else{
    return( NA )
  }
}

rash_duration <- function(responses, format) {
  if( format == "c12_16"){
    # rash indicates skin rash (yes, no, dk)
    # rash_duration gives units (days, weeks, dk) 
    # days_weeks_rash provides value
    # the following computes duration of rash in DAYS
    if( !is.na( responses$rash ) & responses$rash == "yes" ){
      if( !is.na(responses$rash_duration) & responses$rash_duration == "weeks"){
        days_rash <- 7*as.numeric(responses$days_weeks_rash)
      } else if( !is.na(responses$rash_duration) & responses$rash_duration == "days" ){
        days_rash <- as.numeric(responses$days_weeks_rash)
      } else{
        days_rash <- NA
      }
    } else{
      days_rash <- NA
    }
    return(days_rash) 
  } else{
    return(NA)
  }
}

cough_duration <- function(responses, format) {
  if( format == "c12_16"){
    # cough indicates whether the child had a cough (yes, no, dk)
    # cough_duration gives units (days, weeks, dk) 
    # days_weeks_cough provides value
    # the following computes duration of cough in DAYS
    if( !is.na( responses$cough ) & responses$cough == "yes" ){
      if( !is.na(responses$cough_duration) & responses$cough_duration == "weeks"){
        days_cough <- 7*as.numeric(responses$days_weeks_cough)
      } else if( !is.na(responses$cough_duration) & responses$cough_duration == "days" ){
        days_cough <- as.numeric(responses$days_weeks_cough)
      } else{
        days_cough <- NA
      }
    } else{
      days_cough <- NA
    }
    return(days_cough)
  } else{
    return(NA)
  }
}

diarrhea_duration <- function(responses, format) {
  if( format == "c12_16"){
    # diarrhea indicates whether the child had diarrhea (yes, no, dk)
    # diarrhea_duration gives units (days, weeks, dk) 
    # days_weeks_diarrhea provides value
    # the following computes duration of diarrhea in DAYS
    if( !is.na( responses$diarrhea ) & responses$diarrhea == "yes" ){
      if( !is.na(responses$diarrhea_duration) & responses$diarrhea_duration == "weeks"){
        days_diarrhea <- 7*as.numeric(responses$days_weeks_diarrhea)
      } else if( !is.na(responses$diarrhea_duration) & responses$diarrhea_duration == "days" ){
        days_diarrhea <- as.numeric(responses$days_weeks_diarrhea)
      } else{
        days_diarrhea <- NA
      }
    } else{
      days_diarrhea <- NA
    }
    return(days_diarrhea)
  } else{
    return(NA)
  }
}

fast_breathing_duration <- function(responses, format) {
  if( format == "c12_16"){
    # fast_breathing indicates whether the child had fast breathing (yes, no, dk)
    # fast_breathing_duration gives units (days, weeks, dk) 
    # days_weeks_fast_breathing provides value
    # the following computes duration of fast breathing in DAYS
    if( !is.na( responses$fast_breathing ) & responses$fast_breathing == "yes" ){
      if( !is.na(responses$fast_breathing_duration) & responses$fast_breathing_duration == "weeks"){
        days_fast_breathing <- 7*as.numeric(responses$days_weeks_fast_breathing)
      } else if( !is.na(responses$fast_breathing_duration) & responses$fast_breathing_duration == "days" ){
        days_fast_breathing <- as.numeric(responses$days_weeks_fast_breathing)
      } else{
        days_fast_breathing <- NA
      }
    } else{
      days_fast_breathing <- NA
    }
    return(days_fast_breathing)
  } else{
    return(NA)
  }
}

# Injury -----
injury <- function( responses, format ){
  return(FALSE)
}

# AIDS -----
aids <- function( responses, format ){
  return(FALSE)
}

# Malnutrition (underlying) -----
underlying_malnutrition <- function( responses, format ){
  return(FALSE)
}

# Measles -----
measles <- function( responses, format ){
  return(FALSE)
}

# Meningitis -----
meningitis <- function( responses, format ){
  return(FALSE)
}

# Dysentery -----
dysentery <- function( responses, format ){
  return(FALSE)
}

# Diarrhea -----
diarrhea <- function( responses, format ){
  return(FALSE)
}

# Pertussis -----
pertussis <- function( responses, format ){
  return(FALSE)
}

# Pneumonia -----
pneumonia <- function( responses, format ){
  return(FALSE)
}

# Malaria -----
malaria <- function( responses, format ){
  return(FALSE)
}

# Possible dysentery -----
possible_dysentery <- function( responses, format ){
  return(FALSE)
}

# Possible diarrhea -----
possible_diarrhea <- function( responses, format ){
  return(FALSE)
}

# Possible pneumonia -----
possible_pneumonia <- function( responses, format ){
  return(FALSE)
}

# Hemorrhagic fever -----
hemorrhagic_fever <- function( responses, format ){
  return(FALSE)
}

# Other infection -----
other_infection <- function( responses, format ){
  return(FALSE)
}

# Residual infection -----
residual_infection <- function( responses, format ){
  return(FALSE)
}

# Malnutrition -----
malnutrition <- function( responses, format ){
  return(FALSE)
}
