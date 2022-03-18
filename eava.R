# implements the hierarchical expert algorithm for verbal autopsy described in 
# [Kalter et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4416334/) (doi: 10.7189/jogh.05.010415)

library(dplyr)
library(tidyr)
library(lubridate)
# library(vida)
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
  
  # for later use, determine durations of conditions in DAYS
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
  
  if (measles(responses, format, days_rash, days_fever )) {
    return("Measles")
  }
  
  if (meningitis(responses, format)) {
    return("Meningitis")
  }
  
  if (dysentery(responses, format, days_diarrhea)) {
    return("Dysentery")
  }
  
  if (diarrhea(responses, format, days_diarrhea)) {
    return("Diarrhea")
  }
  
  if (pertussis(responses, format, days_cough)) {
    return("Pertussis")
  }
  
  if (pneumonia(responses, format, days_cough, days_fast_breathing)) {
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
  
  # if (residual_infection(responses, format)) {
  #   return("Residual infection")
  # }
  
  if (possible_malaria(responses, format)) {
    return("Possible malaria")
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
  if( format == "c12_16"){
    questions <- c("die_at_site_of_injury_accident", "injury_accident", "road_accident",
                   "injury_fall", "drown", "accidentally_poisoned", "poisoning",
                   "animal", "venomous_animal", "burn", "assault", "other_injury" )
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( all_of(questions) )
    if(any(!is.na(answers))) {
      if (any(answers == "yes", na.rm = TRUE)) {
        return(TRUE)
      } else{
        return(FALSE)
      }
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

# AIDS -----

aids <- function( responses, format ){
  if( format == "c12_16" ){
    # duration of diarrhea, fever, skin rash all need to be checked!! 
    
    questions <- c("swell_armpits", "rash_mouth")
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( questions )
    
    if(any(!is.na(answers))) {
      if( any( answers == "yes", na.rm=TRUE ) ){
        questions <- c("thin", "protruding_abdomen", "diarrhea", "fever", 
                       "rash", "fast_breathing", "chest_pull_in")
        questions <- questions[ which( questions %in% names(responses)) ]
        answers <- responses %>% select( questions )
        if( length( which( answers == "yes")) >= 3 ){
          return(TRUE)
        } else{
          return(FALSE)
        }
      } else{
        return(FALSE)
      }
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

# Malnutrition (underlying) -----

underlying_malnutrition <- function( responses, format ){
  if( format == "c12_16" ){
    questions <- c("thin", "swell_feet", "swell_leg")
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( questions )
    if(any(!is.na(answers))) {
      if( any( answers == "yes", na.rm=TRUE )){
        return(TRUE)
      } else{
        return(FALSE)
      }
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Measles -----

measles <- function( responses, format, days_rash, days_fever ){
  if( format == "c12_16" ){
    # get age in days :
    age <- difftime( mdy(responses$date_death_deceased), mdy(responses$date_birth_deceased), units="days")
    if( !is.na( age ) & age > 120 ){
      if( !is.na(days_rash) & (days_rash > 3) & !is.na(days_fever) & (days_fever > 3)){
        return(TRUE)
      } else{
        return(FALSE)
      }
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Meningitis -----

meningitis <- function( responses, format ){
  if( format == "c12_16" ){
    questions <- c("fever")
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( questions )
    if(any(!is.na(answers))) {
      if( any( answers == "yes", na.rm=TRUE )){
        questions <- c("stiff_neck", "bulging_fontanelle")
        questions <- questions[ which( questions %in% names(responses)) ]
        answers <- responses %>% select( questions )
        if( any( answers == "yes", na.rm=TRUE )){
          return(TRUE)
        } else{
          return(FALSE)
        }
      } else{
        return(FALSE)
      }
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Dysentery -----

dysentery <- function( responses, format, days_diarrhea ){
  if( format == "c12_16" ){
    if( (!is.na( days_diarrhea ) & (days_diarrhea > 14)) &
        (!is.na( responses$bloody_stool ) & (responses$bloody_stool == "yes")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Diarrhea -----

diarrhea <- function( responses, format, days_diarrhea ){
  if( format == "c12_16" ){
    if( (!is.na( days_diarrhea ) & (days_diarrhea > 14)) &
        (!is.na( responses$bloody_stool ) & (responses$bloody_stool == "no")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Pertussis -----

pertussis <- function( responses, format, days_cough ){
  if( format == "c12_16" ){
    if( !is.na(days_cough) & (days_cough > 14) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Pneumonia -----

pneumonia <- function( responses, format, days_cough, days_fast_breathing ){
  if( format == "c12_16" ){
    if( ( !is.na(responses$difficulty_breathing) & (responses$difficulty_breathing=="yes")) | 
        ( !is.na(days_cough) & (days_cough > 2) ) ){
      if( (!is.na( days_fast_breathing ) & (days_fast_breathing > 2)) | 
          (!is.na( responses$noisy_breathing) & (responses$noisy_breathing=="yes" )) |
          (!is.na( responses$chest_pull_in) & (responses$chest_pull_in=="yes" )) ){
        return(TRUE)
      } else{
        return(FALSE)
      }
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Malaria -----

malaria <- function( responses, format ){
  if( format == "c12_16" ){
    # fever continued until death?
    if( (!is.na( responses$fever ) & (responses$fever == "yes")) &
        (!is.na( responses$stiff_neck ) & (responses$stiff_neck == "no")) &
        # (!is.na( responses$bulging_fontanelle ) & (responses$bulging_fontanelle == "no")) &
        ( (!is.na(responses$difficulty_breathing) & (responses$difficulty_breathing == "yes")) |
          (!is.na(responses$convulsions) & (responses$convulsions == "yes")) |
          (!is.na(responses$unconscious) & (responses$unconscious == "yes")) ) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Possible dysentery -----

possible_dysentery <- function( responses, format ){
  if( format == "c12_16" ){
    if( (!is.na(responses$diarrhea) & (responses$diarrhea == "yes")) &
        (!is.na( responses$bloody_stool ) & (responses$bloody_stool == "yes")) &
        ( (!is.na(responses$fever) & (responses$fever == "yes")) | 
          (!is.na(responses$convulsions) & (responses$convulsions == "yes")) |
          (!is.na(responses$unconscious) & (responses$unconscious == "yes")) ) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Possible diarrhea -----

possible_diarrhea <- function( responses, format ){
  if( format == "c12_16" ){
    if( (!is.na(responses$diarrhea) & (responses$diarrhea == "yes")) &
        (!is.na( responses$bloody_stool ) & (responses$bloody_stool == "no")) &
        ( (!is.na(responses$fever) & (responses$fever == "yes")) | 
          (!is.na(responses$convulsions) & (responses$convulsions == "yes")) |
          (!is.na(responses$unconscious) & (responses$unconscious == "yes")) ) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Possible pneumonia -----

possible_pneumonia <- function( responses, format ){
  if( format == "c12_16" ){
    if( ( ( (!is.na(responses$cough) & (responses$cough=="yes")) |
            (!is.na(responses$difficulty_breathing) & (responses$difficulty_breathing == "yes")) ) |
          ( (!is.na(responses$fast_breathing) & (responses$fast_breathing == "yes")) &
            ( (!is.na(responses$chest_pull_in) & (responses$chest_pull_in == "yes")) |
              (!is.na(responses$noisy_breathing) & (responses$noisy_breathing == "yes")) ) ) ) &
        ( (!is.na(responses$cough) & (responses$cough=="yes")) |
          (!is.na(responses$fast_breathing) & (responses$fast_breathing=="yes")) |
          (!is.na(responses$chest_pull_in) & (responses$chest_pull_in=="yes")) |
          (!is.na(responses$noisy_breathing) & (responses$noisy_breathing=="yes")) |
          (!is.na(responses$fever) & (responses$fever=="yes")) |
          (!is.na(responses$convulsions) & (responses$convulsions=="yes")) |
          (!is.na(responses$unconscious) & (responses$unconscious=="yes")) ) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Hemorrhagic fever -----

hemorrhagic_fever <- function( responses, format ){
  if( format == "c12_16" ){
    if( (!is.na(responses$fever) & (responses$fever=="yes")) &
        (!is.na(responses$bleed_nose_mouth_anus) & (responses$bleed_nose_mouth_anus == "yes")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Other infection -----

other_infection <- function( responses, format ){
  if( format == "c12_16" ){
    if( (!is.na(responses$fever) & (responses$fever=="yes")) & 
        ( ( !is.na(responses$rash) & (responses$rash=="yes")) |
          (!is.na(responses$convulsions) & (responses$convulsions=="yes")) | 
          (!is.na(responses$unconscious) & (responses$unconscious=="yes")) ) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Residual infection -----
#
# residual_infection <- function( responses, format ){
#   if( format == "c12_16" ){
#     
#   } else{ 
#     return(FALSE)
#   }
# }

# Possible malaria -----

possible_malaria <- function( responses, format ){
  if( format == "c12_16" ){
    if( !is.na(responses$fever) & (responses$fever == "yes") ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# Malnutrition -----

malnutrition <- function( responses, format ){
  if( format == "c12_16" ){
    if( (!is.na(responses$thin) & (responses$thin == "yes")) |
        (!is.na(responses$swell_feet) & (responses$swell_feet == "yes")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}
