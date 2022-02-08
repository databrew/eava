# implements the hierarchical expert algorithm for verbal autopsy described in 
# [Kalter et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4416334/) (doi: 10.7189/jogh.05.010415)

library(vida)
library(dplyr)
library(tidyr)
library(lubridate)
data('master')

#' Determine causes of death for ALL decedents in a verbal autopsy (VA) dataset that has been
#' translated into `babel` format with the `vida` package
#' 
#' @param babel_data : a data.frame of VA responses for multiple decedents (typically from one study site)
#' @return causes : causes of death determined by the hierarchical expert algorithm 
#' 
get_causes <- function( babel_data ){
  causes <- vector(mode="character")
  for( i in 1:nrow(babel_data)){
    causes[i] <- cod( babel_data[i,])
  }
  return(causes)
}

#' Determine ONE decedent's cause of death from verbal autopsy (VA) data that has been
#' translated into `babel` format with the `vida` package
#' 
#' @param responses : a data.frame of VA responses for one person (one row from a larger VA dataset)
#' @return cod : the cause of death determined by the hierarchical expert algorithm 
#' 
cod <- function(responses) {
  
  # injury -----
  
  questions <- c("die_at_site_of_injury_accident", "injury_accident", "road_accident",
                 "injury_fall", "drown", "accidentally_poisoned", "poisoning",
                 "animal", "venomous_animal", "burn", "assault", "other_injury",
                 "sign_injury_broken_bones")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  
  # this is going to barf somewhere on files that don't use "yes", "no" !!
  if(any(!is.na(answers))) {
    if (any(answers == "yes", na.rm = TRUE)) {
      return("injury")
    }
  }
  
  # AIDS -----
  
  questions <- c("swell_armpits", "rash_mouth")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  
  # duration of diarrhea, fever, skin rash all need to be checked!! but...
  # rash_duration gives _units_ (e.g., weeks) in VA_ChildForm_WHO2012_babel ,
  # does not exist in VA_Child_WHO2016_babel or in
  # VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel
  
  if(any(!is.na(answers))) {
    if( any( answers == "yes", na.rm=TRUE ) ){
      questions <- c("thin", "protruding_abdomen", "diarrhea", "fever", 
                     "rash", "fast_breathing", "chest_pull_in")
      questions <- questions[ which( questions %in% names(responses)) ]
      answers <- responses %>% select( questions )
      if( length( which( answers == "yes")) >= 3 ){
        return("AIDS")
      }
    }
  }
  
  # malnutrition (underlying) -----
  
  questions <- c("thin", "swell_feet", "swell_leg")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  if(any(!is.na(answers))) {
    if( any( answers == "yes", na.rm=TRUE )){
      return( "malnutrition (underlying)")
    }
  }
  
  # measles -----
  
  # get age in days :
  age <- mdy(responses$date_death_deceased) - mdy(responses$date_birth_deceased)
  if( !is.na( age ) & age > 120 ){
    questions <- c("rash", "rash_face", "measles_rash", "fever")
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( questions )
    if(any(!is.na(answers))) {
      if( all( answers == "yes", na.rm=TRUE )){
        return( "measles")
      }
    }
  }
  
  # meningitis -----
  
  questions <- c("fever")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  if(any(!is.na(answers))) {
    if( any( answers == "yes", na.rm=TRUE )){
      questions <- c("stiff_neck", "bulging_fontanelle")
      questions <- questions[ which( questions %in% names(responses)) ]
      answers <- responses %>% select( questions )
      if( any( answers == "yes", na.rm=TRUE )){
        return("meningitis")
      }
    }
  }
  
  # dysentery -----
  
  questions <- c("diarrhea", "bloody_stool")
  # both have to be answered affirmatively!
  # questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select(questions)
  if (any(!is.na(answers))) {
    if (all(answers == "yes", na.rm = TRUE)) {
      # questions <- c("times_passed_stool",
      #                "times_diarrhea",
      #                "number_stools_per_day")
      # questions <- questions[which(questions %in% names(responses))]
      # answers <- responses %>% select(questions)
      # if (any(!is.na(answers))) {
      #   if (all(answers > 4, na.rm = TRUE)) {
          return("dysentery")
      #  }
      # }
    }
  }
  
  questions <- c("diarrhea", "bloody_stool")
  # both have to be answered affirmatively!
  # questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select(questions)
  if (any(!is.na(answers))) {
    if (all(answers == "yes", na.rm = TRUE)) {
      # NEED TO DEAL WITH THIS CONDITION!! INCONSISTENCY IN TRANSLATED DATASETS...
      # compare what happens here with VA_Child_WHO2016_babel.csv (Gambia) and 
      # VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel (Mali)
      questions <- c("days_diarrhea", "diarrhea_duration", "days_weeks_diarrhea")
      questions <- questions[ which( questions %in% names(responses)) ]
      answers <- responses %>% select(questions)
      # if (any(!is.na(answers))) {
        # if (all(answers > 4, na.rm = TRUE)) {
        #  return("dysentery")
        # }
      # }
    }
  }
  
  # diarrhea -----
  
  questions <- c("diarrhea")
  answers <- responses %>% select(questions)
  if (any(!is.na(answers))) {
    if (all(answers == "yes", na.rm = TRUE)) {
      # questions <- c("times_passed_stool",
      #                "times_diarrhea",
      #                "number_stools_per_day")
      # questions <- questions[which(questions %in% names(responses))]
      # answers <- responses %>% select(questions)
      # if (any(!is.na(answers))) {
      #   if (all(answers > 4, na.rm = TRUE)) {
          questions <- c("bloody_stool")
          answers <- responses %>% select(questions)
          if (any(!is.na(answers))) {
            if (all(answers == "no", na.rm = TRUE)) {
              return("diarrhea")
            }
          }
      #  }
      # }
    }
  }
  
  questions <- c("diarrhea", "bloody_stool")
  # both have to be answered affirmatively!
  # questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select(questions)
  if (any(!is.na(answers))) {
    if (all(answers == "yes", na.rm = TRUE)) {
      questions <- c("times_passed_stool",
                     "times_diarrhea",
                     "number_stools_per_day")
      questions <- questions[which(questions %in% names(responses))]
      answers <- responses %>% select(questions)
      if (any(!is.na(answers))) {
        if (all(answers > 4, na.rm = TRUE)) {
          return("dysentery")
        }
      }
    }
  }
  
  # pertussis -----
  
  # pneumonia -----
  
  # malaria -----
  
  # possible dysentery -----
  
  # possible diarrhea -----
  
  # possible pneumonia -----
  
  # hemorrhagic fever -----
  
  # other infection -----
  
  # residual infection -----
  
  # malnutrition -----
  
  # unspecified if none of the above
  return("unspecified")
}

# utility functions for testing/development

check_all <- function( answers ){
  for( i in 1:nrow(answers) ){
    if(any(!is.na(answers))) {
      if( all( answers[i,] == "yes", na.rm=TRUE)){
        cat("Nailed it!\n")
      } else{
        cat("Nothin'\n")
      }
    } else{
      cat("All NAs\n")
    }
  }
}

check_any <- function( answers ){
  for( i in 1:nrow(answers)){
    if(any(!is.na(answers))) {
      if( any( answers[i,] == "yes", na.rm=TRUE)){
        cat("Nailed it!\n")
      } else{
        cat("Nothin'\n")
      }
    } else{
      cat("All NAs\n")
    }
  }
}
