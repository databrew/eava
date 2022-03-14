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
  # same as JHU sas implementation
  
  questions <- c("die_at_site_of_injury_accident", "injury_accident", "road_accident",
                 "injury_fall", "drown", "accidentally_poisoned", "poisoning",
                 "animal", "venomous_animal", "burn", "assault", "other_injury" )
                 # "sign_injury_broken_bones")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  
  # concern: this is going to barf somewhere on files that don't use "yes", "no" !!
  if(any(!is.na(answers))) {
    if (any(answers == "yes", na.rm = TRUE)) {
      return("injury")
    }
  }
  
  # AIDS -----
  # duration of diarrhea, fever, skin rash all need to be checked!! but...
  # we lack duration variables, e.g., fever for > 30 days, diarrhea for > 30 days
  
  # rash_duration gives _units_ (e.g., weeks) in VA_ChildForm_WHO2012_babel ,
  # does not exist in VA_Child_WHO2016_babel or in
  # VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel
  
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
  
  # need to check duration of fever and rash!
  # fever :
  # for VA_ChildForm_WHO2012_babel, fever_duration provides units and days_weeks_fever provides values; no days_fever column
  # for VA_Child_WHO2016_babel, days_fever does the job
  # for VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel, days_fever again
  # rash :
  # for VA_ChildForm_WHO2012_babel, days_weeks_rash (?)
  # for VA_Child_WHO2016_babel, days_rash
  # for VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel, days_rash
  
  # get age in days :
  age <- difftime( mdy(responses$date_death_deceased), mdy(responses$date_birth_deceased), units="days")
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
  
  questions <- c("diarrhea")
  answers <- responses %>% select(questions)
  if (any(!is.na(answers))) {
    if (all(answers == "yes", na.rm = TRUE)) {
      questions <- c("bloody_stool_until_death", "bloody_stool", "diarrhea_bloody")
      questions <- questions[ which( questions %in% names(responses)) ]
      answers <- responses %>% select(questions)
      if (any(!is.na(answers))) {
        if (any(answers == "yes", na.rm = TRUE)) {
          return("dysentery") 
        }
      }
    }
  }
  
  # need to check frequency / duration!
      # questions <- c("times_passed_stool",
      #                "times_diarrhea",
      #                "number_stools_per_day")
      # questions <- questions[which(questions %in% names(responses))]
      # answers <- responses %>% select(questions)
      # if (any(!is.na(answers))) {
      #   if (all(answers > 4, na.rm = TRUE)) {
      #    return("dysentery")
      #  }
      # }
  
  # compare what happens with VA_Child_WHO2016_babel.csv (Gambia) and 
  # VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel (Mali)
  # for these questions:
  # questions <- c("days_diarrhea", "diarrhea_duration", "days_weeks_diarrhea")
  
  # diarrhea -----
  
  # as for dysentery, need to check frequency / duration
  
  questions <- c("diarrhea")
  answers <- responses %>% select(questions)
  if (any(!is.na(answers))) {
    if (all(answers == "yes", na.rm = TRUE)) {
      questions <- c("bloody_stool_until_death", "bloody_stool", "diarrhea_bloody")
      questions <- questions[which(questions %in% names(responses))]
      answers <- responses %>% select(questions)
      if (any(!is.na(answers))) {
        if (all(answers == "no", na.rm = TRUE)) {
          return("diarrhea")
        }
      }
    }
  }
  
  # pertussis -----
  
  # have to check duration of cough ; similar issues to above:
  # VA_ChildForm_WHO2012_babel has cough_duration (units) and days_weeks_cough (values)
  # VA_Child_WHO2016_babel uses days_cough
  # VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel uses days_cough
  
  # questions <- c("days_cough")
  # answers <- responses %>% select(questions)
  # if( answers > 14 ){
  #   questions <- c("cough_severe", "cough_vomit")
  #   questions <- questions[ which( questions %in% names(responses)) ]
  #   answers <- responses %>% select(questions)
  #   if (any(!is.na(answers))) {
  #     if (all(answers == "no", na.rm = TRUE)) {
  #       return("diarrhea")
  #     }
  #   }
  # }
  
  # pneumonia -----
  
  # have to check duration of cough, fast breathing
  
  # malaria -----
  
  # need to know if fever continued until death...
  # VA_ChildForm_WHO2012_babel uses fever_duration and days_weeks_fever
  # VA_Child_WHO2016_babel uses fever_continue
  # VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel uses fever_continue
  
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
