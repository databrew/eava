# implements the hierarchical expert algorithm for verbal autopsy described in 
# [Kalter et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4416334/) (doi: 10.7189/jogh.05.010415)

library(dplyr)
library(tidyr)
library(lubridate)
# library(vida)
# data('master')

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
  
  # symptom/illness durations for later use -----
  
  # for c12_16_map :
  # fever indicates fever (yes, no, dk)
  # fever_duration gives units (days, weeks, dk) 
  # days_weeks_fever provides value
  # the following computes duration of fever in DAYS
  if( !is.na( responses$fever ) & responses$fever == "yes" ){
    if( !is.na(responses$fever_duration) & (responses$fever_duration == "weeks") ){
      fever_duration <- 7*responses$days_weeks_fever
    } else if( !is.na(responses$fever_duration) & (responses$fever_duration == "days") ){
      fever_duration <- responses$days_weeks_fever
    } else{
      fever_duration <- NA
    }
  } else{
    fever_duration <- NA
  }
  
  # for c12_16_map :
  # rash indicates skin rash (yes, no, dk)
  # rash_duration gives units (days, weeks, dk) 
  # days_weeks_rash provides value
  # the following computes duration of rash in DAYS
  if( !is.na( responses$rash ) & responses$rash == "yes" ){
    if( !is.na(responses$rash_duration) & responses$rash_duration == "weeks"){
      rash_duration <- 7*responses$days_weeks_rash
    } else if( !is.na(responses$rash_duration) & responses$rash_duration == "days" ){
      rash_duration <- responses$days_weeks_rash
    } else{
      rash_duration <- NA
    }
  } else{
    rash_duration <- NA
  }
  
  # for c12_16_map :
  # cough indicates whether the child had a cough (yes, no, dk)
  # cough_duration gives units (days, weeks, dk) 
  # days_weeks_cough provides value
  # the following computes duration of cough in DAYS
  if( !is.na( responses$cough ) & responses$cough == "yes" ){
    if( !is.na(responses$cough_duration) & responses$cough_duration == "weeks"){
      cough_duration <- 7*responses$days_weeks_cough
    } else if( !is.na(responses$cough_duration) & responses$cough_duration == "days" ){
      cough_duration <- responses$days_weeks_cough
    } else{
      cough_duration <- NA
    }
  } else{
    cough_duration <- NA
  }
  
  # for c12_16_map :
  # diarrhea indicates whether the child had diarrhea (yes, no, dk)
  # diarrhea_duration gives units (days, weeks, dk) 
  # days_weeks_diarrhea provides value
  # the following computes duration of diarrhea in DAYS
  if( !is.na( responses$diarrhea ) & responses$diarrhea == "yes" ){
    if( !is.na(responses$diarrhea_duration) & responses$diarrhea_duration == "weeks"){
      diarrhea_duration <- 7*responses$days_weeks_diarrhea
    } else if( !is.na(responses$diarrhea_duration) & responses$diarrhea_duration == "days" ){
      diarrhea_duration <- responses$days_weeks_diarrhea
    } else{
      diarrhea_duration <- NA
    }
  } else{
    diarrhea_duration <- NA
  }
  
  # for c12_16_map :
  # fast_breathing indicates whether the child had fast breathing (yes, no, dk)
  # fast_breathing_duration gives units (days, weeks, dk) 
  # days_weeks_fast_breathing provides value
  # the following computes duration of fast breathing in DAYS
  if( !is.na( responses$fast_breathing ) & responses$fast_breathing == "yes" ){
    if( !is.na(responses$fast_breathing_duration) & responses$fast_breathing_duration == "weeks"){
      fast_breathing_duration <- 7*responses$days_weeks_fast_breathing
    } else if( !is.na(responses$fast_breathing_duration) & responses$fast_breathing_duration == "days" ){
      fast_breathing_duration <- responses$days_weeks_fast_breathing
    } else{
      fast_breathing_duration <- NA
    }
  } else{
    fast_breathing_duration <- NA
  }
  
  # injury -----
  # same as JHU sas implementation
  
  questions <- c("die_at_site_of_injury_accident", "injury_accident", "road_accident",
                 "injury_fall", "drown", "accidentally_poisoned", "poisoning",
                 "animal", "venomous_animal", "burn", "assault", "other_injury" )
                 # "sign_injury_broken_bones")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  
  if(any(!is.na(answers))) {
    if (any(answers == "yes", na.rm = TRUE)) {
      return("injury")
    }
  }
  
  # AIDS -----
  
  # duration of diarrhea, fever, skin rash all need to be checked!! but...

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
  
  # get age in days :
  age <- difftime( mdy(responses$date_death_deceased), mdy(responses$date_birth_deceased), units="days")
  if( !is.na( age ) & age > 120 ){
    if( !is.na(rash_duration) & (rash_duration > 3) & !is.na(fever_duration) & (fever_duration > 3)){
      return( "measles")
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
  
  if( (!is.na( diarrhea_duration ) & (diarrhea_duration > 14)) &
      (!is.na( responses$bloody_stool ) & (responses$bloody_stool == "yes")) ){
    return("dysentery")
  }
  
  # diarrhea -----
  
  if( (!is.na( diarrhea_duration ) & (diarrhea_duration > 14)) &
      (!is.na( responses$bloody_stool ) & (responses$bloody_stool == "no")) ){
    return("diarrhea")
  }
  
  # pertussis -----
  
  if( !is.na(cough_duration) & (cough_duration > 14) ){
    return("pertussis")
  }
  
  # pneumonia -----
  if( ( !is.na(responses$difficulty_breathing) & (responses$difficulty_breathing=="yes")) | 
      ( !is.na(cough_duration) & (cough_duration > 2) ) ){
    if( (!is.na( fast_breathing_duration ) & (fast_breathing_duration > 2)) | 
        (!is.na( responses$noisy_breathing) & (responses$noisy_breathing=="yes" )) |
        (!is.na( responses$chest_pull_in) & (responses$chest_pull_in=="yes" )) ){
      return("pneumonia")
    }
  }
  
  # malaria -----
  
  # fever continued until death?
  if( (!is.na( responses$fever ) & (responses$fever == "yes")) &
      (!is.na( responses$stiff_neck ) & (responses$stiff_neck == "no")) &
      # (!is.na( responses$bulging_fontanelle ) & (responses$bulging_fontanelle == "no")) &
      ( (!is.na(responses$difficulty_breathing) & (responses$difficulty_breathing == "yes")) |
        (!is.na(responses$convulsions) & (responses$convulsions == "yes")) |
        (!is.na(responses$unconscious) & (responses$unconscious == "yes")) ) ){
    return("malaria")
  }
  
  # possible dysentery -----
  
  if( (!is.na(responses$diarrhea) & (responses$diarrhea == "yes")) &
      (!is.na( responses$bloody_stool ) & (responses$bloody_stool == "yes")) &
      ( (!is.na(responses$fever) & (responses$fever == "yes")) | 
        (!is.na(responses$convulsions) & (responses$convulsions == "yes")) |
        (!is.na(responses$unconscious) & (responses$unconscious == "yes")) ) ){
    return("possible dysentery")
  }
  
  # possible diarrhea -----
  
  if( (!is.na(responses$diarrhea) & (responses$diarrhea == "yes")) &
      (!is.na( responses$bloody_stool ) & (responses$bloody_stool == "no")) &
      ( (!is.na(responses$fever) & (responses$fever == "yes")) | 
        (!is.na(responses$convulsions) & (responses$convulsions == "yes")) |
        (!is.na(responses$unconscious) & (responses$unconscious == "yes")) ) ){
    return("possible diarrhea")
  }
  
  # possible pneumonia -----
  
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
    return("possible pneumonia")
  }
  
  # hemorrhagic fever -----
  
  if( (!is.na(responses$fever) & (responses$fever=="yes")) &
      (!is.na(responses$bleed_nose_mouth_anus) & (responses$bleed_nose_mouth_anus == "yes")) ){
    return("hemorrhagic fever")
  }
  
  # other infection -----
  
  if( (!is.na(responses$fever) & (responses$fever=="yes")) & 
      ( ( !is.na(responses$rash) & (responses$rash=="yes")) |
        (!is.na(responses$convulsions) & (responses$convulsions=="yes")) | 
        (!is.na(responses$unconscious) & (responses$unconscious=="yes")) ) ){
    return("other infection")
  }
  
  # possible malaria -----
  
  if( !is.na(responses$fever) & (responses$fever == "yes") ){
    return("possible malaria")
  }
  
  # malnutrition -----
  
  if( (!is.na(responses$thin) & (responses$thin == "yes")) |
      (!is.na(responses$swell_feet) & (responses$swell_feet == "yes")) ){
    return("malnutrition")
  }
  
  # unspecified if none of the above
  return("unspecified")
}
