# implements the hierarchical expert algorithm for verbal autopsy described in 
# [Kalter et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4416334/) (doi: 10.7189/jogh.05.010415)

library(dplyr)
library(tidyr)
library(lubridate)
# library(vida)
# data('master')

supported_formats <- c("c12_16", "c16")

#' Determine causes of death for ALL decedents in a verbal autopsy (VA) dataset that has been
#' translated into `babel` format with the `vida` package
#' 
#' @param babel_data : a data.frame of VA responses for multiple decedents (typically from one study site)
#' @param format : mapper used in babel translation
#' @return causes : causes of death determined by the hierarchical expert algorithm 
#' 
get_causes <- function(babel_data, format) {
  
  # check availability of certain indicators -- depends on format, not on responses for an individual decedent
  question_names <- names( babel_data )
  cough_severe_available <- "cough_severe" %in% question_names
  cough_vomit_available <- "cough_vomit" %in% question_names
  difficulty_breathing_available <- "difficulty_breathing" %in% question_names
  days_difficulty_breathing_available <- "days_difficulty_breathing" %in% question_names
  noisy_breathing_available <- "noisy_breathing" %in% question_names
  fever_continue_available <- "fever_continue" %in% question_names
  fever_pattern_available <- "fever_pattern" %in% question_names

  if( format %in% supported_formats ){
    causes <- vector(mode = "character")
    for (i in 1:nrow(babel_data)) {
      causes[i] <- cod(babel_data[i,], 
                       format, 
                       cough_severe_available, 
                       cough_vomit_available,
                       difficulty_breathing_available,
                       days_difficulty_breathing_available,
                       noisy_breathing_available,
                       fever_continue_available,
                       fever_pattern_available)
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
cod <- function(responses, 
                format, 
                cough_severe_available, 
                cough_vomit_available, 
                difficulty_breathing_available, 
                days_difficulty_breathing_available,
                noisy_breathing_available,
                fever_continue_available,
                fever_pattern_available) {
  
  # durations of decedent's various conditions in DAYS
  days_fever <- fever_duration(responses, format) 
  days_rash <- rash_duration(responses, format) 
  days_cough <- cough_duration(responses, format) 
  days_diarrhea <- diarrhea_duration(responses, format) 
  days_fast_breathing <- fast_breathing_duration(responses, format) 
  days_difficulty_breathing <- difficulty_breathing_duration(responses, days_difficulty_breathing_available) 
  
  # calculate age in DAYS from dates of birth, death (more reliable than age provided in dataset)
  age <- age_in_days(responses, format) 
  
  # binaries for decedent's conditions 
  fever <- fever_p(responses, format)
  stiff_neck <- stiff_neck_p(responses, format)
  bulging_fontanelle <- bulging_fontanelle_p(responses, format)
  diarrhea <- diarrhea_p(responses, format)
  thin_limbs <- thin_limbs_p(responses, format)
  swollen_legs_feet <- swollen_legs_feet_p(responses, format)
  protruding_abdomen <- protruding_abdomen_p(responses, format)
  fast_breathing <- fast_breathing_p(responses, format)
  chest_indrawing <- chest_indrawing_p(responses, format)
  grunting <- grunting_p(responses, format)
  armpits <- armpits_p(responses, format)
  rash_mouth <- rash_mouth_p(responses, format)
  bloody_stool <- bloody_stool_p(responses, format)
  cough_severe <- cough_severe_p(responses, cough_severe_available)
  cough_vomit <- cough_vomit_p(responses, cough_vomit_available)
  difficulty_breathing <- difficulty_breathing_p(responses, difficulty_breathing_available)
  noisy_breathing <- noisy_breathing_p(responses, noisy_breathing_available)
  fever_continue <- fever_continue_p(responses, fever_continue_available)
  # START HERE NEXT!!!! AND CHECK FEVER PATTERN IN MALARIA
  fever_severe <- fever_severe_p( responses, format )
  convulsions <- convulsions_p( responses, format )
  unconscious <- unconscious_p( responses, format )
  
  # now check specific causes of death...
  
  if( injury(responses, format) ){
    return("Injury")
  }
  
  if( aids(armpits, rash_mouth, thin_limbs, protruding_abdomen, fast_breathing, chest_indrawing, days_diarrhea, days_fever, days_rash) ){
    return("AIDS")
  }
  
  if( underlying_malnutrition(thin_limbs, swollen_legs_feet) ){
    return("Malnutrition (underlying)")
  }
  
  if( measles(age, days_rash, days_fever) ){
    return("Measles")
  }
  
  if( meningitis(fever, stiff_neck, bulging_fontanelle) ){
    return("Meningitis")
  }
  
  if( dysentery(days_diarrhea, bloody_stool) ){
    return("Dysentery")
  }
  
  if( diarrhea(days_diarrhea, bloody_stool) ){
    return("Diarrhea")
  }
  
  if( pertussis(days_cough, cough_severe_available, cough_severe, cough_vomit_available, cough_vomit) ){
    return("Pertussis")
  }
  
  if( pneumonia(days_cough, days_difficulty_breathing, days_fast_breathing, chest_indrawing, noisy_breathing) ){
    return("Pneumonia")
  }
  
  if(malaria(responses, format)){
    return("Malaria")
  }
  
  if(possible_dysentery(responses, format)){
    return("Possible dysentery")
  }
  
  if(possible_diarrhea(responses, format)){
    return("Possible diarrhea")
  }
  
  if(possible_pneumonia(responses, format)){
    return("Possible pneumonia")
  }
  
  if(hemorrhagic_fever(responses, format)){
    return("Hemorrhagic fever")
  }
  
  if(other_infection(responses, format)){
    return("Other infection")
  }
  
  if(possible_malaria(responses, format)){
    return("Possible malaria")
  }
  
  if(malnutrition(responses, format)){
    return("Malnutrition")
  }
  
  # reaching this point means that no specific cause has been determined 
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
        return( 7*as.numeric(responses$days_weeks_fever) )
      } else if( !is.na(responses$fever_duration) & (responses$fever_duration == "days") ){
        return( as.numeric(responses$days_weeks_fever) )
      } else{
        return(0)
      }
    } else{
      return(0)
    }
  } else if( format == "c16" ){ 
    # days_fever is exactly what the name indicates
    if( !is.na( responses$days_fever) ){
      return( as.numeric(responses$days_fever) )
    } else{
      return(0)
    }
  } else{
    return(0)
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
        return( 7*as.numeric(responses$days_weeks_rash) )
      } else if( !is.na(responses$rash_duration) & responses$rash_duration == "days" ){
        return( as.numeric(responses$days_weeks_rash) )
      } else{
        return(0)
      }
    } else{
      return(0)
    }
  } else if( format == "c16" ){
      # days_rash is exactly what the name indicates
      if( !is.na(responses$days_rash) ){
        return(as.numeric(responses$days_rash))
      } else{
        return(0)
      }
  } else{
      return(0)
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
        return( 7*as.numeric(responses$days_weeks_cough) )
      } else if( !is.na(responses$cough_duration) & responses$cough_duration == "days" ){
        return( as.numeric(responses$days_weeks_cough) )
      } else{
        return(0)
      }
    } else{
      return( 0 )
    }
  } else if( format == "c16"){
    # days_cough is exactly what the name indicates
    if( !is.na( responses$days_cough) ){
      return(as.numeric(responses$days_cough))
    } else{
      return(0)
    }
  } else{
    return(0)
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
        return( 7*as.numeric(responses$days_weeks_diarrhea) )
      } else if( !is.na(responses$diarrhea_duration) & responses$diarrhea_duration == "days" ){
        return( as.numeric(responses$days_weeks_diarrhea) )
      } else{
        return(0)
      }
    } else{
      return(0)
    }
  } else if( format == "c16" ){
    # days_diarrhea is exactly what the name indicates
    if( !is.na( responses$days_diarrhea) ){
      return( as.numeric(responses$days_diarrhea) )
    } else{
      return(0)
    }
  } else{
    return(0)
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
        return( 7*as.numeric(responses$days_weeks_fast_breathing) )
      } else if( !is.na(responses$fast_breathing_duration) & responses$fast_breathing_duration == "days" ){
        return( as.numeric(responses$days_weeks_fast_breathing) )
      } else{
        return(0)
      }
    } else{
      return(0)
    }
  } else if( format == "c16" ){
    # days_fast_breathing is exactly what the name indicates
    if( !is.na(responses$days_fast_breathing) ){
      return( as.numeric(responses$days_fast_breathing) )
    } else{
      return(0)
    }
  } else{
    return(0)
  }
}

difficulty_breathing_duration <- function( days_difficulty_breathing_available ){
  if( days_difficulty_breathing_available ){
    if( !is.na(responses$days_difficulty_breathing) ){
      return( as.numeric(responses$days_difficulty_breathing) )
    } else{
      return(0)
    }
  } else{
    return(0)
  }
}

# Age -----

age_in_days <- function( responses, format ){
  # CHECK THE USE OF mdy HERE!!!
  if( format == "c12_16"){
    return( difftime( mdy(responses$date_death_deceased), mdy(responses$date_birth_deceased), units="days") )
  } else if( format == "c16" ){
    return( difftime( mdy(responses$date_death_deceased), mdy(responses$date_birth_deceased), units="days") )
  } else{
    return(0)
  }
}

# Binaries for conditions -----

fever_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$fever ) & (responses$fever=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

stiff_neck_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$stiff_neck ) & (responses$stiff_neck=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

bulging_fontanelle_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$bulging_fontanelle ) & (responses$bulging_fontanelle=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

diarrhea_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$diarrhea ) & (responses$diarrhea=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

thin_limbs_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$thin ) & (responses$thin=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

swollen_legs_feet_p <- function(responses, format){
  if( (format=="c12_16") ){
    if( !is.na( responses$swell_feet ) & (responses$swell_feet=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( format == "c16"){
    if( (!is.na(responses$swell_feet) & (responses$swell_feet=="yes")) | 
        (!is.na(responses$swell_leg) & (responses$swell_leg=="yes")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

protruding_abdomen_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$protruding_abdomen ) & (responses$protruding_abdomen=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

fast_breathing_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$fast_breathing ) & (responses$fast_breathing=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

chest_indrawing_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$chest_pull_in ) & (responses$chest_pull_in=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

armpits_p <- function(responses, format){
  if( format=="c12_16"){
    if( !is.na( responses$swell_armpits ) & (responses$swell_armpits=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( format=="c16"){
    if( !is.na( responses$lumps_armpit ) & (responses$lumps_armpit=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

rash_mouth_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$rash_mouth ) & (responses$rash_mouth=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

bloody_stool_p <- function(responses, format){
  if( (format=="c12_16") | (format=="c16") ){
    if( !is.na( responses$bloody_stool ) & (responses$bloody_stool=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

cough_severe_p <- function(responses, cough_severe_available ){
  if( cough_severe_available ){
    if( !is.na(responses$cough_severe) & (responses$cough_severe=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

cough_vomit_p <- function(responses, cough_vomit_available){
  if( cough_vomit_available ){
    if( !is.na(responses$cough_vomit) & (responses$cough_vomit=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

difficulty_breathing_p <- function(responses, difficulty_breathing_available){
  if( difficulty_breathing_available ){
    if( !is.na(responses$difficulty_breathing) & (responses$difficulty_breathing=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

noisy_breathing_p <- function(responses, noisy_breathing_available){
  if( noisy_breathing_available ){
    if( !is.na(responses$noisy_breathing) & (responses$noisy_breathing=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

fever_continue_p <- function(responses, fever_continue_available){
  if( fever_continue_available ){
    if( !is.na(responses$fever_continue) & (responses$fever_continue %in% c("yes", "continuous", "on_and_off", "on_off")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

# Injury -----

injury <- function( responses, format ){
  if( format == "c12_16"){
    questions <- c("injury_accident", "road_accident", "injury_fall", "drown", 
                   "poisoning", "animal", "burn", "assault", "other_injury" )
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( all_of(questions) )
  } else if( format == "c16" ){
    questions <- c("injury_accident", "road_accident", "venomous_animal", "burn",
                   "drown", "injury_fall", "other_injury", "poisoning", "assault" )
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( all_of(questions) )
  }
  
  if(any(!is.na(answers))) {
    if (any(answers == "yes", na.rm = TRUE)) {
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{ 
    return(FALSE)
  }
}

# AIDS -----

aids <- function( armpits, rash_mouth, thin_limbs, protruding_abdomen, fast_breathing, chest_indrawing, days_diarrhea, days_fever, days_rash ){
  if( (armpits | rash_mouth) ){
    signs <- thin_limbs + protruding_abdomen + fast_breathing + chest_indrawing + (days_diarrhea > 30) + (days_fever > 30) + (days_rash > 30) 
    if( signs >= 3 ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

# Malnutrition (underlying) -----

underlying_malnutrition <- function( thin_limbs, swollen_legs_feet ){
  return( thin_limbs | swollen_legs_feet )
}

# Measles -----

measles <- function( age, days_rash, days_fever ){
  if( (age > 120) & (days_rash >= 3) & (days_fever >= 3) ){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

# Meningitis -----

meningitis <- function( fever, stiff_neck, bulging_fontanelle ){
  return( fever & ( stiff_neck | bulging_fontanelle ) )
}

# Dysentery -----

dysentery <- function( days_diarrhea, bloody_stool ){
  return( (days_diarrhea > 14) & bloody_stool )
}

# Diarrhea -----

diarrhea <- function( days_diarrhea, bloody_stool ){
  return( (days_diarrhea > 14) & !bloody_stool )
}

# Pertussis -----

pertussis <- function( days_cough, cough_severe_available, cough_severe, cough_vomit_available, cough_vomit ){
  if( cough_severe_available & cough_vomit_available ){
    return( (days_cough > 14) & (cough_severe | cough_vomit) )
  } else if( cough_severe_available & !cough_vomit_available){
    return( (days_cough > 14) & cough_severe ) 
  } else if( !cough_severe_available & cough_vomit_available){
    return( (days_cough > 14) & cough_vomit ) 
  } else{ 
    return( (days_cough > 14) )
  }
}

# Pneumonia -----

pneumonia <- function( days_cough, days_difficulty_breathing, days_fast_breathing, chest_indrawing, noisy_breathing ){
  if( ( (days_cough > 2) | (days_difficulty_breathing > 2)) & 
      ( (days_fast_breathing > 2) | chest_indrawing | noisy_breathing ) ){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

# Malaria -----

malaria <- function( responses, fever_continue, fever_pattern_available, stiff_neck, 
                     bulging_fontanelle, difficulty_breathing, convulsions, unconscious ) {
  
  # if( format == "c12_16" ){
  #   # fever continued until death?
  #   if( (!is.na( responses$fever ) & (responses$fever == "yes")) &
  #       (!is.na( responses$stiff_neck ) & (responses$stiff_neck == "no")) &
  #       # (!is.na( responses$bulging_fontanelle ) & (responses$bulging_fontanelle == "no")) &
  #       ( (!is.na(responses$difficulty_breathing) & (responses$difficulty_breathing == "yes")) |
  #         (!is.na(responses$convulsions) & (responses$convulsions == "yes")) |
  #         (!is.na(responses$unconscious) & (responses$unconscious == "yes")) ) ){
  #     return(TRUE)
  #   } else{
  #     return(FALSE)
  #   }
  # } else{ 
  #   return(FALSE)
  # }
  
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
