# implements the hierarchical expert algorithm for verbal autopsy described in 
# [Liu et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4467513/) (doi: 10.7189/jogh.05.010414)

library(dplyr)
library(tidyr)
library(lubridate)
# library(vida)
# data('master')

supported_formats <- c("champs1", "champs2", "champs3", "child_mali", 
                       "va_4_weeks_to_59_months", "core_2", "core", "who2007_2",
                       "who2007", "who2010_2", "who2010", "who2012_2", "who2012",
                       "who2016", "c2012", "c2016", "c08_12", "c12_16", "c16")

death_dates <- function( babel_data, format ){
  if( format != "c08_12"){
    dates <- range( mdy(babel_data$date_death_deceased ), na.rm = TRUE)
    return( data.frame( "First death" = dates[1], "Last death"= dates[2] ) )
  } else{
    return( return( data.frame( "First death" = NA, "Last death"= NA ) ) )
  }
}

interview_dates <- function( babel_data, format ){
  question_names <- names( babel_data )
  interview_date <- "interview_date" %in% question_names
  visit_date <- "visit_date" %in% question_names
  if( interview_date ){
    dates <- range( mdy(babel_data$interview_date ), na.rm = TRUE)
    return( data.frame( "First interview" = dates[1], "Last interview"= dates[2] ) )
  } else if( visit_date ){
    dates <- range( mdy(babel_data$visit_date ), na.rm = TRUE)
    return( data.frame( "First visit" = dates[1], "Last visit"= dates[2] ) )
  } else{
    return( data.frame( "First interview" = NA, "Last interview"= NA ) )
  }
}

#' Determine causes of death for ALL decedents in a verbal autopsy (VA) dataset that has been
#' translated into `babel` format with the `vida` package
#' 
#' @param babel_data : a data.frame of VA responses for multiple decedents (typically from one study site)
#' @param format : mapper used in babel translation
#' @return causes : causes of death determined by the hierarchical expert algorithm 
#' 
get_causes <- function(babel_data, format) {
  
  # check availability of certain indicators -- depends on format, not on responses for an individual decedent
  question_names <<- names( babel_data )
  fever_available <<- "fever" %in% question_names
  cough_available <<- "cough" %in% question_names
  stiff_neck_available <<- "stiff_neck" %in% question_names
  bulging_fontanelle_available <<- "bulging_fontanelle" %in% question_names
  diarrhea_available <<- "diarrhea" %in% question_names
  swell_feet_available <<- "swell_feet" %in% question_names
  swell_leg_available <<- "swell_leg" %in% question_names
  swell_armpits_available <<- "swell_armpits" %in% question_names
  lumps_armpit_available <<- "lumps_armpit" %in% question_names
  rash_mouth_available <<- "rash_mouth" %in% question_names
  bloody_stool_available <<- "bloody_stool" %in% question_names
  thin_available <<- "thin" %in% question_names
  protruding_abdomen_available <<- "protruding_abdomen" %in% question_names
  fast_breathing_available <<- "fast_breathing" %in% question_names
  chest_indrawing_available <<- "chest_pull_in" %in% question_names
  cough_severe_available <<- "cough_severe" %in% question_names
  cough_vomit_available <<- "cough_vomit" %in% question_names
  difficulty_breathing_available <<- "difficulty_breathing" %in% question_names
  days_difficulty_breathing_available <<- "days_difficulty_breathing" %in% question_names
  noisy_breathing_available <<- "noisy_breathing" %in% question_names
  fever_continue_available <<- "fever_continue" %in% question_names
  fever_pattern_available <<- "fever_pattern" %in% question_names
  fever_severe_available <<- "fever_severe" %in% question_names
  fever_severity_available <<- "fever_severity" %in% question_names
  convulsions_available <<- "convulsions" %in% question_names
  unconscious_available <<- "unconscious" %in% question_names
  skin_black_available <<- "skin_black" %in% question_names
  place_rash_available <<- "place_rash" %in% question_names
  rash_body_available <<- "rash_body" %in% question_names
  rash_trunk_available <<- "rash_trunk" %in% question_names
  measles_rash_available <<- "measles_rash" %in% question_names
  rash_face_available <<- "rash_face" %in% question_names
  rash_look_available <<- "rash_look" %in% question_names
  red_eyes_available <<- "red_eyes" %in% question_names
  unresponsive_available <<- "unresponsive" %in% question_names
  yellow_eyes_available <<- "yellow_eyes" %in% question_names
  yellow_skin_available <<- "yellow_skin" %in% question_names
  pale_available <<- "pale" %in% question_names
  hair_red_yellow_available <<- "hair_red_yellow" %in% question_names
  tb_available <<- "tb" %in% question_names
  times_diarrhea_available <<- "times_diarrhea" %in% question_names
  number_stools_per_day_available <<- "number_stools_per_day" %in% question_names
  times_passed_stool_available <<- "times_passed_stool" %in% question_names
  sunken_eye_available <<- "sunken_eye" %in% question_names
  sunken_fontanelle_available <<- "sunken_fontanelle" %in% question_names
  flaring_nostrils_available <<- "flaring_nostrils" %in% question_names

  if( format %in% supported_formats ){
    causes <- vector(mode = "character")
    for (i in 1:nrow(babel_data)) {
      causes[i] <- cod( babel_data[i,], format )
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
cod <- function(responses, format){
  
  # calculate age in DAYS from dates of birth, death (more reliable than age provided in dataset (?) )
  age <- age_in_days(responses, format) 
  
  # if( is.na(age) | (age < 29 ) ){
  #   # apply neonatal algorithm instead!
  #   return("N/A")
  # }
  
  # durations of decedent's various conditions in DAYS
  days_fever <- fever_duration(responses, format) 
  days_rash <- rash_duration(responses, format) 
  days_cough <- cough_duration(responses, format) 
  days_diarrhea <- diarrhea_duration(responses, format) 
  days_fast_breathing <- fast_breathing_duration(responses, format) 
  days_difficulty_breathing <- difficulty_breathing_duration( responses ) 
  
  # peak stool count
  number_stools <- stool_count( responses )
  
  # binaries for decedent's conditions 
  fever <- fever_p( responses )
  cough <- cough_p( responses )
  stiff_neck <- stiff_neck_p( responses )
  bulging_fontanelle <- bulging_fontanelle_p( responses )
  diarrhea <- diarrhea_p( responses )
  thin_limbs <- thin_p( responses )
  swollen_legs_feet <- swollen_legs_feet_p( responses )
  protruding_abdomen <- protruding_abdomen_p( responses )
  fast_breathing <- fast_breathing_p( responses )
  chest_indrawing <- chest_indrawing_p( responses )
  armpits <- armpits_p( responses )
  rash_mouth <- rash_mouth_p( responses )
  bloody_stool <- bloody_stool_p( responses )
  cough_severe <- cough_severe_p( responses )
  cough_vomit <- cough_vomit_p( responses )
  difficulty_breathing <- difficulty_breathing_p( responses )
  # noisy_breathing a.k.a. grunting
  noisy_breathing <- noisy_breathing_p( responses )
  fever_continue <- fever_continue_p( responses )
  fever_on_off <- fever_on_off_p( responses )
  fever_severe <- fever_severe_p( responses )
  convulsions <- convulsions_p( responses )
  unconscious <- unconscious_p( responses )
  skin_black <- skin_black_p( responses )
  bled_anywhere <- bled_anywhere_p( responses )
  rash_trunk <- rash_trunk_p( responses )
  measles_rash <- measles_rash_p( responses )
  red_eyes <- red_eyes_p( responses )
  unresponsive <- unresponsive_p( responses )
  jaundice <- jaundice_p( responses )
  pale <- pale_p( responses )
  hair_change <- hair_change_p( responses )
  tb <- tb_p( responses )
  sunken_eye <- sunken_eye_p( responses )
  sunken_fontanelle <- sunken_fontanelle_p( responses )
  flaring_nostrils <- flaring_nostrils_p( responses )
  
  # now check specific causes of death...
  
  if( injury( responses ) ){
    return("Injury")
  }
  
  if( measles(age, fever, measles_rash, cough, red_eyes ) ){
    return("Measles")
  }
  
  if( meningitis(fever, convulsions, stiff_neck, bulging_fontanelle, unconscious) ){
    return("Meningitis")
  }
  
  if( malaria(fever, difficulty_breathing, convulsions, unresponsive) ){
    return("Malaria")
  }
  
  if( aids(jaundice, fever, days_diarrhea, days_fever, pale, hair_change, swollen_legs_feet, days_cough, days_difficulty_breathing, tb) ){
    return("AIDS")
  }
  
  if( diarrhea(diarrhea, days_diarrhea, number_stools, sunken_eye, sunken_fontanelle) ){
    return("Diarrhea")
  }
  
  if( ari(days_cough, difficulty_breathing, noisy_breathing, chest_indrawing, flaring_nostrils) ){
    return("Acute Respiratory Infection")
  }
  
  if( possible_pneumonia(cough, difficulty_breathing, chest_indrawing, fever, convulsions) ){
    return("Possible pneumonia")
  }
  
  if( possible_diarrhea(diarrhea, difficulty_breathing, chest_indrawing, fever, convulsions) ){
    return("Possible diarrhea")
  }
  
  # reaching this point means that no specific cause has been determined 
  return("Unspecified")
  
}

# Durations -----

fever_duration <- function(responses, format) {
  if( format %in% c("c12_16", "c2012") ){
    # fever indicates fever (yes, no, dk)
    # fever_duration gives units (days, weeks, dk) 
    # days_weeks_fever provides value
    # the following computes duration of fever in DAYS
    if( !is.na( responses$fever ) & responses$fever == "yes" ){
      if( !is.na(responses$fever_duration) & (responses$fever_duration == "weeks") ){
        if( !is.na(as.numeric(responses$days_weeks_fever))){
          return( 7*as.numeric(responses$days_weeks_fever) )
        } else{
          return(0)
        }
      } else if( !is.na(responses$fever_duration) & (responses$fever_duration == "days") ){
        if( !is.na(as.numeric(responses$days_weeks_fever))){
          return( as.numeric(responses$days_weeks_fever) )
        } else{
          return(0)
        }
      } else{
        return(0)
      }
    } else{
      return(0)
    }
  } else if( format %in% c("c08_12", "c16", "c2016", "champs1", "champs2", "champs3", "child_mali", 
                           "core", "core_2", "va_4_weeks_to_59_months", "who2007", "who2007_2", 
                           "who2010", "who2010_2", "who2012_2", "who2016") ){ 
    # days_fever is exactly what the name indicates
    if( !is.na( responses$days_fever) & !is.na(as.numeric(responses$days_fever))){
      return( as.numeric(responses$days_fever) )
    } else{
      return(0)
    }
  } else{
    return(0)
  }
}

rash_duration <- function(responses, format) {
  if( format %in% c("c12_16", "c2012") ){
    # rash indicates skin rash (yes, no, dk)
    # rash_duration gives units (days, weeks, dk) 
    # days_weeks_rash provides value
    # the following computes duration of rash in DAYS
    if( !is.na( responses$rash ) & responses$rash == "yes" ){
      if( !is.na(responses$rash_duration) & responses$rash_duration == "weeks"){
        if( !is.na( as.numeric(responses$days_weeks_rash) )){
          return( 7*as.numeric(responses$days_weeks_rash) )
        } else{
          return(0)
        }
      } else if( !is.na(responses$rash_duration) & responses$rash_duration == "days" ){
        if( !is.na( as.numeric(responses$days_weeks_rash) )){
          return( as.numeric(responses$days_weeks_rash) )
        } else{
          return(0)
        }
      } else{
        return(0)
      }
    } else{
      return(0)
    }
  } else if( format %in% c("c16", "c2016", "champs1", "champs2", "champs3", "child_mali", 
                           "core_2", "va_4_weeks_to_59_months", "who2007_2", 
                           "who2010_2", "who2012_2", "who2016")  ){
    # days_rash is exactly what the name indicates
    if( !is.na(responses$days_rash) & !is.na(as.numeric(responses$days_rash)) ){
      return(as.numeric(responses$days_rash))
    } else{
      return(0)
    }
  } else{
    return(0)
  }
}

cough_duration <- function(responses, format) {
  if( format %in% c("c12_16", "c2012") ){
    # cough indicates whether the child had a cough (yes, no, dk)
    # cough_duration gives units (days, weeks, dk) 
    # days_weeks_cough provides value
    # the following computes duration of cough in DAYS
    if( !is.na( responses$cough ) & responses$cough == "yes" ){
      if( !is.na(responses$cough_duration) & responses$cough_duration == "weeks"){
        if(!is.na(as.numeric(responses$days_weeks_cough))){
          return( 7*as.numeric(responses$days_weeks_cough) )
        } else{
          return(0)
        }
      } else if( !is.na(responses$cough_duration) & responses$cough_duration == "days" ){
        if(!is.na(as.numeric(responses$days_weeks_cough))){
          return( as.numeric(responses$days_weeks_cough) )
        } else{
          return(0)
        }
      } else{
        return(0)
      }
    } else{
      return( 0 )
    }
  } else if( format %in% c("c08_12", "c16", "c2016", "champs1", "champs2", "champs3", "child_mali", 
                           "core", "core_2", "va_4_weeks_to_59_months", "who2007", "who2007_2", 
                           "who2010", "who2010_2", "who2012_2", "who2016") ){
    # days_cough is exactly what the name indicates
    if( !is.na( responses$days_cough) & !is.na(as.numeric(responses$days_cough)) ){
      return(as.numeric(responses$days_cough))
    } else{
      return(0)
    }
  } else{
    return(0)
  }
}

diarrhea_duration <- function(responses, format) {
  if( format %in% c("c12_16", "c2012") ){
    # diarrhea indicates whether the child had diarrhea (yes, no, dk)
    # diarrhea_duration gives units (days, weeks, dk) 
    # days_weeks_diarrhea provides value
    # the following computes duration of diarrhea in DAYS
    if( !is.na( responses$diarrhea ) & responses$diarrhea == "yes" ){
      if( !is.na(responses$diarrhea_duration) & responses$diarrhea_duration == "weeks"){
        if(!is.na(as.numeric(responses$days_weeks_diarrhea))){
          return( 7*as.numeric(responses$days_weeks_diarrhea) )
        } else{
          return(0)
        }
      } else if( !is.na(responses$diarrhea_duration) & responses$diarrhea_duration == "days" ){
        if(!is.na(as.numeric(responses$days_weeks_diarrhea))){
          return( as.numeric(responses$days_weeks_diarrhea) )
        } else{
          return(0)
        }
      } else{
        return(0)
      }
    } else{
      return(0)
    }
  } else if( format %in% c("c08_12", "c16", "c2016", "champs1", "champs2", "champs3", "child_mali", 
                           "core", "core_2", "va_4_weeks_to_59_months", "who2007", "who2007_2", 
                           "who2010", "who2010_2", "who2012_2", "who2016") ){
    # days_diarrhea is exactly what the name indicates
    if( !is.na( responses$days_diarrhea) & !is.na(as.numeric(responses$days_diarrhea)) ){
      return( as.numeric(responses$days_diarrhea) )
    } else{
      return(0)
    }
  } else{
    return(0)
  }
}

fast_breathing_duration <- function(responses, format) {
  if( format %in% c("c12_16", "c2012") ){
    # fast_breathing indicates whether the child had fast breathing (yes, no, dk)
    # fast_breathing_duration gives units (days, weeks, dk) 
    # days_weeks_fast_breathing provides value
    # the following computes duration of fast breathing in DAYS
    if( !is.na( responses$fast_breathing ) & responses$fast_breathing == "yes" ){
      if( !is.na(responses$fast_breathing_duration) & responses$fast_breathing_duration == "weeks"){
        if(!is.na(as.numeric(responses$days_weeks_fast_breathing))){
          return( 7*as.numeric(responses$days_weeks_fast_breathing) )
        } else{
          return(0)
        }
      } else if( !is.na(responses$fast_breathing_duration) & responses$fast_breathing_duration == "days" ){
        if(!is.na(as.numeric(responses$days_weeks_fast_breathing))){
          return( as.numeric(responses$days_weeks_fast_breathing) )
        } else{
          return(0)
        }
      } else{
        return(0)
      }
    } else{
      return(0)
    }
  } else if( format %in% c("c16", "c2016", "champs1", "champs2", "champs3", "child_mali", 
                           "core", "va_4_weeks_to_59_months", "who2016") ){
    # days_fast_breathing is exactly what the name indicates
    if( !is.na(responses$days_fast_breathing) & !is.na(as.numeric(responses$days_fast_breathing)) ){
      return( as.numeric(responses$days_fast_breathing) )
    } else{
      return(0)
    }
  } else{
    return(0)
  }
}

difficulty_breathing_duration <- function( responses ){
  if( days_difficulty_breathing_available ){
    if( !is.na(responses$days_difficulty_breathing) & !is.na(as.numeric(responses$days_difficulty_breathing)) ){
      return( as.numeric(responses$days_difficulty_breathing) )
    } else{
      return(0)
    }
  } else{
    return(0)
  }
}

# Stool count -----

stool_count <- function( responses ){
  if( times_diarrhea_available ){
    if( !is.na(responses$times_diarrhea) & !is.na(as.numeric(responses$times_diarrhea)) ){
      return( as.numeric(responses$times_diarrhea) )
    } else{
      return(0)
    }
  } else if( number_stools_per_day_available ){
    if( !is.na(responses$number_stools_per_day) & !is.na(as.numeric(responses$number_stools_per_day)) ){
      return( as.numeric(responses$number_stools_per_day) )
    } else{
      return(0)
    }
  } else if( times_passed_stool_available ){
    if( !is.na(responses$times_passed_stool) & !is.na(as.numeric(responses$times_passed_stool)) ){
      return( as.numeric(responses$times_passed_stool) )
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
  if( format != "c08_12"){
    return( difftime( mdy(responses$date_death_deceased), mdy(responses$date_birth_deceased), units="days") )
  } else{
    return(0)
  }
}

# Binaries for conditions -----

fever_p <- function(responses){
  if( fever_available ){
    if( !is.na( responses$fever ) & (responses$fever=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

cough_p <- function(responses){
  if( cough_available ){
    if( !is.na( responses$cough ) & (responses$cough=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

stiff_neck_p <- function(responses){
  if( stiff_neck_available ){
    if( !is.na( responses$stiff_neck ) & (responses$stiff_neck=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

bulging_fontanelle_p <- function(responses){
  if( bulging_fontanelle_available ){
    if( !is.na( responses$bulging_fontanelle ) & (responses$bulging_fontanelle=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

diarrhea_p <- function(responses){
  if( diarrhea_available ){
    if( !is.na( responses$diarrhea ) & (responses$diarrhea=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

thin_p <- function(responses){
  if( thin_available ){
    if( !is.na( responses$thin ) & (responses$thin=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

swollen_legs_feet_p <- function(responses){
  if( swell_leg_available & swell_feet_available ){
    if( (!is.na(responses$swell_feet) & (responses$swell_feet=="yes")) | 
        (!is.na(responses$swell_leg) & (responses$swell_leg=="yes")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( swell_feet_available & !swell_leg_available ){
    if( !is.na( responses$swell_feet ) & (responses$swell_feet=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( swell_leg_available & !swell_feet_available ){
    if( !is.na(responses$swell_leg) & (responses$swell_leg=="yes") ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

protruding_abdomen_p <- function(responses){
  if( protruding_abdomen_available ){
    if( !is.na( responses$protruding_abdomen ) & (responses$protruding_abdomen=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

fast_breathing_p <- function(responses){
  if( fast_breathing_available ){
    if( !is.na( responses$fast_breathing ) & (responses$fast_breathing=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

chest_indrawing_p <- function(responses){
  if( chest_indrawing_available ){
    if( !is.na( responses$chest_pull_in ) & (responses$chest_pull_in=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

armpits_p <- function(responses){
  if( swell_armpits_available ){
    if( !is.na( responses$swell_armpits ) & (responses$swell_armpits=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( lumps_armpit_available ){
    if( !is.na( responses$lumps_armpit ) & (responses$lumps_armpit=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

rash_mouth_p <- function(responses){
  if( rash_mouth_available ){
    if( !is.na( responses$rash_mouth ) & (responses$rash_mouth=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

bloody_stool_p <- function(responses){
  if( bloody_stool_available ){
    if( !is.na( responses$bloody_stool ) & (responses$bloody_stool=="yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

cough_severe_p <- function(responses ){
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

cough_vomit_p <- function(responses){
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

difficulty_breathing_p <- function(responses){
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

noisy_breathing_p <- function(responses){
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

fever_continue_p <- function(responses){
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

fever_on_off_p <- function( responses ){
  if( fever_pattern_available ){
    if( !is.na(responses$fever_pattern) & (responses$fever_pattern =="on_and_off")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( fever_continue_available ){
    if( !is.na(responses$fever_continue) & (responses$fever_continue %in% c("on_and_off", "on_off")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

fever_severe_p <- function( responses ){
  if( fever_severe_available ){
    if( !is.na(responses$fever_severe) & (responses$fever_severe %in% c("yes","severe")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( fever_severity_available ){
    if( !is.na(responses$fever_severity) & (responses$fever_severity=="severe")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

convulsions_p <- function( responses ){
  if( convulsions_available ){
    if(!is.na(responses$convulsions) & (responses$convulsions == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

unconscious_p <- function( responses ){
  if( unconscious_available ){
    if(!is.na(responses$unconscious) & (responses$unconscious == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

skin_black_p <- function(responses){
  if( skin_black_available ){
    if(!is.na(responses$skin_black) & (responses$skin_black == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

bled_anywhere_p <- function( responses ){
  questions <- c("bloody_stool", "cough_blood", "bloody_vomit", "urine_blood", "bleed_nose_mouth_anus", "bleed" )
  questions <- questions[ which( questions %in% names( responses )) ]
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
}

rash_trunk_p <- function( responses ){
  if( place_rash_available ){
    if( !is.na(responses$place_rash) & (responses$place_rash %in% c("trunk","everywhere")) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( rash_trunk_available ){
    if( !is.na( responses$rash_trunk) & (responses$rash_trunk == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( rash_body_available ){
    if( !is.na( responses$rash_body) & (responses$rash_body == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

measles_rash_p <- function( responses ){
  if( measles_rash_available ){
    if( !is.na(responses$measles_rash) & (responses$measles_rash == "yes") ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( rash_body_available & rash_face_available ){
    if( !is.na( responses$rash_body) & (responses$rash_body == "yes") &
        !is.na( responses$rash_face) & (responses$rash_face == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( rash_body_available & !rash_face_available ){
    if( !is.na( responses$rash_body) & (responses$rash_body == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( !rash_body_available & rash_face_available ){
    if( !is.na( responses$rash_face) & (responses$rash_face == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( rash_look_available ){
    if( !is.na( responses$rash_look) & (responses$rash_look == "measles_rash")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

red_eyes_p <- function( responses ){
  if( red_eyes_available ){
    if(!is.na(responses$red_eyes) & (responses$red_eyes == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

unresponsive_p <- function( responses ){
  if( unresponsive_available ){
    if(!is.na(responses$unresponsive) & (responses$unresponsive == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

jaundice_p <- function( responses ){
  if( yellow_eyes_available & yellow_skin_available ){
    if(!is.na(responses$yellow_eyes) & (responses$yellow_eyes == "yes") &
       !is.na(responses$yellow_skin) & (responses$yellow_skin == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( yellow_eyes_available & !yellow_skin_available ){
    if( !is.na( responses$yellow_eyes) & (responses$yellow_eyes == "yes") ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else if( !yellow_eyes_available & yellow_skin_available ){
    if( !is.na( responses$yellow_skin) & (responses$yellow_skin == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

pale_p <- function( responses ){
  if( pale_available ){
    if(!is.na(responses$pale) & (responses$pale == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

hair_change_p <- function( responses ){
  if( hair_red_yellow_available ){
    if(!is.na(responses$hair_red_yellow) & (responses$hair_red_yellow == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

tb_p <- function( responses ){
  if( tb_available ){
    if(!is.na(responses$tb) & (responses$tb == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

sunken_eye_p <- function( responses ){
  if( sunken_eye_available ){
    if(!is.na(responses$sunken_eye) & (responses$sunken_eye == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

sunken_fontanelle_p <- function( responses ){
  if( sunken_fontanelle_available ){
    if(!is.na(responses$sunken_fontanelle) & (responses$sunken_fontanelle == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

flaring_nostrils_p <- function(responses ){
  if( flaring_nostrils_available ){
    if(!is.na(responses$flaring_nostrils) & (responses$flaring_nostrils == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

# Injury -----

injury <- function( responses ){
  questions <- c("injury_accident", "road_accident", "venomous_animal", "animal", "burn",
                 "drown", "injury_fall", "other_injury", "poisoning", "assault")
  questions <- questions[ which( questions %in% names( responses )) ]
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
}

# Measles -----

measles <- function( age, fever, measles_rash, cough, red_eyes ){
  return( (age > 180) & measles_rash & fever & (cough | red_eyes) )
}

# Meningitis -----

meningitis <- function( fever, convulsions, stiff_neck, bulging_fontanelle, unconscious ){
  return( fever & convulsions & ( stiff_neck | bulging_fontanelle ) & unconscious )
}

# Malaria -----

malaria <- function( fever, difficulty_breathing, convulsions, unresponsive ) {
  return( fever & (difficulty_breathing | convulsions | unresponsive) )
}

# AIDS -----

aids <- function( jaundice, fever, days_diarrhea, days_fever, pale, hair_change, swollen_legs_feet, days_cough, days_difficulty_breathing, tb ){
  return( jaundice | (days_diarrhea >= 29) | (days_fever >= 29) | ( pale & hair_change & swollen_legs_feet ) | 
            ( ((days_cough >= 3) | (days_difficulty_breathing >= 3)) & fever & !tb ) )
}

# Diarrhea -----

diarrhea <- function( diarrhea, days_diarrhea, number_stools, sunken_eye, sunken_fontanelle ){
  return( (diarrhea & (days_diarrhea < 14) & (number_stools >= 6) & (sunken_eye & sunken_fontanelle)) | 
            (diarrhea & (days_diarrhea >= 14)) )
}

# Acute Respiratory Infection -----

ari <- function( days_cough, difficulty_breathing, noisy_breathing, chest_indrawing, flaring_nostrils ){
  return( ((days_cough >= 3) | difficulty_breathing ) & ( noisy_breathing + chest_indrawing + flaring_nostrils >= 2 ) )
}

# Possible pneumonia -----

possible_pneumonia <- function( cough, difficulty_breathing, chest_indrawing, fever, convulsions ){
  return( ( difficulty_breathing + chest_indrawing + convulsions + fever >= 2 ) & (cough | difficulty_breathing) )
}

# Possible diarrhea -----

possible_diarrhea <- function( diarrhea, difficulty_breathing, chest_indrawing, fever, convulsions ){
  return( ( difficulty_breathing + chest_indrawing + convulsions + fever >= 2 ) & diarrhea )
}

# for testing :
get_ages <- function( babel_data, format ){
  ages <- numeric()
  for(i in 1:nrow(babel_data)){
    ages[i] <- age_in_days(babel_data[i,], format)
  }
  return(ages)
}
