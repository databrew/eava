# implements the hierarchical expert algorithm for verbal autopsy described in 
# [Kalter et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4416334/) (doi: 10.7189/jogh.05.010415)

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
  question_names <- names( babel_data )
  fever_available <- "fever" %in% question_names
  cough_available <- "cough" %in% question_names
  stiff_neck_available <- "stiff_neck" %in% question_names
  bulging_fontanelle_available <- "bulging_fontanelle" %in% question_names
  diarrhea_available <- "diarrhea" %in% question_names
  swell_feet_available <- "swell_feet" %in% question_names
  swell_leg_available <- "swell_leg" %in% question_names
  swell_armpits_available <- "swell_armpits" %in% question_names
  lumps_armpit_available <- "lumps_armpit" %in% question_names
  rash_mouth_available <- "rash_mouth" %in% question_names
  bloody_stool_available <- "bloody_stool" %in% question_names
  thin_available <- "thin" %in% question_names
  protruding_abdomen_available <- "protruding_abdomen" %in% question_names
  fast_breathing_available <- "fast_breathing" %in% question_names
  chest_indrawing_available <- "chest_pull_in" %in% question_names
  cough_severe_available <- "cough_severe" %in% question_names
  cough_vomit_available <- "cough_vomit" %in% question_names
  difficulty_breathing_available <- "difficulty_breathing" %in% question_names
  days_difficulty_breathing_available <- "days_difficulty_breathing" %in% question_names
  noisy_breathing_available <- "noisy_breathing" %in% question_names
  fever_continue_available <- "fever_continue" %in% question_names
  fever_pattern_available <- "fever_pattern" %in% question_names
  fever_severe_available <- "fever_severe" %in% question_names
  fever_severity_available <- "fever_severity" %in% question_names
  convulsions_available <- "convulsions" %in% question_names
  unconscious_available <- "unconscious" %in% question_names
  skin_black_available <- "skin_black" %in% question_names
  place_rash_available <- "place_rash" %in% question_names
  rash_body_available <- "rash_body" %in% question_names
  rash_trunk_available <- "rash_trunk" %in% question_names

  if( format %in% supported_formats ){
    causes <- list(mode = "character")
    for (i in 1:nrow(babel_data)) {
      causes[[i]] <- cod(babel_data[i,], 
                       format,
                       fever_available,
                       cough_available,
                       stiff_neck_available,
                       bulging_fontanelle_available,
                       diarrhea_available,
                       swell_feet_available,
                       swell_leg_available,
                       swell_armpits_available,
                       lumps_armpit_available,
                       rash_mouth_available,
                       bloody_stool_available,
                       thin_available,
                       protruding_abdomen_available,
                       fast_breathing_available,
                       chest_indrawing_available,
                       cough_severe_available, 
                       cough_vomit_available,
                       difficulty_breathing_available,
                       days_difficulty_breathing_available,
                       noisy_breathing_available,
                       fever_continue_available,
                       fever_pattern_available,
                       fever_severe_available,
                       fever_severity_available,
                       convulsions_available,
                       unconscious_available,
                       skin_black_available,
                       place_rash_available,
                       rash_body_available,
                       rash_trunk_available)
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
                fever_available,
                cough_available,
                stiff_neck_available,
                bulging_fontanelle_available,
                diarrhea_available,
                swell_feet_available,
                swell_leg_available,
                swell_armpits_available,
                lumps_armpit_available,
                rash_mouth_available,
                bloody_stool_available,
                thin_available,
                protruding_abdomen_available,
                fast_breathing_available,
                chest_indrawing_available,
                cough_severe_available, 
                cough_vomit_available,
                difficulty_breathing_available,
                days_difficulty_breathing_available,
                noisy_breathing_available,
                fever_continue_available,
                fever_pattern_available,
                fever_severe_available,
                fever_severity_available,
                convulsions_available,
                unconscious_available,
                skin_black_available,
                place_rash_available,
                rash_body_available,
                rash_trunk_available) {
  
  causes <- character()
  
  # durations of decedent's various conditions in DAYS
  days_fever <- fever_duration(responses, format) 
  days_rash <- rash_duration(responses, format) 
  days_cough <- cough_duration(responses, format) 
  days_diarrhea <- diarrhea_duration(responses, format) 
  days_fast_breathing <- fast_breathing_duration(responses, format) 
  days_difficulty_breathing <- difficulty_breathing_duration(responses, days_difficulty_breathing_available) 
  
  # calculate age in DAYS from dates of birth, death (more reliable than age provided in dataset)
  age <- age_in_days(responses, format) 
  
  if( is.na(age) | (age < 29 ) ){
    # apply neonatal algorithm instead!
    return("Neonate")
  }
  
  # binaries for decedent's conditions 
  fever <- fever_p(responses, fever_available)
  cough <- cough_p(responses, cough_available)
  stiff_neck <- stiff_neck_p(responses, stiff_neck_available)
  bulging_fontanelle <- bulging_fontanelle_p(responses, bulging_fontanelle_available)
  diarrhea <- diarrhea_p(responses, diarrhea_available)
  thin_limbs <- thin_p(responses, thin_available)
  swollen_legs_feet <- swollen_legs_feet_p(responses, swell_feet_available, swell_leg_available)
  protruding_abdomen <- protruding_abdomen_p(responses, protruding_abdomen_available)
  fast_breathing <- fast_breathing_p(responses, fast_breathing_available)
  chest_indrawing <- chest_indrawing_p(responses, chest_indrawing_available)
  armpits <- armpits_p(responses, swell_armpits_available, lumps_armpit_available)
  rash_mouth <- rash_mouth_p(responses, rash_mouth_available)
  bloody_stool <- bloody_stool_p(responses, bloody_stool_available)
  cough_severe <- cough_severe_p(responses, cough_severe_available)
  cough_vomit <- cough_vomit_p(responses, cough_vomit_available)
  difficulty_breathing <- difficulty_breathing_p(responses, difficulty_breathing_available)
  # noisy_breathing a.k.a. grunting
  noisy_breathing <- noisy_breathing_p(responses, noisy_breathing_available)
  fever_continue <- fever_continue_p(responses, fever_continue_available)
  fever_on_off <- fever_on_off_p( responses, fever_continue_available, fever_pattern_available)
  fever_severe <- fever_severe_p( responses, fever_severe_available, fever_severity_available )
  convulsions <- convulsions_p( responses, convulsions_available )
  unconscious <- unconscious_p( responses, unconscious_available )
  skin_black <- skin_black_p(responses, skin_black_available)
  bled_anywhere <- bled_anywhere_p( responses )
  rash_trunk <- rash_trunk_p( responses, place_rash_available, rash_body_available, rash_trunk_available )
  
  # now check specific causes of death...
  
  if( injury(responses) ){
    # return("Injury")
    causes <- c( causes, "Injury" )
  }
  
  if( aids(armpits, rash_mouth, thin_limbs, protruding_abdomen, fast_breathing, 
           chest_indrawing, days_diarrhea, days_fever, days_rash) ){
    # return("AIDS")
    causes <- c( causes, "AIDS" )
  }
  
  if( underlying_malnutrition(thin_limbs, swollen_legs_feet) ){
    # return("Malnutrition (underlying)")
    causes <- c( causes, "Malnutrition (underlying)" )
  }
  
  if( measles(age, days_rash, days_fever) ){
    # return("Measles")
    causes <- c( causes, "Measles" )
  }
  
  if( meningitis(fever, stiff_neck, bulging_fontanelle) ){
    # return("Meningitis")
    causes <- c( causes, "Meningitis" )
  }
  
  if( dysentery(days_diarrhea, bloody_stool) ){
    # return("Dysentery")
    causes <- c( causes, "Dysentery" )
  }
  
  if( diarrhea(days_diarrhea, bloody_stool) ){
    # return("Diarrhea")
    causes <- c( causes, "Diarrhea" )
  }
  
  if( pertussis(days_cough, cough_severe_available, cough_severe, cough_vomit_available, cough_vomit) ){
    # return("Pertussis")
    causes <- c( causes, "Pertussis" )
  }
  
  if( pneumonia(days_cough, days_difficulty_breathing, days_fast_breathing, chest_indrawing, noisy_breathing) ){
    # return("Pneumonia")
    causes <- c( causes, "Pneumonia" )
  }
  
  if(malaria(fever_continue, fever_on_off, fever_severe, stiff_neck, bulging_fontanelle, 
             difficulty_breathing, convulsions, unconscious)){
    # return("Malaria")
    causes <- c( causes, "Malaria" )
  }
  
  if(possible_dysentery(diarrhea, fever, convulsions, unconscious, bloody_stool)){
    # return("Possible dysentery")
    causes <- c( causes, "Possible dysentery" )
  }
  
  if(possible_diarrhea(diarrhea, fever, convulsions, unconscious, bloody_stool)){
    # return("Possible diarrhea")
    causes <- c( causes, "Possible diarrhea" )
  }
  
  if(possible_pneumonia(cough, difficulty_breathing, fast_breathing, chest_indrawing, noisy_breathing, 
                        cough_severe, cough_vomit, fever, convulsions, unconscious )){
    # return("Possible pneumonia")
    causes <- c( causes, "Possible pneumonia" )
  }
  
  if(hemorrhagic_fever(fever, bled_anywhere, skin_black)){
    # return("Hemorrhagic fever")
    causes <- c( causes, "Hemorrhagic fever" )
  }
  
  if(other_infection(fever, rash_trunk, convulsions, unconscious)){
    # return("Other infection")
    causes <- c( causes, "Other infection" )
  }
  
  if(possible_malaria(fever)){
    # return("Possible malaria")
    causes <- c( causes, "Possible malaria" )
  }
  
  if(malnutrition(thin_limbs, swollen_legs_feet)){
    # return("Malnutrition")
    causes <- c( causes, "Malnutrition" )
  }
  
  if( length( causes ) > 0){
    return(causes)
  } else{
    return("Unspecified")
  }
  
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

difficulty_breathing_duration <- function( responses, days_difficulty_breathing_available ){
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

fever_p <- function(responses, fever_available){
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

cough_p <- function(responses, cough_available){
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

stiff_neck_p <- function(responses, stiff_neck_available){
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

bulging_fontanelle_p <- function(responses, bulging_fontanelle_available){
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

diarrhea_p <- function(responses, diarrhea_available){
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

thin_p <- function(responses, thin_available){
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

swollen_legs_feet_p <- function(responses, swell_feet_available, swell_leg_available){
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

protruding_abdomen_p <- function(responses, protruding_abdomen_available){
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

fast_breathing_p <- function(responses, fast_breathing_available){
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

chest_indrawing_p <- function(responses, chest_indrawing_available){
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

armpits_p <- function(responses, swell_armpits_available, lumps_armpit_available){
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

rash_mouth_p <- function(responses, rash_mouth_available){
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

bloody_stool_p <- function(responses, bloody_stool_available){
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

fever_on_off_p <- function( responses, fever_continue_available, fever_pattern_available){
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

fever_severe_p <- function( responses, fever_severe_available, fever_severity_available ){
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

convulsions_p <- function( responses, convulsions_available ){
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

unconscious_p <- function( responses, unconscious_available ){
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

skin_black_p <- function(responses, skin_black_available){
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
}

rash_trunk_p <- function( responses, place_rash_available, rash_body_available, rash_trunk_available ){
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

# Injury -----

injury <- function( responses ){
  questions <- c("injury_accident", "road_accident", "venomous_animal", "animal", "burn",
                 "drown", "injury_fall", "other_injury", "poisoning", "assault")
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
}

# AIDS -----

aids <- function( armpits, rash_mouth, thin_limbs, protruding_abdomen, fast_breathing, 
                  chest_indrawing, days_diarrhea, days_fever, days_rash ){
  if( (armpits | rash_mouth) ){
    signs <- thin_limbs + protruding_abdomen + fast_breathing + chest_indrawing + (days_diarrhea > 30) + (days_fever > 30) + (days_rash > 30) 
    return( signs >= 3 )
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
  return( (age > 120) & (days_rash >= 3) & (days_fever >= 3) )
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
  return( ( (days_cough > 2) | (days_difficulty_breathing > 2)) & 
            ( (days_fast_breathing > 2) | chest_indrawing | noisy_breathing ) )
}

# Malaria -----

malaria <- function( fever_continue, fever_on_off, fever_severe, stiff_neck, bulging_fontanelle, 
                     difficulty_breathing, convulsions, unconscious ) {
  return( fever_continue & (fever_on_off | fever_severe) & !stiff_neck & !bulging_fontanelle & 
            (difficulty_breathing | convulsions | unconscious) )
}

# Possible dysentery -----

possible_dysentery <- function( diarrhea, fever, convulsions, unconscious, bloody_stool ){
  return( diarrhea & (fever | convulsions | unconscious) & bloody_stool )
}

# Possible diarrhea -----

possible_diarrhea <- function( diarrhea, fever, convulsions, unconscious, bloody_stool ){
  return( diarrhea & (fever | convulsions | unconscious) & !bloody_stool )
}

# Possible pneumonia -----

possible_pneumonia <- function( cough, difficulty_breathing, fast_breathing, chest_indrawing, noisy_breathing, 
                                cough_severe, cough_vomit, fever, convulsions, unconscious ){
  return( ( (cough | difficulty_breathing) | ( fast_breathing & (chest_indrawing | noisy_breathing) ) ) & 
          ( cough_severe | cough_vomit | fast_breathing | chest_indrawing | noisy_breathing | fever | convulsions | unconscious ) )
}

# Hemorrhagic fever -----

hemorrhagic_fever <- function( fever, bled_anywhere, skin_black ){
  return( fever & (bled_anywhere | skin_black) )
}

# Other infection -----

other_infection <- function( fever, rash_trunk, convulsions, unconscious ){
  return( fever & (rash_trunk | convulsions | unconscious) )
}

# Possible malaria -----

possible_malaria <- function( fever ){
  return( fever )
}

# Malnutrition -----

malnutrition <- function( thin_limbs, swollen_legs_feet ){
  return( thin_limbs | swollen_legs_feet )
}
