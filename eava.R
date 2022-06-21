
# implements the hierarchical expert algorithms for verbal autopsy (EAVA) described in 
# [Kalter et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4416334/) (doi: 10.7189/jogh.05.010415)
# and in
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

#' Determine causes of death for ALL decedents in a verbal autopsy (VA) dataset that has been
#' translated into `babel` format with the `vida` package
#' 
#' @param babel_data : a data.frame of VA responses for multiple decedents (typically from one study site)
#' @param format : mapper used in babel translation
#' @param algo : hierarchical algorithm to use; options; "kalter", "liu"
#' @return causes : causes of death determined by the hierarchical expert algorithm 
#' 
get_causes <- function(babel_data, format, algo = "kalter") {
  
  question_names <<- names( babel_data )
  current_row <<- 1
  # check availability of certain indicators -- avoids errors when trying to access nonexistent columns by name 
  # depends on format, not on responses for an individual decedent, so precedes response-dependent functions
  # (intentionally global for easy access in other EAVA functions!)
  
  age_days_available <<- "age_days" %in% question_names
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
  suckle_feed_available <<- "suckle_feed" %in% question_names
  stop_suckle_available <<- "stop_suckle" %in% question_names
  baby_cry_available <<- "baby_cry" %in% question_names
  cry_after_birth_available <<- "cry_after_birth" %in% question_names
  baby_cry_after_birth_available <<- "baby_cry_after_birth" %in% question_names
  stop_ability_to_cry_available <<- "stop_ability_to_cry" %in% question_names
  malformation_available <<- "malformation" %in% question_names
  baby_breathe_after_birth_available <<- "baby_breathe_after_birth" %in% question_names
  lethargic_available <<- "lethargic" %in% question_names
  bruises_injury_available <<- "bruises_injury" %in% question_names
  sign_injury_broken_bones_available <<- "sign_injury_broken_bones" %in% question_names
  cold_touch_available <<- "cold_touch" %in% question_names
  vomit_available <<- "vomit" %in% question_names
  die_suddenly_available <<- "die_suddenly" %in% question_names
  appear_healthy_available <<- "appear_healthy" %in% question_names

  if( format %in% supported_formats ){
    # causes <- list(mode = "character")
    causes <- data.frame( )
    for (i in 1:nrow(babel_data)) {
      # causes[[i]] <- cod( babel_data[i,], format, algo )
      causes <- rbind( causes, cod( babel_data[i,], format, algo ))
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
cod <- function(responses, format, algo ){
  
  causes <- character()

  # calculate age in DAYS from dates of birth, death 
  # NB: assumes that the age_days variable has been cleaned as in the script `clean_ages.R` !!
  if( age_days_available ){
    if( !is.na( responses$age_days) ){
      age_days <- responses$age_days
    } else{
      age_days <- NA
    }
  } else{
    age_days <- NA
  } 
  
  # validate age -----
  
  if( is.na( age_days ) | age_days < 0){
    return( data.frame( age_days=age_days, age_group="Unknown", causes="Unknown") )
  } else if( age_days < 29 ){
    age_group <- "Neonate"
  } else{
    age_group <- "Child"
  }
  
  # durations of decedent's various conditions in indicated units -----
  days_fever <- fever_duration(responses, format) 
  days_rash <- rash_duration(responses, format) 
  days_cough <- cough_duration(responses, format) 
  days_diarrhea <- diarrhea_duration(responses, format) 
  days_fast_breathing <- fast_breathing_duration(responses, format) 
  days_difficulty_breathing <- difficulty_breathing_duration( responses ) 
  months_pregnancy <- pregnancy_duration( responses )
  
  # peak stool count -----
  number_stools <- stool_count( responses )
  
  # binaries for decedent's conditions -----
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
  suckle_feed <- suckle_feed_p( responses )
  stop_suckle <- stop_suckle_p( responses )
  baby_cry <- baby_cry_p( responses )
  cry_after_birth <- cry_after_birth_p( responses )
  baby_cry_after_birth <- baby_cry_after_birth_p( responses )
  stop_ability_to_cry <- stop_ability_to_cry_p( responses )
  malformation <- malformation_p( responses )
  baby_breathe_after_birth <- baby_breathe_after_birth_p( responses )
  lethargic <- lethargic_p( responses )
  bruises_injury <- bruises_injury_p( responses )
  sign_injury_broken_bones <- sign_injury_broken_bones_p( responses )
  cold_touch <- cold_touch_p( responses )
  vomit <- vomit_p( responses )
  die_suddenly <- die_suddenly_p( responses )
  appear_healthy <- appear_healthy_p( responses )
  
  # now check specific causes of death -----
  
  if( algo == "kalter" & age_group == "Child"){
    
    # Kalter child -----
    
    if( injury(responses) ){
      # return("Injury")
      causes <- c( causes, "Injury" )
    }
    
    if( aids_kalter(armpits, rash_mouth, thin_limbs, protruding_abdomen, fast_breathing, 
             chest_indrawing, days_diarrhea, days_fever, days_rash) ){
      causes <- c( causes, "AIDS" )
    }
    
    if( underlying_malnutrition_kalter(thin_limbs, swollen_legs_feet) ){
      causes <- c( causes, "Malnutrition (underlying)" )
    }
    
    if( measles_kalter(age_days, days_rash, days_fever) ){
      causes <- c( causes, "Measles" )
    }
    
    if( meningitis_kalter(fever, stiff_neck, bulging_fontanelle) ){
      causes <- c( causes, "Meningitis" )
    }
    
    if( dysentery_kalter(days_diarrhea, bloody_stool) ){
      causes <- c( causes, "Dysentery" )
    }
    
    if( diarrhea_kalter(diarrhea, days_diarrhea, bloody_stool, number_stools) ){
      causes <- c( causes, "Diarrhea" )
    }
    
    if( pertussis_kalter(days_cough, cough_severe_available, cough_severe, cough_vomit_available, cough_vomit) ){
      causes <- c( causes, "Pertussis" )
    }
    
    if( pneumonia_kalter(days_cough, days_difficulty_breathing, days_fast_breathing, chest_indrawing, noisy_breathing) ){
      causes <- c( causes, "Pneumonia" )
    }
    
    if( malaria_kalter(fever_continue, fever_on_off, fever_severe, stiff_neck, bulging_fontanelle, 
               difficulty_breathing, convulsions, unconscious) ){
      causes <- c( causes, "Malaria" )
    }
    
    if( possible_dysentery_kalter(diarrhea, fever, convulsions, unconscious, bloody_stool) ){
      causes <- c( causes, "Possible dysentery" )
    }
    
    if( possible_diarrhea_kalter(diarrhea, fever, convulsions, unconscious, bloody_stool) ){
      causes <- c( causes, "Possible diarrhea" )
    }
    
    if( possible_pneumonia_kalter(cough, difficulty_breathing, fast_breathing, chest_indrawing, noisy_breathing, 
                          cough_severe, cough_vomit, fever, convulsions, unconscious ) ){
      causes <- c( causes, "Possible pneumonia" )
    }
    
    if( hemorrhagic_fever_kalter(fever, bled_anywhere, skin_black) ){
      causes <- c( causes, "Hemorrhagic fever" )
    }
    
    if( other_infection_kalter(fever, rash_trunk, convulsions, unconscious) ){
      causes <- c( causes, "Other infection" )
    }
    
    if( possible_malaria_kalter(fever) ){
      causes <- c( causes, "Possible malaria" )
    }
    
    if( malnutrition_kalter(thin_limbs, swollen_legs_feet) ){
      causes <- c( causes, "Malnutrition" )
    }
    
    if( length( causes ) > 0){
      return( data.frame( age_days=age_days, age_group=age_group, causes=paste0(causes, collapse=", ")) )
    } else{
      return( data.frame( age_days=age_days, age_group=age_group, causes="Unspecified") )
    }
    
  } else if( algo == "liu" & age_group == "Child"){ 
    
    # Liu child -----
    
    if( injury( responses ) ){
      causes <- c( causes, "Injury" )
    }
  
    if( measles_liu( age_days, fever, measles_rash, cough, red_eyes ) ){
      causes <- c( causes, "Measles" )
    }
  
    if( meningitis_liu( fever, convulsions, stiff_neck, bulging_fontanelle, unconscious ) ){
      causes <- c( causes, "Meningitis" )
    }
  
    if( malaria_liu( fever, difficulty_breathing, convulsions, unresponsive ) ){
      causes <- c( causes, "Malaria" )
    }
  
    if( aids_liu( jaundice, fever, days_diarrhea, days_fever, pale, hair_change, swollen_legs_feet, 
                 days_cough, days_difficulty_breathing, tb ) ){
      causes <- c( causes, "AIDS" )
    }
  
    if( diarrhea_liu( diarrhea, days_diarrhea, number_stools, sunken_eye, sunken_fontanelle ) ){
      causes <- c( causes, "Diarrhea" )
    }
  
    if( ari_liu( days_cough, difficulty_breathing, noisy_breathing, chest_indrawing, flaring_nostrils ) ){
      causes <- c( causes, "Acute Respiratory Infection" )
    }
  
    if( possible_pneumonia_liu( cough, difficulty_breathing, chest_indrawing, fever, convulsions ) ){
      causes <- c( causes, "Possible pneumonia" )
    }
  
    if( possible_diarrhea_liu( diarrhea, difficulty_breathing, chest_indrawing, fever, convulsions ) ){
      causes <- c( causes, "Possible diarrhea" )
    }
  
    if( length( causes ) > 0){
      return( data.frame( age_days=age_days, age_group=age_group, causes=paste0(causes, collapse=", ")) )
    } else{
      return( data.frame( age_days=age_days, age_group=age_group, causes="Unspecified") )
    }
    
  } else if( algo == "kalter" & age_group == "Neonate"){
    
    # Kalter neonate -----
    
    if( neonatal_tetanus_kalter( age_days, convulsions, suckle_feed, stop_suckle, 
                                 cry_after_birth, baby_cry_after_birth, stop_ability_to_cry ) ){
      causes <- c( causes, "Neonatal tetanus" )
    }
    
    if( malformation_kalter( malformation ) ){
      causes <- c( causes, "Congenital malformation")
    }
    
    if( asphyxia_kalter( age_days, cry_after_birth, baby_cry_after_birth, baby_breathe_after_birth, 
                         suckle_feed, convulsions, lethargic ) ){
      causes <- c( causes, "Birth asphyxia")
    }
    
    if( birth_injury_kalter( bruises_injury, sign_injury_broken_bones ) ){
      causes <- c( causes, "Birth injury")
    }
    
    if( preterm_respiratory_distress_kalter( months_pregnancy, fast_breathing, fever, cold_touch) ){
      causes <- c( causes, "Preterm delivery with respiratory distress syndrome")
    }
    
    if( neonatal_meningitis_kalter( fever, bulging_fontanelle, convulsions, lethargic, unconscious ) ){
      causes <- c( causes, "Meningitis")
    }
    
    if( neonatal_diarrhea_kalter( diarrhea, number_stools ) ){
      causes <- c( causes, "Diarrhea")
    }
    
    if( neonatal_pneumonia_kalter( days_difficulty_breathing, days_fast_breathing, chest_indrawing, 
                                   noisy_breathing, baby_cry, stop_ability_to_cry ) ){
      causes <- c( causes, "Pneumonia")
    }
    
    if( neonatal_possible_diarrhea_kalter( diarrhea, fever, cold_touch, suckle_feed, stop_suckle, convulsions, vomit, 
                                           stop_ability_to_cry, lethargic, unconscious, chest_indrawing, noisy_breathing) ){
      causes <- c( causes, "Possible diarrhea")
    } 
    
    if( neonatal_possible_pneumonia_kalter( difficulty_breathing, fever, cold_touch, suckle_feed, stop_suckle, convulsions, vomit, 
                                            stop_ability_to_cry, lethargic, unconscious, chest_indrawing, noisy_breathing) ){
      causes <- c( causes, "Possible pneumonia")
    }
    
    if( sepsis_kalter( fever, cold_touch, suckle_feed, stop_suckle, convulsions, vomit, 
                       stop_ability_to_cry, lethargic, unconscious, chest_indrawing, noisy_breathing) ){
      causes <- c( causes, "Sepsis")
    }
    
    if( neonatal_jaundice_kalter( jaundice, stop_suckle, lethargic, unconscious, fever ) ){
      causes <- c( causes, "Neonatal jaundice")
    }
    
    if( neonatal_hemorrhagic_kalter( bled_anywhere, fever, cold_touch ) ){
      causes <- c( causes, "Neonatal hemorrhagic syndrome")
    }
    
    if( neonatal_sudden_kalter( die_suddenly, appear_healthy ) ){
      causes <- c( causes, "Sudden unexplained death")
    }
    
    if( preterm_kalter( months_pregnancy ) ){
      causes <- c( causes, "Preterm delivery")
    }
    
    if( length( causes ) > 0){
      return( data.frame( age_days=age_days, age_group=age_group, causes=paste0(causes, collapse=", ")) )
    } else{
      return( data.frame( age_days=age_days, age_group=age_group, causes="Unspecified") )
    }
    
  } else if( algo == "liu" & age_group == "Neonate"){
    
    # Liu neonate -----
    
    if( length( causes ) > 0){
      return( data.frame( age_days=age_days, age_group=age_group, causes=paste0(causes, collapse=", ")) )
    } else{
      return( data.frame( age_days=age_days, age_group=age_group, causes="Unspecified") )
    }
    
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
    if( "days_fever" %in% question_names ){
      if( !is.na( responses$days_fever) & !is.na(as.numeric(responses$days_fever))){
        return( as.numeric(responses$days_fever) )
      } else{
        return(0)
      } 
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
    if( "days_rash" %in% question_names ){
      if( !is.na(responses$days_rash) & !is.na(as.numeric(responses$days_rash)) ){
        return(as.numeric(responses$days_rash))
      } else{
        return(0)
      }
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
    if( "days_cough" %in% question_names ){
      if( !is.na( responses$days_cough) & !is.na(as.numeric(responses$days_cough)) ){
        return(as.numeric(responses$days_cough))
      } else{
        return(0)
      }
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
    if( "days_diarrhea" %in% question_names ){
      if( !is.na( responses$days_diarrhea) & !is.na(as.numeric(responses$days_diarrhea)) ){
        return( as.numeric(responses$days_diarrhea) )
      } else{
        return(0)
      }
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
    if( "days_fast_breathing" %in% question_names ){
      if( !is.na(responses$days_fast_breathing) & !is.na(as.numeric(responses$days_fast_breathing)) ){
        return( as.numeric(responses$days_fast_breathing) )
      } else{
        return(0)
      }
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

pregnancy_duration <- function( responses ){
  # possibilities: months_pregnancy, weeks_pregnancy, duration_pregnancy_weeks, duration_pregnancy_months
  if( "months_pregnancy" %in% question_names ){
    if( !is.na(responses$months_pregnancy) & !is.na( as.numeric( responses$months_pregnancy )) ){
      return( as.numeric( responses$months_pregnancy ) )
    }
  }
  if( "weeks_pregnancy" %in% question_names ){
    if( !is.na(responses$weeks_pregnancy) & !is.na( as.numeric( responses$weeks_pregnancy )) ){
      return( as.numeric( responses$weeks_pregnancy )/4 ) 
    }
  }
  if( "duration_pregnancy_months" %in% question_names ){
    if( !is.na(responses$duration_pregnancy_months) & !is.na( as.numeric( responses$duration_pregnancy_months )) ){
      return( as.numeric( responses$duration_pregnancy_months ) )
    }
  }
  if( "duration_pregnancy_weeks" %in% question_names ){
    if( !is.na(responses$duration_pregnancy_weeks) & !is.na( as.numeric( responses$duration_pregnancy_weeks )) ){
      return( as.numeric( responses$duration_pregnancy_weeks ) )
    }
  }
  # at this point, have exhausted the possibilities, so
  return(0)
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

suckle_feed_p <- function(responses ){
  if( suckle_feed_available ){
    if(!is.na(responses$suckle_feed) & (responses$suckle_feed == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

stop_suckle_p <- function(responses ){
  if( stop_suckle_available ){
    if(!is.na(responses$stop_suckle) & (responses$stop_suckle == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

baby_cry_p <- function(responses ){
  if( baby_cry_available ){
    if(!is.na(responses$baby_cry) & (responses$baby_cry == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

cry_after_birth_p <- function(responses ){
  if( cry_after_birth_available ){
    if(!is.na(responses$cry_after_birth) & (responses$cry_after_birth == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

baby_cry_after_birth_p <- function(responses ){
  if( baby_cry_after_birth_available ){
    if(!is.na(responses$baby_cry_after_birth) & (responses$baby_cry_after_birth == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

stop_ability_to_cry_p <- function(responses ){
  if( stop_ability_to_cry_available ){
    if(!is.na(responses$stop_ability_to_cry) & (responses$stop_ability_to_cry == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

malformation_p <- function( responses ){
  if( malformation_available ){
    if(!is.na(responses$malformation) & (responses$malformation == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

baby_breathe_after_birth_p <- function( responses ){
  if( baby_breathe_after_birth_available ){
    if(!is.na(responses$baby_breathe_after_birth) & (responses$baby_breathe_after_birth == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

lethargic_p <- function( responses ){
  if( lethargic_available ){
    if(!is.na(responses$lethargic) & (responses$lethargic == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

bruises_injury_p <- function( responses ){
  if( bruises_injury_available ){
    if(!is.na(responses$bruises_injury) & (responses$bruises_injury == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

sign_injury_broken_bones_p <- function( responses ){
  if( sign_injury_broken_bones_available ){
    if(!is.na(responses$sign_injury_broken_bones) & (responses$sign_injury_broken_bones == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}
 
cold_touch_p <- function( responses ){
  if( cold_touch_available ){
    if(!is.na(responses$cold_touch) & (responses$cold_touch == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

vomit_p <- function( responses ){
  if( vomit_available ){
    if(!is.na(responses$vomit) & (responses$vomit == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

die_suddenly_p <- function( responses ){
  if( die_suddenly_available ){
    if(!is.na(responses$die_suddenly) & (responses$die_suddenly == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

appear_healthy_p <- function( responses ){
  if( appear_healthy_available ){
    if(!is.na(responses$appear_healthy) & (responses$appear_healthy == "yes")){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}

# Injury -----

# Kalter :

# Suffered from motor vehicle accident, fall, drowning, poisoning, venomous bite or sting, burn, violence or other injury
# AND 
# (Died 1 day or less after the injury AND the illness lasted 1 day or less) OR 
# (Injury and No other VA diagnosis (except malnutrition allowed)) OR 
# (Injury that was the first illness sign/symptom AND had VA other infection or fever))

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

# CHILD causes of death according to Kalter -----

# AIDS (Kalter) -----

# (Swelling in the armpits OR a whitish rash inside the mouth/on the tongue)
# AND
# 3 or more of the following 6 signs: 
# (limbs became very thin, protruding belly, more frequent loose/liquid stools than usual for more than 30 days, 
# fever or a skin rash for more than 30 days, fast breathing, chest indrawing)

aids_kalter <- function( armpits, rash_mouth, thin_limbs, protruding_abdomen, fast_breathing, 
                  chest_indrawing, days_diarrhea, days_fever, days_rash ){
  if( (armpits | rash_mouth) ){
    signs <- thin_limbs + protruding_abdomen + fast_breathing + chest_indrawing + (days_diarrhea > 30) + (days_fever > 30) + (days_rash > 30) 
    return( signs >= 3 )
  } else{
    return(FALSE)
  }
}

# Malnutrition (underlying) (Kalter) -----

# Limbs became very thin during the fatal illness OR had swollen legs or feet during the illness
# AND
# One of these was the first symptom of the illness

underlying_malnutrition_kalter <- function( thin_limbs, swollen_legs_feet ){
  return( thin_limbs | swollen_legs_feet )
}

# Measles (Kalter) -----

# Child's age greater than or equal to 120 days 
# AND 
# rash for 3 or more days 
# AND
# fever for 3 or more days 
# AND 
# the rash started on the face

measles_kalter <- function( age_days, days_rash, days_fever ){
  return( (age_days > 120) & (days_rash >= 3) & (days_fever >= 3) )
}

# Meningitis (Kalter) -----

# Fever AND (stiff neck OR bulging fontanelle)

meningitis_kalter <- function( fever, stiff_neck, bulging_fontanelle ){
  return( fever & ( stiff_neck | bulging_fontanelle ) )
}

# Dysentery (Kalter) -----

# More frequent loose or liquid stools than usual AND more than 4 stools on the day with the most stools AND blood in the stools
# OR
# More frequent loose or liquid stools than usual for more than 14 days AND blood in the stools

dysentery_kalter <- function( days_diarrhea, bloody_stool ){
  return( (days_diarrhea > 14) & bloody_stool )
}

# Diarrhea (Kalter) -----

# More frequent loose or liquid stools than usual AND more than 4 stools on the day with the most stools AND No blood in the stools
# OR
# More frequent loose or liquid stools than usual for more than 14 days AND No blood in stools

diarrhea_kalter <- function( diarrhea, days_diarrhea, bloody_stool, number_stools ){
  return( (diarrhea & (number_stools > 4) & !bloody_stool ) | 
          ( (days_diarrhea > 14) & !bloody_stool ) )
}

# Pertussis (Kalter) -----

# Cough more than 14 days AND 
# (severe cough OR vomited after coughing OR stridor)

pertussis_kalter <- function( days_cough, cough_severe_available, cough_severe, cough_vomit_available, cough_vomit ){
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

# Pneumonia (Kalter) -----

# (Cough more than 2 days OR difficult breathing more than 2 days)
# AND
# (Fast breathing more than 2 days OR chest indrawing OR grunting)

pneumonia_kalter <- function( days_cough, days_difficulty_breathing, days_fast_breathing, chest_indrawing, noisy_breathing ){
  return( ( (days_cough > 2) | (days_difficulty_breathing > 2)) & 
            ( (days_fast_breathing > 2) | chest_indrawing | noisy_breathing ) )
}

# Malaria (Kalter) -----

# Fever that continued till death AND was on and off in character AND No stiff neck AND 
# No bulging fontanelle AND (pallor OR difficult breathing OR convulsions OR unconscious till death)
# OR
# Fever that continued till death AND was severe fever AND No stiff neck AND 
# No bulging fontanelle AND (pallor OR convulsions OR unconscious till death)

malaria_kalter <- function( fever_continue, fever_on_off, fever_severe, stiff_neck, bulging_fontanelle, 
                     difficulty_breathing, convulsions, unconscious ) {
  return( fever_continue & (fever_on_off | fever_severe) & !stiff_neck & !bulging_fontanelle & 
            (difficulty_breathing | convulsions | unconscious) )
}

# Possible dysentery (Kalter) -----

# More frequent loose or liquid stools than usual AND 
# (fever OR convulsions OR unconscious up till death) AND 
# blood in the stools AND 
# No VA dysentery

possible_dysentery_kalter <- function( diarrhea, fever, convulsions, unconscious, bloody_stool ){
  return( diarrhea & (fever | convulsions | unconscious) & bloody_stool )
}

# Possible diarrhea (Kalter) -----

# More frequent loose or liquid stools than usual AND 
# (fever OR convulsions OR unconscious up till death) AND 
# No blood in the stools AND 
# No VA diarrhea

possible_diarrhea_kalter <- function( diarrhea, fever, convulsions, unconscious, bloody_stool ){
  return( diarrhea & (fever | convulsions | unconscious) & !bloody_stool )
}

# Possible pneumonia (Kalter) -----

# (Cough or difficult breathing) OR (Fast breathing AND (chest indrawing OR stridor OR grunting OR wheezing))
# AND
# (Severe cough OR post–tussive vomiting OR fast breathing OR chest indrawing OR grunting OR stridor OR wheezing OR 
# fever OR convulsions OR unconscious up till death) 
# AND 
# No VA Pertussis AND No VA pneumonia

possible_pneumonia_kalter <- function( cough, difficulty_breathing, fast_breathing, chest_indrawing, noisy_breathing, 
                                cough_severe, cough_vomit, fever, convulsions, unconscious ){
  return( ( (cough | difficulty_breathing) | ( fast_breathing & (chest_indrawing | noisy_breathing) ) ) & 
            ( cough_severe | cough_vomit | fast_breathing | chest_indrawing | noisy_breathing | fever | convulsions | unconscious ) )
}

# Hemorrhagic fever (Kalter) -----

# Fever AND (bled from anywhere OR had areas of the skin that turned black)

hemorrhagic_fever_kalter <- function( fever, bled_anywhere, skin_black ){
  return( fever & (bled_anywhere | skin_black) )
}

# Other infection (Kalter) -----

# Fever AND 
# (rash on trunk, abdomen or everywhere OR convulsions OR unconscious up till death)

other_infection_kalter <- function( fever, rash_trunk, convulsions, unconscious ){
  return( fever & (rash_trunk | convulsions | unconscious) )
}

# Possible malaria (Kalter) -----

# Fever AND No other VA infectious causes of death

possible_malaria_kalter <- function( fever ){
  return( fever )
}

# Malnutrition (Kalter) -----

# Limbs became very thin during the fatal illness OR 
# had swollen legs or feet during the illness

malnutrition_kalter <- function( thin_limbs, swollen_legs_feet ){
  return( thin_limbs | swollen_legs_feet )
}

# CHILD causes of death according to Liu -----

# Measles (Liu) -----

# a. Age at death>=6 months;
# AND
# b. Measles-type rash on body and face;
# AND
# c. Accompanied by fever;
# AND
# d. With at least 1 of the following specific symptoms: dry cough, or red or runny eyes.

measles_liu <- function( age_days, fever, measles_rash, cough, red_eyes ){
  return( (age_days > 180) & measles_rash & fever & (cough | red_eyes) )
}

# Meningitis (Liu) -----

# a. Age at death>=29 days;
# AND
# b. Fever;
# AND
# c. Convulsions;
# AND
# d1. Stiff neck;
# OR
# d2. Bulging fontanelle;
# AND
# e. With at least 1 of the following specific symptoms: unconscious.

meningitis_liu <- function( fever, convulsions, stiff_neck, bulging_fontanelle, unconscious ){
  return( fever & convulsions & ( stiff_neck | bulging_fontanelle ) & unconscious )
}

# Malaria (Liu) -----

# a. Age at death>=29 days;
# AND
# b. Fever;
# AND
# c. With at least 1 of the following specific symptoms: convulsions, difficult breathing, unresponsive, or pallor;

malaria_liu <- function( fever, difficulty_breathing, convulsions, unresponsive ) {
  return( fever & (difficulty_breathing | convulsions | unresponsive) )
}

# AIDS (Liu) -----

# a. Age at death>=29 days;
# AND
# b1. Jaundice;
# OR
# b2. Chronic diarrhea > 1 month;
# OR
# b3. Chronic fever > 1 month;
# OR
# b4. Wasting defined as having all the 4 following
# symptoms: paleness, hair color change, edema legs, dry
# scaly skin;
# OR
# b5. Cough or trouble breathing lasting 3-27 days with
# fever but not recent TB.

aids_liu <- function( jaundice, fever, days_diarrhea, days_fever, pale, hair_change, swollen_legs_feet, days_cough, days_difficulty_breathing, tb ){
  return( jaundice | (days_diarrhea >= 29) | (days_fever >= 29) | ( pale & hair_change & swollen_legs_feet ) | 
            ( ((days_cough >= 3) | (days_difficulty_breathing >= 3)) & fever & !tb ) )
}

# Diarrhea (Liu) -----

# a. Age at death>=29 days;
# AND
# b1. Frequent loose or liquid stools starting from 1 to 13
# days before death and continued until death;
# AND
# b2. With a peak number of 6 or more stools in 24 hours;
# AND
# b3. At least 2 of the 6 following specific symptoms were
# reported: weakness, dry mouth, sunken eyes, loose skin,
# depressed fontanels, and no or very little urine;

diarrhea_liu <- function( diarrhea, days_diarrhea, number_stools, sunken_eye, sunken_fontanelle ){
  return( (diarrhea & (days_diarrhea < 14) & (number_stools >= 6) & (sunken_eye & sunken_fontanelle)) | 
            (diarrhea & (days_diarrhea >= 14)) )
}

# Acute Respiratory Infection (Liu) -----

# a. Age at death>=29 days;
# AND
# b1. Had a cough that started at least 3 days before
# death;
# OR
# b2. Difficulty breathing that started at least 1 day before
# death;
# AND
# c. Had at least 2 of the following 6 specific symptoms: noisy
# breathing, grunting, wheezing, nostril flaring, or chest indrawing.

ari_liu <- function( days_cough, difficulty_breathing, noisy_breathing, chest_indrawing, flaring_nostrils ){
  return( ((days_cough >= 3) | difficulty_breathing ) & ( noisy_breathing + chest_indrawing + flaring_nostrils >= 2 ) )
}

# Possible pneumonia (Liu) -----

# a. Age at death>=29 days;
# AND
# b. Had at least 2 of the following signs of serious infection:
#   difficult breathing, chest indrawing, convulsions, and fever;
# AND
# c. Cough or difficult breathing.

possible_pneumonia_liu <- function( cough, difficulty_breathing, chest_indrawing, fever, convulsions ){
  return( ( difficulty_breathing + chest_indrawing + convulsions + fever >= 2 ) & (cough | difficulty_breathing) )
}

# Possible diarrhea (Liu) -----

# a. Age at death>=29 days ;
# AND
# b. The child had 2+ signs of serious infection: difficult
# breathing, chest indrawing, convulsions, and fever;
# AND
# c. Diarrhea.

possible_diarrhea_liu <- function( diarrhea, difficulty_breathing, chest_indrawing, fever, convulsions ){
  return( ( difficulty_breathing + chest_indrawing + convulsions + fever >= 2 ) & diarrhea )
}

# NEONATE causes of death according to Kalter

# Neonatal tetanus (Kalter) -----

# (Age 3–27 days at death AND convulsions or spasms)
# AND EITHER 
# ((Able to suckle normally during the first day of life and stopped being able to suckle) 
# OR 
# (cried within 5 minutes after birth and stopped being able to cry))

neonatal_tetanus_kalter <- function( age_days, convulsions, suckle_feed, stop_suckle, 
                                     cry_after_birth, baby_cry_after_birth, stop_ability_to_cry ){
  return( ( (age_days >=3) & (age_days <= 27) & convulsions ) & 
          ( ( suckle_feed & stop_suckle ) | ( (cry_after_birth | baby_cry_after_birth) & stop_ability_to_cry ) ) )
}

# Congenital malformation (Kalter) -----

# Gross malformation present at birth

malformation_kalter <- function( malformation ){
  return( malformation )
}

# Birth asphyxia (Kalter) -----

# Neonatal respiratory depression: (Did not cry within 5 minutes after birth OR did not breathe immediately after birth)
# AND
# Neonatal encephalopathy: (Not able to suckle normally in the first day of life OR convulsions/spasms OR lethargy) OR 0 days old at death

asphyxia_kalter <- function( age_days, cry_after_birth, baby_cry_after_birth, baby_breathe_after_birth, 
                             suckle_feed, convulsions, lethargic ){
  return( ( !(cry_after_birth | baby_cry_after_birth) | !baby_breathe_after_birth ) &
          ( !suckle_feed  | convulsions | lethargic | (age_days==0) ) )
}

# Birth injury (Kalter) -----

# Bruises or signs of injury on the body at birth

birth_injury_kalter <- function( bruises_injury, sign_injury_broken_bones ){
  return( bruises_injury | sign_injury_broken_bones )
}

# Preterm delivery with respiratory distress syndrome (Kalter) -----

# Pregnancy duration less than 9 months
# AND
# (Fast breathing starting on day 0 AND no fever AND no cold to touch)

preterm_respiratory_distress_kalter <- function( months_pregnancy, fast_breathing, fever, cold_touch ){
  return( ( months_pregnancy < 9 ) & ( fast_breathing & !fever & !cold_touch ) )
}

# Neonatal meningitis (Kalter) -----

# Fever AND (bulging fontanelle OR convulsions) AND (lethargic OR unresponsive/unconscious)

neonatal_meningitis_kalter <- function( fever, bulging_fontanelle, convulsions, lethargic, unconscious ){
  return( fever & (bulging_fontanelle | convulsions) & (lethargic | unconscious) )
}

# Neonatal diarrhea (Kalter) -----

# More frequent loose or liquid stools than usual AND more than 4 stools on the day the diarrhea was most frequent

neonatal_diarrhea_kalter <- function( diarrhea, number_stools ){
  return( diarrhea & (number_stools > 4) )
}

# Neonatal pneumonia (Kalter) -----

# (Fast breathing lasting 1 day or more OR difficult breathing lasting 1 day or more and lasting until death)
# AND
# 2 or more of the following 3 signs: (chest indrawing, grunting, never cried OR stopped crying)

neonatal_pneumonia_kalter <- function( days_difficulty_breathing, days_fast_breathing, chest_indrawing, 
                                       noisy_breathing, baby_cry, stop_ability_to_cry ){
  return( ( ( days_fast_breathing >= 1 ) | ( days_difficulty_breathing >= 1) ) &
          ( chest_indrawing + noisy_breathing + (!baby_cry | stop_ability_to_cry) > 2 ) )
}

# Neonatal possible diarrhea (Kalter) -----

#  More frequent loose or liquid stools than usual AND VA sepsis (see below) AND No VA diarrhea

neonatal_possible_diarrhea_kalter <- function( diarrhea, fever, cold_touch, suckle_feed, stop_suckle, convulsions, vomit, 
                                               stop_ability_to_cry, lethargic, unconscious, chest_indrawing, noisy_breathing){
  return( diarrhea & sepsis_kalter(fever, cold_touch, suckle_feed, stop_suckle, convulsions, vomit, 
                                   stop_ability_to_cry, lethargic, unconscious, chest_indrawing, noisy_breathing) )
}

# Neonatal possible pneumonia (Kalter) -----

# Difficult breathing AND VA sepsis AND No VA pneumonia

neonatal_possible_pneumonia_kalter <- function( difficulty_breathing, fever, cold_touch, suckle_feed, stop_suckle, convulsions, vomit, 
                                                stop_ability_to_cry, lethargic, unconscious, chest_indrawing, noisy_breathing){
  return( difficulty_breathing & sepsis_kalter(fever, cold_touch, suckle_feed, stop_suckle, convulsions, vomit, 
                                               stop_ability_to_cry, lethargic, unconscious, chest_indrawing, noisy_breathing) )
}

# Sepsis (Kalter) -----

# Fever OR cold to touch OR 2 or more of the following 7 signs: 
# (fever OR cold to touch, did not suckle normally on the first day of life OR stopped suckling, convulsions, 
# vomited everything, stopped crying, lethargic OR unconscious, chest indrawing OR grunting)

sepsis_kalter <- function( fever, cold_touch, suckle_feed, stop_suckle, convulsions, vomit, 
                           stop_ability_to_cry, lethargic, unconscious, chest_indrawing, noisy_breathing){
  return( (fever | cold_touch) |
          ( (fever | cold_touch) + (!suckle_feed | stop_suckle) + convulsions + vomit + stop_ability_to_cry + 
              (lethargic | unconscious) + (chest_indrawing | noisy_breathing) > 2 ) )
}

# Neonatal jaundice (Kalter) -----

# Yellow skin or yellow eyes 
# AND 
# (stopped being able to suckle normally OR lethargic OR unresponsive/unconscious) 
# AND 
# No fever or hypothermia

neonatal_jaundice_kalter <- function( jaundice, stop_suckle, lethargic, unconscious, fever ){
  return( jaundice & ( stop_suckle | lethargic | unconscious ) & !fever )
}

# Neonatal hemorrhagic syndrome (Kalter) -----

# Bleeding from anywhere AND No fever or cold to touch

neonatal_hemorrhagic_kalter <- function( bled_anywhere, fever, cold_touch ){
  return( bled_anywhere & (!fever | cold_touch) )
}

# Sudden unexplained death (Kalter) -----

# Died suddenly without appearing ill AND No illness signs or symptoms

neonatal_sudden_kalter <- function( die_suddenly, appear_healthy ){
  return( die_suddenly | appear_healthy )
}

# Preterm delivery (Kalter) -----

# Pregnancy duration less than 8 months

preterm_kalter <- function( months_pregnancy ){
  return( months_pregnancy < 8 )
}

# NEONATE causes of death according to Liu
