# implements the hierarchical expert algorithm for verbal autopsy described in 
# [Kalter et al. 2015](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4416334/) (doi: 10.7189/jogh.05.010415)

library(vida)
library(dplyr)
data('master')

#' Determine a decedent's cause of death from verbal autopsy (VA) data that has been
#' translated into `babel` format with the `vida` package
#' 
#' @param responses : a data.frame of VA responses for one person (one row from a larger VA dataset)
#' @return cod : the cause of death determined by the hierarchical expert algorithm 
#' 
cod <- function(responses) {
  
  # injury -----
  
  questions <- c("die_at_site_of_injury_accident",
                 "injury_accident",
                 "road_accident",
                 "injury_fall",
                 "drown",
                 "accidentally_poisoned",
                 "poisoning",
                 "animal",
                 "venomous_animal",
                 "burn",
                 "assault",
                 "other_injury",
                 "sign_injury_broken_bones")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  
  # this is going to barf somewhere on files that don't use "yes", "no" !!
  if( any( answers == "yes", na.rm=TRUE ) ){
    return( "injury" )
  } 
  
  # AIDS -----
  
  questions <- c("swell_armpits", "rash_mouth")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  # duration of diarrhea, fever, skin rash all need to be checked!
  if( any( answers == "yes", na.rm=TRUE ) ){
    questions <- c("thin", "protruding_abdomen", "diarrhea", "fever", "rash", "fast_breathing", "chest_pull_in")
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( questions )
    if( length( which( answers == "yes")) >= 3 ){
      return("AIDS")
    }
  }
  
  # malnutrition (underlying) -----
  
  questions <- c("thin", "swell_feet", "swell_leg")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  if( any( answers == "yes", na.rm=TRUE )){
    return( "malnutrition (underlying)")
  }
  
  # measles -----
  
  # meningitis -----
  
  questions <- c("fever")
  questions <- questions[ which( questions %in% names(responses)) ]
  answers <- responses %>% select( questions )
  if( any( answers == "yes", na.rm=TRUE )){
    questions <- c("stiff_neck", "bulging_fontanelle")
    questions <- questions[ which( questions %in% names(responses)) ]
    answers <- responses %>% select( questions )
    if( any( answers == "yes", na.rm=TRUE )){
      return("meningitis")
    }
  }
  
  # dysentery -----
  
  # diarrhea -----
  
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
