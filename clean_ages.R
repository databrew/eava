library(tidyverse)
library(lubridate)

# clean up / add age_days variable to each available VA data set
# if desired/needed, undo these results by sourcing translate_data.R to translate 
# the original VA data into babel format 

get_ages <- function( babel_data ){
  ages <- numeric()
  for(i in 1:nrow(babel_data)){
    ages[i] <- age_in_days( babel_data[i,] )
  }
  return(ages)
}

age_in_days <- function( responses ){
  # mdy() does not work for format c08_12
  return( difftime( mdy(responses$date_death_deceased), mdy(responses$date_birth_deceased), units="days") )
}

# Mali data -----

# df <- read_csv("data/mali_babel/CHAMPS_Verbal_Autopsy_Form_results_babel.csv")
# ages <- get_ages( df )
# df$age_days <- ages
# write_csv( df, "data/mali_babel/CHAMPS_Verbal_Autopsy_Form_results_babel.csv" )

# df <- read_csv("data/mali_babel/CHAMPS_Verbal_Autopsy_Form_v2_04_1_results_babel.csv")
# ages <- get_ages( df )
# df$age_days <- ages
# write_csv( df, "data/mali_babel/CHAMPS_Verbal_Autopsy_Form_v2_04_1_results_babel.csv" )

# df <- read_csv("data/mali_babel/CHAMPS_Verbal_Autopsy_Form_v2_04_results_babel.csv")
# df$age_days was already fine!

# df <- read_csv("data/mali_babel/VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel.csv")
# df$date_death_deceased <- ymd(strptime(df$date_death_deceased,format="%d-%b-%Y"))
# df$date_birth_deceased <- ymd(strptime(df$date_birth_deceased,format="%d-%b-%Y"))
# df$age_days <- difftime( df$date_death_deceased, df$date_birth_deceased, unit="days")
# write_csv( df, "data/mali_babel/VA_Death_of_a_child_under_4_weeks_to_59_months_results_21MAR2018_babel.csv")

# df <- read_csv("data/gambia_babel/bn_hdss/VA_Child_WHO2016_babel.csv")
# ages <- get_ages(df)
# df$age_days <- ages
# write_csv( df, "data/gambia_babel/bn_hdss/VA_Child_WHO2016_babel.csv")

# df <- read_csv("data/gambia_babel/bn_hdss/VA_ChildForm_WHO2012_babel.csv")
# ages <- get_ages( df )
# df$date_death_deceased
# df$date_birth_deceased
# # someone born in 1957?? see ages[65], df$date_birth_deceased[65], df$date_death_deceased[65]
# df$age_days <- ages
# write_csv(df, "data/gambia_babel/bn_hdss/VA_ChildForm_WHO2012_babel.csv")

# df <- read_csv("data/gambia_babel/bs_hdss/VA_Child_Indepth_2008-2012_clean duration_babel.csv")
# ages <- get_ages( df )
# df$date_death_deceased
# df$date_birth_deceased
# NOT GOOD! no date_birth_deceased variable; many many NAs in date_death_deceased
# no age information in format c08_12

# df <- read_csv("data/gambia_babel/bs_hdss/VA_Child_WHO2012-mid 2016_babel.csv")
# ages <- get_ages( df )
# df$date_death_deceased
# df$date_birth_deceased
# df$age_days <- ages
# write_csv( df, "data/gambia_babel/bs_hdss/VA_Child_WHO2012-mid 2016_babel.csv")

# df <- read_csv("data/gambia_babel/bs_hdss/VA_Child_WHO2016_babel.csv")
# ages <- get_ages( df )
# df$date_death_deceased
# df$date_birth_deceased
# # NOT GOOD! some negative ages... quite a few typos in dates... obvious ones fixed manually:
# idx <- which( ages > 2000)
# ages[idx]
# df$date_death_deceased[idx]
# df$date_death_deceased[ idx[1] ] <- "9/14/2014"
# df$date_death_deceased[ idx[2] ] <- "11/1/2015"
# df$date_death_deceased[ idx[3] ] <- "11/1/2013"
# df$date_birth_deceased[idx]
# idx <- which( ages < 0)
# ages[idx]
# df$date_death_deceased[idx]
# df$date_birth_deceased[idx]
# df$date_birth_deceased[ idx[4] ] <- "3/1/2014"
# df$date_birth_deceased[ idx[6] ] <- "5/17/2013"
# leaves some erroneous birth/death dates...
# ages <- get_ages( df )
# df$age_days <- ages
# write_csv(df, "data/gambia_babel/bs_hdss/VA_Child_WHO2016_babel.csv")
