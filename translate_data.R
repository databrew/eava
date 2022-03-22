library(vida)
library(tidyverse)
library(readxl)
library(flextable)
library(stringr)
library(pander)
library(RColorBrewer)
library(janitor)

# Translate data to `babel` format

## set main working directory (change as needed!)
setwd( "/home/mbr/databrew/eava/" )

## set directory for working with VA data
owd <- setwd("data")

## log output in case it's useful later
con <- file("babel_translation.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

## Mali VA data

path <- "Mali VA/WHO 2017/"
files <- dir(path)
for(i in 1:length(files)){
  if( grepl(".csv", files[i])){
    fname <- paste0(path,files[i])
    cat(paste0("\ntranslating ", fname, "\n-----------\n"))
    df <- as.data.frame( read_csv(fname, show_col_types = FALSE))
    translation <- translate( df )
    parts <- unlist( strsplit( files[i], ".", fixed=TRUE))
    write_csv(translation, paste0("mali_babel/",parts[1],"_babel.",parts[2]))
    cat("\ndone!\n")
  }
}

## Gambia VA data

path <- "VA Data-Gambia Site_31Oct2017/BN HDSS/csv/"
files <- dir(path)
for(i in 1:length(files)){
  fname <- paste0(path,files[i])
  cat(paste0("\ntranslating ", fname, "\n-----------\n"))
  df <- as.data.frame( read_csv(fname, show_col_types = FALSE))
  translation <- translate( df )
  parts <- unlist( strsplit( files[i], ".", fixed=TRUE))
  write_csv(translation, paste0("gambia_babel/bn_hdss/",parts[1],"_babel.",parts[2]))
  cat("\ndone!\n")
}

path <- "VA Data-Gambia Site_31Oct2017/BS HDSS/csv/"
files <- dir(path)
for(i in 1:length(files)){
  fname <- paste0(path,files[i])
  cat(paste0("\ntranslating ", fname, "\n-----------\n"))
  df <- as.data.frame( read_csv( fname, show_col_types = FALSE))
  translation <- translate( df )
  parts <- unlist( strsplit( files[i], ".", fixed=TRUE))
  write_csv(translation, paste0("gambia_babel/bs_hdss/",parts[1],"_babel.",parts[2]))
  cat("\ndone!\n")
}

## return output to terminal (stdout)
sink() 
sink(type="message")

## return to main working directory
setwd(owd)
