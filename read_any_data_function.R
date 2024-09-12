suppressPackageStartupMessages({
library(tcltk)
library(readr)
library(stringr)
library(openxlsx)})

read_data <- function(theFile = "", thePrompt = ""){if(theFile == ""){theFile <- tcltk::tk_choose.files(multi = FALSE, caption = thePrompt)}
        if(str_detect(theFile, ".xlsx")){theFile <- read.xlsx(theFile)}
   else if(str_detect(theFile, ".csv" )){theFile <-  read_csv(theFile, show_col_types = FALSE)}   
   else if(str_detect(theFile, ".tsv" )){theFile <-  read_tsv(theFile, show_col_types = FALSE)}
   else if(str_detect(theFile, ".[Rr][Dd][Ss]")){theFile <- read_rds(theFile)} else{tcltk::tk_messageBox(type = "ok", message = "File type not supported")}
}

cat("\014") # Clears log

