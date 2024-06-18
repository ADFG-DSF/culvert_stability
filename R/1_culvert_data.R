
# Culvert Stability
# Reading data and performing some additional data cleanup and formatting

# loading packages
library(tidyverse)  # for data manipulation
library(readxl)     # for reading directly from .xlsx
                    # - NOTE: column names did not read correctly when imported from .csv (newline character?)
library(janitor)

# defining a function to handle some initial cleanup & formatting
data_smasher <- function(df, na_values = c("na", "#DIV/0!")) {
  df <- as.data.frame(df)
  for(j in 1:ncol(df)) {
    df[df[, j] %in% na_values, j] <- NA
    # maybe this should have an as.character in there too
    if(suppressWarnings(sum(!is.na(as.numeric(as.character(df[, j])))) == sum(!is.na(df[, j])))) {
      df[, j] <- as.numeric(df[, j])
    } else {
      # df[, j] <- str_replace_all(df[, j], "[^[:alnum:]]", " ")   # this doesn't work
      for(i in 1:nrow(df)) {
        if(suppressWarnings(is.na(as.numeric(df[i, j])))) {
          df[i, j] <- str_replace_all(df[i, j], "[^[:alnum:]]", " ")
        }
      }
      df[, j] <- tolower(df[, j])
    }
  }
  return(df)
}

# defining a function to convert a two-digit n Excel-style column address
# (e.g. AA) to a single number (e.g. 27).  This was needed to import from .csv
# using tidyverse::read_csv(), but is no longer required.
letter2num <- function(x) {
  theletters <- tolower(strsplit(x, split="")[[1]])
  thenumbers <- c(which(letters==theletters[1]), which(letters==theletters[2]))
  return(26*thenumbers[1] + thenumbers[2])
}


# Reading data!
Designs <- read_xlsx("Data/Assessment Data June 10 2024.xlsx",
                     sheet = "Designs",
                     range="A1:DQ67") %>% data_smasher %>% clean_names
Visual_Stability <- read_xlsx("Data/Assessment Data June 10 2024.xlsx",
                              sheet = "Visual Stability",
                              range="A1:L67") %>% data_smasher %>% clean_names
VTable <- read_xlsx("Data/Assessment Data June 10 2024.xlsx",
                    sheet = "VTable",
                    range="A1:X67") %>% data_smasher %>% clean_names
Exterior_Data <- read_xlsx("Data/Assessment Data June 10 2024.xlsx",
                           sheet = "Exterior Data",
                           range="A1:JX67") %>% data_smasher %>% clean_names
Interior_Data <- read_xlsx("Data/Assessment Data June 10 2024.xlsx",
                           sheet = "Interior Data",
                           range="A1:GW67") %>% data_smasher %>% clean_names
Measured_in_Field <- read_xlsx("Data/Assessment Data June 10 2024.xlsx",
                               sheet = "Measured in Field",
                               range="A1:RS67") %>% data_smasher %>% clean_names
