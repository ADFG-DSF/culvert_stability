# This script retains the original code for reading data, and is kept as an
# archive of the steps taken to compare two versions of the Design data,
# before and after the inclusion of Importance scores:
# * Assessment Data June 10 2024.xlsx, sheet = "Designs"
# * Data/Design variables with importance Matt 8_7_24.xlsx, sheet = "Designs"



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


### reading the data annotation and comparing to original data

Design_annotation <- read_xlsx("Data/Design variables with importance Matt 8_7_24.xlsx",
                               sheet = "Designs",
                               range="A1:DR3") %>%  clean_names

# checking that names are consistent
names1 <- names(Designs)
names2 <- names(Design_annotation)[-4]   # column 4 was added in the new dataset

# which are not equal?
data.frame(names1[names1 != names2], names2[names1 != names2])

## names are basically consistent, except for trailing column number
## trailing column number means names are not unique in spreadsheet:
## should probably append reach number


## checking that the data itself is consistent

Designs2 <- read_xlsx("Data/Design variables with importance Matt 8_7_24.xlsx",
                      sheet = "Designs",
                      range="A1:DR69") %>%  clean_names
importance <- Designs2[1,] %>%
  as.data.frame %>%
  as.character %>%
  (\(x) x[-4])
Designs2 <- Designs2[-(1:2),-4]
Designs2 <- data_smasher(Designs2)
all.equal(Designs,Designs2)
for(i in 1:ncol(Designs)) {
  thetab <- table(Designs[,i] == Designs2[,i], useNA="always")
  print(sum(thetab)==sum(diag(thetab)))
}
## data has not changed!! hoooray

# lets see if i can figure out what's going on with non-unique names
names1 %>%
  strsplit(split="_") %>%
  sapply(\(x) x[length(x)]) %>%
  sapply(\(x) !is.na(as.numeric(x))) %>% suppressWarnings -> isnumber

names1[isnumber] %>%#
  strsplit(split="_") %>%
  sapply(\(x) x[-length(x)]) %>%
  sapply(paste, collapse="_") %>%
  table %>%
  (\(x) names(x)[x>1]) -> multiples

names1 %>%
  strsplit(split="_") %>%
  sapply(\(x) x[-length(x)]) %>%
  sapply(paste, collapse="_") -> firstnames

### these names occur multiple times
names1[firstnames %in% multiples]

### but these are the only ones we really care about
names1[firstnames %in% multiples & importance=="High"]
names1[firstnames %in% multiples & importance %in% c("High","Medium")]
# "banks_y_n_9"         "reach_3_gradient_50"

# finally, which of the important ones have inconsistencies?
which(Designs$banks_y_n_9 != Designs$banks_y_n_121)
which(Designs$reach_3_gradient_50 != Designs$reach_3_gradient_120)
which(is.na(Designs$design_cr == Designs$reach3_design_cr))

names1[importance=="High"]
names1[importance=="Medium"]
importance[is.na(importance)] <- 0
summary(Designs[,importance=="High" | importance=="Medium"])
