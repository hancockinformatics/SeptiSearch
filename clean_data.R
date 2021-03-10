
# Run this script on the downloaded Excel file. It will fix column names and
# perform some basic data cleaning. The table is written using the specified
# options to prevent errors from DataTables' search functionality. We also
# replace the full author list with "First, et al." to make the rows a bit
# smaller and more readable.


# Load required packages
library(readxl)
library(janitor)
library(tidyverse)


# Define the input file for cleaning
input_file <- "fulldata - nov19.xlsx"


# Create the file name/path to save the eventual output
output_file <- paste0(
  "data/fulldata_",
  str_remove_all(Sys.Date(), pattern = "-"),
  ".txt"
)


# Load the data, use janitor to clean the column names, and fix some specific
# names that janitor can't do automatically
data1 <- read_xlsx(paste0("data/", input_file)) %>%
  clean_names("title") %>%
  rename(
    "PMID"         = Pmid,
    "Sex (M/F)"    = `Sex m f`,
    "Signature/DA" = `Signature Da`,
    "GEO ID"       = `Geo Id`,
    "ML Algorithm" = `Ml Algorithm`
  )


# Clean the data by removing certain characters, trim any whitespace from all
# columns, and fix NA values
data2 <- data1 %>%
  mutate(
    across(where(is.character), ~str_remove_all(., pattern = "\'|\"")),
    across(everything(), ~str_trim(., side = "both"))
  ) %>%
  replace(. == "NA", NA)

# Twi data cleaning steps here:
# 1 - Replace the "non-coding RNA" and "HERV" types with "Other"
# 2 - Trim author entries as mentioned above. The regex has been tweaked to
# handle a variety of name formats - remember that the goal is to have the first
# author's last name only, then "et al.".
data3 <- data2 %>%
  mutate(
    `Molecule Type` = str_replace_all(
      `Molecule Type`,
      "Non-coding RNA|HERV",
      "Other"
    ),
    Author = str_replace(Author, " [A-Za-z]?-?[A-Za-z]+,.*", " et al.")
  ) %>%
  arrange(Author, Molecule)


# Save the cleaned data with some specific options; without these, encoding
# issues prevent the DT search functionality from working properly.
write.table(
  x    = data3,
  file = output_file,
  sep  = "\t",
  eol  = "\n",
  row.names    = FALSE,
  fileEncoding = "UTF-8"
)
