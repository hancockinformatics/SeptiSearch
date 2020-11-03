
# Run this script on the downloaded Excel file. It will fix column names and
# perform some basic data cleaning. The table is written using the specified
# function and options to prevent errors from DataTables' search functionality.


# Load required packages
library(readxl)
library(janitor)
library(tidyverse)


# Create the file name/path to save the eventual output
output_file <-
  paste0("data/fulldata_", str_remove_all(Sys.Date(), pattern = "-"), ".txt")


# Load the data, use janitor to clean the column names, and fix some specific
# that janitor can't do automatically
data1 <- read_xlsx("data/fulldata_20201021.xlsx") %>%
  clean_names("title") %>%
  rename(
    "PMID"         = Pmid,
    "Sex (M/F)"    = `Sex m f`,
    "Signature/DA" = `Signature Da`,
    "GEO ID"       = "Geo Id",
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


# Save the cleaned data with some specific options; without these, encoding
# issues prevent the DT search functionality from working properly.
write.table(
  x    = data2,
  file = output_file,
  sep  = "\t",
  eol  = "\n",
  row.names    = FALSE,
  fileEncoding = "UTF-8"
)
