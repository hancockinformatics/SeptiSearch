
# Run this script on the downloaded Excel file. It will fix column names and
# perform some basic data cleaning. The table is written using the specified
# options to prevent errors from DataTables' search functionality. We also
# replace the full author list with "First, et al" to make the rows a bit
# smaller.


# Load required packages
library(readxl)
library(janitor)
library(tidyverse)


# Create the file name/path to save the eventual output
output_file <- paste0(
  "data/fulldata_",
  str_remove_all(Sys.Date(), pattern = "-"),
  ".txt"
)


# Load the data, use janitor to clean the column names, and fix some specific
# names that janitor can't do automatically
data1 <- read_xlsx("data/fulldata - nov19.xlsx") %>%
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


# Trim author entries as mentioned above
data3 <- data2 %>%
  mutate(Author = str_replace(Author, " .*", " et al.")) %>%
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
