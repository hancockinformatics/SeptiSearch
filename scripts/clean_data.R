
# Run this script on the downloaded Excel file. It will fix column names and
# perform some basic data cleaning. The table is written using the specified
# options to prevent errors from DT's search functionality. We also replace the
# full author list with "First, et al." to make the rows a bit smaller and more
# readable.


# Load required packages
library(janitor)
library(tidyverse)


# Define the input file for cleaning
input_file <- "data/sepsis_curation_Sheet1_20210901.tsv"


# Create the file name/path to save the eventual output
output_file <- paste0(
  "data/fulldata_",
  str_remove_all(Sys.Date(), pattern = "-"),
  ".txt"
)


# Load the data, use janitor to clean the column names, and fix some specific
# names that janitor can't do automatically
data0 <- read_tsv(input_file)
colnames(data0) <- str_remove(colnames(data0), " ?\\(.*\\) ?")

data1 <- data0 %>%
  clean_names("title") %>%
  rename(
    "PMID"         = Pmid,
    "Sex (M/F)"    = Sex,
    "Signature/DA" = `Signature or Da`,
    "GEO ID"       = `Geo Id`,
    "Molecule"     = Molecules
  )


# Clean the data by removing certain characters, trim any whitespace from all
# columns, and fix NA values
data2 <- data1 %>%
  mutate(
    across(where(is.character), ~str_remove_all(., pattern = "\'|\"")),
    across(everything(), ~str_trim(., side = "both"))
  ) %>%
  replace(. == "NA", NA)


# Two data cleaning steps here:
# 1 - Replace the "non-coding RNA" and "HERV" types with "Other"
# 2 - Trim authors. The regex has been tweaked to handle a variety of name
#     formats - remember that the goal is to have the first author's last name
#     only, then "et al."
data3 <- data2 %>%
  mutate(
    `Molecule Type` = str_replace_all(
      `Molecule Type`,
      "Non-coding RNA|HERV",
      "Other"
    ),
    `Molecule Type` = str_replace(
      `Molecule Type`,
      "Metabolites",
      "Metabolite"
    ),
    Author = str_replace(Author, " [A-Za-z]?-?[A-Za-z]+,.*", " et al.")
  ) %>%
  arrange(Author, Molecule)

# Split the data so each molecule is it's own row
data4 <- data3 %>%
  separate_rows(Molecule, sep = ", | /// ") %>%
  select(Molecule, everything())


# Save the cleaned data with some specific options; without these, encoding
# issues prevent the DT search functionality from working properly.
write.table(
  x    = data4,
  file = output_file,
  sep  = "\t",
  eol  = "\n",
  row.names    = FALSE,
  fileEncoding = "UTF-8"
)
