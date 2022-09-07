# Run this script on the downloaded Excel file. It will fix column names and
# perform some basic data cleaning. The table is written using the specified
# options to prevent errors from DT's search functionality. We also replace the
# full author list with "First, et al." to make the rows a bit smaller and more
# readable.




# 1. Script setup chunk ---------------------------------------------------

library(janitor)
library(tidyverse)

# Define the input file for cleaning
input_file <- "data/sepsis_curation_v4 - Sheet1 20220906.tsv"

# Create the file name/path to save the eventual output
output_file <- paste0(
  "data/fulldata_",
  str_remove_all(Sys.Date(), pattern = "-"),
  ".txt"
)




# 2. Data cleaning --------------------------------------------------------


# |- 2a. Load the curation data -------------------------------------------

# Use janitor to clean the column names, and fix some specific names that
# janitor can't do automatically
data0_initial <- read_tsv(input_file)
colnames(data0_initial) <- str_remove(colnames(data0_initial), " ?\\(.*\\) ?")

data1_selected <- data0_initial %>%
  clean_names("title") %>%
  select(
    "Molecule" = Molecules,
    Author,
    Title,
    Year,
    "PMID" = Pmid,
    Link,
    `Omic Type`,
    `Transcriptomic Type`,
    "Gene Set Type" = `Signature or Da`,
    Tissue,
    Timepoint,
    `Age Group`,
    "No. Patients" = Observations,
    `Covid Study`,
    `Case Condition`,
    `Control Condition`
  )


# |- 2b. Clean contents of columns ----------------------------------------

data2_filtered <- data1_selected %>%
  filter(`Omic Type` == "Transcriptomics") %>%
  mutate(
    across(where(is.character), ~str_remove_all(., pattern = "\'|\"")),
    across(everything(), ~str_trim(., side = "both")),
    `Gene Set Type` = str_replace(`Gene Set Type`, "DA", "DiffExpr")
  ) %>%
  replace(. == "NA", NA)

# Two data cleaning steps here:
# 1 - Replace the "non-coding RNA" and "HERV" types with "Other"
# 2 - Trim authors. The regex has been tweaked to handle a variety of name
#     formats - remember that the goal is to have the first author's last name
#     only, then "et al."
data3_cleaned <- data2_filtered %>%
  mutate(
    Author_clean = str_remove(Author, " [A-Za-z]?-?[A-Za-z]+,.*"),
    Timepoint = str_trim(str_remove(Timepoint, "^Within"), side = "both")
  ) %>%
  select(-Author) %>%
  arrange(Author_clean, Molecule)


# |- 2c. Split the data so each molecule is it's own row ------------------

data4_separated <- data3_cleaned %>%
  separate_rows(Molecule, sep = ", | /// ")


# |- 2d. Group and summarize the data -------------------------------------

data5_grouped <- data4_separated %>%
  group_by(
    Title, Author_clean, PMID, Timepoint, `Case Condition`, `Control Condition`,
    Tissue, `Gene Set Type`
  ) %>%
  mutate(
    Molecules = paste(Molecule, collapse = ", "),
    `Gene Set Length` = n(),
    .before = 1
  ) %>%
  dplyr::select(-Molecule) %>%
  dplyr::rename("Molecule" = Molecules) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup()


# |- 2e. Split the data into a list by Author -----------------------------

data6_split_by_author <- data5_grouped %>% split(.$Author_clean)

# Identify which authors have more than one gene set, and name them accordingly
authors_more_than_one <- data6_split_by_author %>%
  map(~nrow(.x) > 1) %>%
  flatten_lgl()

data7_authors_multiple <- data6_split_by_author[authors_more_than_one] %>%
  map(~mutate(.x, `Gene Set Name` = paste0(Author_clean, "-", c(1:nrow(.x))))) %>%
  bind_rows()

data7_authors_single <- data6_split_by_author[!authors_more_than_one] %>%
  map(~mutate(.x, `Gene Set Name` = Author_clean)) %>%
  bind_rows()

data7_all_authors <- bind_rows(
  data7_authors_multiple,
  data7_authors_single
)


# |- 2f. Split the data so each row is one Molecule -----------------------

data8_final <- data7_all_authors %>%
  separate_rows(Molecule, sep = ", ") %>%
  select(
    Molecule,
    `Gene Set Name`,
    `Gene Set Length`,
    Title,
    Year,
    PMID,
    Link,
    `Transcriptomic Type`,
    `Gene Set Type`,
    Tissue,
    Timepoint,
    `Age Group`,
    `No. Patients`,
    `Covid Study`,
    `Case Condition`,
    `Control Condition`
  )




# 3. Save the cleaned data -------------------------------------------------

# Use some specific options; without these, encoding issues prevent the DT
# search functionality from working properly

# write.table(
#   x    = data8_final,
#   file = output_file,
#   sep  = "\t",
#   eol  = "\n",
#   row.names    = FALSE,
#   fileEncoding = "UTF-8"
# )
