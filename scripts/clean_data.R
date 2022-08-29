# Run this script on the downloaded Excel file. It will fix column names and
# perform some basic data cleaning. The table is written using the specified
# options to prevent errors from DT's search functionality. We also replace the
# full author list with "First, et al." to make the rows a bit smaller and more
# readable.




# 1. Script setup chunk ---------------------------------------------------

library(janitor)
library(tidyverse)

# Define the input file for cleaning
input_file <- "data/sepsis_curation_v4 - Sheet1.tsv"

# Create the file name/path to save the eventual output
output_file <- paste0(
  "data/fulldata_",
  str_remove_all(Sys.Date(), pattern = "-"),
  ".txt"
)




# 2. Basic data cleaning --------------------------------------------------


# |- 2a. Load the curation data -------------------------------------------

# Use janitor to clean the column names, and fix some specific names that
# janitor can't do automatically
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


# |- 2a. Clean contents of select columns ---------------------------------

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
    Author = str_replace(Author, " [A-Za-z]?-?[A-Za-z]+,.*", " et al."),
    Timepoint = str_trim(str_remove(Timepoint, "^Within"), side = "both")
  ) %>%
  arrange(Author, Molecule) %>%
  replace_na(list(Timepoint = "Not Available"))


# |- 2b. Split the data so each molecule is it's own row ------------------

data4 <- data3 %>%
  separate_rows(Molecule, sep = ", | /// ") %>%
  select(Molecule, everything())




# 3. Extra cleaning steps from Arjun's analysis ---------------------------


# |- 3a. Filter the data we're keeping ------------------------------------
data5_cleaned_filtered <- data4 %>%
  filter(`Omic Type` == "Transcriptomics") %>%
  mutate(Author_clean = str_remove(Author, " et al."), .after = "Author")


# |- 3b. Group and summarize the data -------------------------------------

data6_grouped_summarized <- data5_cleaned_filtered %>%
  group_by(
    Title, Author_clean, PMID, Timepoint,
    `Case Condition`, `Control Condition`
  ) %>%
  summarize(Molecule = paste0(Molecule, collapse = ", "), .groups = "drop") %>%
  ungroup() %>%
  mutate(
    `Gene Set Length` = map_dbl(
      Molecule,
      ~length(str_split(.x, ", ", simplify = TRUE))
    )
  )

# Retrieve the columns which were dropped from the above, and add them back,
# joining with all the "group" columns to ensure we match up the data correctly
data6_dropped_cols <- data5_cleaned_filtered %>%
  group_by(
    Title, Author_clean, PMID, Timepoint, `Case Condition`, `Control Condition`,
    .drop = FALSE
  ) %>%
  distinct(
    Title, Author_clean, PMID, Timepoint, `Case Condition`, `Control Condition`,
    .keep_all = TRUE
  ) %>%
  ungroup() %>%
  select(-c(Molecule))

data6_grouped_all_cols <- left_join(
  data6_grouped_summarized,
  data6_dropped_cols
)


# |- 3c. Split the data into a list by Author -----------------------------

data7_split_by_author <- data6_grouped_all_cols %>% split(.$Author_clean)


# Identify which authors have more than one gene set, and name them accordingly
authors_more_than_one <- data7_split_by_author %>%
  map(~nrow(.x) > 1) %>%
  flatten_lgl()

data8_authors_multiple <- data7_split_by_author[authors_more_than_one] %>%
  map(~mutate(.x, `Study Label` = paste0(Author_clean, " (", c(1:nrow(.x)), ")"))) %>%
  bind_rows()

data8_authors_single <- data7_split_by_author[!authors_more_than_one] %>%
  map(~mutate(.x, `Study Label` = Author_clean)) %>%
  bind_rows()

data8_all_authors <- bind_rows(
  data8_authors_multiple,
  data8_authors_single
)


# |- 3d. Split the data so each row is one Molecule -----------------------

data9_final <- data8_all_authors %>%
  separate_rows(Molecule, sep = ", ") %>%
  select(-Author_clean) %>%
  select(
    Molecule, `Study Label`, Title, Author, Year, PMID, Link, Timepoint, Tissue,
    Infection, `Case Condition`, `Control Condition`
  )




# 4. Save the cleaned data -------------------------------------------------

# Use some specific options; without these, encoding issues prevent the DT
# search functionality from working properly

# write.table(
#   x    = data9_final,
#   file = output_file,
#   sep  = "\t",
#   eol  = "\n",
#   row.names    = FALSE,
#   fileEncoding = "UTF-8"
# )
