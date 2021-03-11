
# Load packages -----------------------------------------------------------

library(DT)
library(plotly)
library(tidyverse)


# Load data ---------------------------------------------------------------

current_data <-
  list.files("data", "fulldata_[0-9]{8}\\.txt", full.names = TRUE) %>%
  dplyr::last()

if (is.na(current_data)) {
  stop("Data is missing!")
} else {
  full_data <- read_tsv(current_data, col_types = cols()) %>%
    filter(!is.na(Molecule)) %>%
    mutate(PMID = as.character(PMID))
}

message(paste0("\nUsing data file: '", current_data, "'"))


# Create tab-specific tables ----------------------------------------------

# Explore Data in a Table
full_data_table_tab <- full_data %>%
  select(
    Molecule,
    PMID,
    `Omic Type`,
    `Molecule Type`,
    Tissue,
    Timepoint,
    `Case Condition`,
    `Control Condition`,
    Infection,
    `Age Group`
  )

# Explore Data by Study
by_study_grouped_static_table <- full_data %>%
  select(
    Title,
    Author,
    PMID,
    `Omic Type`,
    Molecule
  ) %>%
  group_by(across(c(-Molecule))) %>%
  summarise(`No. Molecules` = n(), .groups = "keep") %>%
  mutate(PMID = case_when(
    !is.na(PMID) ~ paste0(
      "<a target='_blank' href='",
      "https://pubmed.ncbi.nlm.nih.gov/",
      PMID, "'>", PMID, "</a>"
    ),
    TRUE ~ ""
  ))



# Create JS function ------------------------------------------------------

# Function that allows long strings in DT tables to be trimmed, with the full
# content displayed as a tooltip on hover
DT_ellipsis_render <- JS(
  "function(data, type, row, meta) {",
  "if ( type !== 'display' ) {",
  "return data;",
  "}",
  "if ( typeof data !== 'number' && typeof data !== 'string' ) {",
  "return data;",
  "}",
  "data = data.toString();",
  "if ( data.length < 50 ) {",
  "return data;",
  "}",
  "var shortened = data.substr(0, 49);",
  "shortened = shortened.replace(/,?\\s([^\\s]*)$/, '');",
  "return '<span class=\"ellipsis\" title=\"'+data+'\">'+",
  "shortened+'&#8230;</span>';",
  "}"
)
