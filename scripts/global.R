
# Load packages -----------------------------------------------------------

library(ReactomePA)
library(enrichR)
library(magrittr)
library(janitor)
library(DT)
library(plotly)
library(tidyverse)

import::from("scripts/functions.R", .all = TRUE)


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

message(paste0("\nUsing data file: '", current_data, "'."))

# Load the biomaRt data for ID mapping. All columns need to be coerced to
# character type to prevent mapping errors, namely with Entrez IDs.
biomart_current <-
  list.files("data", "biomart_table_[0-9]{8}\\.Rds", full.names = TRUE) %>%
  dplyr::last()

biomart_table <- readRDS(biomart_current) %>%
  mutate(across(everything(), as.character))

message(paste0("Using biomaRt file: '", biomart_current, "'.\n"))


# Create tab-specific tables ----------------------------------------------

# Explore Data by Study - No longer used since addition of molecule filtering
# for this tab. Kept around for now just in case.
# by_study_grouped_static_table <- full_data %>%
#   dplyr::select(
#     Title,
#     Author,
#     PMID,
#     `Omic Type`,
#     Molecule
#   ) %>%
#   group_by(across(c(-Molecule))) %>%
#   summarise(`No. Molecules` = n(), .groups = "keep") %>%
#   mutate(PMID = case_when(
#     !is.na(PMID) ~ paste0(
#       "<a target='_blank' href='",
#       "https://pubmed.ncbi.nlm.nih.gov/",
#       PMID, "'>", PMID, "</a>"
#     ),
#     TRUE ~ ""
#   ))

# Visualize Molecule Occurrence
full_data_viz_tab <- full_data %>%
  dplyr::select(
    Molecule,
    PMID,
    Author,
    `Omic Type`,
    `Molecule Type`,
    Tissue,
    Timepoint,
    `Case Condition`,
    `Control Condition`,
    Infection,
    `Age Group`
  )
