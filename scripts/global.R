
# Load packages -----------------------------------------------------------

library(ReactomePA)
library(enrichR)
library(GSVA)
library(pheatmap)
library(RColorBrewer)
library(magrittr)
library(janitor)
library(DT)
library(plotly)
library(tidyverse)

import::from("scripts/functions.R", .all = TRUE)

# Increase max file size upload for GSVA tab
options(shiny.maxRequestSize = 200 * 1024 ^ 2)


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


# Create tab-specific data objects ----------------------------------------

# Visualize Molecule Occurrence
full_data_viz_tab <- full_data %>%
  dplyr::select(
    Molecule,
    PMID,
    Link,
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

# GSVA with sepsis signatures
full_data_gsva_tab_genesets <- full_data %>%
  clean_names() %>%
  dplyr::select(
    molecule,
    omic_type,
    author,
    pmid
  ) %>%
  mutate(
    author = str_replace_all(str_remove(author, " et al."), " ", "_"),
    study_label = case_when(
      !is.na(pmid) ~ paste0(author, "_", pmid),
      TRUE ~ author
    )
  ) %>%
  split(.$study_label) %>%
  map(
    ~distinct(., molecule, .keep_all = TRUE) %>%
      left_join(., biomart_table, by = c("molecule" = "hgnc_symbol")) %>%
      pull(ensembl_gene_id) %>%
      not_NA() %>%
      unique()
  ) %>%
  discard(~length(.x) < 2)

full_data_gsva_tab <- full_data %>%
  mutate(
    Author = str_replace(str_remove(Author, " et al."), " ", "_"),
    study_label = case_when(
      !is.na(PMID) ~ paste0(Author, "_", PMID),
      TRUE ~ Author
    )
  ) %>%
  dplyr::select(
    study_label,
    Title
  ) %>%
  distinct()
