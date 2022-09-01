# Load packages, source functions -----------------------------------------

# Throughout the app we make extensive use of "::" notation, to minimize the
# number of packages we need to load, improving the loading time substantially
library(DT)
library(dplyr)
library(purrr)
library(stringr)

import::from("scripts/functions.R", .all = TRUE, .into = "")

# Increase max file size upload for GSVA tab
options(shiny.maxRequestSize = 200 * 1024 ^ 2)


# Load data ---------------------------------------------------------------

current_data <-
  list.files("data", "fulldata_[0-9]{8}\\.txt", full.names = TRUE) %>%
  dplyr::last()

if (is.na(current_data)) {
  stop("\n==ERROR: Data is missing!")
} else {
  full_data <- readr::read_tsv(current_data, col_types = readr::cols()) %>%
    filter(!is.na(Molecule)) %>%
    mutate(
      PMID = as.character(PMID),
      Timepoint = factor(Timepoint, levels = c(
        "12 hrs",
        "24 hrs",
        "48 hrs",
        "72 hrs",
        "8 days",
        "1 month",
        "Not Available"
      ))
    )
}

# Load the biomaRt data for ID mapping. All columns need to be coerced to
# character type to prevent mapping errors, namely with Entrez IDs.
biomart_table <- readRDS("data/biomart_table.Rds") %>%
  mutate(across(everything(), as.character))

# Print messages about data being used
message(paste0("\n==INFO: Using data file: '", current_data, "'."))

# Load example data for GSVA tab
tabGSVA_example_data <- readRDS("example_data/GSE65682_expr_meta_data_slim.Rds")

# Load example data for Enrichment tab
tabEnrich_example_data <-
  readr::read_lines("example_data/example_data_ensembl.txt")


# Create tab-specific data objects ----------------------------------------

### Visualize the Database
full_data_viz_tab <- full_data %>%
  dplyr::select(
    Molecule,
    `Study Label`,
    PMID,
    Link,
    # Author,
    Tissue,
    Timepoint,
    `Case Condition`,
    `Control Condition`,
    Infection
  )

### Test for Enriched Sepsis Gene Sets
full_data_gsva_tab_genesets <- full_data %>%
  janitor::clean_names() %>%
  dplyr::select(
    molecule,
    study_label,
    author,
    pmid
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
  dplyr::select(
    `Study Label`,
    Title
  ) %>%
  distinct()
