# Load packages, source functions -----------------------------------------

# Throughout the app we make extensive use of "::" notation, to minimize the
# number of packages we need to load, improving the loading time substantially
library(DT)
library(dplyr)
library(purrr)
library(stringr)

# Increase max file size upload for GSVA tab
options(shiny.maxRequestSize = 200 * 1024 ^ 2)


# Load data ---------------------------------------------------------------

current_data <-
  list.files("data", "appdata_[0-9]{8}\\.txt", full.names = TRUE) %>%
  dplyr::last()

if (is.na(current_data)) {
  stop("\n==ERROR: Data is missing!")
} else {
  full_data <- readr::read_tsv(current_data, col_types = readr::cols()) %>%
    tidyr::replace_na(list(
      Timepoint = "N/A",
      `Age Group` = "N/A",
      `Transcriptomic Type` = "N/A"
    )) %>%
    mutate(
      PMID = as.character(PMID),
      Timepoint = factor(Timepoint, levels = c(
        "12 hrs",
        "24 hrs",
        "48 hrs",
        "72 hrs",
        "8 days",
        "1 month",
        "Various",
        "N/A"
      )),
      `Age Group` = factor(`Age Group`, levels = c(
        "Neonate",
        "Pediatric",
        "Adult",
        "Pediatric, adult",
        "Neonate, pediatric, adult",
        "N/A"
      )),
      `Tissue Class` = factor(`Tissue Class`, levels = c(
        "Blood",
        "Blood Cells (Leukocytes)",
        "Blood Cells (Neutrophils)",
        "Blood Cells (PBMC)",
        "Lung Cells (BALF)",
        "Lung Cells (NHBE)",
        "Lung Cells (Tissue)",
        "Other Cells (Nasopharyngeal)"
      ))
    )
}

biomart_table <- readRDS("data/biomart_table.Rds")
tabGSVA_example_data <- readRDS("example_data/GSE65682_expr_meta_data_slim.Rds")
tabEnrich_example_data <- readLines("example_data/example_data_ensembl.txt")


# Create gene sets for GSVA -----------------------------------------------

full_data_gsva_tab_genesets <- full_data %>%
  janitor::clean_names() %>%
  dplyr::select(
    molecule,
    gene_set_name,
    pmid
  ) %>%
  split(.$gene_set_name) %>%
  map(
    ~distinct(., molecule, .keep_all = TRUE) %>%
      left_join(
        x = .,
        y = biomart_table,
        by = c("molecule" = "hgnc_symbol"),
        multiple = "all"
      ) %>%
      pull(ensembl_gene_id) %>%
      not_NA() %>%
      unique()
  ) %>%
  discard(~length(.x) < 2)

full_data_gsva_tab <- full_data %>%
  dplyr::select(
    `Gene Set Name`,
    Title
  ) %>%
  distinct()
