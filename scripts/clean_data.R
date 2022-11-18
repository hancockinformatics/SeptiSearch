# Run this script on the downloaded spreadsheet from Google Drive to clean and
# reorganize the data. We first do some simple data cleaning, then create the
# gene sets and tidy the data as necessary, and perform some ID mapping on the
# molecules. Three versions of the data are saved: 1) "fulldata" which contains
# all omics and includes minimal data cleaning; 2) "db_x" objects which are used
# for creating figures and tables for the paper; 3) "appdata" for the app, which
# contains additional columns and a bit of extra tidying that's needed for
# SeptiSearch.




# 1. Script setup chunk ---------------------------------------------------

library(janitor)
library(tidyverse)

# Define the input file for cleaning, downloaded from the master spreadsheet
my_input_file <- list.files(
  path = "data",
  pattern = "sepsis_curation_v5 - Sheet1.*\\.tsv",
  full.names = TRUE
) %>% last()

# Output file for the "fulldata" (contains all omics, no gene sets)
output_file_fulldata <- paste0(
  "data/fulldata_",
  str_remove_all(Sys.Date(), pattern = "-"),
  ".txt"
)

# Output file for the "appdata", which is what's used by SeptiSearch
output_file_appdata <- paste0(
  "data/appdata_",
  str_remove_all(Sys.Date(), pattern = "-"),
  ".txt"
)




# 2. Initial data cleaning ------------------------------------------------


# |- 2a. Load the curation data -------------------------------------------

# Use janitor to clean the column names, and fix some specific names that
# janitor can't do automatically
data0_initial <- read_tsv(my_input_file)
colnames(data0_initial) <- str_remove(colnames(data0_initial), " ?\\(.*\\) ?")

data1_selected <- data0_initial %>%
  clean_names("title") %>%
  rename(
    "PMID"         = Pmid,
    "Sex (M/F)"    = Sex,
    "Signature/DA" = `Signature or Da`,
    "GEO ID"       = `Geo Id`,
    "Molecule"     = Molecules
  )


# |- 2b. Clean contents of columns ----------------------------------------

data2_cleaned <- data1_selected %>%
  mutate(
    across(where(is.character), ~str_remove_all(., pattern = "\'|\"")),
    across(everything(), ~str_trim(., side = "both")),
  ) %>%
  replace(. == "NA", NA)

data3_tidied <- data2_cleaned %>%
  mutate(
    `Molecule Type` = str_replace_all(
      `Molecule Type`,
      c("Non-coding RNA|HERV" = "Other", "Metabolites" = "Metabolite")
    ),
    Author    = str_replace(Author, " [A-Za-z]?-?[A-Za-z]+,.*", " et al."),
    Timepoint = str_trim(Timepoint, side = "both")
  ) %>%
  arrange(Author, Molecule) %>%
  replace_na(list(Timepoint = "Not Available"))


# |- 2c. Split the data so each molecule is it's own row ------------------

# Inconsistencies in the separators means we need a filter step to remove empty
# strings and "NA" strings (not `NA`) from the Molecule column
data4_separated <- data3_tidied %>%
  separate_rows(Molecule, sep = ", |,| /// ") %>%
  filter(Molecule != "", Molecule != "NA") %>%
  drop_na(Molecule) %>%
  select(Molecule, everything())


# |- 2d. Save this as "fulldata" ------------------------------------------

# This version of the data contains not only transcriptomics, and is NOT
# intended to be used in the app. Its chief purpose is in generation of
# supplementary figures for the paper.
# write_tsv(data4_separated, output_file_fulldata)




# 3. Create gene sets -----------------------------------------------------


# |- 3a. Filter for transcriptomics and group -----------------------------

data5_filtered <- data4_separated %>%
  clean_names() %>%
  mutate(author_clean = str_remove(author, " et al.")) %>%
  filter(omic_type == "Transcriptomics")

data5_grouped <- data5_filtered %>%
  group_by(title, author_clean, pmid, timepoint, case_condition,
           control_condition, tissue, signature_da) %>%
  distinct(molecule, .keep_all = TRUE) %>%
  summarize(molecule = paste(molecule, collapse = ", ")) %>%
  distinct() %>%
  ungroup()


# |- 3b. Split the data into lists by author ------------------------------

data6_split_by_author <- data5_grouped %>% split(.$author_clean)

# Identify which authors have more than one gene set, and name them accordingly
my_authors_more_than_one <- data6_split_by_author %>%
  map(~nrow(.x) > 1) %>%
  flatten_lgl()

data7_authors_multiple <- data6_split_by_author[my_authors_more_than_one] %>%
  map(~mutate(.x, study_lab = paste0(author_clean, "-", c(1:nrow(.x))))) %>%
  bind_rows()

data7_authors_single <- data6_split_by_author[!my_authors_more_than_one] %>%
  map(~mutate(.x, study_lab = author_clean)) %>%
  bind_rows()

data7_sig_length <- bind_rows(data7_authors_multiple, data7_authors_single) %>%
  separate_rows(molecule, sep = ", ") %>%
  group_by(study_lab) %>%
  summarise(sig_length = n())

data7_all_authors <- bind_rows(data7_authors_multiple, data7_authors_single) %>%
  left_join(data7_sig_length) %>%
  relocate(sig_length, .before = "study_lab")


# |- 3c. Split gene sets into a list --------------------------------------

list1_gene_sets <- data7_all_authors %>%
  separate_rows(molecule, sep = ", ") %>%
  split(.$study_lab)




# 4. Perform gene ID mapping ----------------------------------------------

human_mart <- biomaRt::useEnsembl(
  biomart = "ensembl",
  dataset = "hsapiens_gene_ensembl"
)

all_septisearch_genes <- unique(unlist(map(list1_gene_sets, ~.x$molecule)))

septisearch_gene_mapping_universe <- biomaRt::getBM(
  mart = human_mart,
  attributes = c(
    "ensembl_gene_id",
    "hgnc_symbol",
    "entrezgene_id",
    "description"
  ),
  filters = "hgnc_symbol",
  values  = all_septisearch_genes
)

mapped_gene_sets <- list1_gene_sets %>% map(
  ~tibble(hgnc_symbol = .x$molecule) %>%
    distinct() %>%
    left_join(septisearch_gene_mapping_universe, by = "hgnc_symbol")
)

# Using ">=" here means we aren't removing any gene sets...is this intended?
mapped_gene_sets_filtered <- mapped_gene_sets %>%
  keep(~length(na.omit(.x$ensembl_gene_id)) >= 0)

list2_filtered_gene_sets <- list1_gene_sets %>%
  keep(names(.) %in% names(mapped_gene_sets_filtered))

data8_filtered_gene_sets <- data7_all_authors %>%
  filter(study_lab %in% names(mapped_gene_sets_filtered))




# 5. Add other columns back -----------------------------------------------

extra_data <- data5_filtered %>%
  select(one_of(
    "title",
    "author_clean",
    "pmid",
    "link",
    "transcriptomic_type",
    "year",
    "timepoint",
    "observations",
    "age_group",
    "tissue",
    "tissue_class",
    "case_condition",
    "control_condition",
    "transcriptomic_type",
    "platform",
    "signature_da",
    "covid_study"
  )) %>%
  distinct()


# |- 5a. Data objects for the paper and figures ---------------------------

data9_full_data <- data8_filtered_gene_sets %>%
  left_join(extra_data) %>%
  select(one_of(
    "title",
    "study_lab",
    "pmid",
    "observations",
    "case_condition",
    "tissue",
    "tissue_class",
    "timepoint",
    "control_condition",
    "age_group",
    "covid_study",
    "signature_da",
    "sig_length"
  )) %>%
  arrange(study_lab)


# |- 5b. Data objects for the app -----------------------------------------

data9_app_data <- data8_filtered_gene_sets %>%
  left_join(extra_data) %>%
  select(
    "Molecule"            = molecule,
    "Gene Set Name"       = study_lab,
    "Gene Set Length"     = sig_length,
    "Title"               = title,
    "Year"                = year,
    "PMID"                = pmid,
    "Link"                = link,
    "Transcriptomic Type" = transcriptomic_type,
    "Gene Set Type"       = signature_da,
    "Tissue"              = tissue,
    "Tissue Class"        = tissue_class,
    "Timepoint"           = timepoint,
    "Age Group"           = age_group,
    "No. Patients"        = observations,
    "Covid Study"         = covid_study,
    "Case Condition"      = case_condition,
    "Control Condition"   = control_condition
  ) %>%
  arrange(`Gene Set Name`) %>%
  mutate(
    Timepoint = str_remove(Timepoint, "^Within "),
    Timepoint = str_replace(Timepoint, "Not Available", "N/A"),
    `Gene Set Type` = str_replace(`Gene Set Type`, "DA", "Diff. Expr.")
  ) %>%
  replace_na(list(`Age Group` = "N/A")) %>%
  separate_rows(Molecule, sep = ", ")




# 6. Check the final data -------------------------------------------------

# We should get the same numbers when comparing a molecule's results from
# `count()` and the number of sets obtained from the `filter()` call
result_a <- count(data9_app_data, Molecule, sort = TRUE, name = "n_count") %>%
  head(10)

result_b <- count(data9_app_data, Molecule, sort = TRUE) %>%
  pull(Molecule) %>%
  head(10) %>%
  set_names() %>%
  map_dbl(
    ~filter(data9_app_data, Molecule == .x) %>%
      distinct(`Gene Set Name`) %>%
      nrow()
  ) %>%
  enframe("Molecule", "n_filter")

left_join(result_a, result_b) %>%
  mutate(same = if_else(n_count == n_filter, TRUE, FALSE))



# 7. Save the cleaned data -------------------------------------------------


# |- 7a. Data for the app -------------------------------------------------

# Use some specific options; without these, encoding issues prevent the DT
# search functionality from working properly
write.table(
  x    = data9_app_data,
  file = output_file_appdata,
  sep  = "\t",
  eol  = "\n",
  row.names    = FALSE,
  fileEncoding = "UTF-8"
)


# |- 7b. Data for the paper/figures ---------------------------------------

write_csv(data9_full_data, "data/db_filt_titles.csv")

db_final <- list(
  db_wide               = data7_all_authors,
  db_wide_extra_dat     = data9_full_data,
  gene_sets             = list1_gene_sets,
  gene_sets_mapped      = mapped_gene_sets,
  gene_mapping_universe = septisearch_gene_mapping_universe
)

write_rds(db_final, "data/db_clean.rds")
