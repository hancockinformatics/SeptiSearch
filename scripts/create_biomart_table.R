# Load the required packages
library(biomaRt)
library(tidyverse)

# Get the date used to name the output file
today <- gsub(Sys.Date(), pattern = "-", replacement = "")

# Use `biomaRt::getBM()` to create the conversion table, with the three most
# common human ID types.
biomart_table_1 <- getBM(
  attributes = c("ensembl_gene_id", "hgnc_symbol", "entrezgene_id"),
  mart = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
)

# Replace empty values with NA
biomart_table_2 <- biomart_table_1 %>% replace(. == "", NA)

# Keep only one row for each Ensembl gene
biomart_table_3 <- biomart_table_2 %>%
  rename("entrez_gene_id" = entrezgene_id) %>%
  arrange(ensembl_gene_id, hgnc_symbol, entrez_gene_id) %>%
  distinct(ensembl_gene_id, .keep_all = TRUE)

# Save the table as an RDS object in the home directory, with the date so we
# know how new or old the data is when used inside a project.
saveRDS(biomart_table_3, file = paste0("data/biomart_table_", today, ".Rds"))
