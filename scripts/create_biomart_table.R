# Load the required packages
library(biomaRt)
library(dplyr)

# Use `biomaRt::getBM()` to create the conversion table, with the three human ID
# types needed for the app
biomart_table_1 <- getBM(
  attributes = c("ensembl_gene_id", "hgnc_symbol", "entrezgene_id"),
  mart = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
)

# Replace empty values with NA
biomart_table_2 <- biomart_table_1 %>% replace(. == "", NA)

# Keep only one row for each Ensembl gene
biomart_table_3 <- biomart_table_2 %>%
  as_tibble() %>%
  rename("entrez_gene_id" = entrezgene_id) %>%
  mutate(across(everything(), as.character)) %>%
  arrange(hgnc_symbol, ensembl_gene_id, entrez_gene_id)

# Save the table as an RDS object
saveRDS(biomart_table_3, file = "data/biomart_table.Rds")
