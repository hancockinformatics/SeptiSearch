
# Run this script on the downloaded Excel file. It will fix column names and
# perform some basic data cleaning. The table is written using the specified
# function and options to prevent errors from DataTables' search functionality.

library(readxl)
library(tidyverse)


data1 <- read_xlsx("data/fulldata_20201021.xlsx") %>%
  janitor::clean_names("title") %>%
  rename(
    "PMID" = Pmid,
    "Sex (M/F)" = `Sex m f`,
    "Signature/DA" = `Signature Da`,
    "GEO ID" = "Geo Id"
  )

data2 <- data1 %>%
  mutate_if(is.character, str_remove_all, pattern = "\'|\"") %>%
  mutate_if(is.character, str_trim, side = "both") %>%
  replace(. == "NA", NA)


write.table(
  x = data2,
  file = "data/fulldata_20201021.txt",
  sep = "\t",
  eol = "\n",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
