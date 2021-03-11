
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
