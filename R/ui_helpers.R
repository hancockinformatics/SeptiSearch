#' ellipsis_render
#'
#' @param l Desired length of string at which truncation will occur
#'
#' @return JS function which trims strings at desired length, appends an
#'   ellipsis to the end, and gives them hover text containing the full sting.
#'
#' @export
#'
ellipsis_render <- function(l) {
  JS(paste0(
    "function(data, type, row, meta) {",
    "if ( type !== 'display' ) {",
    "return data;",
    "}",
    "if ( typeof data !== 'number' && typeof data !== 'string' ) {",
    "return data;",
    "}",
    "data = data.toString();",
    "if ( data.length < ", l, " ) {",
    "return data;",
    "}",
    "var shortened = data.substr(0, ", l, ");",
    "shortened = shortened.replace(/,?\\s([^\\s]*)$/, '');",
    "return '<span class=\"ellipsis\" title=\"'+data+'\">'+",
    "shortened+'&#8230;</span>';",
    "}"
  ))
}


#' conditional_filter
#'
#' @param condition Test condition; typically we check the length of one of the
#'   Shiny inputs
#' @param success Desired return when `condition` is satisfied; typically a
#'   filter statement based on input in `condition`
#'
#' @return Statement to be used to filter the data, to go inside a
#'   dplyr::filter() call
#'
#' @export
#'
#' @description Simple helper function that allows filtering the data on
#'   multiple parameters, without the need for multiple step-wise filters.
#'
conditional_filter <- function(condition, success) {
  if (condition) {
    return(success)
  } else {
    return(TRUE)
  }
}


#' create_selectInput
#'
#' @param column_name Name of the column to filter on; used to name the input
#'   and select the appropriate column
#' @param tab Name of the tab into which this UI object is inserted, used to
#'   build the ID
#'
#' @return Shiny `selectInput` object to be used in UI creation
#'
#' @export
#'
create_selectInput <- function(column_name, tab, tooltip) {
  selectInput(
    inputId = paste0(
      tab,
      "_",
      janitor::make_clean_names(column_name),
      "_input"
    ),
    label = HTML(paste0(
      "<p title='",
      tooltip,
      "';>",
      column_name,
      "</p>"
    )),
    choices  = sort(unique(not_NA(full_data[[column_name]]))),
    multiple = TRUE
  )
}


#' make_success_message
#'
#' @param x Table of mapped genes
#'
#' @return UI elements for success message
#'
#' @export
#'
#' @description Conditionally creates and returns the appropriate UI element to
#'   be inserted into the sidebar, informing the user about their input and
#'   mapped genes. Placed into a separate function to make the main app code
#'   cleaner.
#'
make_mapping_success_message <- function(x) {

  input_type <- attr(x, "id_type")

  if (input_type == "Ensembl") {
    p(
      "Success! Your ",
      length(unique(x$ensembl_gene_id)),
      " unique Ensembl genes were mapped to ",
      length(unique(x$hgnc_symbol)),
      " HGNC symbols, and ",
      length(unique(x$entrez_gene_id)),
      " Entrez IDs."
    )

  } else if (input_type == "Entrez") {
    p(
      "Success! Your ",
      length(unique(x$entrez_gene_id)),
      " unique Entrez genes were mapped to ",
      length(unique(x$hgnc_symbol)),
      " HGNC symbols, and ",
      length(unique(x$ensembl_gene_id)),
      " Ensembl IDs."
    )

  } else if (input_type == "HGNC") {
    p(
      "Success! Your ",
      length(unique(x$hgnc_symbol)),
      " unique HGNC symbols were mapped to ",
      length(unique(x$entrez_gene_id)),
      " Entrez IDs, and ",
      length(unique(x$ensembl_gene_id)),
      " Ensembl IDs."
    )

  } else {
    p(
      "It seems there was a problem with mapping your input genes. ",
      "Please check your inputs and try again."
    )
  }
}


make_enrichment_success_message <- function(x) {

  n_ReactomePA <- nrow(x[["ReactomePA"]])
  n_enrichR <- nrow(x[["enrichR"]])

  part1 <- "With your input genes we found "
  part2 <- "Use the buttons below to download your results as a tab-delimited text file."

  if (n_ReactomePA > 0 & n_enrichR > 0) {
    p(paste0(
      part1,
      n_ReactomePA, " pathways from ReactomePA and ",
      n_enrichR, " terms from enrichR. ",
      part2
    ))
  } else if (n_ReactomePA > 0 & n_enrichR == 0) {
    p(paste0(
      part1,
      n_ReactomePA, " pathways from ReactomePA and ",
      "no terms from enrichR. ",
      part2
    ))
  } else if (n_ReactomePA == 0 & n_enrichR > 0) {
    p(paste0(
      part1,
      "no pathways from ReactomePA and ",
      n_enrichR, " terms from enrichR. ",
      part2
    ))
  } else {
    p(paste0(
      "With your input genes we were unable to find any significant ",
      "pathways from ReactomePA or tems from enrichR. Please ensure your ",
      "input meets the requirements above, or try again with a different ",
      "list of input genes."
    ))
  }
}
