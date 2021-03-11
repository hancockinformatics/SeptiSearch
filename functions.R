#' conditional_filter
#'
#' @param condition Test condition; typically we check the length of one of the
#'   Shiny inputs
#' @param success Desired return when `condition` is satisfied; typically a
#'   filter statement based on input in `condition`
#'
#' @return
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


#' not_NA
#'
#' @param vector Input vector to be cleaned
#'
#' @return Vector stripped of any NA values.
#' @export
#'
#' @description Simple function to remove NA values from input vector
#'
not_NA <- function(vector) {
  vector <- vector[!is.na(vector)]
  return(vector)
}


#' create_selectInput
#'
#' @param column_name Name of the column to filter on, used to name the input
#'   and select the appropriate column
#' @param tab Name of the tab into which this UI object is inserted, used to
#'   build the ID
#'
#' @return
#' @export
#'
#' @description Creates a selectInput object to be inserted into Shiny UI
#'
create_selectInput <- function(column_name, tab) {
  selectInput(
    inputId  = paste0(tab, "_", janitor::make_clean_names(column_name), "_input"),
    label    = column_name,
    choices  = unique(not_NA(full_data[[column_name]])),
    multiple = TRUE
  )
}
