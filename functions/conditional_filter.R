#' conditional_filter
#'
#' @param condition Test condition; typically we check the length of one of the
#'   Shiny inputs
#' @param success Desired return when `condition` is satisfied; typically a
#'   filter statement based on input in `condition`
#'
#' @return
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
