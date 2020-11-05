#' conditional_filter
#'
#' @param condition
#' @param success
#'
#' @return
#'
#' @export
#'
conditional_filter <- function(condition, success) {
  if (condition) {
    return(success)
  } else {
    return(TRUE)
  }
}
