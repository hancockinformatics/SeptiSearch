#' list_item
#'
#' @param link Link to a website
#' @param name Name for the link
#' @param description Short description to accompany the link
#'
#' @return HTML wrapping up a dependency entry
#'
list_item <- function(link, name, description) {
  tagList(
    tags$dt(
      a(
        href = link,
        target = "_blank",
        rel = "noopener noreferrer",
        name
      )
    ),
    tags$dd(description)
  )
}


#' not_NA
#'
#' @param vector Input vector to be cleaned
#'
#' @return Vector stripped of any NA values.
#'
#' @export
#'
#' @description Simple function to remove NA values from input vector, without
#'   the extra class elements included in na.omit(), which can cause errors in
#'   other functions
#'
not_NA <- function(vector) {
  vector <- vector[!is.na(vector)]
  return(vector)
}


#' null_or_nrow0
#'
#' @param x Data frame to be tested
#'
#' @return Logical
#'
#' @description Tests if a data frame is NULL or has 0 rows. Just a simple
#' wrapper allowing us not to worry about `nrow()` returning an error when the
#' input is NULL
#'
#' @export
#'
null_or_nrow0 <- function(x) {
  if (is.null(x)) {
    TRUE
  } else if (nrow(x) == 0) {
    TRUE
  } else {
    FALSE
  }
}


#' wrapList
#'
#' @param x A tibble of dependencies to wrap up into the UI
#'
#' @return A div which splits the dependency entries into two columns
#'
wrap_list <- function(x) {
  col_1 <- seq(1, ceiling(nrow(x) / 2))
  col_2 <- seq(max(col_1) + 1, nrow(x))

  tagList(
    div(
      class = "row align-items-start",
      div(
        class = "col",
        tags$dl(purrr::pmap(x[col_1, ], list_item))
      ),
      div(
        class = "col",
        tags$dl(purrr::pmap(x[col_2, ], list_item))
      )
    )
  )
}
