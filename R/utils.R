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
