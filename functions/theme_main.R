#' theme_main
#'
#' @param baseSize Base font size for the theme
#' @param baseFamily Base font family
#'
#' @return Theme object
#'
#' @export
#'
#' @import ggplot2
#'
theme_main <- function(baseSize = 18, baseFamily = "") {

  theme_light(base_size = baseSize, base_family = baseFamily) +
    theme(
      text             = element_text(colour = "black"),
      axis.text        = element_text(colour = "black"),
      axis.text.x      = element_text(angle = -45, hjust = 0),
      axis.ticks       = element_line(colour = "black", size = 0.5),
      panel.grid.major = element_line(colour = "grey", size = 0.5),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "black", size = 1),
      strip.background = element_rect(fill = NA),
      strip.text       = element_text(colour = "black", face = "bold", size = 14),
      legend.position  = "bottom",
      plot.margin      = margin(r = 30)
    )

}
