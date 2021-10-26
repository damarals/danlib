#' dan_theme
#'
#' Minimal ggplot2 theme using the Poppins and Roboto Mono fonts
#'
#' @param ggp_object ggplot2 object
#' @return ggplot2 custom theme
#' @export
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom ggplot2 theme_light theme element_text element_blank
dan_theme <- function(ggp_object) {
  font_add_google("Poppins", "Poppins")
  font_add_google("Roboto Mono", "Roboto Mono")
  showtext_auto()

  ggp_object +
    theme_light(base_size = 18, base_family = "Poppins") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 14),
      axis.text.x = element_text(family = "Roboto Mono", size = 11),
      plot.caption = element_text(size = 12, color = "gray40"),
      panel.grid = element_blank()
    )
}
