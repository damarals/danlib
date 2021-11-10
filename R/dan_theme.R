#' Personal theme
#'
#' Minimal ggplot2 theme using the Poppins and Roboto Mono fonts
#'
#' @return ggplot2 custom theme
#' @export
#' @importFrom sysfonts font_add_google font_families
#' @importFrom showtext showtext_auto
#' @importFrom purrr walk
#' @importFrom ggplot2 theme_light theme element_text element_blank
dan_theme <- function() {
  fonts <- c("Poppins", "Roboto Mono")
  walk(fonts, \(fnt) {
    if(!(fnt %in% font_families())) font_add_google(fnt, fnt)
  })
  showtext_auto()

  theme_light(base_size = 18, base_family = fonts[1]) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 14),
    axis.text.x = element_text(family = fonts[2], size = 11),
    plot.caption = element_text(size = 12, color = "gray40"),
    panel.grid = element_blank()
  )
}
