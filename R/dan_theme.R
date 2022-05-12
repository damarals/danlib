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
dan_theme <- function(base_size = 18, axis_title_size = 14, axis_text_x_size = 11,
                      caption_plot_size = 12) {
  fonts <- c("Poppins", "Roboto Mono")
  walk(fonts, \(fnt) {
    if(!(fnt %in% font_families())) font_add_google(fnt, fnt)
  })
  showtext_auto()

  theme_light(base_size = base_size, base_family = fonts[1]) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = axis_title_size),
    axis.text.x = element_text(family = fonts[2], size = axis_text_x_size),
    plot.caption = element_text(size = caption_plot_size, color = "gray40"),
    panel.grid = element_blank()
  )
}
