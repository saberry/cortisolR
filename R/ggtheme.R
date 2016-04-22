#' Clean ggplot2 Theme
#'
#' @usage ggtheme
#' @export
#' @examples
#'
#' @importFrom ggplot2 theme

ggtheme = function() {
  theme(
    axis.text.x = element_text(colour = 'gray50'),
    axis.text.y = element_text(colour = 'gray50'),
    legend.key = element_rect(fill = 'white'),
    legend.background = element_rect(fill = 'white'),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank()
  )
}
