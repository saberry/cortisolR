#' Clean ggplot2 Theme
#'
#' @usage ggtheme
#' @export
#' @examples
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_blank

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
