#'  Map the color values to the group names, auto rep to Extend color length
#' @param color vector of color value,
#'    like: c('red', 'blue') or c('#F8766D', '#00BFC2')
#' @param group vector of group name
#' @return color_map a named vector with group name as names
#' and color value as values
#' @export
#' @importFrom utils head
color_map <- function(color, group) {
  color <- as.vector(color)
  group <- as.vector(group)
  check_vector(color, "color must be a vector!")
  check_vector(group, "group must be a vector!")
  g <- unique(group)
  n <- length(g)
  color <- head(rep(color, ceiling(n / length(color))), n)
  names(color) <- g
  color
}