
#' All extents
#'
#' (This function probably belongs in spex). Find the extent of all paths within an object.
#'
#' The `path_` identifier is included, but won't be of use without an
#' existing `PATH` object. The path order is implicit as per the gibble
#' geometry map.
#' @param x Object with paths
#'
#' @return a dataframe of object and extent values (xmin, xmax, ymin, ymax)
#' @export
#'
#' @examples
#' data("minimal_mesh", package = "silicate")
#' extents(minimal_mesh)
extents <- function(x) {
  UseMethod("extents")
}
#' @export
extents.default <- function(x) extents(silicate::PATH(x))
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr %>%
extents.PATH <- function(x) {
  x[["path"]] %>% dplyr::select(.data$path_) %>%
    dplyr::inner_join(x[["path_link_vertex"]], "path_") %>%
    dplyr::inner_join(x[["vertex"]], "vertex_") %>%
    dplyr::group_by(.data$path_) %>%
    dplyr::summarize(xmn = min(.data$x_), xmx = max(.data$x_), ymn = min(.data$y_), ymx = max(.data$y_))
}
