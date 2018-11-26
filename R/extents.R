
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
extents.SC <- function(x) {
  x0 <- x[["edge"]] %>%
    dplyr::inner_join(x[["vertex"]], c(".vx0" = "vertex_"))  %>%
    dplyr::transmute(x0 = x_, y0 = y_)
  x1 <- x[["edge"]] %>%
    dplyr::inner_join(x[["vertex"]], c(".vx1" = "vertex_"))  %>%
    dplyr::transmute(x1 = x_, y1 = y_, edge_ = edge_)

  edges <- dplyr::bind_cols(x0, x1)
  tibble::tibble(xmn = pmin(edges$x0, edges$x1), xmx = pmax(edges$x0, edges$x1),
                     ymn = pmin(edges$y0, edges$y1), ymx = pmax(edges$y0, edges$y1))
}



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

