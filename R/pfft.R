
#' Triangles
#'
#' Build triangles from PATH
#'
#' Input edges in the form of `silicate::PATH` and return a RTriangle object.
#'
#' @param x PATH
#' @param ... arguments passed to `RTriangle::triangulate`
#'
#' @return RTriangle triangulation
#' @export
#'
#' @examples
#' data("minimal_mesh", package = "silicate")
#' library(RTriangle)
#' mm <- silicate::PATH(minimal_mesh)
#' plot(edge_RTriangle(mm), asp = 1)
#' plot(edge_RTriangle(mm, D= TRUE), asp = 1)
edge_RTriangle <- function(x, ...) {
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_", "y_")]),
                        S = matrix(match(silicate::sc_edge(x) %>%
                                           dplyr::select(.data$.vertex0, .data$.vertex1) %>%
                                           as.matrix() %>% t() %>% as.vector(), x[["vertex"]][["vertex_"]]), ncol = 2, byrow = TRUE))
  RTriangle::triangulate(ps, ...)
}
#' Triangles in paths
#'
#' Build a map of triangles to paths (polygon ring)
#' @param x PATH object
#' @param RTri RTriangle triangulation
#'
#' @return  data frame mapping triangles to their containing paths
#' @export
#'
#' @examples
#' data("minimal_mesh", package = "silicate")
#' p <- silicate::PATH(minimal_mesh)
#' tr <- edge_RTriangle(p)
#' path_triangle_map(p, tr)
#'
#' library(ggplot2)
#' library(dplyr)
#' library(purrr)
#' ggplot(path_triangle_map(p, tr) %>%
#' inner_join(reduce(p[c("path", "path_link_vertex", "vertex")], inner_join))) +
#' geom_polygon(aes(x_, y_, fill = path_)) + facet_wrap(~path_)
#'
path_triangle_map <- function(x, RTri) {
  centroids <- matrix(unlist(lapply(split(RTri[["P"]][t(RTri[["T"]]), ], rep(seq(nrow(RTri$T)), each = 3)), .colMeans, 3, 2)),
                      ncol = 2, byrow = TRUE)
  ex <- extents(x)
  gm <- x[["path"]]
  ## map of which points to look up
  pipmap <- split(ex, ex$path_) %>%
    purrr::map(~ (centroids[,1] >= .x[["xmn"]] &
                    centroids[,1] <= .x[["xmx"]] &
                    centroids[, 2] >= .x[["ymn"]] &
                    centroids[,2] <= .x[["ymx"]]))
  pipmap <- pipmap[gm$path_]
  len <- purrr::map_int(pipmap, sum)
  ## now the lookup
  lc <- split(silicate::sc_coord(x), rep(seq_len(nrow(gm)), gm$ncoords_))
  ## this is the result
  pip <- pipmap
  for (i in seq_along(pipmap)) {
    if (len[i] > 0) {
      ## replace this with a generic native function
      pip[[i]][pipmap[[i]]] <-    sp::point.in.polygon(centroids[pipmap[[i]], 1], centroids[pipmap[[i]],2], lc[[i]][["x_"]], lc[[i]][["y_"]]) > 0
    } else {
      pip[[i]][] <- FALSE
    }
  }
  ix <- lapply(pip, which)
  tibble::tibble(path_ = rep(names(ix), lengths(ix)),
                 triangle_idx = unlist(ix))
}
