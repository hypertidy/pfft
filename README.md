# pfft

Polygon finder for triangles

This workflow came from anglr, the current scheme is to triangulate set-wide, on the entire layer and then find out which polygon each triangle belongs to. This is because polygons that wrap around a void but only touch at a single vertex end up with that void being kept as only the regions inside holes was being removed. 

If we do it set-wide we can't check each feature individually (a feature might be inside a hole of another feature), and we can't sensibly normalize triangles that fall into overlapping features. 

Together this gives the best of all (that's the theory, today!) and solves a bunch of lingering problems that anglr has. 

If we provide a native point-in-polygon routine here then we can abandon dependencies that bring in other unused libs. 

This looks good, though maybe geometry or ptinpoly or something like that: 

https://stackoverflow.com/a/36684485/355270


This also means we can use z-fighting, easily visible with rgl, to indicate when we have overlapping features - and also ilustrate how we need to de-normalize for discrete features in 3D, etc. etc. 

```R
library(raster)
library(spex)
library(sf)
# gibble_pip <- function(pts, gm, coords) {
#   object_id <- rep(seq_len(nrow(gm)), gm[["nrow"]])
#   tibble::tibble(ipoly = sp::over(SpatialPoints(as.matrix(pts)), 
#                                   sp::SpatialPolygons(purrr::imap(split(coords, object_id), 
#                                                                   ~ sp::Polygons(list(sp::Polygon(as.matrix(.x))), .y)))
#   ))
# }
extents <- function(x) {
  UseMethod("extents")
}
extents.default <- function(x) extents(PATH(x))
extents.PATH <- function(x) {
  x[["path"]] %>% dplyr::select(.data$path_) %>% 
    dplyr::inner_join(x[["path_link_vertex"]], "path_") %>% 
    dplyr::inner_join(x[["vertex"]], "vertex_") %>% 
    dplyr::group_by(.data$path_) %>% 
    dplyr::summarize(xmn = min(x_), xmx = max(x_), ymn = min(y_), ymx = max(y_))
}





data("holey", package = "spbabel")
x <- st_as_sf(spbabel::sp(holey))
max_area <- NULL
p <- silicate::PATH(x)  

gm <- p$path
coords <- p$path_link_vertex %>% inner_join(p$vertex) %>% dplyr::select(x_, y_)

## replace  this block with sc_edge(p) something something
vertex <- p[["vertex"]]
vertex <- dplyr::mutate(vertex, countingIndex = row_number())
nonuq <- dplyr::inner_join(p[["path_link_vertex"]], vertex, "vertex_")
ps <- RTriangle::pslg(P = as.matrix(vertex[c("x_", "y_")]),
                      S = do.call(rbind, lapply(split(nonuq, nonuq[["path_"]]),
                                                function(x) path2seg(x[["countingIndex"]]))))
tr <- RTriangle::triangulate(ps, a = max_area)

ex <- extents(pp)           

## map of which points to look up
pipmap <- split(ex, ex$path_) %>% 
  purrr::map(~ which(pt[,1] >= .x[["xmn"]] & pt[,1] <= .x[["xmx"]] & pt[, 2] >= .x[["ymn"]] & pt[,2] <= .x[["ymx"]]))
len <- lengths(pipmap) > 0
## now the lookup
lc <- split(coords, rep(seq_len(nrow(gm)), gm$nrow)) 
## this is the result
pip <- vector("list", length(pipmap))
for (i in seq_along(pipmap)) {
  if (len[i]) {
    ## replace this with a generic native function
   pip[[i]] <-    sp::point.in.polygon(pt[, 1], pt[,2], lc[[i]][["x_"]], lc[[i]][["y_"]])
  }
}

```
