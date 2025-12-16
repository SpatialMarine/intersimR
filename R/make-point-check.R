#' Create a point.check function for availability::surrogateAR()
#'
#' Returns a function with signature function(tm, pt) that evaluates whether a
#' candidate point is valid (at sea) using a 0/1 oceanmask.
#'
#' @param oceanmask A terra::SpatRaster with values 0=ocean, 1=land.
#' @param outside How to treat points outside the mask extent or returning NA.
#'   One of c("land","sea"). Default "land" (conservative; matches your current logic).
#'
#' @return A function(tm, pt) -> logical, TRUE if the point is at sea.
#' @export
make_point_check <- function(oceanmask,
                             outside = c("land", "sea")) {

  outside <- match.arg(outside)

  if (!inherits(oceanmask, "SpatRaster")) {
    stop("`oceanmask` must be a terra::SpatRaster.", call. = FALSE)
  }

  # The simulator typically passes pt as list(x, y), but we support a few forms.
  function(tm, pt) {
    # Extract lon/lat from pt
    if (is.list(pt) && length(pt) >= 2L) {
      x <- pt[[1]]
      y <- pt[[2]]
    } else if (is.numeric(pt) && length(pt) >= 2L) {
      x <- pt[1]
      y <- pt[2]
    } else if (is.matrix(pt) || is.data.frame(pt)) {
      x <- pt[1, 1]
      y <- pt[1, 2]
    } else {
      stop("Unsupported `pt` format passed to point.check().", call. = FALSE)
    }

    xy <- cbind(x, y)

    # terra::extract returns a data.frame with ID + layer column(s)
    v <- terra::extract(oceanmask, xy)

    # If point is outside extent -> NA
    # Convention: 0=ocean, 1=land
    val <- v[[2]]

    if (is.na(val)) {
      return(outside == "sea")
    }

    (val == 0)
  }
}
