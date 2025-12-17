#' Create a point.check function for availability::surrogateAR()
#'
#' @param oceanmask terra::SpatRaster with values 0=ocean, 1=land
#' @return function(tm, pt) -> logical (TRUE if at sea)
#' @export
make_point_check <- function(oceanmask) {

  if (!inherits(oceanmask, "SpatRaster")) {
    stop("`oceanmask` must be a terra::SpatRaster.", call. = FALSE)
  }

  # terra::extract() signature differs across versions
  has_ID_arg <- "ID" %in% names(formals(terra::extract))

  function(tm, pt) {
    # Extract lon/lat from pt (availability uses list(x, y))
    if (is.list(pt) && length(pt) >= 2L) {
      x <- pt[[1]]; y <- pt[[2]]
    } else if (is.numeric(pt) && length(pt) >= 2L) {
      x <- pt[1];  y <- pt[2]
    } else if (is.matrix(pt) || is.data.frame(pt)) {
      x <- pt[1, 1]; y <- pt[1, 2]
    } else {
      stop("Unsupported `pt` format passed to point.check().", call. = FALSE)
    }

    xy <- cbind(x, y)

    v <- if (has_ID_arg) {
      terra::extract(oceanmask, xy, ID = FALSE)
    } else {
      terra::extract(oceanmask, xy)
    }

    # If extract fails or returns nothing, treat as outside
    if (is.null(v) || (is.data.frame(v) && nrow(v) == 0L)) {
      return(FALSE)
    }

    # `v` is typically a data.frame. Determine which column holds raster values.
    # Common cases:
    #  - with ID: (ID, lyr1) => values in column 2
    #  - without ID: (lyr1)  => values in column 1
    #  - multiple layers: choose the first layer column
    if (is.data.frame(v)) {
      if (ncol(v) == 1L) {
        val <- v[[1]]
      } else {
        # Heuristic: if first column looks like an ID (integer sequence), use column 2
        if (is.numeric(v[[1]]) && length(v[[1]]) == 1L && !is.na(v[[1]])) {
          # for single-row extracts, treat first col as ID if additional cols exist
          val <- v[[2]]
        } else {
          # otherwise assume first column is already a value (rare, but safe fallback)
          val <- v[[1]]
        }
      }
    } else {
      # some terra versions may return vector/matrix; handle conservatively
      val <- v[1]
    }

    if (length(val) == 0L || is.na(val)) {
      return(FALSE)
    }

    # mask convention: 0=ocean, 1=land
    (val == 0)
  }
}
