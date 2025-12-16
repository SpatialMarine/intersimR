#' Create a land/ocean mask raster for track simulations
#'
#' Creates a raster mask with values:
#' - 0 = ocean
#' - 1 = land
#'
#' The input polygon can represent either land or ocean; set `polygon_type`
#' accordingly. Areas not covered by the input polygon are assigned the
#' complementary class (land if polygon is ocean; ocean if polygon is land).
#'
#' @param bbox Numeric vector of length 4: c(xmin, xmax, ymin, ymax) in lon/lat.
#' @param res Numeric. Raster resolution in degrees (e.g., 0.01).
#' @param polygon Spatial polygon defining either land or ocean. Either a
#'   terra::SpatVector or an sf object.
#' @param polygon_type Character. One of c("land","ocean") indicating what
#'   `polygon` represents.
#' @param crs Character. Target CRS; default is EPSG:4326.
#' @param touches Logical. Passed to terra::rasterize(); if TRUE, cells touched
#'   by polygon boundaries are included (often preferable near coastlines).
#'
#' @return A terra::SpatRaster with values 0 (ocean) and 1 (land).
#' @export
create_oceanmask <- function(bbox,
                             res,
                             polygon,
                             polygon_type = c("land", "ocean"),
                             crs = "EPSG:4326",
                             touches = TRUE) {

  polygon_type <- match.arg(polygon_type)
  stopifnot(is.numeric(bbox), length(bbox) == 4L,
            is.numeric(res), length(res) == 1L, res > 0)

  # Coerce polygon to SpatVector
  if (inherits(polygon, "sf") || inherits(polygon, "sfc")) {
    polygon <- terra::vect(polygon)
  }
  if (!inherits(polygon, "SpatVector")) {
    stop("`polygon` must be a terra::SpatVector or an sf object.", call. = FALSE)
  }

  # Base raster for target domain
  r <- terra::rast(
    xmin = bbox[1], xmax = bbox[2],
    ymin = bbox[3], ymax = bbox[4],
    resolution = res,
    crs = crs
  )

  # Project polygon to mask CRS if needed
  if (!is.na(terra::crs(polygon)) && terra::crs(polygon) != terra::crs(r)) {
    polygon <- terra::project(polygon, terra::crs(r))
  }

  if (polygon_type == "land") {
    # Land polygon: land=1, background=ocean=0
    mask <- terra::rasterize(
      x = polygon,
      y = r,
      field = 1,
      background = 0,
      touches = touches
    )
    # ensure 0/1 and integer
    mask <- terra::ifel(is.na(mask), 0L, mask)
    mask <- terra::as.int(mask)

  } else {
    # Ocean polygon: ocean=0 inside polygon; background (outside) is land=1
    # Rasterize polygon as 1 (ocean), then invert to ocean=0 / land=1.
    ocean01 <- terra::rasterize(
      x = polygon,
      y = r,
      field = 1,
      background = NA,   # outside polygon remains NA; we will set to land=1
      touches = touches
    )

    # inside ocean polygon: 1 -> ocean (0)
    # outside polygon (NA): land (1)
    mask <- terra::ifel(is.na(ocean01), 1L, 0L)
    mask <- terra::as.int(mask)
  }

  attr(mask, "mask_convention") <- "0=ocean, 1=land"
  attr(mask, "polygon_type") <- polygon_type
  attr(mask, "res_degrees") <- res
  mask
}
