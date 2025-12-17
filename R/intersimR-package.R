#' intersimR: Simulation-based detection of animal–vessel interaction events
#'
#' Tools to detect and quantify local interaction events from animal and vessel
#' tracking data using simulation-based null models (attraction, following, etc.).
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom geosphere distGeo
#' @importFrom availability surrogateARModel surrogateAR
#' @importFrom data.table as.data.table rbindlist setkey setorder foverlaps :=
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_pad
#' @importFrom stats approx
#' @importFrom terra rast rasterize extract wrap unwrap project as.int ifel vect
#' @importFrom foreach %dopar% foreach
#' @import parallel
## usethis namespace: end
NULL
