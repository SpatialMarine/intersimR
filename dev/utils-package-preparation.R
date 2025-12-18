

devtools::document()
devtools::check()

# for external package
usethis::use_package("terra", type = "Imports")
usethis::use_package("dplyr", type = "Imports")

# then add functions in intersimR-package.R
