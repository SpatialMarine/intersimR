

devtools::document()
devtools::check()

# for external package
usethis::use_package("terra", type = "Imports")
# then add functions in intersimR-package.R
