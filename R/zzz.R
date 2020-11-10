.onLoad <- function(libname,pkgname) {
  msg <- "This package is intended for use by NIH Data Access Committees.
  Written and maintained by Hoyin Chu and Christopher Steven Marcum
  chris.marcum@nih.gov. Support for the development of this package was
  provided by Civic Data Fellows Program of the Office of Science Policy
  at the National Institutes of Health and the Office of Data Science and
  Emerging Technology at the National Institute of Allergies and Infectious
  Diseases"
  packageStartupMessage(msg)
}
