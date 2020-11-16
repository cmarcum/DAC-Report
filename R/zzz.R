.onLoad <- function(libname,pkgname) {
  msg <- "This package is intended for use by NIH Data Access Committees.
Written and maintained by Hoyin Chu and Christopher Steven Marcum
chris.marcum@nih.gov. Support for the development of this package was
provided by Civic Data Fellows Program of the Office of Science Policy
at the National Institutes of Health and the Office of Data Science and
Emerging Technology at the National Institute of Allergies and Infectious
Diseases."
  update.msg <- "Run get.latest.approved.dar.date() to get the latest date in
locally stored data.\nRun update.all.tables() to update to a more recent version
if necessary."
  latest.update.date <- get.latest.approved.dar.date()
  update.msg <- sprintf("\nThe latest date in the locally stored data is : %s.
Run update.all.tables() to update to a more recent version if necessary.",latest.update.date)
  packageStartupMessage(msg)
  packageStartupMessage(update.msg)
}
