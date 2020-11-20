.onLoad <- function(libname,pkgname) {
  msg <- "This package is intended for use by NIH Data Access Committees. Written
and maintained by Hoyin Chu and Christopher Steven Marcum chris.marcum@nih.gov.
This project was funded by the National Institutes of Health through the Office
of Data Science Strategy in partnership with the Coding It Forward Civic Digital
Fellows Program and the Office of Data Science and Emerging Technologies at the
National Institute of Allergy and Infectious Diseases"
  latest.update.date <- get.latest.approved.dar.date()
  update.msg <- sprintf("\nThe latest date in the locally stored data is : %s.
Run dac.data.update.all() to update to a more recent version if necessary.",latest.update.date)
  packageStartupMessage(msg)
  packageStartupMessage(update.msg)
}
