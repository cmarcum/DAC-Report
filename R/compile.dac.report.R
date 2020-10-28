compile.dac.report <- function(dac, author, start.date, end.date,...) {

  this.file<- system.file("rmd", "report.Rmd", package = "DACReportingTool")

  title <- paste(toupper(dac), "Data Access Committee dbGaP Activity Report \n", paste(start.date,end.date,sep="-"), sep=" ")

  rmarkdown::render(this.file, params = list(
  title = title,
  start.date = start.date,
  end.date = end.date,
  author = author,
  dac=dac
   ), ...)
}
