compile.dac.report <- function(dac, author, start.date, end.date,...) {

  this.file<- system.file("inst", "report.Rmd", package = "DACReportingTool")

  title <- paste(toupper(dac), "Data Access Committee dbGaP Activity Report \n", paste(start.date,end.date,sep="-"), sep=" ")

  timeline.summary.table <- dar.review.timeline.summary(start.date,end.date)

  rmarkdown::render(this.file, params = list(
  title = title,
  start.date = start.date,
  end.date = end.date,
  author = author,
  dac=dac,
  timeline.summary.table=timeline.summary.table
   ),...)
}
