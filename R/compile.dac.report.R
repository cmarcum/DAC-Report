compile.dac.report <- function(dac, author, start.date, end.date,...) {

  this.file<- system.file("inst", "report.Rmd", package = "DACReportingTool")

  title <- paste(toupper(dac), "Data Access Committee dbGaP Activity Report \n", paste(start.date,end.date,sep="-"), sep=" ")

  timeline.summary.table <- dar.review.timeline.summary(start.date,end.date)
  dac.row <- timeline.summary.table[timeline.summary.table['DAC'] == dac,]
  print(dac.row)
  rmarkdown::render(this.file, params = list(
  title = title,
  start.date = start.date,
  end.date = end.date,
  author = author,
  dac=dac,
  total.number.of.dars=0,
  total.number.of.projects=0,
  num.dars.in.window=dac.row['TotalRequests'],
  number.of.accepted=dac.row['TotalApproved'],
  number.of.rejected=dac.row['TotalRejected'],
  avg.final=0,
  avg.accept=dac.row['AvgApprovalTime'],
  avg.reject=dac.row['AvgRejectionTime']
   ),...)
}
