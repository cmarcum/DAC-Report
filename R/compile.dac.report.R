compile.dac.report <- function(dac, author, start.date, end.date,...) {

  this.file<- system.file("inst", "report.Rmd", package = "DACReportingTool")

  title <- paste(toupper(dac), "Data Access Committee dbGaP Activity Report \n", paste(start.date,end.date,sep="-"), sep=" ")

  timeline.summary.table <- dar.review.timeline.summary(start.date,end.date)
  print(timeline.summary.table)
  dac.row <- timeline.summary.table[timeline.summary.table['DAC'] == dac,]

  num.dars.in.window <- dac.row[,'TotalRequests']
  number.of.accepted <- dac.row[,'TotalApproved']
  number.of.rejected <- dac.row[,'TotalRejected']

  avg.accept <- dac.row[,'AvgApprovalTime']
  avg.reject <- dac.row[,'AvgRejectionTime']

  dac.total.dars <- nrow(nih_dac_action_table[nih_dac_action_table['DAC'] == dac,])
  dac.total.project <- length(unique(nih_dac_action_table[nih_dac_action_table['DAC'] == dac,]['Project']))

  avg.final <- (avg.accept*number.of.accepted + avg.reject*number.of.rejected) / (number.of.accepted+number.of.rejected)


  rmarkdown::render(this.file, params = list(
  title = title,
  start.date = start.date,
  end.date = end.date,
  author = author,
  dac=dac,
  total.number.of.dars=dac.total.dars,
  total.number.of.projects=dac.total.project,
  num.dars.in.window=num.dars.in.window,
  number.of.accepted=number.of.accepted,
  number.of.rejected=number.of.rejected,
  avg.final=round(avg.final,2),
  avg.accept=round(avg.accept,2),
  avg.reject=round(avg.reject,2),
  timeline.summary.table=timeline.summary.table
   ),...)
}
