#' Compile DAC Report
#'
#' Creates a report on the DAR (Data Access Request) status of the given DAC
#' (Data Access Commitee) and compare it to the rest of the NIH institutes. The
#' generated report will be a word document.
#'
#' @param dac String, the name of the DAC from which the report will be generated for, one of
#' `r get.supported.dacs()`
#'
#' @param author String, The name of the author who created this report
#' @param start.date String, date in "yyyy-mm-dd" format, the start date of the timeframe
#' in which the analysis will be based on
#' @param end.date String, date in "yyyy-mm-dd" format, the end date of the timeframe in
#' which the analysis will be based on
#' @param ... extra arguments to be provided to rmarkdown::render
#'
#' @return NULL
#'
#' @examples \dontrun{
#' compile.dac.report('NIAID','Hoyin Chu, Christopher Marcum', '2015-01-01', '2019-12-31')
#' }
#'
#' @export
compile.dac.report <- function(dac, author, start.date, end.date,...) {
  # Markdown template to use
  this.file<- system.file("report.Rmd", package = "DACReportingTool")

  title <- paste(toupper(dac), "Data Access Committee dbGaP Activity Report \n", paste(start.date,end.date,sep="-"), sep=" ")

  dac.specific.studies.table <- all_nih_dac_studies_table[all_nih_dac_studies_table["DAC"] == dac,]
  dac.specific.action.table <- nih_dac_action_table[nih_dac_action_table["DAC"] == dac,]

  timeline.summary.table <- dar.review.timeline.summary(start.date,end.date)
  study.summary.table <- get.study.summary.table(start.date,end.date)

  study.status.table.all <- get.monthly.study.status.table('2000-01-01',Sys.Date())
  study.status.table.all <- study.status.table.all[study.status.table.all$Month >= as.Date('2015-01-01'),]

  pi.requests.table.overall <- get.pi.table('2000-01-01',Sys.Date())
  pi.requests.table.selected.time <- get.pi.table(start.date,end.date)

  study.status.table.dac <- get.monthly.study.status.table('2000-01-01',Sys.Date(), dac.specific.studies.table,dac.specific.action.table)
  print(study.status.table.dac)

  study.status.table.dac <- study.status.table.dac[study.status.table.dac$Month >= as.Date('2015-01-01'),]

  print(study.status.table.dac)

  approval.time.moving.avg.table <- get.approval.time.moving.average(start.date,end.date,dac.specific.action.table)

  rmarkdown::render(this.file, params = list(
  title = title,
  start.date = start.date,
  end.date = end.date,
  author = author,
  dac=dac,
  timeline.summary.table=timeline.summary.table,
  study.summary.table=study.summary.table,
  study.status.table.all=study.status.table.all,
  study.status.table.dac=study.status.table.dac,
  pi.requests.table.overall=pi.requests.table.overall,
  pi.requests.table.selected.time=pi.requests.table.selected.time,
  approval.time.moving.avg.table=approval.time.moving.avg.table
   ),...)

  shell.exec(system.file("report.docx", package="DACReportingTool"))

}
