#' Compile DAC Report
#'
#' Creates a report on the DAR (Data Access Request) status of the given DAC
#' (Data Access Commitee) and compare it to the rest of the NIH institutes. The
#' generated report will be a word document.
#'
#' @param dac String, the name of the DAC from which the report will be generated for, one of
#' "CDAC", "ES DAC", "JAAMH", "Kids First DAC", "NCATS", "NCI DAC", "NEI", "NHGRI",
#' "NHLBI", "NIAID", "NIAMS", "NICHD", "NIDCD DAC", "NIDCR", "NIDDK", "NIGMS", "NINDS"
#' "NINR DAC"
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

  this.file<- system.file("inst", "report.Rmd", package = "DACReportingTool")

  title <- paste(toupper(dac), "Data Access Committee dbGaP Activity Report \n", paste(start.date,end.date,sep="-"), sep=" ")

  timeline.summary.table <- dar.review.timeline.summary(start.date,end.date)
  # study.summary.table <- get.study.summary.table(start.date,end.date)
  study.summary.table <- test.study.summary.table
  study.status.table <- get.monthly.study.status('2000-01-01',Sys.Date())
  study.status.table <- filter(study.status.table, study.status.table$Month >= as.Date('2015-01-01'))

  rmarkdown::render(this.file, params = list(
  title = title,
  start.date = start.date,
  end.date = end.date,
  author = author,
  dac=dac,
  timeline.summary.table=timeline.summary.table,
  study.summary.table=study.summary.table,
  study.status.table=study.status.table
   ),...)
}
