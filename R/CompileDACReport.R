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
#' compile.dac.report('CDAC','Committee Chair', '2019-01-01', '2020-01-01')
#' }
#'
#' @export
compile.dac.report <- function(dac, author, start.date, end.date,...) {
  all_nih_dac_studies_table <- get.all.nih.dac.studies.table()
  nih_dac_action_table <- get.nih.dac.action.table()
  # Markdown template to use
  this.file<- system.file("report.Rmd", package = "DACReportingTool")

  title <- paste(toupper(dac), "Data Access Committee dbGaP Activity Report \n", paste(start.date,end.date,sep="-"), sep=" ")

  dac.specific.studies.table <- all_nih_dac_studies_table[all_nih_dac_studies_table["DAC"] == dac,]
  dac.specific.action.table <- nih_dac_action_table[nih_dac_action_table["DAC"] == dac,]

  print('Computing Review Timelime Summary Table...')
  timeline.summary.table <- dar.review.timeline.summary(start.date,end.date)

  print('Computing Study Summary Table...')
  study.summary.table <- get.study.summary.table(start.date,end.date)

  print("Computing Top Study Term Frequency Table...")
  dac.study.summary.table <- study.summary.table[study.summary.table['DAC'] == dac,]
  dac.study.summary.table <- get.df.within.range(dac.study.summary.table,start.date,end.date,"Study Release Date")
  dac.study.summary.table.ordered <- dac.study.summary.table[order(-dac.study.summary.table$TotalRequest),]
  phs.popular.this.dac <- dac.study.summary.table.ordered[1,]['StudyAccesion']$StudyAccesion
  top.study.term.freq.table <- get.phs.study.term.frequency.table(phs.popular.this.dac)

  print('Computing Monthly Study Status Table...')
  study.status.table.all <- get.monthly.study.status.table('2000-01-01',Sys.Date())
  study.status.table.all <- study.status.table.all[study.status.table.all$Month >= as.Date('2015-01-01'),]
  study.status.table.dac <- get.monthly.study.status.table('2000-01-01',Sys.Date(), dac.specific.studies.table,dac.specific.action.table)
  study.status.table.dac <- study.status.table.dac[study.status.table.dac$Month >= as.Date('2015-01-01'),]

  print('Computing PI Table...')
  pi.requests.table.overall <- get.pi.table('2000-01-01',Sys.Date())
  pi.requests.table.selected.time <- get.pi.table(start.date,end.date)

  print('Computing Monthly Approval Time Median Table...')
  approval.time.diff.table <- get.approval.time.diff.table(start.date,end.date,dac.specific.action.table)

  print('All tables calculated. Rendering...')

  rmarkdown::render(this.file, params = list(
  title = title,
  start.date = start.date,
  end.date = end.date,
  author = author,
  dac=dac,
  nih.dac.action.table=nih_dac_action_table,
  timeline.summary.table=timeline.summary.table,
  study.summary.table=study.summary.table,
  top.study.term.freq.table=top.study.term.freq.table,
  study.status.table.all=study.status.table.all,
  study.status.table.dac=study.status.table.dac,
  pi.requests.table.overall=pi.requests.table.overall,
  pi.requests.table.selected.time=pi.requests.table.selected.time,
  approval.time.diff.table=approval.time.diff.table
   ),...)

  shell.exec(system.file("report.docx", package="DACReportingTool"))

}
