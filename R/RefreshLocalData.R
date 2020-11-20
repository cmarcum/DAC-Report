#' Refresh Local Data
#'
#' In case of data corruption, request all data and overwrite the existing locally
#' stored data. Use
#'
#' @param start.date the earliest date to get data from. Defaults to be 2000-01-01
#' @param end.date the latest date to get data from. Default to be the system date
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{
#' refresh.local.data()
#' }
#'
refresh.local.data <- function(start.date=as.Date("2000-01-01"),end.date=Sys.Date()) {
  print("Downloading tables to be used locally for DACReportingTool...")
  all.dac.action.table <- request.all.dac.action.table(format(start.date,"%m/%d/%Y"),format(end.date,"%m/%d/%Y"))
  all.nih.dac.studies.table <- request.all.nih.dac.studies.table(start.date,end.date)
  print("Overwriting existing tables...")
  save(all.dac.action.table,file = system.file("nih_dac_action_table.rda", package = "DACReportingTool"), compress = "xz")
  save(all.nih.dac.studies.table, file = system.file("all_nih_dac_studies_table.rda", package = "DACReportingTool"), compress="xz")
  print("Refresh Complete")
}
