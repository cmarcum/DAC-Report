#' Refresh Local Data
#'
#' Request all data and overwrite the existing locally
#' stored data. May take 1-2 hours
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

  nih_dac_action_table <- request.all.dac.action.table(format(start.date,"%m/%d/%Y"),format(end.date,"%m/%d/%Y"))
  nih_dac_action_table <- nih_dac_action_table[!duplicated(nih_dac_action_table$DAR, fromLast=T),]
  all_nih_dac_studies_table <- request.all.nih.dac.studies.table(format(start.date,"%m/%d/%Y"),format(end.date,"%m/%d/%Y"))
  unique.study.ids <- unique(all_nih_dac_studies_table['Study accesion'])$`Study accesion`
  gs_citation_table <- request.google.scholar.citation.table(unique.study.ids,2)

  print("Overwriting existing tables...")
  save(nih_dac_action_table,file = system.file("nih_dac_action_table.rda", package = "DACReportingTool"), compress = "xz")
  save(all_nih_dac_studies_table, file = system.file("all_nih_dac_studies_table.rda", package = "DACReportingTool"), compress="xz")
  save(gs_citation_table, file = system.file("gs_citation_table.rda", package = "DACReportingTool"), compress="xz")

  print("Refresh Complete")
}
