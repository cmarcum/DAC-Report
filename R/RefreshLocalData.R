#' Refresh Local Data
#'
#' Request all data and overwrite the existing locally
#' stored data.
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
  date.ranges <- as.list(seq(start.date,end.date,by="1 year"))
  date.ranges[[length(date.ranges) + 1]] <- end.date
  table.list <- list()
  for (i in 1:(length(date.ranges)-1)) {
    action.table <- request.all.dac.action.table(format(date.ranges[[i]],"%m/%d/%Y"),format(date.ranges[[i+1]],"%m/%d/%Y"))
    if (nrow(action.table) > 0) {
      table.list[[i]] <- action.table
    }
  }
  nih_dac_action_table <- data.table::rbindlist(table.list,fill = TRUE)
  nih_dac_action_table <- nih_dac_action_table[!duplicated(nih_dac_action_table$DAR, fromLast=T),]

  all_nih_dac_studies_table <- request.all.nih.dac.studies.table(format(start.date,"%m/%d/%Y"),format(end.date,"%m/%d/%Y"))

  print("Overwriting existing tables...")
  save(nih_dac_action_table,file = system.file("nih_dac_action_table.rda", package = "DACReportingTool"), compress = "xz")
  save(all_nih_dac_studies_table, file = system.file("all_nih_dac_studies_table.rda", package = "DACReportingTool"), compress="xz")
  print("Refresh Complete")
}
