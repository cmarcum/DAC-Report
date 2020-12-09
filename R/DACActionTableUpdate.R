#' Updating the DAC action table
#'
#' Call this function to update the existing DAC action table data so it has the latest data
#'
#' @param update.to String, date in"mm/dd/yyyy" format, by default it's the system date (today)
#' @param overwrite logical, whether the updated table should overwrite the previously saved table, TRUE by default
#' @param return.table logical, whether the function should return the updated table, FALSE by default
#'
#' @return (optionally) the updated table
#'
#' @examples \dontrun{
#' dac.action.table.update()
#' }
#'
#' @export
dac.action.table.update <- function(update.to=format(Sys.Date(),"%m/%d/%Y"),overwrite=TRUE,return.table=FALSE) {
  print("Updating DAC Action Table...")
  nih_dac_action_table <- get.nih.dac.action.table()
  update.to <- as.Date(update.to,"%m/%d/%Y")
  cur.table.latest <- get.latest.approved.dar.date()

  # Get table starting from latest date, append to current table
  new.table <- request.all.dac.action.table(format(cur.table.latest,"%m/%d/%Y"),format(update.to,"%m/%d/%Y"))
  combined.table <- dplyr::bind_rows(nih_dac_action_table,new.table)

  # Drop rows with same DAR id (only keep the latest one)
  combined.table <- combined.table[!duplicated(combined.table$DAR, fromLast=T),]
  nih_dac_action_table <- combined.table
  print("DAC Action Table update completed")

  if (overwrite) {
    save(nih_dac_action_table,file = system.file("nih_dac_action_table.rda", package = "DACReportingTool"), compress = "xz")
  }
  if (return.table) {
    return(nih_dac_action_table)
  }
}
