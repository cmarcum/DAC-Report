#' Update all DAC data
#'
#' Updates every table that are locally stored in the package. Including
#' the nih_dac_actions_table and the all_nih_dac_studies_table
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{
#' dac.data.update.all()
#' }
#'
dac.data.update.all <- function(update.to=format(Sys.Date(),"%m/%d/%Y"),wait.for=2) {
  dac.action.table.update(update.to=update.to,overwrite=TRUE,return.table=FALSE)
  update.phs.studies.table(overwrite=TRUE,return.table=FALSE,wait.for=wait.for)
  all_nih_dac_studies_table <- request.all.nih.dac.studies.table()
  save(all_nih_dac_studies_table, file = system.file("all_nih_dac_studies_table.rda", package = "DACReportingTool"), compress="xz")
  print("All tables updated")
}
