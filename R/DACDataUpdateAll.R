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
dac.data.update.all <- function() {
  dac.action.table.update()
  update.phs.studies.table()
  all_nih_dac_studies_table <- get.all.nih.dac.studies.table()
  save(all_nih_dac_studies_table, file = system.file("data","all_nih_dac_studies_table.rda", package = "DACReportingTool"), compress="xz")
  print("All tables updated")
}
