#' Update all DAC data
#'
#' Updates every table that are locally stored in the package. Including
#' the nih_dac_actions_table and the all_nih_dac_studies_table. The
#' gs_citation_table is skipped by default for long update time but can be
#' enabled as a parameter
#'
#' @return NULL
#' @export
#'
#' @param update.to String, date in 'yyyy-mm-dd' format. The date to update the data to
#' @param wait.for number, seconds to wait for before sending another request to retrieve phs study
#' @param skip.gs logical, whether to skip updating the gs_citation_table
#'
#' @examples \dontrun{
#' dac.data.update.all()
#' }
#'
dac.data.update.all <- function(update.to=format(Sys.Date(),"%m/%d/%Y"),wait.for=2, skip.gs=TRUE) {
  dac.action.table.update(update.to=update.to,overwrite=TRUE,return.table=FALSE)
  #update.phs.studies.table(overwrite=TRUE,return.table=FALSE,wait.for=wait.for)
  all_nih_dac_studies_table <- request.all.nih.dac.studies.table()
  save(all_nih_dac_studies_table, file = system.file("all_nih_dac_studies_table.rda", package = "DACReportingTool"), compress="xz")
  if (!skip.gs) {
    gs_citation_table <- get.google.scholar.citation.table()
    save(gs_citation_table, file = system.file("gs_citation_table.rda", package = "DACReportingTool"), compress="xz")
  }
  print("All tables updated")
}
