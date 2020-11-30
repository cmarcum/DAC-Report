#' Returns The NIH DAC Studies Table
#'
#' Returns the locally stored dataframe containing all NIH DAC studies
#'
#' This table contains all phs studies along with the DAC that they associated to and some of their
#' basic study information. This table is an exact copy of tablea1 on the Data Use Summary
#' Page.
#'
#' @format A data frame with 5 variables:
#' \describe{
#'   \item{DAC}{The DAC (Data Access Committee) in charge of the study}
#'   \item{Study Accession}{The phs id of the study}
#'   \item{Study Release Date}{The date in which the study was released}
#'   \item{Embargo End Date}{The date in which the embargo of the study ends}
#'   \item{Study Name}{The name of the study}
#' }
#' @source \url{https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?stDate=05%2F13%2F2020&endDate=11%2F13%2F2020&retTable=tablea1}
#'
#' @return Dataframe
#'
#' @export
get.all.nih.dac.studies.table <- function() {
  load(system.file("all_nih_dac_studies_table.rda", package = "DACReportingTool"))
  if(!exists("all_nih_dac_studies_table")) {
    stop("all_nih_dac_studies_table variable not found. Was the locally file renamed and overwritten?
         To obtain a fresh copy use refresh.local.data()")
  }
  return(all_nih_dac_studies_table)
}
