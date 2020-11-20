#' Get phs Studies Table
#'
#' This table contains some of the meta information about each studies that are listed
#' under the all_nih_dac_actions table. The data is obtained through scraping the content
#' on dbGAP
#'
#' @format A data frame with 7 variables:
#' \describe{
#'   \item{Consent group}{Ways in which the data can be used for}
#'   \item{Is IRBrequired?}{Whether IRB permission is needed to use the data}
#'   \item{Data Access Committee}{The DAC to contact for access to the study}
#'   \item{phs_id}{The phs_id associated to the study}
#'   \item{study_key}{The internal key of the study, currently used only for web scraping purposes}
#'   \item{PI}{The principle investigators responsible for the study. Separated by semicolon}
#'   \item{Number ofparticipants}{The number of participants included in the study}
#' }
#'
#' @return
#' @export
#'
#' @examples
get.phs.studies.table <- function() {
  load(system.file("phs_studies_table.rda", package = "DACReportingTool"))
  return(phs_studies_table)
}
