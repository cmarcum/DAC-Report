#' Returns the locally stored DAC action table
#'
#' @return The locally stored DAC action table
#'
#' @format A data frame with 11 variables:
#' \describe{
#'   \item{DAC}{The DAC (Data Access Committee) in which the DAR was from}
#'   \item{PI}{ID of the PI who made the request}
#'   \item{Project}{ID of the project in which the request was made}
#'   \item{DAR}{The ID of the request}
#'   \item{Study accesion}{The phs number of the study in which the DAR was made for}
#'   \item{Submitted by PI}{The time when the DAR was made}
#'   \item{Approved by SO}{The time when the DAR is approved by SO}
#'   \item{Approved by DAC}{The time when the DAR is approved by DAC}
#'   \item{Rejected by DAC}{The time when the DAR is rejected by DAC}
#'   \item{Revision requested by DAC}{The time when the DAR is requested a revision by DAC}
#'   \item{Data downloaded}{Whether the data requested is downloaded}
#' }
#' @source \url{https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=10/13/2020&endDate=10/23/2020}
#'
#' @examples \dontrun{
#' > get.nih.dac.action.table()
#'      DAC    PI Project       DAR  Study accesion  Submitted by PI   ... Data downloaded
#' 2    CDAC   563     501  65316.v4 phs000688.v1.p1 09/07/2020 07:22  ... no
#' 3    CDAC   563   11888  74027.v3 phs000688.v1.p1 09/09/2020 09:31  ... no
#' 4    CDAC  1280     849  73340.v3 phs000688.v1.p1 08/28/2020 11:46  ... yes in previous version
#' 5    CDAC  2418    1395  85271.v3 phs000688.v1.p1 09/02/2020 14:55  ... no
#' 6    CDAC  3464    1955  48719.v9 phs000688.v1.p1 09/21/2020 15:05  ... no
#' ......
#' }
#'
get.nih.dac.action.table <- function() {
  load(system.file("nih_dac_action_table.rda", package = "DACReportingTool"))
  if(!exists("nih_dac_action_table")) {
    stop("nih_dac_action_table variable not found. Was the locally file renamed and overwritten?
         To obtain a fresh copy use refresh.local.data()")
  }
  return(nih_dac_action_table)
}
