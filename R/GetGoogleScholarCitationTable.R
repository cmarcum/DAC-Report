# Returns the google scholar citation table
#' Get Google Scholar Citation Table
#'
#' Retrieves the locally stored google scholar citation table.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{phs_id}{phs id of the study}
#'   \item{gs_citation_count}{number of search results returned when the phs_id is searched
#'   without including version number (ex. phs000501.v1.p1 -> phs000501)}
#'   \item{last_update}{The date in which the result was obtained on}
#' }
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#' gs.table <- get.google.scholar.citation.table()
#' >             phs_id gs_citation_count last_update
#' 1    phs000501.v1.p1                 5  2020-12-07
#' 2    phs000688.v1.p1                 9  2020-12-07
#' 3    phs001516.v1.p1                 1  2020-12-07
#' 4    phs001477.v1.p1                 0  2020-12-07
#' 5    phs000710.v1.p1                 1  2020-12-07
#' 6    phs000449.v2.p1                 2  2020-12-07
#' 7    phs000678.v1.p1                 3  2020-12-07
#' 8    phs000853.v1.p1                19  2020-12-07
#' }
get.google.scholar.citation.table <- function() {
  load(system.file("gs_citation_table.rda", package = "DACReportingTool"))
  if(!exists("gs_citation_table")) {
    stop("gs_citation_table variable not found. Was the locally file renamed and overwritten?
         To obtain a fresh copy use refresh.local.data()")
  }
  return(gs_citation_table)
}
