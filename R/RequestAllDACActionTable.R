#' Get all DAC action table
#'
#' Makes a request to the Data Use Summary CGI script and scraps the table data.
#' For dataframe detail see documentation for get.nih.dac.action.table()
#'
#' @param start.date String, date in "mm/dd/yyyy" format
#' @param end.date String, date in "mm/dd/yyyy" format
#'
#' @return Dataframe containing all DAC actions performed within the specified time range
#'
#' @examples \dontrun{
#' > request.all.dac.action.table('09/01/2020','10/01/2020')
#'      DAC    PI Project       DAR  Study accesion  Submitted by PI   ... Data downloaded
#' 2    CDAC   563     501  65316.v4 phs000688.v1.p1 09/07/2020 07:22  ... no
#' 3    CDAC   563   11888  74027.v3 phs000688.v1.p1 09/09/2020 09:31  ... no
#' 4    CDAC  1280     849  73340.v3 phs000688.v1.p1 08/28/2020 11:46  ... yes in previous version
#' 5    CDAC  2418    1395  85271.v3 phs000688.v1.p1 09/02/2020 14:55  ... no
#' 6    CDAC  3464    1955  48719.v9 phs000688.v1.p1 09/21/2020 15:05  ... no
#' ......
#' }
#'
#' @export
request.all.dac.action.table <- function(start.date,end.date) {
  table.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=%s&endDate=%s"
  table.xpath <- "//tr"
  request.url <- sprintf(table.url,utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE))
  print(sprintf("Sending request for DAC action table from %s to %s...",start.date,end.date))
  return(get.data.use.summary.table(request.url,table.xpath))
}
