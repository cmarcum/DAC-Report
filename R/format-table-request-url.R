
#' formatTableRequestUrl
#'
#' Formats the given parameters as a query string that can be accepted by the dbGaP Data Access and Use Report
#' site (https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi)
#' table.name is one of table<1-9>, tablea1, tableb1, tablec1, tabled1
#'
#' @param table.name String, name of table to query from
#' @param start.date String, date in mm/dd/yyyy format
#' @param end.date String, date in mm/dd/yyyy format
#'
#' @return the formated query string to be sent
#' @export
#'
#' @examples
#' formatTableRequestUrl('table2','01/01/2020','10/20/2020')
formatTableRequestUrl <- function(table.name,start.date,end.date) {
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?stDate=%s&endDate=%s&retTable=%s"
  request.url <- sprintf(base.url, URLencode(start.date,reserved = TRUE), URLencode(end.date,reserved = TRUE), table.name)
  return(request.url)
}
