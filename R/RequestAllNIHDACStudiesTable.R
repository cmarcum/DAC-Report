#' Request all nih dac studies table (tablea1)
#'
#' Scrapes tablea1, see get.all.nih.dac.studies.table for detail
#'
#' @param start.date String, date in 'mm/dd/yyyy' format
#' @param end.date String, date in 'mm/dd/yyyy' format
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#' tablea1 <- request.all.nih.dac.studies.table()
#' }
request.all.nih.dac.studies.table <- function(start.date="01/01/2000",end.date=format(Sys.Date(),"%m/%d/%Y")) {
  table.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?stDate=%s&endDate=%s&retTable=tablea1"
  table.xpath <- "//tr"
  request.url <- sprintf(table.url,utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE))
  print("Sending request for All NIH DAC Studies (tablea1) table")
  return(get.data.use.summary.table(request.url,table.xpath))
}




