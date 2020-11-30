#' Get Study Monthly Growth
#'
#' Returns a data frame with month and the number of times the study is requested per Month
#'
#' @param phs.id String, the phs id of the study
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#' get.study.momthly.growth('phs001542.v1.p1')
#' >       Month DAR
#' 1  2019-08-01   1
#' 2  2019-09-01   1
#' 3  2019-10-01   2
#' 4  2019-11-01   1
#' 5  2019-12-01   2
#' 6  2020-02-01   2
#' 7  2020-03-01   1
#' 8  2020-05-01   2
#' 9  2020-06-01   4
#' 10 2020-08-01   5
#' 11 2020-09-01   6
#' 12 2020-10-01   4
#' 13 2020-11-01   1
#'
#' }
get.study.momthly.growth <- function(phs.id) {
  dac.action.table <- get.nih.dac.action.table()
  dac.action.table <- dac.action.table[dac.action.table['Study accesion'] == phs.id,]
  dac.action.table$ReleaseMonth <- lubridate::floor_date(as.Date(dac.action.table[,'Submitted by PI'], format = "%m/%d/%Y"),"month")
  monthly.requests.table <- stats::aggregate(dac.action.table['ReleaseMonth'], by=list(Month=dac.action.table$ReleaseMonth), FUN=length)
  names(monthly.requests.table)[names(monthly.requests.table) == 'ReleaseMonth'] <- 'DAR'
  return(monthly.requests.table)
}
