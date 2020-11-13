#' Get Monthly Study Status Table
#'
#' Returns a table comparing study released and requests made each month
#'
#' @format
#' \describe{
#'   \item{StudiesReleased}{The number of studies released that month in the given studies information dataframe}
#'   \item{TotalRequests}{The number of requests made that made in the given DAC action information dataframe}
#'   \item{StudiesReleasedCummulative}{The cummulative number of studies released up until that row}
#'   \item{TotalRequestsCummulative}{The cummulative number of DAR made up until that row}
#'   \item{StudiesReleasedGrowth}{The percentage growth of studies released compared to previous month}
#'   \item{TotalRequestsGrowth}{The percentage growth of DAR compared to previous month}
#' }
#'
#' @param start.date String, date in 'yyyy-mm-dd' format. Start date of the timeframe
#' @param end.date String, date in 'yyyy-mm-dd' format. End date of the timeframe
#' @param studies.df optional, dataframe. The dataframe containing studies information
#' to be used in the analysis, by default it is the all_nih_dac_studies_table dataframe
#' @param action.table.df optional, dataframe. The dataframe containing DAC action information
#' to be used in the analysis, by default it is the nih_dac_action_table dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples \dontrun{
#' > get.monthly.study.status('2020-01-01','2020-12-31')
#'         Month StudiesReleased TotalRequests StudiesReleasedCummulative TotalRequestsCummulative ...
#' 1  2020-01-01              17          1514                         17                     1514 ...
#' 2  2020-02-01              29          2837                         46                     4351 ...
#' 3  2020-03-01              26          2696                         72                     7047 ...
#' 4  2020-04-01              30          3261                        102                    10308 ...
#' 5  2020-05-01              21          2922                        123                    13230 ...
#' 6  2020-06-01              11          3716                        134                    16946 ...
#' 7  2020-07-01              16          3144                        150                    20090 ...
#' 8  2020-08-01               8          3131                        158                    23221 ...
#' }
#'
#'
get.monthly.study.status.table <- function(start.date,end.date,studies.df=all_nih_dac_studies_table,action.table.df=nih_dac_action_table) {

  # Aggregate of studies released by month
  temp_studies_table <- studies.df
  temp_studies_table$ReleaseMonth <- lubridate::floor_date(as.Date(temp_studies_table[,'Study Release Date'], format = "%m/%d/%Y"),"month")
  monthly.study.released.df <- stats::aggregate(temp_studies_table['Study Name'], by=list(Month=temp_studies_table$ReleaseMonth), FUN=length)

  # Aggreagate of requests made by month
  temp_nih_dac_action_table <- action.table.df
  temp_nih_dac_action_table$ApprovalMonth <- lubridate::floor_date(to.time(temp_nih_dac_action_table[,'Approved by DAC']),"month")
  monthly.dac.approval.df <- stats::aggregate(temp_nih_dac_action_table['DAR'], by=list(Month=temp_nih_dac_action_table$ApprovalMonth), FUN=length)
  monthly.dac.approval.df$Month <- as.Date(monthly.dac.approval.df$Month)

  # Join the two tables
  temp.df <- data.frame(Month=seq(as.Date(start.date),as.Date(end.date), by='month'))
  big.df <- merge(temp.df, monthly.study.released.df,by='Month', all.x=TRUE)
  big.df <- merge(big.df, monthly.dac.approval.df, by.x='Month', by.y='Month', all.x=TRUE)
  big.df[is.na(big.df)] <- 0

  # Rename columns appropriately
  names(big.df)[names(big.df) == 'Study Name'] <- 'StudiesReleased'
  names(big.df)[names(big.df) == 'DAR'] <- 'TotalRequests'

  big.df$StudiesReleasedCummulative <- cumsum(big.df[,"StudiesReleased"])
  big.df$TotalRequestsCummulative <- cumsum(big.df[,"TotalRequests"])
  big.df$StudiesReleasedGrowth <- big.df$StudiesReleasedCummulative / lag(big.df$StudiesReleasedCummulative) - 1
  big.df$TotalRequestsGrowth <- big.df$TotalRequestsCummulative / lag(big.df$TotalRequestsCummulative) - 1

  return(big.df)
}
