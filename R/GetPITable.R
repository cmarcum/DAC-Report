#' Get PI table
#'
#' Returns a table about PI (Principal Investigator) and the status of their DARs
#'
#' @format A dataframe with 7 variables
#' \describe{
#'   \item{PI}{The ID of the PI}
#'   \item{DAR}{The number of DAR submitted by the PI within the timeframe}
#'   \item{DAC}{The number of DACs the PI submitted DAR to within the timeframe}
#'   \item{Project}{The number of projects the PI submitted DAR to within the timeframe}
#'   \item{Approved by DAC}{The number of DAR submitted by the PI that are approved wiuthin the timeframe}
#'   \item{Rejected by DAC}{The number of DAR submitted by the PI that are rejected within the timeframe}
#'   \item{Data downloaded}{The number of datasets that the PI downloaded within the timeframe}
#' }
#'
#' @param start.date String, date in 'yyyy-mm-dd' format. The start of the timeframe
#' @param end.date String, date in 'yyyy-mm-dd' format. The end of the timeframe
#' @param action.table.df Optional, Dataframe. The table to perform analysis on, defaults to be the nih_dac_action_table.
#'
#' @return dataframe
#' @export
#'
#' @examples \dotnrun {
#' > get.pi.table("2020-01-01","2020-12-31")
#'     PI DAR DAC Project Approved by DAC Rejected by DAC Data downloaded
#' 1   10008   1   1       1               1               0               0
#' 2   10024   1   1       1               1               0               1
#' 3   10035   1   1       1               1               0               0
#' 4   10040   8   3       2               8               0               8
#' 5   10046   5   2       3               4               1               1
#' 6   10047   1   1       1               1               0               0
#' 7    1006   5   1       1               5               5               3
#' 8   10067   2   2       1               2               0               0
#' 9   10074   4   1       4               4               2               0
#' 10  10078   4   2       1               4               0               3
#' }
#'
#'
#'
get.pi.table <- function(start.date,end.date,action.table.df=nih_dac_action_table) {
  action.table.df <- get.df.within.range(action.table.df,start.date,end.date,date.col="Submitted by PI")
  pi.dar.submitted.table <- stats::aggregate(action.table.df[c("DAR","DAC","Project")], by=list(PI=action.table.df$PI), FUN=function(x){length(unique(x))})
  pi.approval.table <- stats::aggregate(action.table.df[c('Approved by DAC', 'Rejected by DAC')], by=list(PI=action.table.df$PI), FUN=function(x){sum(x != "")})
  pi.data.downloaded.table <- stats::aggregate(action.table.df['Data downloaded'], by=list(PI=action.table.df$PI), FUN=function(x){sum(x == 'yes') + sum(x == 'yes in previous version')})
  pi.big.table <- merge(pi.dar.submitted.table,pi.approval.table,by="PI")
  pi.big.table <- merge(pi.big.table,pi.data.downloaded.table,by="PI")
  return(pi.big.table)
}
