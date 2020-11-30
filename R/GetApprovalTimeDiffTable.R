#' Get Approval time difference table
#'
#' Returns a table with the approval time of all studies approved by dac in the given timeframe
#'
#' @param start.date String, date in 'yyyy-mm-dd' format
#' @param end.date String, date in 'yyyy-mm-dd' format
#' @param action.table.df Dataframe the time difference will be calculated on, by default it is the
#' locally stored nih dac action table
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#' table.with.new.columns <- get.approval.time.diff.table()
#' }
get.approval.time.diff.table <- function(start.date,end.date,action.table.df=get.nih.dac.action.table()) {
  temp.action.table.df <- get.df.within.range(action.table.df,start.date,end.date,date.col="Approved by DAC")
  temp.action.table.df$ApprovedMonth <- lubridate::floor_date(as.Date(temp.action.table.df[,'Approved by DAC'], format = "%m/%d/%Y"),"month")
  temp.action.table.df$ApprovalTimeFromPIToDAC <- difftime(to.time(temp.action.table.df[,"Approved by DAC"]), to.time(temp.action.table.df[,"Submitted by PI"]),units = "days")
  temp.action.table.df$ApprovalTimeFromPIToSO <- difftime(to.time(temp.action.table.df[,"Approved by SO"]), to.time(temp.action.table.df[,"Submitted by PI"]),units = "days")
  temp.action.table.df$ApprovalTimeFromSOToDAC <- difftime(to.time(temp.action.table.df[,"Approved by DAC"]), to.time(temp.action.table.df[,"Approved by SO"]),units = "days")

  return(temp.action.table.df)
}
