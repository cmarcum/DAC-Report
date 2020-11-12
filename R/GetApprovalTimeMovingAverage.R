# Returns a table with the moving average of all studies approved by dac in the given timeframe
# TODO confirm whether looking at approved time is intended behavior
get.approval.time.moving.average <- function(start.date,end.date,action.table.df=nih_dac_action_table) {
  temp.action.table.df <- get.df.within.range(action.table.df,start.date,end.date,date.col="Approved by DAC")
  temp.action.table.df$ApprovedMonth <- lubridate::floor_date(as.Date(temp.action.table.df[,'Approved by DAC'], format = "%m/%d/%Y"),"month")
  temp.action.table.df$ApprovalTimeFromPIToDAC <- difftime(to.time(temp.action.table.df[,"Approved by DAC"]), to.time(temp.action.table.df[,"Submitted by PI"]),units = "days")
  temp.action.table.df$ApprovalTimeFromPIToSO <- difftime(to.time(temp.action.table.df[,"Approved by SO"]), to.time(temp.action.table.df[,"Submitted by PI"]),units = "days")
  temp.action.table.df$ApprovalTimeFromSOToDAC <- difftime(to.time(temp.action.table.df[,"Approved by DAC"]), to.time(temp.action.table.df[,"Approved by SO"]),units = "days")

  return(temp.action.table.df)
  #
  #   monthly.study.approved.df <- stats::aggregate(temp.action.table.df['ApprovalTime'], by=list(Month=temp.action.table.df$ApprovedMonth), FUN=mean)
  #
  #   temp.df <- data.frame(Month=seq(as.Date(start.date),as.Date(end.date), by='month'))
  #   big.df <- merge(temp.df, monthly.study.approved.df,by='Month', all.x=TRUE)
  #   return(big.df)
}
