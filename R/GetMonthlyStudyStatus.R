# Returns a table with monthly comparison of Total DAR and Studies Released
get.monthly.study.status <- function(start.date,end.date,studies.df=all_nih_dac_studies_table,action.table.df=nih_dac_action_table) {
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

  return(big.df)
}
