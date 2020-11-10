# Returns a table about PI and their DAR status
get.pi.table <- function(start.date,end.date,action.table.df=nih_dac_action_table) {
  action.table.df <- get.df.within.range(action.table.df,start.date,end.date,date.col="Submitted by PI")
  pi.dar.submitted.table <- stats::aggregate(action.table.df[c("DAR","DAC","Project")], by=list(PI=action.table.df$PI), FUN=function(x){length(unique(x))})
  pi.approval.table <- stats::aggregate(action.table.df[c('Approved by DAC', 'Rejected by DAC')], by=list(PI=action.table.df$PI), FUN=function(x){sum(x != "")})
  pi.data.downloaded.table <- stats::aggregate(action.table.df['Data downloaded'], by=list(PI=action.table.df$PI), FUN=function(x){sum(x == 'yes') + sum(x == 'yes in previous version')})
  pi.big.table <- merge(pi.dar.submitted.table,pi.approval.table,by="PI")
  pi.big.table <- merge(pi.big.table,pi.data.downloaded.table,by="PI")
  return(pi.big.table)
}
