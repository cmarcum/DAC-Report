#' DAR Review Timeline Summary
#'
#' Returns a summary dataframe of all DAC which had DAR submitted in the given timeframe
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{DAC}{The name of the DAC (Data Access Committee)}
#'   \item{AvgApprovalTime}{The average number of days for DARs from submitted by PI to be approved by the DAC}
#'   \item{MedApprovalTime}{The median number of days for DARs from submitted by PI to be approved by the DAC}
#'   \item{DARDailyAvg}{The average number of DAR submitted each day in the given timeframe}
#'   \item{DARDailySD}{The standard deviation of DAR submitted each day in the given timeframe}
#'   \item{DARTotal}{The total number of DAR submitted in the given timeframe}
#' }
#'
#' @param start.date String, date in "yyyy-mm-dd" format
#' @param end.date String, date in "yyyy-mm-dd" format
#' @param df optional, the dataframe to be analyzed, by default it is the nih_dac_action_table dataframe
#'
#' @return dataframe
#'
#' @examples \dontrun{
#' > get.dar.review.timeline.summary.table("2020-01-01","2020-10-15")
# DAC TotalApproved AvgApprovalTime MedApprovalTime TotalRejected ...
# 1            CDAC           212  13.339377 days  10.846528 days ...
# 2          ES DAC            69  12.886101 days  11.211111 days ...
# 3           JAAMH          4556  25.447434 days  19.697917 days ...
# 4  Kids First DAC           237  12.969336 days   6.742361 days ...
# 5           NCATS            NA         NA days         NA days ...
#' }
#'
#' @export
get.dar.review.timeline.summary.table <- function(start.date,end.date,df=get.nih.dac.action.table()) {

  submitted.df <- get.df.within.range(df,start.date,end.date,date.col="Submitted by PI")
  submitted.df["time_from_PI_submission_to_DAC_approval"] <- difftime(to.time(submitted.df[,"Approved by DAC"]), to.time(submitted.df[,"Submitted by PI"]),units = "days")
  submitted.df["time_from_PI_submission_to_DAC_rejection"] <- difftime(to.time(submitted.df[,"Rejected by DAC"]), to.time(submitted.df[,"Submitted by PI"]),units = "days")
  total.submitted.count.df <- dplyr::count(submitted.df,DAC, name="TotalRequests")
  # colnames(total.submitted.count.df)[colnames(total.submitted.count.df) == 'n'] <- 'TotalRequests'
  total.downloaded.df <- stats::aggregate(submitted.df["Data downloaded"], by=list(DAC=submitted.df$DAC), FUN=function(x){length(which(x=='yes'))})
  colnames(total.downloaded.df)[colnames(total.downloaded.df) == 'Data downloaded'] <- 'Downloaded'
  previously.downloaded.df <- stats::aggregate(submitted.df["Data downloaded"], by=list(DAC=submitted.df$DAC), FUN=function(x){length(which(x=='yes in previous version'))})
  colnames(previously.downloaded.df)[colnames(previously.downloaded.df) == 'Data downloaded'] <- 'PreviouslyDownloaded'

  # Table of DARs that are approved by DAC
  approved.df <- submitted.df[submitted.df['time_from_PI_submission_to_DAC_approval'] != "", ]
  total.approved.df <- stats::aggregate(approved.df["Approved by DAC"], by=list(DAC=approved.df$DAC), FUN=length)
  avg.approval.days.df <- stats::aggregate(approved.df["time_from_PI_submission_to_DAC_approval"], by=list(DAC=approved.df$DAC), FUN=function(x){mean(x,na.rm = TRUE)})
  med.approval.days.df <- stats::aggregate(approved.df["time_from_PI_submission_to_DAC_approval"], by=list(DAC=approved.df$DAC), FUN=function(x){stats::median(x,na.rm = TRUE)})
  approved.summary.df <- data.frame(DAC=total.approved.df[,'DAC'],
                                    TotalApproved=total.approved.df[,"Approved by DAC"],
                                    AvgApprovalTime=avg.approval.days.df[,"time_from_PI_submission_to_DAC_approval"],
                                    MedApprovalTime=med.approval.days.df[,"time_from_PI_submission_to_DAC_approval"]
  )

  # Table of DARs that are rejected by DAC
  rejected.df <- submitted.df[submitted.df['time_from_PI_submission_to_DAC_rejection'] != "", ]
  total.rejected.df <- stats::aggregate(rejected.df["Rejected by DAC"], by=list(DAC=rejected.df$DAC), FUN=length)
  avg.rejection.days.df <- stats::aggregate(rejected.df["time_from_PI_submission_to_DAC_rejection"], by=list(DAC=rejected.df$DAC), FUN=function(x){mean(x,na.rm = TRUE)})
  med.rejection.days.df <- stats::aggregate(rejected.df["time_from_PI_submission_to_DAC_rejection"], by=list(DAC=rejected.df$DAC), FUN=function(x){stats::median(x,na.rm = TRUE)})
  rejected.summary.df <- data.frame(DAC=total.rejected.df[,'DAC'],
                                    TotalRejected=total.rejected.df[,"Rejected by DAC"],
                                    AvgRejectionTime=avg.rejection.days.df[,"time_from_PI_submission_to_DAC_rejection"],
                                    MedRejectionTime=med.rejection.days.df[,"time_from_PI_submission_to_DAC_rejection"]
  )

  summary.df <- data.frame(DAC=unique(submitted.df['DAC']))
  summary.df <- merge(summary.df,approved.summary.df,by.x='DAC',by.y='DAC',all.x=TRUE)
  summary.df <- merge(summary.df,rejected.summary.df,by.x='DAC',by.y='DAC',all.x=TRUE)
  summary.df <- merge(summary.df,total.submitted.count.df,by.x='DAC',by.y='DAC',all.x=TRUE)
  summary.df <- merge(summary.df,total.downloaded.df,by.x='DAC',by.y='DAC',all.x=TRUE)
  summary.df <- merge(summary.df,previously.downloaded.df,by.x='DAC',by.y='DAC',all.x=TRUE)

  return(summary.df)
}
