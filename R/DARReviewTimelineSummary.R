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
#' > dar.review.timeline.summary("2020-01-01","2020-10-15",df=nih_dac_action_table)
#'               DAC AvgApprovalTime MedApprovalTime DARDailyAvg DARDailySD DARTotal
#' 1            CDAC  14.916372 days  11.356944 days  0.66782007  2.5414008      193
#' 2          ES DAC  24.247849 days  12.220139 days  0.21453287  0.7788412       62
#' 3           JAAMH  26.784148 days  20.125694 days 15.78892734 55.4647074     4563
#' 4  Kids First DAC  14.286924 days   8.137500 days  0.70934256  2.4206754      205
#' 5         NCI DAC   6.313179 days   3.250000 days 31.04152249 42.8157998     8971
#' 6             NEI  34.319430 days  32.668056 days  1.31141869  7.4582105      379
#' 7           NHGRI   9.485872 days   5.884028 days 17.10380623 23.3027244     4943
#' 8           NHLBI  18.941702 days  10.945833 days 20.68512111 38.4412587     5978
#' 9          NIAID   42.239651 days  30.909028 days  0.32179931  1.4204272       93
#' 10          NIAMS  15.745469 days  14.146528 days  0.93771626  3.4725505      271
#' 11          NICHD  12.744192 days   6.727778 days  0.69550173  2.1206689      201
#' 12          NIDCR  18.579025 days  16.580903 days  0.49134948  1.9792573      142
#' 13          NIDDK  54.891503 days  55.802778 days  2.10380623 11.2569247      608
#' 14          NIGMS  33.638505 days  31.440278 days  0.90657439  4.8578679      262
#' 15          NINDS  10.718166 days   7.164583 days  3.26989619  8.2274414      945
#' 16       NINR DAC  12.314583 days  12.727083 days  0.01730104  0.1549361        5
#' }
#'
#' @export
dar.review.timeline.summary <- function(start.date,end.date,df=DACReportingTool::nih_dac_action_table) {

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
