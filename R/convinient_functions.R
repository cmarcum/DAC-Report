
#' DAR Review Timeline Summary
#'
#' Returns a summary dataframe of all DAC which had DAR approved in the given timeframe
#' .
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
#' @param df the dataframe to be analyzed, this should be the nih_dac_action_table dataframe
#' @param start.date String, date in "yyyy-mm-dd" format,
#' @param end.date String, date in "yyyy-mm-dd" format
#'
#' @return dataframe
#'
#' @examples \dontrun{
#' > dar.review.timeline.summary(nih_dac_action_table,"2020-01-01","2020-10-15")
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
dar.review.timeline.summary <- function(df,start.date,end.date) {
  approved.df <- get.df.within.range(df,start.date,end.date,date.col="Approved by DAC")
  approved.df["time_from_PI_submission_to_DAC_approval"] <- difftime(to.time(approved.df[,"Approved by DAC"]), to.time(approved.df[,"Submitted by PI"]),units = "days")

  avg.approval.days <- aggregate(approved.df["time_from_PI_submission_to_DAC_approval"], by=list(DAC=approved.df$DAC), FUN=mean)
  med.approval.days <- aggregate(approved.df["time_from_PI_submission_to_DAC_approval"], by=list(DAC=approved.df$DAC), FUN=median)

  summary.df <- avg.approval.days
  summary.df['time_from_PI_submission_to_DAC_approval'] <- NULL
  summary.df['AvgApprovalTime'] <- avg.approval.days['time_from_PI_submission_to_DAC_approval']
  summary.df['MedApprovalTime'] <- med.approval.days['time_from_PI_submission_to_DAC_approval']

  daily.vector.list <- list()
  for (i in 1:nrow(summary.df)) {
    print(sprintf("Computing daily data for %s",summary.df[i,'DAC']))
    daily.vector.list[[i]] <- get.dac.daily.approved.dar.vector(df,summary.df[i,'DAC'],start.date,end.date)
  }

  summary.df['DARDailyAvg'] <- unlist(lapply(daily.vector.list,mean))
  summary.df['DARDailySD'] <- unlist(lapply(daily.vector.list,sd))
  summary.df['DARTotal'] <- unlist(lapply(daily.vector.list,sum))

  return(summary.df)
}

# returns a vector of length (start.date - end.date) where each element = DAR report sent that day
get.dac.daily.approved.dar.vector <- function(df,dac.name,start.date,end.date) {
  dac.df <- df[df['DAC'] == dac.name,]
  approved.df <- get.df.within.range(dac.df,start.date,end.date,date.col="Approved by DAC")
  dac.avg.time <- difftime(to.time(approved.df[,"Approved by DAC"]), to.time(approved.df[,"Submitted by PI"]),units = "days")
  approved.df['time_from_PI_submission_to_DAC_approval'] <- dac.avg.time
  date.seq <- seq(as.Date(start.date),as.Date(end.date),by="days")
  dar.number.list <- list()
  for (i in 1:length(date.seq)) {
    dar.number.list[i] <- nrow(get.df.within.range(approved.df,date.seq[[i]],date.seq[[i]]))
  }
  dar.number.vector <- unlist(dar.number.list)
  return(dar.number.vector)
}

# Produce subset of df that falls within the given time range
get.df.within.range <- function(df,start.date,end.date,date.col="Approved by DAC") {
  return(subset(df, as.Date(df[,date.col], format="%m/%d/%Y") >= start.date & as.Date(df[,date.col], format="%m/%d/%Y") <= end.date))
}

# Converts the given column to POSIX time
to.time <- function(col) {
  return(as.POSIXct(col, format="%m/%d/%Y %H:%M", tz="EST"))
}
