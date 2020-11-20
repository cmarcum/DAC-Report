#' Get Study Summary Table
#'
#' Returns a summary dataframe of all studies that received requests in the given timeframe. Note that some studies
#' can be both approved and rejected (ex. approved but later rejected ), or neither (still in process of approval)
#'
#' @param start.date String, date in "yyyy-mm-dd" format
#' @param end.date String, date in "yyyy-mm-dd" format
#' @param df optional, the dataframe to be analyzed, by default it is the nih_dac_action_table dataframe
#'
#' @format A data frame with 12 variables:
#' \describe{
#'   \item{StudyAccession}{The phs number of the study}
#'   \item{TotalRequest}{Total number of DAR submitted by PI for the study in the given timeframe}
#'   \item{TotalApproval}{Total number of DAR approved by DAC for the study in the given timeframe}
#'   \item{TotalRejected}{Total number of DAR rejected by DAC for the study in the given timeframe}
#'   \item{TotalDownloaded}{Total number of times the study is downloaded}
#'   \item{AvgApprovalTime}{The average number of days it takes for a DAR for the study to be approved}
#'   \item{ApprovalRate}{Proportion of DAR that are approved. Calcualted by TotalApproved / (TotalApproved+TotalRejected)}
#'   \item{DownloadRate}{Proportion of DAR that are downloaded. Calculated by TotalDownload / TotalRequest}
#'   \item{DAC}{Name of the DAC that submitted the DAR for the study}
#'   \item{Study Release Date}{Date in which the study is released}
#'   \item{Embargo End Date}{Date in which the embargo of the study is lifted}
#'   \item{Study Name}{Name of the study}
#' }
#'
#' @return dataframe
#'
#' @examples \dontrun{
#' > get.study.summary.table("2020-01-01","2020-10-15")
#'        StudyAccesion TotalRequest TotalApproved TotalRejected TotalDownload AvgApprovalTime ...
#' 1    phs000001.v3.p1          106            94             5            59       31.205245 ...
#' 2  phs000007.v31.p12          440           312           133           228       17.452357 ...
#' 3    phs000016.v2.p2           27            24             0             9       21.924392 ...
#' 4    phs000017.v3.p1          188           171             0           108       22.340318 ...
#' 5    phs000018.v2.p1           16            12             0             7       57.746181 ...
#' 6    phs000019.v1.p1           45            36             8            16       11.507002 ...
#' 7    phs000020.v2.p1           51            49             0            22       21.797038 ...
#' 8    phs000021.v3.p2          196           184             0            99       21.470720 ...
#' 9    phs000048.v1.p1           34            30             5            15        7.769923 ...
#' 10   phs000086.v3.p1           37            31             0            20       49.872446 ...
#' ......
#'}
#'
#' @export
get.study.summary.table <- function(start.date = '2000-01-01',end.date=format(Sys.Date()), df=get.nih.dac.action.table()) {
  submitted.df <- get.df.within.range(df,start.date,end.date,date.col="Submitted by PI")
  submitted.df$`TimeToApproval` <- difftime(to.time(submitted.df[,'Approved by DAC']),to.time(submitted.df[,'Submitted by PI']), units="days")

  all.studies <- unique(submitted.df[,'Study accesion'])
  studies.status.table <- stats::aggregate(submitted.df[c('Submitted by PI','Approved by DAC','Rejected by DAC')], by=list(StudyAccession=submitted.df$`Study accesion`), FUN=function(x){sum(x!='')})
  studies.download.table <- stats::aggregate(submitted.df['Data downloaded'], by=list(StudyAccession=submitted.df$`Study accesion`), FUN=function(x){sum(x == 'yes') + sum(x == 'yes in previous version')})
  studies.avg.approval.time.table <-  stats::aggregate(submitted.df['TimeToApproval'], by=list(StudyAccession=submitted.df$`Study accesion`), FUN=function(x){mean(x,na.rm = TRUE)})
  studies.big.table <- merge(studies.status.table,studies.download.table,by="StudyAccession")
  studies.big.table <- merge(studies.big.table,studies.avg.approval.time.table, by="StudyAccession")
  colnames(studies.big.table) <- c("StudyAccesion","TotalRequest","TotalApproved","TotalRejected","TotalDownload","AvgApprovalTime")

  studies.big.table <- merge(studies.big.table,get.all.nih.dac.studies.table(), by.x='StudyAccesion', by.y='Study Accession',all.x=TRUE)
  return(studies.big.table)
}
