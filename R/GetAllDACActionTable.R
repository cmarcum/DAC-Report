
#' Get all DAC action table
#'
#' @param start.date String, date in "mm/dd/yyyy" format
#' @param end.date String, date in "mm/dd/yyyy" format
#'
#' @return Dataframe containing all DAC actions performed within the specified time range
#'
#' @examples \dontrun{
#' > get.all.dac.action.table('09/01/2020','10/01/2020')
#'      DAC    PI Project       DAR  Study accesion  Submitted by PI   ... Data downloaded
#' 2    CDAC   563     501  65316.v4 phs000688.v1.p1 09/07/2020 07:22  ... no
#' 3    CDAC   563   11888  74027.v3 phs000688.v1.p1 09/09/2020 09:31  ... no
#' 4    CDAC  1280     849  73340.v3 phs000688.v1.p1 08/28/2020 11:46  ... yes in previous version
#' 5    CDAC  2418    1395  85271.v3 phs000688.v1.p1 09/02/2020 14:55  ... no
#' 6    CDAC  3464    1955  48719.v9 phs000688.v1.p1 09/21/2020 15:05  ... no
#' ......
#' }
#'
#' @export
get.all.dac.action.table <- function(start.date,end.date) {
  table.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=%s&endDate=%s"
  table.xpath <- "//tr"
  request.url <- sprintf(table.url,utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE))
  print(sprintf("Sending request for DAC action table from %s to %s...",start.date,end.date))
  return(get.data.use.summary.table(request.url,table.xpath))
}

#' Updating the DAC action table
#'
#' Call this function to update the existing DAC action table data so it has the latest data
#'
#' @param update.to String, date in"mm/dd/yyyy" format, by default it's the system date (today)
#' @param overwrite logical, whether the updated table should overwrite the previously saved table, TRUE by default
#' @param return.table logical, whether the function should return the updated table, FALSE by default
#'
#' @return (optionally) the updated table
#'
#' @examples \dontrun{
#' dac.action.table.update()
#' }
#'
#' @export
dac.action.table.update <- function(update.to=format(Sys.Date(),"%m/%d/%Y"),overwrite=TRUE,return.table=FALSE) {
  print("Updating DAC Action Table...")
  load(system.file("data", "nih_dac_action_table.rda", package = "DACReportingTool"))
  update.to <- as.Date(update.to,"%m/%d/%Y")
  cur.table.latest <- get.latest.approved.dar.date()
  # Get table starting from latest date, append to current table
  new.table <- get.all.dac.action.table(format(cur.table.latest,"%m/%d/%Y"),format(update.to,"%m/%d/%Y"))
  combined.table <- dplyr::bind_rows(nih_dac_action_table,new.table)

  # Drop rows with same DAR id (only keep the latest one)
  combined.table <- combined.table[!duplicated(combined.table$DAR, fromLast=T),]
  nih_dac_action_table <- combined.table
  print("DAC Action Table update completed")

  if (overwrite) {
    save(nih_dac_action_table,file = system.file("data", "nih_dac_action_table.rda", package = "DACReportingTool"), compress = "xz")
  }
  if (return.table) {
    return(nih_dac_action_table)
  }
}

# Retrieves table a1 (table is time invariant)
get.all.nih.dac.studies.table <- function(start.date="01/01/2000",end.date=format(Sys.Date(),"%m/%d/%Y")) {
  table.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?stDate=%s&endDate=%s&retTable=tablea1"
  table.xpath <- "//tr"
  request.url <- sprintf(table.url,utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE))
  print("Sending request for All NIH DAC Studies (tablea1) table")
  return(get.data.use.summary.table(request.url,table.xpath))
}

# Note: This retrieves all DARs approved by SO within the time range
get.submitted.dar.by.dac <- function(dac.name,start.date,end.date) {
  table.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=%s&diff=tot&stat=avg&stDate=%s&endDate=%s"
  table.xpath <- "//tr[not(position()=2)]"
  request.url <- sprintf(table.url,utils::URLencode(dac.name,reserved = TRUE),utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE))
  print(sprintf("Sending request for DARs submitted by %s from %s to %s...",dac.name,start.date,end.date))
  return(get.data.use.summary.table(request.url,table.xpath))
}

# Helper functions that grabs tables from the DataUseSummary.cgi page
get.data.use.summary.table <- function(request.url,table.xpath) {
  big.df <- get.multi.row.span.table(request.url,table.xpath)
  # Use first row as column headers
  names(big.df) <- big.df[1,]
  big.df <- big.df[-1,]
  # remove beginning and trailing white spaces
  clean.df <- as.data.frame(apply(big.df,2, trimws, which="both"))
  return(clean.df)
}


