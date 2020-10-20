#' @importFrom magrittr "%>%"

# Helper function, formats the given parameters as a request url
format.table.request.url <- function(table.name,start.date,end.date) {
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?stDate=%s&endDate=%s&retTable=%s"
  request.url <- sprintf(base.url, utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE), table.name)
  return(request.url)
}

#' Download DAR Tables
#'
#' Download all daily data from the specified table within the time range as a csv file
#' This function scrapes the data available on the dbGaP Data Access and Use Report page
#' (https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi)
#' table.name is one of table<1-9>, tablea1, tableb1, tablec1, tabled1
#' Currently supports scraping: table1,2,6,a1
#' No data: 4
#' Not currently supported due to malformatted row: 3,5,B1,C1,D1
#' Support for 3,7 will come later
#' Note: tablea1 is time invariant (same content regardless of start or end date)# Currently Supports: table1,2,6,a1
#' The example will send 365 requests consecutively, each spaced by 5 seconds, to query table2
#' with the date intervals being 01/01/2020-01/02/2020, ..., 12/31/2020-01/01/2021
#' and store the result at './data/table2_2020.csv'
#'
#' @param table.name String, name of the table to query from
#' @param start.date String, the date to start scraping in mm/dd/yyyy format
#' @param end.date String, the date to end scraping in mm/dd/yyyy format
#' @param directory String, the path (file name included) in which the scraped data should be stored
#' @param wait.for integer, how many seconds to wait before sending another request to the server
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' download.dar.tables('table2','01/01/2020','12/31/2020','./data/table2_2020.csv',5)
#' }
#' @export
download.dar.tables <- function(table.name,start.date,end.date,directory,wait.for=5) {
  if (table.name == 'tablea1') {
    table.a1.url <- format.table.request.url(table.name,start.date,end.date)
    table.a1.xpath <- "//tr"
    big.df <- get.multi.row.span.table(table.a1.url,table.a1.xpath)
    names(big.df) <- big.df[1,]
    big.df <- big.df[-1,]
  }
  else {
    df.list <- get.table.from.time(table.name,start.date,end.date,wait.for)
    big.df <- data.table::rbindlist(df.list)
  }
  utils::write.csv(big.df,directory,row.names = FALSE)
}

# Helper function, fetches the table as a dataframe parsed by htmltab
fetch.dar.table <- function(table.name,start.date,end.date) {
  request.url <- format.table.request.url(table.name,start.date,end.date)
  print(sprintf("Sending request for %s from %s to %s...",table.name,start.date,end.date))
  #lines <- rvest::html_nodes(xml2::read_html(request.url),xpath="//tr/ancestor::table")
  # get the lines of the table
  # lines <- request.url %>%
  #   read_html() %>%
  #   html_nodes(xpath="//tr")
  #
  # html.tab.table <- htmltab::htmltab(doc = lines)
  # return(html.tab.table)
  html.tab.table <- htmltab::htmltab(doc = request.url, which = "//tr/ancestor::table")
  return(html.tab.table)
}

# Helper function, cleans up the fetched table by converting non-utf-8 characters to NA
# and changing column names
clean.dar.table <- function(table.name,html.tab.table) {
  html.tab.table.numeric <- dplyr::mutate_all(html.tab.table, function(x) as.numeric(as.character(x)))
  html.tab.table.numeric$DAC <- html.tab.table$DAC
  html.tab.table.numeric <- as.data.frame(html.tab.table.numeric)
  if (table.name == 'table1') {
    table1.top.col.names <- c('DAR','Project','PI')
    table1.sub.col.names <- c('Total.Submitted','Approved','Disapproved','Revision.Requested','In.Process')
    table1.expanded.col.names <- as.vector(t(outer(table1.top.col.names,table1.sub.col.names,paste,sep="_")))
    colnames(html.tab.table.numeric) <- c('DAC',table1.expanded.col.names)
  }
  else if (table.name == 'table2') {
    table2.top.col.names <- c('PI.Request.to.SO.Approval',
                              'SO.Approval.to.DAC.Claim',
                              'DAC.Claim.to.DAC.Approval',
                              'DAC.Claim.to.DAC.Reject',
                              'DAC.Claim.to.DAC.Revision',
                              'Total.Time.for.DAC.Processing')
    table2.sub.col.names <- c('Min','Max','Average')
    table2.expanded.col.names <- as.vector(t(outer(table2.top.col.names,table2.sub.col.names,paste,sep="_")))
    colnames(html.tab.table.numeric) <- c('DAC',table2.expanded.col.names)
  }
  return(html.tab.table.numeric)
}

# Returns the specified table with data specified within the time range.
# Nested header names are prepended with >> as separator
get.dar.table <- function(table.name,start.date,end.date){
  htmltab.table <- fetch.dar.table(table.name,start.date,end.date)
  cleaned.table <- clean.dar.table(table.name,htmltab.table)
  return(cleaned.table)
}

# Given time range and table name, return a list of dataframe with all DAC activities within that timeframe, daily
get.table.from.time <- function(table.name,start.date,end.date,wait.for=5) {
  start.date.as.date <- as.Date(start.date, "%m/%d/%Y")
  end.date.as.date <- as.Date(end.date, "%m/%d/%Y")
  date.seq <- seq(start.date.as.date,end.date.as.date, by = "day")
  df.list = list()
  for (i in 1:length(date.seq)) {
    cur.table <- get.dar.table(table.name,format(date.seq[[i]], "%m/%d/%Y"),format(date.seq[[i]]+1, "%m/%d/%Y"))
    cur.table$Date <- format(date.seq[[i]], "%m/%d/%Y")
    df.list[[i]] <- cur.table
    Sys.sleep(wait.for)
  }
  return(df.list)
}

# Helper function, reads a list of dataframes from given directory
# If first.col.as.row.names is true, the first column of the dataframe is converted to rownames
read.df.list.from <- function(directory,first.col.as.row.names=FALSE) {
  temp = list.files(path=directory,pattern="*.csv",full.names = TRUE)
  df.list = lapply(temp, utils::read.csv)
  if (first.col.as.row.names) {
    df.list <- lapply(df.list, function(df) {
      df.alt <- df[,-1]
      rownames(df.alt) <- df[,1]
      return(df.alt)
    })
  }
  return(df.list)
}

# Helper function, given a directory of csv with same columns, combine them into one csv and write it to the given directory
combine.csv.and.write.to <- function(read.directory.name,write.file.name){
  df.list <- read.df.list.from(read.directory.name)
  big.df <- data.table::rbindlist(df.list)
  utils::write.csv(big.df,write.file.name,row.names = FALSE)
}


# Reference: https://stackoverflow.com/questions/57279093/rvest-read-table-with-cells-that-span-multiple-rows
# Parse a table with cells that span more than one row given url and xpath
get.multi.row.span.table <- function(table.url,xpath){
  # get the lines of the table
  lines <- table.url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath=xpath)

  #define the empty table
  ncol <-  lines %>%
    .[[1]] %>%
    rvest::html_children()%>%
    length()
  nrow <- length(lines)
  table <- as.data.frame(matrix(nrow = nrow,ncol = ncol))

  # fill the table
  for(i in 1:nrow){
    # get content of the line
    linecontent <- lines[[i]]%>%
      rvest::html_children()%>%
      rvest::html_text()%>%
      gsub("\n","",.)

    # attribute the content to free columns
    colselect <- is.na(table[i,])
    table[i,colselect] <- linecontent

    # get the line repetition of each columns
    repetition <- lines[[i]]%>%
      rvest::html_children()%>%
      rvest::html_attr("rowspan")%>%
      ifelse(is.na(.),1,.) %>% # if no rowspan, then it is a normal row, not a multiple one
      as.numeric

    # repeat the cells of the multiple rows down
    for(j in 1:length(repetition)){
      span <- repetition[j]
      if(span > 1){
        table[(i+1):(i+span-1),colselect][,j] <- rep(linecontent[j],span-1)
      }
    }
  }
  return(table)
}

