#' @importFrom magrittr "%>%"

# Helper function, formats the given parameters as a request url
format.table.request.url <- function(table.name,start.date,end.date) {
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?stDate=%s&endDate=%s&retTable=%s"
  request.url <- sprintf(base.url, utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE), table.name)
  return(request.url)
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

# Retrieve the review timeline table
get.review.timeline.table <-function(dac.name,start.date,end.date) {
  table.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=%s&diff=tot&stat=max&stDate=%s&endDate=%s"
  request.url <- sprintf(table.url,utils::URLencode(dac.name,reserved = TRUE), utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE))
  big.df <- request.url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath='/html/body/table') %>%
    rvest::html_table(fill = TRUE)
  big.df <- big.df[[1]][-1,]
  #big.df <- get.multi.row.span.table(request.url,"/html/body/table")
  return(big.df)
}

# Retrieve Table A1: List of All NIH DAC Studies table
get.list.of.all.nih.dac.studies.table <- function(start.date,end.date) {
  table.url <- format.table.request.url('tablea1',start.date,end.date)
  big.df <- get.multi.row.span.table(table.url,'//tr')
  names(big.df) <- big.df[1,]
  big.df <- big.df[-1,]
  return(big.df)
}

# Removes any non-ascii characters from the given dataframe
remove.non.ascii.from.df <-function(df) {
  return(data.frame(lapply(df, iconv, from="latin1",to="ASCII",sub="")))
}

# Fetch and parse the table
get.all.dac.action.table <- function(start.date,end.date) {
  table.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=%s&endDate=%s"
  request.url <- sprintf(table.url,utils::URLencode(start.date,reserved = TRUE), utils::URLencode(end.date,reserved = TRUE))
  print(sprintf("Sending request for DAC action table from %s to %s...",start.date,end.date))
  big.df <- get.multi.row.span.table(request.url,"//tr")
  # Use first row as column headers
  names(big.df) <- big.df[1,]
  big.df <- big.df[-1,]
  return(big.df)
}

# Update table to the date given (system date to default)
update.dac.action.table <- function(update.to=format(Sys.Date(),"%m/%d/%Y"),overwrite=TRUE,return.table=FALSE) {
  update.to <- as.Date(update.to,"%m/%d/%Y")
  # Loads the latest nih dac action table into the session
  load('./data/nih_dac_action_table.rda')
  converted.date <- as.POSIXct(nih_dac_action_table[,'Approved by DAC'], format="%m/%d/%Y %H:%M", tz="EST")
  # Use the latest date in the approved by dac column as the last updated date
  cur.table.latest <- as.Date(max(converted.date ,na.rm = TRUE))
  if (cur.table.latest >= update.to) {
    stop('The table is already updated to the specified date!')
  }
  # Get table starting from latest date, append to current table
  new.table <- get.all.dac.action.table(format(cur.table.latest,"%m/%d/%Y"),format(update.to,"%m/%d/%Y"))
  combined.table <- dplyr::bind_rows(nih_dac_action_table,new.table)
  # Drop rows with same DAR (only keep the latest one)
  combined.table <- combined.table[!duplicated(combined.table$DAR, fromLast=T),]
  nih_dac_action_table <- combined.table
  if (overwrite) {
    save(nih_dac_action_table,file = "./data/nih_dac_action_table.rda")
  }
  if (return.table) {
    return(nih_dac_action_table)
  }
}
