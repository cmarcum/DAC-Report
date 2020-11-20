#' @importFrom magrittr "%>%"

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

# Produce subset of df that falls within the given time range
get.df.within.range <- function(df,start.date,end.date,date.col="Approved by DAC") {
  return(subset(df, as.Date(df[,date.col], format="%m/%d/%Y") >= start.date & as.Date(df[,date.col], format="%m/%d/%Y") <= end.date))
}

# Converts the given column to POSIX time
to.time <- function(col) {
  return(as.POSIXct(col, format="%m/%d/%Y %H:%M", tz="EST"))
}

# Returns a list of currently supported dac names
get.supported.dacs <- function() {
  df <- get.nih.dac.action.table()
  return(paste(shQuote(unique(df$DAC)), collapse=", "))
}

#' Get Latest Approved Table
#'
#' Get the latest DAR approval date stored in nih_dac_action_table
#'
#' @return Date object representing the latest approved DAR date
#'
#' @examples \dontrun{
#' get.latest.approved.dar.date()
#' > "2020-11-13"
#' }
#'
#' @export
#'
get.latest.approved.dar.date <- function() {
  df <- get.nih.dac.action.table()
  converted.date <- as.POSIXct(df[,'Approved by DAC'], format="%m/%d/%Y %H:%M", tz="EST")
  # Use the latest date in the approved by dac column as the last updated date
  cur.table.latest <- as.Date(max(converted.date ,na.rm = TRUE))
  return(cur.table.latest)
}
