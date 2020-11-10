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
  return(paste(shQuote(unique(nih_dac_action_table$DAC)), collapse=", "))
}

# Get the latest DAR approval date stored in nih_dac_action_table
get.latest.approved.dar.date <- function() {
  converted.date <- as.POSIXct(nih_dac_action_table[,'Approved by DAC'], format="%m/%d/%Y %H:%M", tz="EST")
  # Use the latest date in the approved by dac column as the last updated date
  cur.table.latest <- as.Date(max(converted.date ,na.rm = TRUE))
  return(cur.table.latest)
}
