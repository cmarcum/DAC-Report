# Produce subset of df that falls within the given time range
get.df.within.range <- function(df,start.date,end.date,date.col="Approved by DAC") {
  return(subset(df, as.Date(df[,date.col], format="%m/%d/%Y") >= start.date & as.Date(df[,date.col], format="%m/%d/%Y") <= end.date))
}

# Counts number of entries by DAC in given df
count.DAC.df <- function(df) {
  return(df %>% as_tibble() %>% count(DAC))
}

# aggregate by DAC
aggregate.by.dac <- function(df,dac.name,start.date,end.date) {
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
  print(sum(dar.number.vector))
  print(mean(dar.number.vector))
  print(sd(dar.number.vector))
  print(median(dar.number.vector))
  return(dar.number.vector)
}

dar.review.timeline.summary <- function(df,start.date,end.date) {
  approved.df <- get.df.within.range(df,start.date,end.date,date.col="Approved by DAC")
  approved.df["time_from_PI_submission_to_DAC_approval"] <- difftime(to.time(approved.df[,"Approved by DAC"]), to.time(approved.df[,"Submitted by PI"]),units = "days")
  approved.df["time_from_PI_submission_to_DAC_approval"] <- difftime(to.time(approved.df[,"Approved by DAC"]), to.time(approved.df[,"Submitted by PI"]),units = "days")
  approved.df.by.dac <- aggregate(approved.df["time_from_PI_submission_to_DAC_approval"], by=list(DAC=approved.df$DAC), FUN=mean)

  dar_total <- count.DAC.df(approved.df)[,"n"]
  approved.df.by.dac["dar_total"] <- dar_total



  # daily average / std
  # weekly average /
  return(approved.df.by.dac)
}

to.time <- function(col) {
  return(as.POSIXct(col, format="%m/%d/%Y %H:%M", tz="EST"))
}

# Reproduce Table 1 from DAC table

# aggregate.by.time <- function(df,time.step="month") {
#
# }
