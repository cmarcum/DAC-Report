# Given phs.id, returns dataframe containingi information about request access and PI
get.phs.study.info <- function(phs.id) {
  study.key <- get.study.key(phs.id)
  if(!is.na(study.key)) {
    restricted.access.table <- get.phs.restricted.access.table(phs.id,study.key)
    principle.investigators.str <- paste(get.phs.study.attribution(phs.id)$PI, collapse = ";")
    if (principle.investigators.str == "NA") {
      restricted.access.table$PI <- NA
    } else {
      restricted.access.table$PI <- principle.investigators.str
    }
    return(restricted.access.table)
  }
  placeholder.table <- data.frame(phs_id=phs.id,study_key=NA,PI=NA)
  return(placeholder.table)
}

# Gets the internal study key for the given phs id
get.study.key <- function(phs.id) {
  #print("Finding study key...")
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/collection.cgi?study_id=%s"
  # TODO error handling
  request.url <- sprintf(base.url,phs.id)
  webpage <- xml2::read_html(request.url)
  body.element <- rvest::html_nodes(webpage, css="body")
  onload.attributes <- rvest::html_attr(body.element[[1]], 'onload')
  if(!is.na(onload.attributes)) {
    onload.text.splitted <- strsplit(onload.attributes,";")[[1]]
    if (length(onload.text.splitted) >= 2) {
      restricted.access.text <- onload.text.splitted[2]
      matched.parenthesis <- regmatches(restricted.access.text, gregexpr("\\(.*?\\)", restricted.access.text))
      restricted.access.args.text <- gsub("[\\(\\)]", "", matched.parenthesis[[1]])
      study.key <-  strsplit(restricted.access.args.text, ",")[[1]][2]
      study.key.clean <- trimws(study.key, which = "both")
      return(study.key.clean)
    }
  }
  return(NA)
}

# Returns a table with column Consent group, Is IRB required, DAC, Numebr of participants
# of the given phs id and study key
get.phs.restricted.access.table <- function(phs.id,study.key) {
  #print("Finding restricted access table")
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetRestrictedAccess.cgi?study_id=%s&study_key=%s"
  request.url <- sprintf(base.url,phs.id,study.key)
  webpage <- xml2::read_html(request.url)
  table.html <- rvest::html_nodes(webpage, css="table")
  if (length(table.html) > 0) {
    table.html.element <- table.html[[1]]
    table.as.df <- rvest::html_table(table.html.element)
    table.as.df$phs_id <- phs.id
    table.as.df$study_key <- study.key
  } else {
    table.as.df <- data.frame(phs_id=phs.id, study_key=study.key)
  }
  return(table.as.df)
}

# Given phs id, returns a dataframe with the names of the PI responsible for the study
get.phs.study.attribution <- function(phs.id) {
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetStudyAttribution.cgi?study_id=%s"
  request.url  <- sprintf(base.url,phs.id)
  webpage <- xml2::read_html(request.url)
  principal.investigators.list <- rvest::html_nodes(webpage, css="ul > li:first-child > ul > li")
  if (length(principal.investigators.list) > 0) {
    pi.list.cleaned <- sapply(principal.investigators.list, function(x) {rvest::html_text(x,trim = TRUE)})
    pi.df <- data.frame(phs_id=phs.id,PI=pi.list.cleaned)
  } else {
    pi.df <- data.frame(phs.id=phs.id,PI=NA)
  }
  return(pi.df)
}

# Given a list of phs id, send request to get their information and combine into one dataframe
get.phs.studies.table <- function(phs.id.list,wait.for=2) {
  df.list <- list()
  for (i in 1:length(phs.id.list)) {
    print(sprintf("Currently retrieing study %s : %s",i,phs.id.list[[i]]))
    cur.table <- get.phs.study.info(phs.id.list[[i]])
    df.list[[i]] <- cur.table
    Sys.sleep(wait.for)
  }
  big.df <- data.table::rbindlist(df.list,fill=TRUE)
  return(big.df)
}


# Retrieve phs studies that are present in nih_dac_action_table but not phs_studies_table and updates the table
update.phs.studies.table <- function(overwrite=TRUE,return.table=FALSE,wait.for=2) {
  print("Updating phs studies table...")
  load(system.file("data", "phs_studies_table.rda", package = "DACReportingTool"))
  load(system.file("data", "nih_dac_action_table.rda", package = "DACReportingTool"))

  all.existing.ids <- unique(phs_studies_table$phs_id)
  all.ids.in.action.table <- unique(nih_dac_action_table$`Study accesion`)

  ids.to.retrieve <- setdiff(all.ids.in.action.table,all.existing.ids)
  if (length(ids.to.retrieve) == 0) {
    stop("No studies to update")
  }
  print(sprintf("Retrieving %s new phs ids. Estimated wait time: %s seconds", length(ids.to.retrieve), (wait.for+1)*length(ids.to.retrieve)))

  new.table <- get.phs.studies.table(ids.to.retrieve)

  big.df <- data.table::rbindlist(list(phs_studies_table,new.table),fill=TRUE)
  print("phs studies table update completed")
  if (overwrite) {
    phs_studies_table <- big.df
    save(phs_studies_table,file = system.file("data", "phs_studies_table.rda", package = "DACReportingTool"))
  }
  if (return.table) {
    return(big.df)
  }
}