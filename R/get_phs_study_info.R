# Given phs.id, returns dataframe containingi information about request access and PI
get.phs.study.info <- function(phs.id) {
  study.key <- get.study.key(phs.id)
  restricted.access.table <- get.phs.restricted.access.table(phs.id,study.key)
  principle.investigators.str <- paste(get.phs.study.attribution(phs.id)$PI, collapse = ";")
  restricted.access.table$PI <- principle.investigators.str
  return(restricted.access.table)
}

# Gets the internal study key for the given phs id
get.study.key <- function(phs.id) {
  print("Finding study key...")
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/collection.cgi?study_id=%s"
  request.url <- sprintf(base.url,phs.id)
  webpage <- xml2::read_html(request.url)
  body.element <- rvest::html_nodes(webpage, css="body")
  onload.text.splitted <- strsplit(rvest::html_attr(body.element[[1]], 'onload'),";")[[1]]
  restricted.access.text <- onload.text.splitted[2]
  restricted.access.args.text <- gsub("[\\(\\)]", "", regmatches(restricted.access.text, gregexpr("\\(.*?\\)", restricted.access.text))[[1]])
  study.key <-  strsplit(restricted.access.args.text, ",")[[1]][2]
  study.key.clean <- trimws(study.key, which = "both")
  return(study.key.clean)
}

# Returns a table with column Consent group, Is IRB required, DAC, Numebr of participants
# of the given phs id and study key
get.phs.restricted.access.table <- function(phs.id,study.key) {
  print("Finding restricted access table")
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetRestrictedAccess.cgi?study_id=%s&study_key=%s"
  request.url <- sprintf(base.url,phs.id,study.key)
  webpage <- xml2::read_html(request.url)
  table.html <- rvest::html_nodes(webpage, css="table")[[1]]
  table.as.df <- rvest::html_table(table.html)
  table.as.df$phs_id <- phs.id
  table.as.df$study_key <- study.key
  return(table.as.df)
}

# Given phs id, returns a dataframe with the names of the PI responsible for the study
get.phs.study.attribution <- function(phs.id) {
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetStudyAttribution.cgi?study_id=%s"
  request.url  <- sprintf(base.url,phs.id)
  webpage <- xml2::read_html(request.url)
  principal.investigators.list <- rvest::html_nodes(webpage, css="ul > li:first-child > ul > li")
  pi.list.cleaned <- sapply(principal.investigators.list, function(x) {rvest::html_text(x,trim = TRUE)})
  pi.df <- data.frame(phs_id=phs.id,PI=pi.list.cleaned)
  return(pi.df)
}
