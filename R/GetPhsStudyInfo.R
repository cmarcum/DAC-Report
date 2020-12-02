# Given phs.id, returns dataframe containingi information about request access and PI
request.phs.study.info <- function(phs.id) {
  study.key <- request.study.key(phs.id)
  if(!is.na(study.key)) {
    restricted.access.table <- request.phs.restricted.access.table(phs.id,study.key)
    principle.investigators.str <- paste(request.phs.study.attribution(phs.id)$PI, collapse = ";")
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
request.study.key <- function(phs.id) {
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

# Retrieves the number of publications associated to the given phs id
request.phs.publications <- function(phs.id) {
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetReference.cgi?study_id=%s&study_key=%s&page_number=1"
  study.key <- request.study.key(phs.id)
  request.url <- sprintf(base.url,phs.id,study.key)
  webpage <- xml2::read_html(request.url)
  webpage.as.list <- xml2::as_list(webpage)
  if (length(webpage.as.list) > 0) {
    total.pub.div <- webpage.as.list$html$body$div
    if(length(total.pub.div) > 1) {
      total.pub.string <- total.pub.div[[1]]
      total.pub <- as.numeric(gsub("[^0-9]", "", total.pub.string))
      return(total.pub)
    }
  }
  return(NA)
}


# Returns a table with column Consent group, Is IRB required, DAC, Numebr of participants
# of the given phs id and study key
request.phs.restricted.access.table <- function(phs.id,study.key) {
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
request.phs.study.attribution <- function(phs.id) {
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
request.phs.studies.table <- function(phs.id.list,wait.for=2) {
  df.list <- list()
  for (i in 1:length(phs.id.list)) {
    print(sprintf("Currently retrieing study %s : %s",i,phs.id.list[[i]]))
    cur.table <- request.phs.study.info(phs.id.list[[i]])
    df.list[[i]] <- cur.table
    Sys.sleep(wait.for)
  }
  big.df <- data.table::rbindlist(df.list,fill=TRUE)
  return(big.df)
}


# Retrieve phs studies that are present in nih_dac_action_table but not phs_studies_table and updates the table
update.phs.studies.table <- function(overwrite=TRUE,return.table=FALSE,wait.for=2) {
  print("Updating phs studies table...")
  load(system.file("phs_studies_table.rda", package = "DACReportingTool"))
  load(system.file("nih_dac_action_table.rda", package = "DACReportingTool"))

  all.existing.ids <- unique(phs_studies_table$phs_id)
  all.ids.in.action.table <- unique(nih_dac_action_table$`Study accesion`)

  ids.to.retrieve <- setdiff(all.ids.in.action.table,all.existing.ids)
  print(sprintf("Retrieving %s new phs ids. Estimated wait time: %s seconds", length(ids.to.retrieve), (wait.for+1)*length(ids.to.retrieve)))

  new.table <- request.phs.studies.table(ids.to.retrieve)

  big.df <- data.table::rbindlist(list(phs_studies_table,new.table),fill=TRUE)
  print("phs studies table update completed")
  if (overwrite) {
    phs_studies_table <- big.df
    save(phs_studies_table,file = system.file("phs_studies_table.rda", package = "DACReportingTool"), compress = "xz")
  }
  if (return.table) {
    return(big.df)
  }
}

# Retrieve the main disease focus given the phs number
request.phs.study.focus <- function(phs.id) {
  base.url <- "https://www.ncbi.nlm.nih.gov/gap/advanced_search/search/"
  post.options <- list(
    obj_type = "study",
    term = phd.id,
    page_start = 0,
    page_rows = 10,
    facets_applied = list(
      is_exclude_id_type_variable = "no"
    )
  )
  res <- httr::POST(base.url, body = post.options, encode = "json")
  res.content <- httr::content(res)
  report <- res.content$REPORT
  if (length(report) > 0) {
    study <- report[[1]]
    study.main.disease <- study$study_main_disease_txt
    return(study.main.disease)
  }
  return(NA)
}

# Retrieve all approved requester and their research use statement given phd id
request.phs.research.statements <- function(phs.id,as.text=FALSE) {
  base.url <- "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetAuthorizedRequestDownload.cgi?study_id=%s"
  request.url  <- sprintf(base.url,phs.id)
  res <- httr::GET(request.url)
  res.content <- httr::content(res)

  if (as.text) {
    return(res.content)
  }

  content.table <- read.delim2(text=res.content, sep = '\t',header=FALSE)
  emptycols <- sapply(content.table, function (k) all(is.na(k)))
  content.table <- content.table[!emptycols]

  # Use first row as column headers
  names(content.table) <- content.table[1,]
  content.table <- content.table[-1,]

  return(content.table)
}

# Reference: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# Given Phs id return a matrix with words and their frequency
get.phs.study.term.frequency.table <- function(phs.id) {
  raw.text <- request.phs.research.statements(phs.id)$`Technical Research Use Statement`
  docs <- tm::Corpus(tm::VectorSource(raw.text))
  # return(docs)
  to.space <- tm::content_transformer(function (x, pattern) gsub(pattern, " ", x))
  docs <- tm::tm_map(docs, to.space, "/")
  docs <- tm::tm_map(docs, to.space, "@")
  docs <- tm::tm_map(docs, to.space, "\\|")
  # Convert the text to lower case
  docs <- tm::tm_map(docs, tm::content_transformer(tolower))
  # Remove numbers
  docs <- tm::tm_map(docs, tm::removeNumbers)
  # Remove english common stopwords
  docs <- tm::tm_map(docs, tm::removeWords, tm::stopwords("english"))

  custom.stop.words <- c("data","will","phs","study","use","can","dataset","also","use","used","using","datasets")
  docs <- tm::tm_map(docs, tm::removeWords, custom.stop.words)

  # Remove punctuations
  docs <- tm::tm_map(docs, tm::removePunctuation)
  # Eliminate extra white spaces
  docs <- tm::tm_map(docs, tm::stripWhitespace)

  dtm <- tm::TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)

  # return(wordcloud::wordcloud(words = d$word, freq = d$freq, min.freq = 1,
  #           max.words=200, random.order=FALSE, rot.per=0.1, colors=RColorBrewer::brewer.pal(8, "Dark2")))

  return(d)
}

