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
  study.json <- request.phs.advanced.search.result.json(phs.id)
  if (!is.na(study.json)) {
    return(study.json$study_main_disease_txt)
  }
}


# Retrieves the json of the first study result of dbgap advanced search
request.phs.advanced.search.result.json <- function(phs.id) {
  base.url <- "https://www.ncbi.nlm.nih.gov/gap/advanced_search/search/"
  post.options <- list(
    obj_type = "study",
    term = phs.id,
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
    return(study)
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
  # Check if table is malformed
  if (is.character(content.table) ||
      is.null(content.table) ||
      is.na(content.table) ||
      ncol(content.table) != 7) {
    return(NA)
  }

  return(content.table)
}

# Returns google scholar result count
request.google.scholar.result.count <- function(term,start.year='',end.year='') {
  base.url <- 'https://scholar.google.com/scholar?q=%s&as_ylo=%s&as_yhi=%s'
  request.url <- sprintf(base.url,term,start.year,end.year)
  res <- httr::GET(request.url)
  res.content <- httr::content(res)
  res.ele <- rvest::html_node(res.content, css="#gs_ab_md > .gs_ab_mdw")
  res.ele.txt <- rvest::html_text(res.ele)
  if (is.na(res.ele.txt) || res.ele.txt == "") {
    return(0)
  } else {
    res.count <- as.numeric(regmatches(res.ele.txt,regexpr('\\d+',res.ele.txt)))
    return(res.count)
  }
}



# Given list of phs id, create dataframe with their corresponding number
# of search results on google scholar
# Empirically, setting the wait time >= 2 prevents captcha from showing up
request.google.scholar.citation.table <- function(phs.id.list,wait.for=2){
  citation.list <- list()
  for (i in 1:length(phs.id.list)) {
    phs.id.no.version <- extract.phs(phs.id.list[[i]])
    print(sprintf("Retrieving Google Scholar Citation Data for %s",phs.id.no.version))
    citation.list[[i]] <- request.google.scholar.result.count(phs.id.no.version)
    Sys.sleep(wait.for)
  }
  d <- data.frame(I(phs.id.list), I(citation.list))
  d$citation.list <- as.numeric(d$citation.list)
  names(d) <- c('phs_id','gs_citation_count')
  d$`last_update` <- Sys.Date()
  return(d)
}

# Returns the google scholar citation table
get.google.scholar.citation.table <- function() {
  load(system.file("gs_citation_table.rda", package = "DACReportingTool"))
  if(!exists("gs_citation_table")) {
    stop("gs_citation_table variable not found. Was the locally file renamed and overwritten?
         To obtain a fresh copy use refresh.local.data()")
  }
  return(gs_citation_table)
}
