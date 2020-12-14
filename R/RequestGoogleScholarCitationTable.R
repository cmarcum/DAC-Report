#' Request Google Scholar Result Count
#'
#' @param term String, the search term for google scholar
#' @param start.year String, year in 'yyyy' format. The earliest year in which
#' the result should contain
#' @param end.year String, year in 'yyyy' format. The latest year in which the
#' result should contain
#'
#' @return integer
#' @export
#'
#' @examples \dontrun{
#' # The number of citations for phs000449
#' study.gs.citation.count <- request.google.scholar.result.count('phs000449')
#' }
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



#' Request Google Scholar Citation Table
#'
#' Given list of phs id, create a dataframe with their corresponding number
#' of citations (number of results returned from searching) on google scholar
#' Empirically, setting the wait time >= 2 prevents captcha from showing up
#' For table column see documentation for get.google.scholar.citation.table()
#'
#' @param phs.id.list list of phs.id
#' @param wait.for integer, seconds to wait for before sending another request
#' @param ignore.version logical, defaults to false whether only the phs number should be used instead
#' of the full id which contains version numbers
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#' df <- get.nih.dac.action.table()
#' unique.study.ids <- unique(df['Study accesion'])$`Study accesion`
#' gs_citation_table <- request.google.scholar.citation.table(unique.study.ids,2)
#' }
request.google.scholar.citation.table <- function(phs.id.list,wait.for=2,ignore.version=FALSE){
  citation.list <- list()
  for (i in 1:length(phs.id.list)) {
    phs.id <- phs.id.list[[i]]
    if (ignore.version) {
      phs.id <- extract.phs(phs.id)
    }
    print(sprintf("Retrieving Google Scholar Citation Data for %s [%s/%s]",phs.id,i,length(phs.id.list)))
    citation.list[[i]] <- request.google.scholar.result.count(phs.id)
    Sys.sleep(wait.for)
  }
  d <- data.frame(I(phs.id.list), I(citation.list))
  d$citation.list <- as.numeric(d$citation.list)
  names(d) <- c('phs_id','gs_citation_count')
  d$`last_update` <- Sys.Date()
  return(d)
}


