#' Get Phs study term frequency table
#'
#' Look up all research statements associated to the given phs id and return a
#' table with words appeared in the research statements along with their frequency
#'
#' Reference: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
#'
#' @param phs.id the phs id of the study
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#' term.frequency.table <- get.phs.study.term.frequency.table('phs000688.v1.p1')
#' }
#'
get.phs.study.term.frequency.table <- function(phs.id) {
  table.requested <- request.phs.research.statements(phs.id)
  if (!is.na(table.requested)) {
    raw.text <- request.phs.research.statements(phs.id)$`Technical Research Use Statement`
    docs <- tm::Corpus(tm::VectorSource(raw.text))
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

    return(d)
  }
  return(NA)
}
