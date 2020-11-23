#' Construct study-to-study adjacency matrix
#'
#' Given a list of study accession phs ids of length n, return a n-by-n matrix
#' where the entry on i-th row j-th column is the number of times the i-th
#' study and the j-th study been requested by the same PIs
#'
#' @param study.list a list of study accession phs ids
#' @param self.loop If FALSE, comparison of study to itself is set to 0
#'
#' @return matrix
#' @export
#'
#' @examples construct.study.to.study.adj.mat(list("phs000501.v1.p1","phs000688.v1.p1","phs001516.v1.p1"))
#'
construct.study.to.study.adj.mat <- function(study.list,self.loop=FALSE) {
  list.length <- length(study.list)
  mat <- matrix(0L,nrow = length(study.list),ncol = list.length)
  df <- get.nih.dac.action.table()
  print("Precomputing Requested PI lookup...")
  requested.pi <- lapply(study.list,function(x){df[df['Study accesion'] == x,][,'PI']})
  for (i in 1:list.length) {
    for (j in i:list.length) {
        same.pi.num <- length(intersect(requested.pi[[i]],requested.pi[[j]]))
        mat[i,j] <- same.pi.num
        mat[j,i] <- same.pi.num
      }
    if (i %% 100 == 1) {
      print(sprintf("Study to Study Matrix Row %s completed. Total rows: %s", i,list.length))
    }
  }
  if (!self.loop) {
    diag(mat) <- 0
  }
  colnames(mat) <- study.list
  rownames(mat) <- study.list
  print("Study to Study Matrix Table Created")
  return(mat)
}

#' Save Study to Study Adjacency Matrix
#'
#' Saves the study-to-study adjacency matrix. See documentation
#' on construct.study.to.study.adj.mat for detail.
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{
#' save.study.to.study.adj.mat
#' }
save.study.to.study.adj.mat <- function(){
  all.studies.ids <- unique(get.nih.dac.action.table()[,'Study accesion'])
  study.to.study.adj.df <- construct.study.to.study.adj.mat(all.studies.ids)
  print("Saving Study to Study DF")
  save(study.to.study.adj.df, file = "StudyToStudyDF.rda",compress = 'xz')
}
