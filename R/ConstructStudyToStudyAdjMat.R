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
#' save.study.to.study.adj.mat()
#' }
save.study.to.study.adj.mat <- function(){
  all.studies.ids <- unique(get.nih.dac.action.table()[,'Study accesion'])
  study.to.study.adj.df <- construct.study.to.study.adj.mat(all.studies.ids)
  print("Saving Study to Study DF")
  save(study.to.study.adj.df, file = "StudyToStudyDF.rda",compress = 'xz')
}


#' Get Study to Study Adjacency Matrix
#'
#' Returns the locally stored study-to-study adjacency matrix. See documentation
#' on construct.study.to.study.adj.mat for detail.
#'
#' @return matrix
#' @export
#'
#' @examples \dontrun{
#' study.to.study.adj.df <- get.study.to.study.adj.mat()
#' }
get.study.to.study.adj.mat <- function() {
  load(system.file("StudyToStudyDF.rda", package = "DACReportingTool"))
  return(study.to.study.adj.df)
}


#' PCA Plot of network adjacency matrix
#'
#' This function is currently EXPERIMENTAL and the method is not vigorously verified.
#' Any conclusion drawn from the plotted figure should be treated with caution.
#'
#' Given the adjacency matrix of study to study matrix, where edge weight indicates
#' the number of PIs who requested the two studies together, plot a PCA-reduced version
#' of the adjancency matrix in 2 dimension, colored by DAC.
#'
#' The following operations are preformed to produce the plot: The adjacency matrix
#' is first min-max normalized column-wise, NAs are filled by 0, then the adjancency
#' matrix is reduced to 2 dimension.
#'
#' @param adj.mat The adjacency matrix to be used to plot, By default the study to study adjacency matrix
#'
#' @return ggplot figure
#' @export
#'
#' @examples \dontrun{
#' pca.plot.studies()
#' }
pca.plot.studies <- function(adj.mat=get.study.to.study.adj.mat()) {

  # Min max normalize column-wise
  adj.mat.norm <- as.data.frame(apply(adj.mat, 2, FUN=function(x){min_max_norm(x)}))

  # Fill NA with 0
  adj.mat.norm[is.na(adj.mat.norm)] <- 0

  # PCA
  print("PCA-ing...")
  pca.study <- stats::prcomp(adj.mat.norm)

  # Add DAC labels
  dac.df <- get.nih.dac.action.table()[c('Study accesion','DAC')]
  dac.df <- dac.df[!duplicated(dac.df$`Study accesion`),]

  adj.mat.names <- rownames(adj.mat)

  new.df <- data.frame("phs_id"=adj.mat.names)
  new.df.merged <- merge(new.df,dac.df,all.x=TRUE,by.x='phs_id',by.y='Study accesion')

  adj.mat.norm$DAC <- new.df.merged$DAC
  ggplot2::autoplot(pca.study, data=adj.mat.norm, colour='DAC')
}
