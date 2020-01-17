#' Computes diet indices: trophic level and omnivory index
#'
#' @param Adjacency The adjacency matrix of the food web. It must be of class matrix.
#' @param return_TL whether to return only the omnivory index or both metrics.
#'        Trophic Level is computed in any case, so it is not quicker to set `return_TL` to false.
#' @return A dataframe with two variables, `TL` is the Trophic Level (starting at 0) and `OI` is the omnivory index.
#'        (if `return_TL` is set to `FALSE` than it's only only column with the Omnivory Index).
#'
#'  @export
#'
diet_indices <- function(Adjacency = NULL, return_TL = TRUE) {

  # check that Adjacency matrix is indeed an adjacency matrix
  if(!is.matrix(Adjacency) ) stop("Adjacency must be a matrix.")
  if (ncol(Adjacency) != nrow(Adjacency)) stop("Adjacency matrix must be square.")

  # compute transpose of Adjacency matrix
  Tij <- t(Adjacency)

  # compute diet flows
  p <- diet_get(Tij)

  # compute trophic and omnivory index levels
  ncomp <- ncol(Tij)
  A <- -p
  diag(A) <- 1
  B <- rep(1, ncomp)
  TL <- MASS::ginv(A) %*% B
  OI <- vector(length = ncomp)
  for (i in 1:ncomp) OI[i] <- sum((TL - (TL[i] - 1))^2 * p[i, ])
  TL <- TL - 1

  # build and return the required objects
  # a TL OI matrix
  if(return_TL) {
    tloi_mat <- matrix(c(OI,TL), ncol = 2)
    rownames(tloi_mat) <- rownames(Adjacency)
    colnames(tloi_mat) <- c("OI","TL")
    return(tloi_mat)
  }
  # or just a OI matrix
  OI_mat <- matrix(OI, ncol = 1)
  rownames(OI_mat) <- rownames(Adjacency)
  colnames(OI_mat) <- "OI"
  return(OI_mat)
}

diet_get <- function (Tij) {
  IntFlowTo <- rowSums(Tij)
  p <- Tij
  for (i in 1:ncol(Tij)){
    p[i, ] <- Tij[i, ]/IntFlowTo[i]
  }
  p[is.na(p)] <- 0
  return(p)
}

