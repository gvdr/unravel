#' Computes a ggraph layer using the diet indices of the foodweb
#'
#' @details The layer is build by using the Trophic Level as vertical axis and the Omnivory Index as horizontal axis.
#' @param foodweb A foodweb in `tbl_graph` or `igraph` format.
#' @param dodge The random uniform noise used to scatter the nodes along the horizontal axis (so to avoid overlap).
#' @return A matrix with two columns (trophic and omnivory) and as many rows as nodes in the foodweb
#'
#' @importFrom stats runif
#'
#' @export
#'
tl_oi_layer <- function(foodweb, dodge=0.08){

  # get number of vertices
  n_vert <- igraph::gorder(foodweb)
  # get adjacency matrix (not in sparse format)
  Adj <- igraph::get.adjacency(foodweb, sparse = F)

  # compute diet indices (trophic level and omnivory index)
  DietIndices <- diet_indices(Adj)
  if(dodge == 0) return(DietIndices)

  # if we require some dodging to avoid node overlap
  # we add it to the omnivory index
  DietIndices[,"OI"] <- DietIndices[,"OI"] + runif(n_vert, max = dodge)
  return(DietIndices)
}
