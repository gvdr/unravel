#' Adds node size to food web
#'
#' @details Food web must be in `tbl_graph` format.
#'          It overrides any other `size` variable if already present for the web.
#'
#' @param foodweb The food web. It must be in `tbl_graph` format.
#' @param size_fun The **node** function to compute node size (e.g., a centrality metric)
#' @return a `tbl_graph` with one more variable for the nodes, named `size`.
#'
#' @export
#'
fw_node_size <- function(foodweb,
                         size_fun = tidygraph::centrality_degree,
                         ...){
  size_fun <- match.fun(size_fun)
  foodweb <- tidygraph::activate(foodweb,"nodes")
  foodweb <- dplyr::mutate(foodweb,size =  size_fun(...))

  return(foodweb)
}

#' Adds node colour to food web
#'
#' @details Food web must be in `tbl_graph` format.
#'          It overrides any other `colour_index` variable if already present for the web.
#'
#' @param foodweb The food web. It must be in `tbl_graph` format.
#' @param colour_fun The **node** function to compute node size (e.g., a centrality metric)
#' @return a `tbl_graph` with one more variable for the nodes, named `colour`.
#'
#' @export
#'
fw_node_colour <- function(foodweb,
                           colour_fun = tidygraph::centrality_betweenness,
                           ...){
  colour_fun <- match.fun(colour_fun)
  foodweb <- tidygraph::activate(foodweb,"nodes")
  foodweb <- dplyr::mutate(foodweb, colour =  colour_fun(...))

  return(foodweb)
}

#' Adds node colour to food web
#'
#' @details Food web must be in `tbl_graph` format.
#'          It overrides any other `colour_index` variable if already present for the web.
#'
#' @param foodweb The food web. It must be in `tbl_graph` format.
#' @param colour_fun The **node** function to compute node size (e.g., a centrality metric)
#' @return a `tbl_graph` with one more variable for the nodes, named `colour_index`.
#'
#' @export
#'
fw_node_colour <- fw_node_colour
