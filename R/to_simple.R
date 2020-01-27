#' A morpher to simplify tidygraphs
#'
#' It is a direct rip-off from `tidygraph::to_simple()` and should be used within a `morph` situation.
#'
#' @param graph the graph to be simplifies
#' @param remove_multiples whether to remove multiple edges
#' @param remove_loops whether to remove loops
#'
#' @importFrom igraph simplify edge_attr edge_attr<-
#'
#' @export
#'
to_simple <- function(graph, remove_multiples = TRUE, remove_loops = FALSE) {
  edges <- tidygraph::as_tibble(graph, active = "edges")
  graph <- set_edge_attributes(graph, edges[, ".tidygraph_edge_index",
                                            drop = FALSE])
  edges$.tidygraph_edge_index <- NULL
  simple <- tidygraph::as_tbl_graph(simplify(graph,
                                  remove.multiple = remove_multiples,
                                  remove.loops = remove_loops,
                                  edge.attr.comb = list))
  new_edges <- tidygraph::as_tibble(simple, active = "edges")
  new_edges$.orig_data <- lapply(new_edges$.tidygraph_edge_index,
                                 function(i) edges[i, , drop = FALSE])
  simple <- set_edge_attributes(simple, new_edges)
  list(simple = simple)
}

set_edge_attributes <- function(x, value) {
  value <- value[, !names(value) %in% c("from", "to")]
  edge_attr(x) <- as.list(value)
  x
}
