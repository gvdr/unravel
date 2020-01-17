#' Plots a foodweb, using and informative layer, node size, and node colour.
#'
#' It requires a `foodweb` and a foodweb layer, `layer_foodweb`, providing the vertical and
#' horizontal positions of the nodes in the plot. A layer like that can be computed using `tl_oi_layer()`.
#'
#' Moreover, the size and colour of the nodes must be present in the foodweb object.
#'
#' @param foodweb A foodweb in `tbl_graph` or `igraph` format, with *m* species. The node table must have `size` and `colour` variable.
#' @param layer_foodweb A *m x 2* matrix. The first column defines the horizontal position and the second column the vertical position of the nodes.
#' @return A `ggraph` plot.
#'
#' @import ggraph
#' @import ggplot2
#'
#' @export
#'
ggfoodweb <- function(foodweb,
                      layer_foodweb,
                      dodge=0.08,
                      heat=10,
                      min_node_size = 4,
                      max_node_size = 8,
                      max_edge = 1,
                      min_edge = max_edge) {


  ggfw <- ggraph::ggraph(foodweb,
                         layout = layer_foodweb) +
    ggraph::geom_edge_link2(
      edge_colour = "grey",
      aes(alpha = ..index..,
          width = ..index..
      )
    ) +
    ggraph::geom_node_point(aes(colour = colour,
                        size = size)) +
    scale_size(range = c(min_node_size, max_node_size)) +
    scale_edge_width(range = c(min_edge, max_edge), guide = "none") +
    scale_edge_alpha(guide = "none") +
    ggraph::theme_graph() +
    theme(axis.title = element_text(),
          axis.line = element_line(arrow = arrow(type = "closed"))
    )

  return(ggfw)

}
