#' Draws an opinionated plot of a food web.
#'
#' It requires a `foodweb` in `tbl_graph` format.
#' If no foodweb layer is defined by the user, `layer_foodweb` is computed using `tl_oi_layer()`.
#' The vertical position of the species is defined by their trophic level;
#' the horizontal positions of the species is defined by their omnivory index.
#' The size and colour of the nodes, if not provided by the user, are respectively
#' the degree and the betwenness centrality of the nodes.
#'
#' @param foodweb A foodweb in `tbl_graph` or `igraph` format, with *m* species. The node table must have `size` and `colour` variable.
#' @return A `ggraph` plot.
#'
#' @import ggraph
#' @import ggplot2
#'
#' @export
#'
plot_foodweb <- function(foodweb,
                         min_node_size = 4, max_node_size = 8,
                         max_edge = 1,
                         min_edge = max_edge,
                         dodge = 0.08,
                         size = NULL, size_label = NULL,
                         colour = NULL, colour_label = NULL,
                         user_layer = NULL, x_label = NULL, y_label = NULL) {

  # add default size (Degree) and colour (Betwenness)
  if(is.null(size)) foodweb <- fw_node_size(foodweb)
  if(is.null(colour)) foodweb <- fw_node_colour(foodweb)

  # if not user defined layer is provided, it computs the default one
  if(is.null(user_layer)) lf <- tl_oi_layer(foodweb, dodge = dodge)

  # plot using ggfoodweb function
  plot <- ggfoodweb(foodweb,
                    layer_foodweb = lf,
                    min_node_size = min_node_size,
                    max_node_size = max_node_size,
                    max_edge = max_edge,
                    min_edge = min_edge,
                    dodge = dodge)

  # add default labels for guides, if not provided by user
  if(is.null(size)) plot <- plot +
    guides(size = guide_legend(title="Degree"))
  if(is.null(colour)) plot <- plot +
    guides(colour = guide_colourbar(title="Betwenness"))
  if(is.null(user_layer)) {
    plot <- plot +
      scale_x_continuous(name = "Omnivory Index") +
      scale_y_continuous(name = "Trophic Level")
  }

  return(plot)
}
