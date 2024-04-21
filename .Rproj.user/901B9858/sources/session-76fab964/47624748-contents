#' Plotting an option tree
#'
#' Description
#' @param x A matrix of option prices as returned by Derivative_Security.
#' @return Plots the option tree in the viewer plane..
#' @import DiagrammeR
#' @examples
#' PlotTree(Derivative_Security(S, K, N, r, d, u, type));
#' @export
PlotTree <- function(x) {
  # Graph object
  grph <- DiagrammeR::create_graph()

  # Adding nodes
  for (i in 1:nrow(x)) {
    for (j in 1:(2^(i-1))) {
      grph <- grph %>% DiagrammeR::add_node(label = paste0(i, ", ", j))
    }
  }

  # Adding edges
  for (i in 1:(nrow(x) - 1)) {
    children  <- paste0(i + 1, ", ", 1:(2^i))
    for (j in 1:(2^(i-1))) {
      grph <- grph %>% DiagrammeR::add_edge(from = paste0(i, ", ", j), to = children[1], edge_aes = list(label = "T"))
      grph <- grph %>% DiagrammeR::add_edge(from = paste0(i, ", ", j), to = children[2], edge_aes = list(label = "H"))
      children <- children[3:length(children)]
    }
  }

  # Changing labels to values
  grph$nodes_df$label <- unlist(lapply(grph$nodes_df$label, function(y) {
    (unlist(x[as.numeric(gsub(",.*", "", y)), as.numeric(gsub(".*,\\s*", "", y))]))
  }))

  # Plotting the graph
  DiagrammeR::render_graph(graph = grph, layout = "tree")
}
