#' Plotting an option tree
#'
#' Description
#' @param x A matrix of either option prices or deltas as returned by Derivative_Security().
#' @return Plots the option tree in the viewer plane..
#' @import DiagrammeR
#' @examples
#' option <- Derivative_Security(S = 4, K = 4, N = 4, r = 0.25, d = 0.5, u = 2, type = "European_put")
#' PlotTree(option$derivative_prices)
#' PlotTree(option$delta)
#' @export
PlotTree <- function(x) {
  # Graph object
  grph <- DiagrammeR::create_graph()

  if(length(x) < 2){
    return("Matrix must have more than one value")
  }

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
