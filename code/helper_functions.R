plot_arrows_on_top <- function(dag) {
  n_layers <- length(dag$layers)
  temp_layer <- dag$layers[[1]]
  for (layer_idx in 1:(n_layers-1)) {
    dag$layers[[layer_idx]] <-  dag$layers[[layer_idx+1]]
  }
  dag$layers[[n_layers]] <- temp_layer
  plot(dag)
}
