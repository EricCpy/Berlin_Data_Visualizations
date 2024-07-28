plot_arrows_on_top <- function(dag) {
  n_layers <- length(dag$layers)
  temp_layer <- dag$layers[[1]]
  for (layer_idx in 1:(n_layers-1)) {
    dag$layers[[layer_idx]] <-  dag$layers[[layer_idx+1]]
  }
  dag$layers[[n_layers]] <- temp_layer
  return(dag)
}

layer_on_bottom <- function(gg, n) {
  temp_layer <- gg$layers[[n]]
  for (layer_idx in n:2) {
    gg$layers[[layer_idx]] <-  gg$layers[[layer_idx-1]]
  }
  gg$layers[[1]] <- temp_layer
  return(gg)
}
