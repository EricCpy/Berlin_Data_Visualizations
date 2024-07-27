plot_arrows_on_top <- function(dag) {
  dag$layers[[5]] <- dag$layers[[1]]
  plot(dag)
}
