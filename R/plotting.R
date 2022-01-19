
plot_xy <- function(tbl) {
  ggplot(tbl, aes(x1, x2)) +
    geom_tile(aes(fill = y)) +
    geom_text(aes(label = round(y, 1)), color = "white") +
    scale_fill_viridis_c() +
    theme_bw() +
    labs(
      x = expression(X["1"]),
      y = expression(X["2"])
    )
}

plot_x <- function(tbl) {
  ggplot(tbl, aes(x1, x2, group = crowding)) +
    geom_point(aes(color = crowding)) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    labs(
      x = expression(X["1"]),
      y = expression(X["2"])
    )
}

plot_xy_x <- function(tbl_base, tbl_new) {
  ggplot() +
    geom_tile(data = tbl_base, aes(x1, x2, fill = y), alpha = .25) +
    geom_point(data = tbl_new, aes(x1, x2, color = crowding)) +
    geom_density_2d(data = tbl_new, aes(x1, x2), alpha = .25) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_viridis_c() +
    theme_bw() +
    labs(
      x = expression(X["1"]),
      y = expression(X["2"])
    )
}


plot_arrangement <- function(pl, n_cols = 2) {
  #' plot a list of plots on one page
  #' 
  #' @param pl all the ggplots
  #' @param n_cols nr columns of the page layout
  n_plots <- length(pl)
  n_rows <- ceiling(n_plots / n_cols)
  marrangeGrob(pl, nrow = n_rows, ncol = n_cols)
}
