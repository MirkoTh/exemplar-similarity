
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

plot_x_train <- function(tbl) {
  ggplot(tbl, aes(x1, x2, group = crowding)) +
    geom_point(aes(color = crowding)) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    labs(
      x = expression(X["1"]),
      y = expression(X["2"])
    )
}
