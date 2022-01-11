library(tidyverse)
library(docstring)
library(MASS)

# todos
# 1. draw samples from the rough and the smooth functions
# 1.1 create regions with high density and low density of training points
# 
# 2. fit the gp model in both conditions
# 3. evaluate the kernel function for the intended test points
# 4. feed the summed similarity measure into the linear ba model
# 5. manually play around with the other parameters to achieve reasonable predictions


xy_space <- function(x_range, seed,  x_nudge = 1, fn = "sin"){
  if (sum(x_range < 0) != 0 | !is.integer(x_range[1]) | !is.integer(x_range[2])) {
    stop("only positive integers allowed in x range")
  } 
  x <- seq(x_range[1] + x_nudge, x_range[2] - x_nudge, by = 1)
  tbl_x <- crossing(x1 = x, x2 = x)
  set.seed(seed)
  shift_x <- runif(2, 0, 2*pi)
  if (fn == "sin") {
    tbl_x$y <- sin(tbl_x$x1 + shift_x[1]) + sin(tbl_x$x2 + shift_x[2])
  }
  return(tbl_x)
}

xy_sample <- function(tbl_xy, n_train, n_centers, p_crowded) {
  x_range_constrained <- c(min(tbl_xy$x1), max(tbl_xy$x2))
  x_centers <- sample(seq(x_range_constrained[1], x_range_constrained[2], by = 1), n_centers*2, replace = FALSE)
  groups <- rep(seq(1, length(x_centers)/2, by = 1), each = 2)
  l_centers <- split(x_centers, groups)
  n_crowded <- round(n_train * p_crowded)
  n_sparse <- n_train - n_crowded
  x_crowded <- mvrnorm(n_crowded, l_centers[[1]], matrix(c(1, 0, 0, 1), ncol = 2))
  x_sparse <- cbind(
    runif(n_sparse, x_range_constrained[1], x_range_constrained[2]),
    runif(n_sparse, x_range_constrained[1], x_range_constrained[2])
  )
  m_xy_train <- rbind(x_crowded, x_sparse)
  colnames(m_xy_train) <- c("x1", "x2")
  tbl_xy_train <- as_tibble(m_xy_train)
  return(tbl_xy_train)
}

x_range <- c(0L, 15L)
n <- 
tbl_xy <- xy_space(x_range, 2)
tbl_xy_train <- xy_sample(tbl_xy, 100, 3, .8)



ggplot(tbl_xy, aes(x1, x2)) +
  geom_tile(aes(fill = y)) +
  geom_text(aes(label = round(y, 1)), color = "white") +
  scale_fill_viridis_c() +
  theme_bw()




















