library(tidyverse)

xy_space <- function(fn, smoothness, x_range, seed, x_nudge = 1){
  #' @description create xy space only using integers as a grid
  #' @param smoothness stating whether a "smooth" or "rough" function should be sampled from
  #' @param x_range two integers stating min and max of x space
  #' @param seed controlling the sampling from the function relating x to y
  #' @param x_nudge leaving open space at lower and upper boundary of space, default to 1
  #' @return tbl with the grid of xy values
  #' 
  if (sum(x_range < 0) != 0 | !is.integer(x_range[1]) | !is.integer(x_range[2])) {
    stop("only positive integers allowed in x range")
  } 
  x <- seq(x_range[1] + x_nudge, x_range[2] - x_nudge, by = 1)
  tbl_x <- crossing(x1 = x, x2 = x)
  tbl_x$y <- fn(tbl_x, seed, smoothness)
  tbl_x$smoothness <- smoothness
  return(tbl_x)
}

fn_sin <- function(tbl, seed, smoothness) {
  #' @description sampling from a sinus function
  #' @param seed controlling the sampling
  #' @param smoothness stating whether a "smooth" or "rough" function should be sampled from
  #' @return vector with sampled y values
  #' 
  if (smoothness == "smooth") {
    scale <- 3
  } else if (smoothness == "rough") {
    scale <- 1
  }
  set.seed(seed)
  shift_x <- runif(2, 0, 2*pi)
  return(sin(tbl$x1 + shift_x[1]) / scale + sin(tbl$x2 + shift_x[2]) / scale)
}

xy_sample <- function(tbl_xy, n_train, n_centers, p_crowded, l_centers = NULL) {
  #' @description sampling x values and respective y values
  #' a proportion of x values (p_crowded) are sampled from n cluster centers (n_centers)
  #' 
  #' @param tbl_xy tbl spanning xy space
  #' @param n_train nr of training examples
  #' @param n_centers nr of cluster centers
  #' @param p_crowded proportion of examples sampled from cluster centers
  #' @return tbl with sampled xy values given constraints
  #'
  x_range_constrained <- c(min(tbl_xy$x1), max(tbl_xy$x2))
  # make distance between centers large enough
  min_distance <- 0
  if (is.null(l_centers)){
    while(min_distance < 5){
      x_centers <- sample(
        seq(x_range_constrained[1], x_range_constrained[2], by = 1), n_centers*2, replace = FALSE
      )
      grps <- rep(seq(1, length(x_centers)/2, by = 1), each = 2)
      n_grps <- max(grps)
      l_centers <- split(x_centers, grps)
      dist_eucl <- function(a, b, l) sqrt((l[[a]][1] - l[[b]][1])^2 + (l[[a]][2] - l[[b]][2])^2)
      cross_centers <- crossing(a = 1:length(l_centers), b = 1:length(l_centers))
      distances <- unlist(pmap(cross_centers, dist_eucl, l_centers))
      distances <- distances[distances != 0]
      min_distance <- min(distances)
    }
  } else {n_grps <- length(l_centers)}
  n_crowded <- round(n_train * p_crowded)
  unevens <- n_crowded %% n_grps
  n_crowded <- n_crowded + (n_grps - unevens)
  n_sparse <- n_train - n_crowded
  x_crowded <- map(
    l_centers, mvrnorm, n = n_crowded / n_grps, Sigma = matrix(c(.75, 0, 0, .75), ncol = 2)
  ) %>% reduce(rbind)
  x_sparse <- cbind(
    runif(n_sparse, x_range_constrained[1], x_range_constrained[2]),
    runif(n_sparse, x_range_constrained[1], x_range_constrained[2])
  )
  m_xy_train <- rbind(x_crowded, x_sparse)
  colnames(m_xy_train) <- c("x1", "x2")
  tbl_xy_train <- as_tibble(m_xy_train)
  tbl_xy_train$crowding <- factor(c(rep("crowded", n_crowded), rep("sparse", n_sparse)))
  l_out <- list(tbl_xy_train, l_centers)
  return(l_out)
}


space_and_exemplars <- function(smoothness, seed, l_info, fn) {
  #' @description pipeline function to sample train and test data for one smoothness condition
  #' @param seed controlling the sampling
  #' @param l_info list with simulation parameters
  #' @return list with tbl spanning xy space and train & test tbls with sampled values
  #'
  tbl_xy <- xy_space(fn, smoothness, l_info[["x_range"]], seed)
  l_train <- xy_sample(tbl_xy, l_info[["n_train"]], l_info[["n_center"]], l_info[["p_crowded_train"]])
  tbl_xy_train <- l_train[[1]]
  l_centers <- l_train[[2]]
  tbl_xy_train$y <- fn(tbl_xy_train, 2, smoothness)
  tbl_xy_train$smoothness <- smoothness
  print(l_centers)
  l_test <- xy_sample(tbl_xy, l_info[["n_test"]], l_info[["n_center"]], l_info[["p_crowded_test"]], l_centers)
  tbl_xy_test <- l_test[[1]]
  tbl_xy_test$y <- fn(tbl_xy_test, 2, smoothness)
  tbl_xy_test$smoothness <- smoothness
  return(list(tbl_xy, tbl_xy_train, tbl_xy_test))
}


distance_rbf <- function(i, df_x, N, lambda, sigma){
  m <- matrix(unlist(rep(df_x[i, ], N)), N, 2, byrow = TRUE)
  colnames(m) <- c("x_1", "x_2")
  tbl_single <- as_tibble(m)
  tbl_single["sim"] <- - (sqrt(
    (tbl_single$x_1 - tbl_x$x_1)^2 +
      (tbl_single$x_2 - tbl_x$x_2)^2
  )^2) / (2*lambda^2)
  return (tbl_single$sim)
}

similarity_rbf <- function(lambda, sigma, tbl_x, N){
  l <- map(1:N, distance_rbf, tbl_x, N, lambda, sigma)
  m <- matrix(unlist(l), N, N, byrow = TRUE)
  colnames(m) <- str_c("x_", 1:N)
  tbl <- as_tibble(sigma^2 * exp(m))
  return (tbl)
}
