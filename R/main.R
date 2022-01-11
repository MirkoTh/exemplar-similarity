library(tidyverse)
library(docstring)
library(MASS)


l <- c("R/utils.R", "R/plotting.R")
walk(l, source)

# todos
# 1. draw samples from the rough and the smooth functions
# 1.1 create regions with high density and low density of training points
# 
# 2. fit the gp model in both conditions
# 3. evaluate the kernel function for the intended test points
# 4. feed the summed similarity measure into the linear ba model
# 5. manually play around with the other parameters to achieve reasonable predictions


x_range <- c(0L, 15L)
n_train <- 80
n_centers <- 4
n_smoothness <- 2
p_crowded <- .8
l_info <- list(
  "x_range" = x_range, "n_train" = n_train,
  "n_center" = n_centers, "n_smoothness" = n_smoothness,
  "p_crowded" = p_crowded
)



l_smooth <- map(c("smooth", "rough"), space_and_exemplars, seed = 7323, l_info = l_info, fn = fn_sin)
tbl_space <- reduce(map(l_smooth, 1), rbind)
tbl_xy_train <- reduce(map(l_smooth, 2), rbind)
plot_xy(tbl_space) + facet_wrap(~ smoothness)
plot_x_train(tbl_xy_train) + facet_wrap(~ smoothness)







