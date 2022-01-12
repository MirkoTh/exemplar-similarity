
# Load Packages and Home-Grown Functions ----------------------------------

library(tidyverse)
library(docstring)
library(MASS)


l <- c("R/utils.R", "R/plotting.R")
walk(l, source)

# todos
# CHECK 1. draw samples from the rough and the smooth functions
# CHECK 1.1 create regions with high density and low density of training points
# 
# CHECK fit the gp model in both conditions
# CHECK evaluate the kernel function for the intended test points
# 4. feed the summed similarity measure into the linear ba model
# 5. manually play around with the other parameters to achieve reasonable predictions




# Define Simulation Parameters --------------------------------------------


x_range <- c(0L, 15L)
n_train <- 100
n_test <- 100
n_centers <- 4
n_smoothness <- 2
p_crowded_train <- .8
p_crowded_test <- .5
smooth_vals <- c("rough", "smooth")
l_info <- list(
  "x_range" = x_range, 
  "n_train" = n_train, "n_test" = n_test,
  "n_center" = n_centers, "n_smoothness" = n_smoothness,
  "p_crowded_train" = p_crowded_train,
  "p_crowded_test" = p_crowded_test
)
i_vars <- c("x1", "x2")
d_var <- "y"


# Simulate Data From Conditions -------------------------------------------


# draw samples from smooth and rough conditions
# there are x regions with high and with low density (crowded vs. sparse)
# p crowded differs between training and test

l_smooth <- map(smooth_vals, space_and_exemplars, seed = 7323, l_info = l_info, fn = fn_sin)
tbl_space <- reduce(map(l_smooth, 1), rbind)

tbl_xy_train <- reduce(map(l_smooth, 2), rbind)
tbl_xy_train$smoothness <- as.factor(tbl_xy_train$smoothness)
l_tbl_xy_train <- split(tbl_xy_train, ~ smoothness)

tbl_xy_test <- reduce(map(l_smooth, 3), rbind)
tbl_xy_test$smoothness <- as.factor(tbl_xy_test$smoothness)
l_tbl_xy_test <- split(tbl_xy_test, ~ smoothness)

# overview plots
plot_xy(tbl_space) + facet_wrap(~ smoothness)
plot_x(tbl_xy_train) + facet_wrap(~ smoothness)
plot_x(tbl_xy_test) + facet_wrap(~ smoothness)

# save experimental data here that can be loaded from within python
write_json(l_tbl_xy_train, "data/l-data-train.json")



# Execute Model Fitting in Python -----------------------------------------


shell.exec("python\\run-python.bat")
Sys.sleep(5)


# Import Fitted Parameters from Python ------------------------------------

## this again can be iterated over...
tbl <- rbind(l_tbl_xy_test[[1]], l_tbl_xy_train[[1]])

l_params_fitted <- read_json("data/model-params.json")
names(l_params_fitted) <- smooth_vals
N <- nrow(tbl)
tbl_sim_rbf <- similarity_rbf(l_params_fitted[["rough"]]$length_scale, 1, tbl, N)
sims_test <- rowSums(
  tbl_sim_rbf[
    1:l_info[["n_test"]], 
    (l_info[["n_test"]]+1):(l_info[["n_test"]]+l_info[["n_train"]])
  ]
)
cols <- colnames(tbl_sim_rbf)
tbl_sim_rbf$var2 <- cols


tbl_sim_rbf %>% pivot_longer(cols = all_of(cols)) %>%
  mutate(var2 = factor(var2, levels = cols),
         name = factor(name, levels = cols)) %>%
  ggplot() +
  geom_tile(aes(name, var2, color=value)) +
  scale_color_viridis_c()
