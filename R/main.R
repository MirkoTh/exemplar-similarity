
# Load Packages and Home-Grown Functions ----------------------------------

library(tidyverse)
library(docstring)
library(MASS)
library(jsonlite)


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
n_centers <- 2
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

plot_xy_x(tbl_space, tbl_xy_train) + facet_wrap(~ smoothness)
plot_xy_x(tbl_space, tbl_xy_test) + facet_wrap(~ smoothness)


# save experimental data here that can be loaded from within python
write_json(l_tbl_xy_train, "data/l-data-train.json")



# Execute Model Fitting in Python -----------------------------------------


shell.exec("python\\run-python.bat")
#Sys.sleep(20)


# Import Fitted Parameters from Python ------------------------------------

## this again can be iterated over...
l_params_fitted <- read_json("data/model-params.json")
names(l_params_fitted) <- smooth_vals


l_sims <- pmap(
  list(l_tbl_xy_test, l_tbl_xy_train, l_params_fitted),
  similarity_test_to_train,
  l_info
)


# exemplary plot of similarities between all data points (train & test)

tbl_both <- similarities_to_tbl(l_sims)

ggplot(tbl_both) +
  geom_tile(aes(name, var2, color=value)) +
  scale_color_viridis_c(name = "Similarity") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
    ) + facet_wrap(~ Smoothness)

l_tbl_xy_test$rough$sim_to_train <- l_sims[[1]][[1]]
l_tbl_xy_test$smooth$sim_to_train <- l_sims[[2]][[1]]
tbl_xy_test$sim_to_train <- c(l_sims[[1]][[1]], l_sims[[2]][[1]])
ggplot(tbl_xy_test, aes(sim_to_train, group = crowding)) +
  geom_histogram(aes(fill = crowding)) +
  facet_wrap(~ smoothness) +
  scale_fill_brewer(name = "Crowding", palette = "Set1") +
  theme_bw() +
  labs(
    x = "Similarity",
    y = "Counts"
  )





