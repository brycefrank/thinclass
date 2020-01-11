# This demo script reproduces the BLM-SWO thinning classification map.
library(devtools)
library(xgboost)
devtools::load_all("C:\\Users\\frankbr\\Programming\\thinclass")


# Read in plot dataframe
plots <- read.csv("data/plots.csv")

# The plot dataframe must have vol_cubft, ba_ac, and crd columns
colnames(plots)

# Now we need to assign the plots to THIN or NO-THIN categories
# we can adjust the decision boundaries to our liking. These are the
# default settings (i.e. thouse used in the manuscript)
thin_plots <- assign_plots(plots, vol_lb = 3500, vol_ub = 10000, ba_lb = 140, crd_lb = 50)


# Now we retrieve the XGB and RF model configurations that are fit
# using Bayesian Optimization and 10 cross-validation folds
xgb_mod <- optimize_XGB(thin_plots, nfolds=10)
rf_mod <- optimize_RF(thin_plots, nfolds=10)

# To avoid re-fitting, we can save the models as RData objects
saveRDS(xgb_mod, file="data/pre_trained/xgb_mod.RDS")
saveRDS(rf_mod, file="data/pre_trained/rf_mod.RDS")

# Using these models, we can produce continuous thinning classification map for XGB and RF
# First, load the predictor rasters
rasters <- load_predictor_rasters(thin_plots, "data/predictor_rasters/")

# Now predict
xgb_continuous <- predict(rasters, xgb_mod, progress='text', fun=xgb_pred_fun)
rf_continuous <- (1 - predict(rasters, rf_mod, progress='text', type='prob'))

# and write the predict maps to file
writeRaster(xgb_continuous, file="data/demo_maps/xgb_continuous.tif")
writeRaster(rf_continuous,  file="data/demo_maps/rf_continuous.tif")

# For binary maps we need the optimum thresholds
xgb_threshold <- select_threshold(thin_plots, xgb_mod)
rf_threshold <- select_threshold(thin_plots, rf_mod)

# Values exceeding these thresholds are considered "THIN" and "NO THIN" otherwise
xgb_binary <- xgb_continuous > xgb_threshold
rf_binary <- rf_continuous > rf_threshold

# Write them to file
writeRaster(xgb_binary, file="data/demo_maps/xgb_binary.tif")
writeRaster(rf_binary,  file="data/demo_maps/rf_binary.tif")
