library(tools)
library(raster)

#' Assigns field plots to THIN (1) or NO_THIN (0)
#' classes, given a set of basal area, Curtis' relative density
#' and cubic foot volume thresholds. The input plot frame must have the columns
#' 'vol_cubft', 'ba_ac', and 'crd' as well as the plot-level lidar predictors
#' desired for modeling. By default, the thresholds are set to those used in
#' the manuscript.
#'
#' @param plot_frame A dataframe of field plots with the needed columns (see above)
#' @param vol_lb The lower bound for the cubic foot volumes
#' @param vol_ub The upper bound for the cubic foot volumes
#' @param ba_lb The lower bound for the cubic foot volume per acre.
#' @param crd_lb The lower bound for the Curtis' relative density.
#'
#' @return A new dataframe with the binary 'thinnable' column appended.
assign_plots <- function(plot_frame, vol_lb = 3500, vol_ub = 10000, ba_lb = 140, crd_lb = 50) {

  vol_bool <- (plot_frame[,'vol_cubft'] > vol_lb) & (plot_frame[,'vol_cubft'] < 10000)
  ba_bool  <-  plot_frame[,'ba_ac'] > ba_lb
  crd_bool <-  plot_frame[,'crd'] > crd_lb

  thin_bool <- vol_bool * ba_bool * crd_bool
  plot_frame$thinnable <- as.numeric(thin_bool)

  drop_cols <- c("Plot_ID", "vol_cubft", "ba_ac", "crd")

  return(plot_frame[,!(colnames(plot_frame) %in% drop_cols)])
}


#' Check if all predictors exist in the path
validate_predictor_rasters <- function(predictors, path) {
  raster_paths <- list.files(path)
  raster_names <- c()
  for (raster_path in raster_paths) {
    raster_name <- file_path_sans_ext(raster_path)
    raster_names <- c(raster_names, raster_name)
  }

  if (!all(predictors %in% raster_names)) {
    stop("Some predictors defined in the thin_frame do not have matching rasters in the raster directory.")
  }
  return(raster_names)
}

load_predictor_rasters <- function(thin_frame, path) {
  predictors <- colnames(thin_frame)[colnames(thin_frame) != "thinnable"]
  raster_names <- validate_predictor_rasters(predictors, path)

  raster_paths <- list.files(path, full.names=TRUE)

  rasters <- list()
  for (i in seq_along(raster_paths)) {
    raster_name <- raster_names[[i]]
    raster_path <- raster_paths[[i]]

    rasters[[raster_name]] <- raster(raster_path, band=1)
  }
  raster_stack <- stack(rasters)
}

#' An internal function for converting raster inputs into the needed
#' DMatrix for XGB predictions.
xgb_pred_fun <- function(model, data) {
  dmat <- xgb.DMatrix(as.matrix(data[,model$feature_names]))
  p <- predict(model, newdata=dmat)
  return(p)
}
