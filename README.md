`thinclass` is an implementation of the commercial thinning classifier developed in Frank et al. (2019) for the R programming language.
It is meant to provide a re-producable commercial thinning classifier using the XGBoost method, fixed radius plot data, and coincident
aerial lidar acquisition.

## Structure

`thinclass` is structured as an R package and can be deconstructed into three procedures:

1. Data preparation: assigns field plots to thin/no-thin categories, appends lidar predictors
2. Optimization/fitting: selects hyperparameters for the XGB method using a Bayesian hyperparameter optimization method
3. Map production: using a set of input lidar predictor rasters, produces a map of thinning eligibility predictions

The raw code used for the analysis of Frank et al. (2019) was originally implemented in Python, and is available for viewing in `[folder here]`.
However, to develop an understanding of the fundamentals of the study, we suggest reading the source code in the `R` directory.


## References

Frank, Bryce, Francisco Mauro, Hailemariam Temesgen, and Kevin R. Ford. "Analysis of Classification Methods for Identifying Stands for Commercial Thinning Using LiDAR." *Canadian Journal of Remote Sensing* 45, no. 5 (2019): 673-690.
