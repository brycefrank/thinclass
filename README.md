`thinclass` is an implementation of the commercial thinning classifier developed in Frank et al. (2019) for the R programming language.
It is meant to provide a re-producable commercial thinning classifier using the XGBoost (XGB) and Random Forests (RF) methods, fixed radius plot data, and coincident
aerial lidar acquisition.

## Installation

Some dependencies are required for use:

```
install.packages(c('rBayesianOptimization', 'raster', 'tools', 'xgboost', 'randomForest', 'pROC', 'devtools'))
```

Load `thinclass` using `devtools`:

```
library(devtools)
devtools::load_all("path-to-thinclass-directory")
```

## Structure

`thinclass` is structured as an R package and can be deconstructed into two procedures. Each procedure has respective script in the `R` directory.
Comments and documentation strings should clarify the purpose of each function.

1. `data_prep`: assigns field plots to thin/no-thin categories, validates all needed raster files are present, implements some other helper functions.
2. `optimization`: selects hyperparameters for the XGB and RF methods using a Bayesian hyperparameter optimization method (Yan 2016).

The raw code used for the analysis of Frank et al. (2019) was originally implemented in Python, and is available for viewing in `original_scripts_and_data.zip`.
However, to develop an understanding of the fundamentals of the study and to implement the method we suggest reading the source code in the `R`
directory rather than the Python scripts (see below). Also, please refer to the flowchart provided in `demo/flow_chart.pptx`.

## Deviations from Manuscript

For simplicity, a number of differences in the R version and the raw manuscript files exist.

1. Bayesian hyperparameter optimization (Yan 2016) is implemented in favor of the more computationally intensive grid search.
2. Downsampling assessment is removed as this was primarily a research objective and is not necessary for operational applications.
3. Logistic regression is not implemented, as it clearly demonstrated poorer performance than RF and XGB.

## References

Frank, Bryce, Francisco Mauro, Hailemariam Temesgen, and Kevin R. Ford. 2019. "Analysis of Classification Methods for Identifying Stands for Commercial Thinning Using LiDAR." *Canadian Journal of Remote Sensing* Vol. 45(5): 673-690.

Yan, Yachen. 2016. rBayesianOptimization: Bayesian Optimization of Hyperparameters (version 1.1.0). https://CRAN.R-project.org/package=rBayesianOptimization.
