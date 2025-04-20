#' S3 method for printing model selection information
#'
#' After conducting the model fitting procedure on the data set, the best fitting
#' model has to be chosen. The print function shows the R2 and other information
#' on the different best fitting models with increasing number of predictors.
#' @param x The model from the 'bestModel' function or a cnorm object
#' @param ... additional parameters
#' @return A table with information criteria
#' @export
#'
#' @family model
print.cnorm <- function(x, ...) {  UseMethod("print") }

#' S3 method for printing the results and regression function of a cnorm model
#'
#' @param object A regression model or cnorm object
#' @param ... additional parameters
#' @return A report on the regression function, weights, R2 and RMSE
#' @export
#' @family model
summary.cnorm <- function(object, ...) {  UseMethod("summary") }



#' S3 function for plotting cnorm objects
#'
#' @param x the cnorm object
#' @param y the type of plot as a string, can be one of
#' 'raw' (1), 'norm' (2), 'curves' (3), 'percentiles' (4), 'series' (5), 'subset' (6),
#' or 'derivative' (7), either as a string or the according index
#' @param ... additional parameters for the specific plotting function
#'
#' @export
#'
#' @family plot
plot.cnorm <- function(x, y, ...) {  UseMethod("plot") }


print.cnorm <- printSubset
plot.cnorm <- plotCnorm
summary.cnorm <- modelSummary

