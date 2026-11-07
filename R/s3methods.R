#' Model diagnostics for parametric models
#'
#' @param model A cnormBetaBinomial, cnormShash or cnormCMP object
#' @param ... Further arguments (e.g. age, score, weights) passed on
#' @export
#' @family model
diagnostics <- function(model, ...) {
  if (inherits(model, c("cnormBetaBinomial", "cnormBetaBinomial2"))) {
    diagnostics.betabinomial(model, ...)
  } else if (inherits(model, "cnormShash")) {
    diagnostics.shash(model, ...)
  } else if (inherits(model, "cnormCMP")) {
    diagnostics.cmp(model, ...)
  } else {
    stop("No diagnostics available for objects of class '", class(model)[1], "'.")
  }
}

#' Generate norm tables for all model types
#'
#' Convenience function that selects the appropriate implementation based on
#' the class of \code{model}.
#' @param A Age / grouping value(s)
#' @param model A cnorm, cnormModel, cnormBetaBinomial, cnormShash or cnormCMP object
#' @param ... Further arguments passed to the specific implementation
#' @export
#' @family model
normTable <- function(A, model, ...) {
  if (inherits(model, c("cnormBetaBinomial", "cnormBetaBinomial2"))) {
    normTable.betabinomial(model, ages = A, ...)
  } else if (inherits(model, "cnormShash")) {
    normTable.shash(model, ages = A, ...)
  } else if (inherits(model, "cnormCMP")) {
    normTable.cmp(model, ages = A, ...)
  } else {
    normTable.default(A, model, ...)   # cnorm, cnormModel, lm
  }
}

#' General convenience plotting function
#'
#' @param x a cnorm object
#' @param y The type of plot as a string or index: 'raw' (1), 'norm' (2),
#' 'curves' (3), 'percentiles' (4), 'density' (5), 'series' (6),
#' 'subset' (7) or 'derivative' (8). Defaults to 'percentiles'.
#' @param ... additional parameters for the specific plotting function
#'
#' @export
plotCnorm <- function(x, y, ...) {
  if (!isTaylor(x)) {
    message("Please provide a cnorm object as x.")
    return(invisible(NULL))
  }
  if (!is.character(y) && !is.numeric(y)) {
    message("y must be a plot-type string or integer index.")
    return(invisible(NULL))
  }
  if (y == "raw"         || y == 1)
    plotRaw(x, ...)
  else if (y == "norm"        || y == 2)
    plotNorm(x, ...)
  else if (y == "curves"      || y == 3)
    plotNormCurves(x, ...)
  else if (y == "percentiles" || y == 4)
    plotPercentiles(x, ...)
  else if (y == "density"     || y == 5)
    plotDensity(x, ...)
  else if (y == "series"      || y == 6)
    plotPercentileSeries(x, ...)
  else if (y == "subset"      || y == 7)
    plotSubset(x, ...)
  else if (y == "derivative"  || y == 8)
    plotDerivative(x, ...)
  else
    stop("Unknown plot type")
}

#' @export
normTable.cnorm <- function(A, model, ...) normTable.default(A, model, ...)

#' @export
normTable.cnormModel <- function(A, model, ...) normTable.default(A, model, ...)

# ---- Base generics (print, summary, plot): methods only --------------------

#' @rdname printSubset
#' @export
print.cnorm <- function(x, ...) printSubset(x, ...)

#' @rdname modelSummary
#' @export
summary.cnorm <- function(object, ...) modelSummary(object, ...)

#' @rdname plotCnorm
#' @export
plot.cnorm <- function(x, y = "percentiles", ...) {
  plotCnorm(x, y, ...)
}


# ---- normTable methods -----------------------------------------------------

#' @export
normTable.cnormBetaBinomial  <- function(A, model, ...) normTable.betabinomial(A, model, ...)

#' @export
normTable.cnormBetaBinomial2 <- function(A, model, ...) normTable.betabinomial(A, model, ...)

#' @export
normTable.cnormShash <- function(A, model, ...) normTable.shash(A, model, ...)

#' @export
normTable.cnormCMP   <- function(A, model, ...) normTable.cmp(A, model, ...)

