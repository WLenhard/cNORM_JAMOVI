# ---------------------------------------------------------------------------
# modelling.R -- model estimation, selection, consistency screening and
#                validation for cNORM
# ---------------------------------------------------------------------------

#' Determine Regression Model
#'
#' Computes Taylor polynomial regression models by evaluating a series of models
#' with increasing number of predictors. It aims to find a consistent model that
#' effectively captures the variance in the data. It draws on the regsubsets
#' function from the leaps package, builds up to 10 models per number of
#' predictors, screens them for model consistency (monotonicity of the mapping
#' between norm score and raw score over the complete age range) and selects the
#' consistent model with the highest R^2. This automatic model selection should
#' usually be accompanied by visual inspection of the percentile plots and
#' assessment of fit statistics. Set R^2 or the number of terms manually to
#' retrieve a more parsimonious model, if desired.
#'
#' The functions \code{rankBySlidingWindow}, \code{rankByGroup}, \code{bestModel},
#' \code{computePowers} and \code{prepareData} are usually not called directly,
#' but accessed through other functions like \code{cnorm}.
#'
#' Additional functions like \code{plotSubset(model)} and \code{cnorm.cv} can
#' aid in model evaluation.
#'
#' If \code{averaging = TRUE}, the final coefficients are not taken from a
#' single selected model, but computed as a BIC-weighted average across all
#' consistency-screened candidate models (weights \eqn{w_j \propto
#' exp(-\Delta BIC_j / 2)}). Since a convex combination of functions that are
#' all monotone in the same direction is itself monotone, the averaged model is
#' guaranteed to remain consistent, while reducing model selection variance.
#' This replaces the deprecated \code{subsampling} approach.
#'
#' @param data Preprocessed dataset with 'raw' scores, powers, interactions, and
#'   usually an explanatory variable (like age).
#' @param raw Name of the raw score variable (default: 'raw').
#' @param terms Desired number of terms in the model.
#' @param R2 Adjusted R^2 stopping criterion for model building.
#' @param k Power constant influencing model complexity (default: taken from the
#'   data preparation, max: 6).
#' @param t Age power parameter. If unset, taken from the data preparation
#'   (default 3).
#' @param predictors List of predictors or regression formula for model
#'   selection. Overrides 'k' and can include additional variables.
#' @param force.in Variables forcibly included in the regression.
#' @param weights Optional case weights. If set to FALSE, default weights
#'   (if any) are ignored.
#' @param plot If TRUE (default), displays a percentile plot of the model and
#'   information about the regression object. FALSE turns off plotting and report.
#' @param extensive If TRUE (default), screen models for consistency and - if
#'   possible - exclude inconsistent ones.
#' @param averaging If TRUE (default FALSE), apply BIC-weighted model averaging
#'   across the consistency-screened candidate models instead of selecting a
#'   single model. Requires \code{extensive = TRUE} and age-based norming.
#' @param minDip Tolerance for monotonicity check. Allow small violations
#'   (default: .01 or 1% of the raw score range). Decrease e. g. to 1e-6
#'   for strict checking.
#' @return The model. Further exploration can be done using
#'   \code{plotSubset(model)} and \code{plotPercentiles(data, model)}.
#' @examples
#' \dontrun{
#' # It is not recommended to directly use this function. Rather use 'cnorm' instead.
#' normData <- prepareData(elfe)
#' model <- bestModel(normData)
#' plotSubset(model)
#' plotPercentiles(buildCnormObject(normData, model))
#'
#' # Specifying variables explicitly
#' preselectedModel <- bestModel(normData, predictors = c("L1", "L3", "L1A3", "A2", "A3"))
#' print(regressionFunction(preselectedModel))
#' }
#' @seealso plotSubset, plotPercentiles, plotPercentileSeries, checkConsistency
#' @export
#' @family model
bestModel <- function(data,
                      raw = NULL,
                      R2 = NULL,
                      k = NULL,
                      t = NULL,
                      predictors = NULL,
                      terms = 0,
                      weights = NULL,
                      force.in = NULL,
                      plot = TRUE,
                      extensive = TRUE,
                      averaging = FALSE,
                      minDip = .01) {

  # --- retrieve attributes and consolidate defaults -------------------------
  if (is.null(raw)) raw <- attr(data, "raw")
  if (is.null(raw)) raw <- "raw"

  # consolidate case weights into a single vector 'w'
  w <- NULL
  if (is.null(weights)) {
    if (!is.null(attr(data, "weights")) && !is.null(data$weights))
      w <- data$weights
  } else if (isFALSE(weights)) {
    w <- NULL
  } else if (is.numeric(weights) && length(weights) == nrow(data)) {
    w <- weights
  } else {
    warning("Invalid 'weights' argument (must be numeric and of length nrow(data)); ignored.")
  }

  kData <- attr(data, "k")
  if (is.null(k)) {
    k <- if (!is.null(kData)) kData else 5
  } else if (!is.null(kData) && k > kData) {
    warning("k parameter exceeds the power degrees in the dataset. ",
            "Setting to default of k = ", kData)
    k <- kData
  }

  if (is.null(t)) {
    t <- if (!is.null(attr(data, "t"))) attr(data, "t") else 3
  }

  if (is.null(predictors) && (k < 1 || k > 6)) {
    warning("k parameter out of bounds. Please specify a value between 1 and 6. ",
            "Setting to default = 5.")
    k <- 5
  }

  # number of candidate terms
  if (is.null(predictors)) {
    nvmax <- (t + 1) * (k + 1) - 1
  } else if (inherits(predictors, "formula")) {
    nvmax <- length(attr(stats::terms(predictors), "term.labels"))
  } else {
    nvmax <- length(predictors)
  }

  # --- check parameter ranges ------------------------------------------------
  if (!is.null(R2) && (R2 <= 0 || R2 >= 1)) {
    warning("R2 parameter out of bounds. Setting to default R2 = .99")
    R2 <- .99
  }

  if (terms < 0 || terms > nvmax) {
    warning("terms parameter out of bounds. Setting to 5.")
    terms <- 5
  }

  if (!(raw %in% colnames(data)) && !inherits(predictors, "formula")) {
    stop("Raw value variable '", raw, "' does not exist in data object.")
  }

  if (!is.null(predictors) && !inherits(predictors, "formula") &&
      !all(predictors %in% colnames(data))) {
    stop("Missing variables from predictors variable. Please check variable list.")
  }

  # --- set up regression formula ---------------------------------------------
  useAge <- isTRUE(attr(data, "useAge"))
  if (is.null(predictors)) {
    lmX <- buildFunction(raw = raw, k = k, t = t, age = useAge)
  } else if (inherits(predictors, "formula")) {
    lmX <- predictors
  } else {
    lmX <- stats::reformulate(predictors, response = raw)
  }

  big <- nvmax > 50
  if (big && plot)
    message("The computation might take some time ...")

  index <- NULL
  if (!is.null(force.in)) {
    rhs <- attr(stats::terms(lmX), "term.labels")
    index <- match(force.in, rhs)
    if (anyNA(index))
      stop("'force.in' contains variables that are not part of the model formula.")
  }

  nbest <- if (extensive && useAge) 10 else 1

  # --- best subset search -----------------------------------------------------
  # regsubsets() evaluates its 'weights' argument via model.frame(), i.e.
  # first in 'data', then in the environment of the formula. Since lmX was
  # created in buildFunction() (or supplied by the user), 'w' is not visible
  # there. We therefore wrap the formula environment: 'w' becomes findable,
  # while the original environment remains accessible as parent.
  if (!is.null(w)) {
    wEnv <- new.env(parent = environment(lmX))
    wEnv$w <- w
    environment(lmX) <- wEnv

    subsets <- regsubsets(lmX, data = data, nbest = nbest, nvmax = nvmax,
                          force.in = index, really.big = big,
                          weights = w, method = "exhaustive")
  } else {
    subsets <- regsubsets(lmX, data = data, nbest = nbest, nvmax = nvmax,
                          force.in = index, really.big = big,
                          method = "exhaustive")
  }

  results <- summary(subsets)
  highestConsistent <- NULL
  if (extensive && useAge) {
    results <- screenSubset(data, results, data[[raw]], weights = w,
                            minDip =  (max(data[[raw]])-min(data[[raw]])) * minDip)
    highestConsistent <- results$highestConsistent
  }

  # --- model selection strategy ------------------------------------------------
  # 1. If no criterion is specified, take the largest consistent model, if available
  # 2. else take the first model exceeding R2 > .99
  # 3. else fall back to BIC-optimal model
  # 4. if terms are specified, take terms
  # 5. selection based on R2
  n_terms_per_row <- rowSums(results$outmat == "*")
  selectionStrategy <- 0

  if (is.null(R2) && terms == 0) {
    if (!is.null(results$highestConsistent)) {
      i <- results$highestConsistent
      selectionStrategy <- 1
      report <- paste0("Final solution: ", n_terms_per_row[i],
                       " terms (highest consistent model)")
    } else {
      if(!useAge)
        i <- which(results$adjr2 > 0.995)[1]
      else
        i <- which(results$adjr2 > 0.99)[1]
      if (is.na(i)) {
        # fall back to the BIC-optimal model instead of the smallest one
        i <- which.min(results$bic)
        selectionStrategy <- 3
        report <- paste0(
          "Final solution: ", n_terms_per_row[i],
          " terms (BIC-optimal fallback; R2 > .99 not reached)")
    } else {
        selectionStrategy <- 2
        report <- paste0("Final solution: ", n_terms_per_row[i],
                         " terms (model exceeding R2 > .99)")
      }
    }
  } else if (terms > 0) {
    candidate <- which(n_terms_per_row == terms)[1]
    if (is.na(candidate)) {
      candidate <- which.min(abs(n_terms_per_row - terms))
      warning("No model with exactly ", terms,
              " terms is available after consistency screening; ",
              "using closest match (", n_terms_per_row[candidate], " terms).")
    }
    i <- candidate
    selectionStrategy <- 4
    report <- paste0("User specified solution: ", n_terms_per_row[i], " terms")
  } else {
    candidates <- which(results$adjr2 > R2)
    if (length(candidates) == 0L) {
      i <- which.max(results$adjr2)
      selectionStrategy <- 3
      report <- paste0("User specified solution: R2 > ", R2,
                       ", but value not reached. Using best available model (",
                       n_terms_per_row[i], " terms).")
    } else {
      i <- candidates[which.min(n_terms_per_row[candidates])]
      selectionStrategy <- 5
      report <- paste0("User specified solution: R2 > ", R2,
                       " resulting in ", n_terms_per_row[i], " terms")
    }
  }

  # after selection, independent of strategy:
  if (extensive && useAge && is.null(highestConsistent)) {
    warning("No model passed the monotonicity screening. The selected model ",
            "violates consistency somewhere in the norm score / age range. ",
            "Inspect 'plotPercentileSeries(model)', restrict the norm score ",
            "range (e.g. +/- 2.5 SD via minNorm/maxNorm), or reduce k/t.")
  }

  report[2] <- paste0("R-Square Adj. = ", round(results$adjr2[i], digits = 6))

  # --- final model fit -----------------------------------------------------------
  selectedTerms <- colnames(results$outmat)[results$outmat[i, ] == "*"]
  text <- paste0(raw, " ~ ", paste(selectedTerms, collapse = " + "))
  report[3] <- paste0("Final regression model: ", text)

  if (averaging && !is.null(results$consistent) &&
      any(results$consistent, na.rm = TRUE)) {
    bestformula <- weightedAverageModel(results, data, raw, weights = w)
    report[3] <- paste0(report[3],
                        " (BIC-weighted averaging over consistent models applied)")
  } else {
    if (averaging)
      warning("Model averaging requires consistency screening (extensive = TRUE) ",
              "and age-based norming with at least one consistent model. ",
              "Falling back to single model fit.")
    if (is.null(w))
      bestformula <- stats::lm(stats::as.formula(text), data = data)
    else
      bestformula <- stats::lm(stats::as.formula(text), data = data, weights = w)
  }

  # --- fit statistics ---------------------------------------------------------
  tab <- data.frame(raw = data[[raw]], fitted = bestformula$fitted.values)
  tab <- tab[stats::complete.cases(tab), ]
  rmse <- sqrt(mean((tab$raw - tab$fitted)^2))

  # --- model information --------------------------------------------------------
  bestformula$ideal.model <- i
  bestformula$cutoff <- R2
  bestformula$subsets <- results
  bestformula$useAge <- useAge

  if (is.null(data$A1)) {          # conventional norming
    bestformula$minA1 <- 0
    bestformula$maxA1 <- 0
  } else {                         # continuous norming
    bestformula$minA1 <- min(data$A1)
    bestformula$maxA1 <- max(data$A1)
  }

  bestformula$minL1 <- min(data$L1)
  bestformula$maxL1 <- max(data$L1)
  bestformula$minRaw <- min(data[[raw]])
  bestformula$maxRaw <- max(data[[raw]])
  bestformula$raw <- raw
  bestformula$rmse <- rmse
  bestformula$scaleSD <- attr(data, "scaleSD")
  bestformula$scaleM <- attr(data, "scaleM")
  bestformula$descend <- attr(data, "descend")
  bestformula$group <- attr(data, "group")
  bestformula$age <- attr(data, "age")
  bestformula$k <- attr(data, "k")
  bestformula$A <- attr(data, "A")
  bestformula$highestConsistent <- highestConsistent
  bestformula$selectionStrategy <- selectionStrategy

  # --- report ----------------------------------------------------------------
  report[4] <- paste0("Regression function: ",
                      regressionFunction(bestformula, digits = 10))
  report[5] <- paste0("Raw Score RMSE = ", round(rmse, digits = 5))
  if (!is.null(w)) {
    report[6] <- paste0(
      "Post stratification was applied. The weights range from ",
      round(min(w), digits = 3), " to ", round(max(w), digits = 3),
      " (m = ", round(mean(w), digits = 3),
      ", sd = ", round(stats::sd(w), digits = 3), ").")
  }

  bestformula$report <- report

  if (plot) cat(report, sep = "\n")

  if (anyNA(bestformula$coefficients)) {
    warning("The regression contains missing coefficients. No fitting model ",
            "could be found. Please try a different number of terms.")
  }

  nSelected <- length(bestformula$coefficients) - 1L
  if (nSelected < 4 && useAge) {
    message("\nThe model includes a low number of terms. Models with four or ",
            "more predictors are usually more robust. The low number is probably ",
            "the consequence of the rather strict monotonicity checks in cNORM. ",
            "Information functions with 'plotSubset' and 'plotPercentileSeries' ",
            "might help to identify a balanced number of terms. Consider fixing ",
            "the 'terms' parameter to a higher number.")
  }

  if (plot) {
    if (!is.null(data$A1)) {
      cat("\nUse 'printSubset(model)' to get detailed information on the ",
          "different solutions, 'plotPercentiles(model)' to display the ",
          "percentile plot and 'plotSubset(model)' to inspect model fit.", sep = "")
    } else {
      cat("\nConventional norming was applied. Use 'normTable(0, model)' or ",
          "'rawTable(0, model)' to retrieve norm scores. If you would like to ",
          "achieve a closer fit, increase the terms parameter.", sep = "")
    }
  }

  class(bestformula) <- "cnormModel"

  if (plot && bestformula$useAge) {
    tmp <- list(data = data, model = bestformula)
    class(tmp) <- "cnormTemp"
    plotPercentiles(tmp)
  }

  return(bestformula)
}


#' BIC-Weighted Model Averaging Across Consistent Candidate Models
#'
#' Computes a weighted average of the regression coefficients across the
#' consistency-screened candidate models returned by \code{regsubsets} and
#' \code{screenSubset}. Weights are information-theoretic model weights
#' \eqn{w_j \propto exp(-\Delta BIC_j / 2)}, restricted to models within
#' \code{deltaBIC} of the best model. Each candidate is refitted on the
#' complete sample via (weighted) least squares; coefficients of terms not
#' included in a candidate are treated as zero.
#'
#' Because all candidates passed the monotonicity screening (same direction),
#' and the averaging weights are positive and sum to one, the averaged model is
#' itself monotone and thus consistent. In contrast to averaging coefficients
#' over subsamples (deprecated \code{subsample_lm}), this approach targets the
#' actual source of variance - model selection - while each component estimate
#' remains a full-sample least squares fit.
#'
#' @param results A (screened) \code{summary.regsubsets} object, including a
#'   \code{consistent} flag per row.
#' @param data The preprocessed norm data.
#' @param raw Name of the raw score variable.
#' @param weights Optional numeric vector of case weights.
#' @param deltaBIC Only models within this BIC distance of the best candidate
#'   are averaged (default 10; Burnham & Anderson, 2002).
#' @return An \code{lm}-type object with averaged coefficients over the union of
#'   the candidate terms; \code{fitted.values} and \code{residuals} are updated
#'   accordingly.
#' @references Burnham, K. P., & Anderson, D. R. (2002). Model Selection and
#'   Multimodel Inference. Springer.
#' @export
#' @family model
weightedAverageModel <- function(results, data, raw, weights = NULL,
                                 deltaBIC = 10) {
  candidates <- if (!is.null(results$consistent) &&
                    any(results$consistent, na.rm = TRUE)) {
    which(results$consistent)
  } else {
    seq_along(results$bic)
  }

  bic <- results$bic[candidates]
  candidates <- candidates[bic - min(bic) <= deltaBIC]
  bic <- results$bic[candidates]
  wgt <- exp(-0.5 * (bic - min(bic)))
  wgt <- wgt / sum(wgt)

  unionTerms <- colnames(results$outmat)[
    colSums(results$outmat[candidates, , drop = FALSE] == "*") > 0]

  coefMat <- matrix(0, nrow = length(candidates), ncol = length(unionTerms) + 1L,
                    dimnames = list(NULL, c("(Intercept)", unionTerms)))

  for (j in seq_along(candidates)) {
    trms <- colnames(results$outmat)[results$outmat[candidates[j], ] == "*"]
    f <- stats::reformulate(trms, response = raw)
    fitj <- if (is.null(weights)) stats::lm(f, data = data)
    else stats::lm(f, data = data, weights = weights)
    cf <- stats::coef(fitj)
    cf[is.na(cf)] <- 0
    coefMat[j, names(cf)] <- cf
  }

  avg <- as.vector(crossprod(coefMat, wgt))
  names(avg) <- colnames(coefMat)

  fUnion <- stats::reformulate(unionTerms, response = raw)
  final <- if (is.null(weights)) stats::lm(fUnion, data = data)
  else stats::lm(fUnion, data = data, weights = weights)

  X <- stats::model.matrix(final)
  final$coefficients <- avg[colnames(X)]
  final$fitted.values <- as.vector(X %*% final$coefficients)
  final$residuals <- stats::model.response(stats::model.frame(final)) -
    final$fitted.values
  final$averaged <- TRUE
  final$averagingWeights <- wgt
  final
}


#' Print Model Selection Information
#'
#' Displays R^2 and other metrics for models with varying predictors, aiding in
#' choosing the best-fitting model after model fitting. The F-test compares each
#' model to the preceding (smaller) one, with degrees of freedom based on the
#' actual difference in the number of parameters.
#' @param x Model output from 'bestModel' or a cnorm object.
#' @param ... Additional parameters.
#' @return Table with model information criteria.
#' @export
#'
#' @examples
#' \dontrun{
#'   result <- cnorm(raw = elfe$raw, group = elfe$group)
#'   printSubset(result)
#' }
#' @family model
printSubset <- function(x, ...) {
  if (isTaylor(x)) {
    x <- x$model
  }

  rss <- x$subsets$rss
  adjr2 <- x$subsets$adjr2
  nModels <- length(rss)
  nParams <- rowSums(x$subsets$outmat == "*") + 1L   # + intercept
  n <- length(x$fitted.values)

  Fvals <- rep(NA_real_, nModels)
  pvals <- rep(NA_real_, nModels)
  if (nModels > 1L) {
    df1 <- diff(nParams)
    df1[df1 < 1L] <- NA                              # non-nested comparison
    df2 <- n - nParams[-1L]
    Fs <- (-diff(rss) / df1) / (rss[-1L] / df2)
    Fvals[-1L] <- Fs
    pvals[-1L] <- stats::pf(Fs, df1, df2, lower.tail = FALSE)
  }

  table <- data.frame(
    R2adj = adjr2,
    BIC = x$subsets$bic,
    CP = x$subsets$cp,
    RSS = rss,
    RMSE = sqrt(rss / n),
    DeltaR2adj = c(NA, diff(adjr2)),
    F = Fvals,
    p = pvals,
    nr = seq_len(nModels)
  )

  if (!is.null(x$subsets$consistent))
    table$consistent <- x$subsets$consistent

  return(table)
}


#' Check the consistency of the norm data model
#'
#' While abilities increase and decline over age, within one age group, the
#' norm scores always have to show a monotonic increase or decrease with
#' increasing raw scores. Violations of this assumption are an indication for
#' problems in modeling the relationship between raw and norm scores. There are
#' several reasons, why this might occur:
#' \enumerate{
#'   \item Vertical extrapolation: Choosing extreme norm scores, e. g. values
#'   -3 <= x and x >= 3. In order to model these extreme values, a large sample
#'   dataset is necessary.
#'   \item Horizontal extrapolation: Taylor polynomials converge in a certain
#'   radius. Using the model values outside the original dataset may
#'   lead to inconsistent results.
#'   \item The data cannot be modeled with Taylor polynomials, or you need
#'   another power parameter (k) or R2 for the model.
#'  }
#'
#'  In general, extrapolation (point 1 and 2) can carefully be done to a
#'  certain degree outside the original sample, but it should in general
#'  be handled with caution. Please note that at extreme values, the models
#'  most likely become independent and it is thus recommended to restrict the
#'  norm score range to the relevant range of abilities, e.g. +/- 2.5 SD via
#'  the minNorm and maxNorm parameter.
#'
#'  With \code{method = "analytic"} (default), monotonicity within each age is
#'  verified exactly: since the model is polynomial in the norm score L, the
#'  real roots of its derivative are computed via \code{polyroot} and the sign
#'  of the derivative is checked between consecutive roots. This detects even
#'  narrow violations that a coarse norm score grid can miss. Age remains
#'  discretized; violations that are entirely clipped by the raw score bounds
#'  are ignored. \code{method = "grid"} reproduces the classical numerical
#'  check and is used automatically as a fallback for models with non-Taylor
#'  predictors.
#'
#' @param model The model from the bestModel function or a cnorm object
#' @param minAge Age to start with checking
#' @param maxAge Upper end of the age check
#' @param stepAge Stepping parameter for the age check. If NULL, 8 equidistant
#'   age points are used. Lower values indicate higher precision.
#' @param minNorm Lower end of the norm value range
#' @param maxNorm Upper end of the norm value range
#' @param minRaw clipping parameter for the lower bound of raw scores
#' @param maxRaw clipping parameter for the upper bound of raw scores
#' @param stepNorm Stepping parameter for the norm table check within age
#'   (only used with \code{method = "grid"})
#' @param method Monotonicity cgeck. Either "analytic" (default; exact within
#'   age via polynomial derivative roots) or "grid" (numerical check on a norm
#'   score grid)
#' @param warn Retained for backwards compatibility (violations below numerical
#'   tolerance are always suppressed)
#' @param silent turn off messages
#' @param minDip Tolerance for monotonicity check. Allow small violations
#'   (default: .01 or 1% of the raw score range). Decrease e. g. to 1e-6
#'   for strict checking.
#' @return Boolean, indicating model violations (TRUE) or no problems (FALSE)
#' @examples
#' \dontrun{
#'   model <- cnorm(raw = elfe$raw, group = elfe$group, plot = FALSE)
#'   modelViolations <- checkConsistency(model, minNorm = 25, maxNorm = 75)
#'   plotDerivative(model, minNorm = 25, maxNorm = 75)
#' }
#' @export
#' @family model
checkConsistency <- function(model,
                             minAge = NULL,
                             maxAge = NULL,
                             minNorm = NULL,
                             maxNorm = NULL,
                             minRaw = NULL,
                             maxRaw = NULL,
                             stepAge = NULL,
                             stepNorm = 1,
                             method = c("analytic", "grid"),
                             warn = FALSE,
                             silent = FALSE,
                             minDip = .01) {
  if (isTaylor(model)) {
    model <- model$model
  }

  if (!inherits(model, "cnormModel")) {
    stop("Please provide a cnorm model.")
  }

  method <- match.arg(method)

  # fallbacks to model limits
  if (is.null(minAge)) minAge <- model$minA1
  if (is.null(maxAge)) maxAge <- model$maxA1
  if (is.null(minNorm)) minNorm <- model$minL1
  if (is.null(maxNorm)) maxNorm <- model$maxL1
  if (is.null(minRaw)) minRaw <- model$minRaw
  if (is.null(maxRaw)) maxRaw <- model$maxRaw

  descend <- isTRUE(model$descend)

  # age evaluation points (handles conventional norming with minAge == maxAge)
  if (maxAge > minAge) {
    ages <- if (is.null(stepAge)) seq(minAge, maxAge, length.out = 4)
    else seq(minAge, maxAge, by = stepAge)
  } else {
    ages <- minAge
  }

  # analytic check requires pure Taylor coefficient names
  B <- taylorCoefficientMatrix(model$coefficients)
  if (is.null(B) || anyNA(model$coefficients)) method <- "grid"

  if (method == "analytic") {
    tPow <- ncol(B) - 1L
    absDip <- (maxRaw - minRaw) * minDip

    violations <- vapply(ages, function(a) {
      pcoef <- as.vector(B %*% a^(0:tPow))
      polyViolatesMonotonicity(pcoef, minNorm, maxNorm,
                               descend = descend,
                               minRaw = minRaw, maxRaw = maxRaw,
                               minDip = absDip)
    }, logical(1))
  } else {
    norms <- seq(minNorm, maxNorm, by = stepNorm)
    if (length(norms) < 2)
      stop("Range of norm scores too small to check consistency.")

    n_ages <- length(ages)
    n_norms <- length(norms)
    grid_norms <- rep(norms, times = n_ages)
    grid_ages <- rep(ages, each = n_norms)

    raw_preds <- predictRaw(norm = grid_norms, age = grid_ages,
                            coefficients = model$coefficients,
                            minRaw = minRaw, maxRaw = maxRaw)

    pred_mat <- matrix(raw_preds, nrow = n_norms, ncol = n_ages)
    diffs <- pred_mat[-1, , drop = FALSE] - pred_mat[-n_norms, , drop = FALSE]

    tol <- 1e-6
    violations <- if (descend) !apply(diffs <= tol, 2, all)
    else !apply(diffs >= -tol, 2, all)
  }

  results <- round(ages[violations], digits = 1)

  if (length(results) == 0) {
    if (!silent) cat("No relevant violations of model consistency found.\n")
    return(FALSE)
  } else {
    if (!silent) {
      message(paste0(
        "Violations of monotonicity found within the specified range of age ",
        "and norm score at age points: ",
        paste(results, collapse = ", "),
        "\n\nUse 'plotPercentiles' to visually inspect the norm curve or ",
        "'plotDerivative' to identify regions violating the consistency. ",
        "Rerun the modeling with adjusted parameters or restrict the valid ",
        "value range accordingly.\n"))
      cat(rangeCheck(model, minAge, maxAge, minNorm, maxNorm))
      cat("\n")
    }
    return(TRUE)
  }
}


#' Regression function
#'
#' The method builds the regression function for the regression model,
#' including the beta weights.
#' It can be used to predict the raw scores based on age and location.
#' @param model The regression model from the bestModel function or a cnorm object
#' @param raw The name of the raw value variable (default 'raw')
#' @param digits Number of digits for formatting the coefficients
#' @return The regression formula as a string
#'
#' @examples
#' \dontrun{
#'   result <- cnorm(raw = elfe$raw, group = elfe$group)
#'   regressionFunction(result)
#' }
#'
#' @export
#' @family model
regressionFunction <- function(model, raw = NULL, digits = NULL) {
  if (isTaylor(model)) {
    raw <- "raw"
    model <- model$model
  } else if (is.null(raw)) {
    raw <- model$raw
  }

  coefs <- model$coefficients

  if (!is.null(digits)) {
    formatted_coefs <- format(coefs, digits = digits, trim = TRUE,
                              scientific = FALSE)
  } else {
    formatted_coefs <- as.character(coefs)
  }

  intercept <- formatted_coefs[1]

  if (length(coefs) == 1) {
    return(paste(raw, "~", intercept))
  }

  term_strings <- paste0("(", formatted_coefs[-1], "*", names(coefs)[-1], ")")

  formulA <- paste(raw, intercept, sep = " ~ ")
  formulA <- paste(formulA, paste(term_strings, collapse = " + "), sep = " + ")

  return(formulA)
}


#' Derivative of regression model
#'
#' Calculates the derivative of the location / norm value from the regression
#' model with the first derivative as the default. This is useful for finding
#' violations of model assumptions and problematic distribution features as
#' f. e. bottom and ceiling effects, non-progressive norm scores within an age
#' group or in general intersecting percentile curves.
#' @param model The regression model or a cnorm object
#' @param order The degree of the derivative, default: 1
#' @return The derived coefficients
#' @examples
#' \dontrun{
#'   m <- cnorm(raw = elfe$raw, group = elfe$group)
#'   derivedCoefficients <- derive(m, order = 1)
#' }
#' @export
#' @family model
derive <- function(model, order = 1) {
  if (isTaylor(model)) model <- model$model

  coeff <- model$coefficients

  for (o in seq_len(order)) {
    # only terms containing an L power survive derivation w.r.t. L
    coeff <- coeff[grepl("L\\d+", names(coeff))]
    if (length(coeff) == 0) return(coeff)

    nms <- names(coeff)
    lpow <- as.integer(sub("^.*?L(\\d+).*$", "\\1", nms))
    apart <- sub("^L\\d+", "", nms)          # "" or "A<j>"

    newVal <- as.numeric(coeff) * lpow
    newPow <- lpow - 1L
    newNms <- ifelse(newPow == 0L,
                     ifelse(apart == "", "(Intercept)", apart),
                     paste0("L", newPow, apart))

    coeff <- newVal
    names(coeff) <- newNms
  }

  return(coeff)
}


#' Prints the results and regression function of a cnorm model
#'
#' @param object A regression model or cnorm object
#' @param ... additional parameters
#' @return A report on the regression function, weights, R2 and RMSE
#' @export
#' @family model
modelSummary <- function(object, ...) {
  if (isTaylor(object)) {
    object <- object$model
  }
  strat <- c(
    "largest consistent model",
    "first model exceeding R2 > .99",
    "fallback: best / smallest available model",
    "terms specified manually",
    "selection based on R2"
  )

  terms <- length(object$coefficients) - 1
  adj_r_squared <- object$subsets$adjr2[object$ideal.model]
  rmse <- object$rmse
  selection_strategy <- object$selectionStrategy
  highest_consistent <- object$highestConsistent

  cat("cNORM Model Summary\n")
  cat("-------------------\n")
  cat("Number of terms:", terms, "\n")
  cat("Adjusted R-squared:", round(adj_r_squared, 4), "\n")
  cat("RMSE:", round(rmse, 4), "\n")
  cat("Selection strategy:", selection_strategy)
  if (!is.null(selection_strategy) && selection_strategy > 0 &&
      selection_strategy <= length(strat)) {
    cat(",", strat[selection_strategy])
  }
  cat("\nHighest consistent model:",
      if (is.null(highest_consistent)) "none" else highest_consistent, "\n")
  if (isTRUE(object$averaged)) {
    cat("BIC-weighted model averaging was applied across",
        length(object$averagingWeights), "consistent models.\n")
  }
  cat("Raw score variable:", object$raw, "\n")
  cat("Raw score range:", object$minRaw, "to", object$maxRaw, "\n")
  if (isTRUE(object$useAge)) {
    cat("Age range:", object$minA1, "to", object$maxA1, "\n")
  }
  cat("\nRegression function:\n")
  cat(regressionFunction(object), "\n")

  cat(object$report, sep = "\n")
}


#' Check for horizontal and vertical extrapolation
#'
#' Regression models only work in a specific range and extrapolation
#' horizontally (outside the original range) or vertically (extreme norm
#' scores) might lead to inconsistent results. The function generates a
#' message, indicating extrapolation and the range of the original data.
#' @param object The regression model or a cnorm object
#' @param minAge The lower age bound
#' @param maxAge The upper age bound
#' @param minNorm The lower norm value bound
#' @param maxNorm The upper norm value bound
#' @param digits The precision for rounding the norm and age data
#' @param ... additional parameters
#' @return the report
#' @export
#' @examples
#' \dontrun{
#'   m <- cnorm(raw = elfe$raw, group = elfe$group)
#'   rangeCheck(m)
#' }
#' @family model
rangeCheck <- function(object,
                       minAge = NULL,
                       maxAge = NULL,
                       minNorm = NULL,
                       maxNorm = NULL,
                       digits = 3,
                       ...) {
  if (isTaylor(object)) {
    object <- object$model
  }

  summary <- paste0(
    "The original data for the regression model spanned from age ",
    round(object$minA1, digits), " to ", round(object$maxA1, digits),
    ", with a norm score range from ",
    round(object$minL1, digits), " to ", round(object$maxL1, digits),
    ". The raw scores range from ", object$minRaw, " to ", object$maxRaw, ".")

  if (isTRUE(object$descend)) {
    summary <- paste0(summary, " The ranking was done in descending order.")
  }

  reportOnly <- (is.null(minAge) || is.null(maxAge) ||
                   is.null(minNorm) || is.null(maxNorm))

  if (!reportOnly &&
      (minAge < object$minA1 || maxAge > object$maxA1) &&
      (minNorm < object$minL1 || maxNorm > object$maxL1)) {
    summary <- paste(
      "Horizontal and vertical extrapolation detected. Be careful using age groups and extreme norm scores outside the original sample.",
      summary, sep = "\n")
  } else if (!reportOnly && (minAge < object$minA1 || maxAge > object$maxA1)) {
    summary <- paste(
      "Horizontal extrapolation detected. Be careful using age groups outside the original sample.",
      summary, sep = "\n")
  } else if (!reportOnly && (minNorm < object$minL1 || maxNorm > object$maxL1)) {
    summary <- paste(
      "Vertical extrapolation detected. Be careful using extreme norm scores exceeding the scores of the original sample.",
      summary, sep = "\n")
  }

  return(summary)
}


#' Cross-validation for Term Selection in cNORM
#'
#' Assists in determining the optimal number of terms for the regression model
#' using repeated Monte Carlo cross-validation. It leverages an 80-20 split
#' between training and validation data, with stratification by norm group or
#' random sample in case of using sliding window ranking.
#'
#' Successive models, with an increasing number of terms, are evaluated, and the
#' RMSE for raw scores plotted. This encompasses the training, validation, and
#' entire dataset. If `norms` is set to TRUE (default), the function will also
#' calculate the mean norm score reliability and crossfit measures. Note that
#' due to the computational requirements of norm score calculations, execution
#' can be slow, especially with numerous repetitions or terms.
#'
#' When `cv` is set to "full" (default), both test and validation datasets are
#' ranked separately, providing comprehensive cross-validation. For a more
#' streamlined validation process focused only on modeling, a pre-ranked dataset
#' can be used. The output comprises RMSE for raw score models, norm score R^2,
#' delta R^2, crossfit, and the norm score SE according to Oosterhuis, van der
#' Ark, & Sijtsma (2016).
#'
#' This function is not yet prepared for the 'extensive' search strategy,
#' introduced in version 3.3, but instead relies on the first model per number
#' of terms, without consistency check.
#'
#' For assessing overfitting:
#' \deqn{CROSSFIT = R(Training; Model)^2 / R(Validation; Model)^2}
#' A CROSSFIT > 1 suggests overfitting, < 1 suggests potential underfitting, and
#' values around 1 are optimal, given a low raw score RMSE and high norm score
#' validation R^2.
#'
#' Suggestions for ideal model selection:
#' \itemize{
#'   \item Visual inspection of percentiles with `plotPercentiles` or `plotPercentileSeries`.
#'   \item Pair visual inspection with repeated cross-validation (e.g., 10 repetitions).
#'   \item Aim for low raw score RMSE and high norm score R^2, avoiding terms with significant overfit (e.g., crossfit > 1.1).
#' }
#'
#' @param data Data frame of norm sample or a cnorm object. Should have ranking,
#'   powers, and interaction of L and A.
#' @param formula Formula from an existing regression model; min/max functions
#'   ignored. If using a cnorm object, this is automatically fetched.
#' @param repetitions Number of repetitions for cross-validation.
#' @param norms If TRUE, computes norm score crossfit and R^2. Note:
#'   Computationally intensive.
#' @param min Start with a minimum number of terms (default = 4).
#' @param max Maximum terms in model, up to (k + 1) * (t + 1) - 1 (default = 20).
#' @param cv "full" (default) splits data into training/validation, then ranks.
#'   Otherwise, expects a pre-ranked dataset.
#' @param pCutoff Checks stratification for unbalanced data. Performs a t-test
#'   per group. Default set to 0.2 to minimize beta error.
#' @param width If provided, ranking done via `rankBySlidingWindow`. Otherwise,
#'   by group.
#' @param raw Name of the raw score variable.
#' @param age Name of the age variable.
#' @param group Name of the grouping variable.
#' @param weights Name of the weighting parameter.
#'
#' @return Table with results per term number: RMSE for raw scores, R^2 for
#'   norm scores, and crossfit measure.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Plot cross-validation RMSE by number of terms (up to 9) with three repetitions.
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' cnorm.cv(result$data, min = 2, max = 9, repetitions = 3)
#'
#' # Using a cnorm object examines the predefined formula.
#' cnorm.cv(result, repetitions = 1)
#' }
#'
#' @references Oosterhuis, H. E. M., van der Ark, L. A., & Sijtsma, K. (2016).
#'   Sample Size Requirements for Traditional and Regression-Based Norms.
#'   Assessment, 23(2), 191-202. https://doi.org/10.1177/1073191115580638
#' @family model
cnorm.cv <- function(data,
                     formula = NULL,
                     repetitions = 5,
                     norms = TRUE,
                     min = 4,
                     max = 20,
                     cv = "full",
                     pCutoff = NULL,
                     width = NA,
                     raw = NULL,
                     group = NULL,
                     age = NULL,
                     weights = NULL) {

  if (isTaylor(data)) {
    formula <- data$model$terms
    data    <- data$data
  }

  if (is.null(pCutoff)) {
    pCutoff <- if (nrow(data) < 10000) 0.2 else 0.1
  }

  d <- data

  if (is.null(raw)) raw <- attr(d, "raw")
  if (is.null(raw)) stop("Please provide a raw score variable name.")
  if (!(raw %in% colnames(data)))
    stop("The specified raw score variable '", raw,
         "' is not present in the dataset.")

  if (is.null(group)) group <- attr(d, "group")
  if (is.null(age))   age   <- attr(d, "age")

  if (is.na(width) && !is.null(attr(d, "width")))
    width <- attr(d, "width")

  if (is.null(group) || (is.null(age) && is.na(width)))
    stop("Please provide either a grouping variable or age and width.")

  if (is.null(weights)) weights <- attr(d, "weights")

  if (!is.null(weights) && !(weights %in% colnames(data))) {
    warning("Weighting variable not found in dataset. Continuing without weighting.\n")
    weights <- NULL
  } else if (!is.null(weights)) {
    cat("Applying weighting ...\n")
  }

  scaleM  <- attr(d, "scaleMean")
  scaleSD <- attr(d, "scaleSD")
  if (is.null(scaleM)  || is.na(scaleM)  || cv == "full") scaleM  <- 50
  if (is.null(scaleSD) || is.na(scaleSD) || cv == "full") scaleSD <- 10

  k <- attr(d, "k"); if (is.null(k)) k <- 5
  t <- attr(d, "t"); if (is.null(t)) t <- 3

  n.models <- (k + 1) * (t + 1) - 1
  if (is.na(max) || max > n.models || max < 1) max <- n.models

  if (is.null(formula)) {
    lmX <- buildFunction(raw = raw, k = k, t = t, age = TRUE)
  } else {
    lmX <- formula
    n_formula_terms <- length(attr(stats::terms(formula), "term.labels"))
    min <- n_formula_terms
    max <- n_formula_terms
  }

  val.errors      <- rep(0, max)
  train.errors    <- rep(0, max)
  complete.errors <- rep(0, max)
  r2.train        <- rep(0, max)
  r2.test         <- rep(0, max)
  delta           <- rep(NA, max)
  norm.rmse       <- rep(0, max)
  norm.se         <- rep(0, max)
  norm.rmse.min   <- rep(0, max)

  terms_list <- vector("list", repetitions * (max - min + 1L))
  terms_idx  <- 1L

  rankGroup <- TRUE
  if (!is.null(age) && !is.na(width)) {
    cat("Age and width parameters available, switching to rankBySlidingWindow() ...\n")
    rankGroup <- FALSE
  }

  full_formula <- stats::formula(lmX)

  for (a in seq_len(repetitions)) {
    p.value <- 0.01
    n       <- 1L
    train   <- NA
    test    <- NA

    # establish a balanced 80/20 split
    while (p.value < pCutoff) {
      if (n > 100L)
        stop("Could not establish balanced data sets. Try decreasing pCutoff.")
      n <- n + 1L

      if (rankGroup) {
        d  <- d[sample(nrow(d)), ]
        d  <- d[order(d[, group]), ]
        sp <- split(d, list(d[, group]))
        sp <- lapply(sp, function(x) x[sample(nrow(x)), ])

        train <- lapply(sp, function(x) x[c(FALSE, rep(TRUE, 4)), ])
        test  <- lapply(sp, function(x) x[c(TRUE, rep(FALSE, 4)), ])

        p <- vapply(seq_along(train), function(z)
          stats::t.test(train[[z]][, raw], test[[z]][, raw])$p.value, numeric(1))

        p.value <- min(p)
        if (p.value < pCutoff) next

        train <- do.call(rbind, train)
        test  <- do.call(rbind, test)

        if (cv == "full") {
          train <- prepareData(train, raw = raw, group = group, age = age,
                               width = width, weights = weights, silent = TRUE)
          test  <- prepareData(test, raw = raw, group = group, age = age,
                               width = width, weights = weights, silent = TRUE)
        }
      } else {
        d      <- d[sample(nrow(d)), ]
        number <- floor(nrow(d) * 0.8)
        train  <- d[seq_len(number), ]
        test   <- d[(number + 1L):nrow(d), ]

        p.value <- stats::t.test(train[, age], test[, age])$p.value
        if (p.value < pCutoff) next

        train <- rankBySlidingWindow(train, age = age, raw = raw,
                                     weights = weights, width = width,
                                     silent = TRUE)
        test  <- rankBySlidingWindow(test, age = age, raw = raw,
                                     weights = weights, width = width,
                                     silent = TRUE)
        train <- computePowers(train, age = age, k = k, t = t, silent = TRUE)
        test  <- computePowers(test, age = age, k = k, t = t, silent = TRUE)
      }
    }

    subsets <- regsubsets(lmX, data = train, nbest = 1, nvmax = max,
                          really.big = n.models > 25)

    if (norms && is.null(formula)) cat(paste0("Cycle ", a, "\n"))

    # design matrices, built once per repetition
    X_train_full <- stats::model.matrix(full_formula, train)
    X_test_full  <- stats::model.matrix(full_formula, test)
    y_train      <- train[, raw]

    for (i in min:max) {
      variables <- names(coef(subsets, id = i))

      X_train_sub <- X_train_full[, variables, drop = FALSE]
      X_test_sub  <- X_test_full[, variables, drop = FALSE]

      # lm.fit (NOT .lm.fit): returns named, unpivoted coefficients
      # and fitted values
      fit <- stats::lm.fit(X_train_sub, y_train)
      cf <- fit$coefficients
      cf[is.na(cf)] <- 0                      # guard against aliased columns

      test.fitted <- as.vector(X_test_sub %*% cf)

      # lightweight mock object for norm score prediction
      mock_model <- list(
        coefficients = cf,
        k = k,
        minRaw = min(train[, raw], na.rm = TRUE),
        maxRaw = max(train[, raw], na.rm = TRUE),
        scaleM = scaleM,
        scaleSD = scaleSD,
        descend = isTRUE(attr(d, "descend")),
        raw = raw,
        age = age
      )
      class(mock_model) <- "cnormModel"

      terms_list[[terms_idx]] <- variables[variables != "(Intercept)"]
      terms_idx <- terms_idx + 1L

      train.errors[i] <- train.errors[i] +
        sqrt(mean((fit$fitted.values - y_train)^2, na.rm = TRUE))
      val.errors[i]   <- val.errors[i] +
        sqrt(mean((test.fitted - test[, raw])^2, na.rm = TRUE))

      if (norms) {
        train$T <- predictNorm(train[, raw], train[, age], mock_model,
                               min(train$normValue), max(train$normValue),
                               silent = TRUE)
        test$T  <- predictNorm(test[, raw], test[, age], mock_model,
                               min(train$normValue), max(train$normValue),
                               silent = TRUE)

        r2.train[i]  <- r2.train[i] +
          stats::cor(train$normValue, train$T, use = "pairwise.complete.obs")^2
        r2.test[i]   <- r2.test[i] +
          stats::cor(test$normValue, test$T, use = "pairwise.complete.obs")^2
        norm.rmse[i] <- norm.rmse[i] +
          sqrt(mean((test$T - test$normValue)^2, na.rm = TRUE))

        n_valid    <- sum(!is.na(test$T))
        norm.se[i] <- norm.se[i] +
          sqrt(sum((test$T - test$normValue)^2, na.rm = TRUE) /
                 max(n_valid - 2L, 1L))
      }
    }
  }

  norm.rmse.min[1] <- NA
  complete <- regsubsets(lmX, data = d, nbest = 1, nvmax = n.models,
                         really.big = n.models > 25)

  X_full_dataset <- stats::model.matrix(full_formula, d)
  y_full_dataset <- d[, raw]

  for (i in seq_len(max)) {
    variables <- names(coef(complete, id = i))

    fit_complete <- stats::lm.fit(
      X_full_dataset[, variables, drop = FALSE], y_full_dataset)
    complete.errors[i] <- sqrt(mean(
      (fit_complete$fitted.values - y_full_dataset)^2, na.rm = TRUE))

    train.errors[i] <- train.errors[i] / repetitions
    val.errors[i]   <- val.errors[i]   / repetitions

    if (norms) {
      r2.train[i]  <- r2.train[i]  / repetitions
      r2.test[i]   <- r2.test[i]   / repetitions
      norm.rmse[i] <- norm.rmse[i] / repetitions
      norm.se[i]   <- norm.se[i]   / repetitions

      if (i > min) {
        delta[i]         <- r2.test[i] - r2.test[i - 1L]
        norm.rmse.min[i] <- if (norm.rmse[i] > 0)
          norm.rmse[i] - norm.rmse[i - 1L] else NA
      }
    }

    if (i < min) {
      r2.train[i] <- NA
      r2.test[i] <- NA
      val.errors[i] <- NA
      train.errors[i] <- NA
      complete.errors[i] <- NA
      norm.rmse[i] <- NA
    }
    if (i <= min) norm.rmse.min[i] <- NA
  }

  Terms <- unlist(terms_list)

  tab <- data.frame(
    RMSE.raw.train    = train.errors,
    RMSE.raw.test     = val.errors,
    RMSE.raw.complete = complete.errors,
    R2.norm.train     = r2.train,
    R2.norm.test      = r2.test,
    Delta.R2.test     = delta,
    Crossfit          = r2.train / r2.test,
    RMSE.norm.test    = norm.rmse,
    SE.norm.test      = norm.se,
    terms             = seq_len(length(train.errors))
  )

  theme_custom <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  breaks_step_1 <- function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)

  if (is.null(formula)) {
    p1 <- ggplot(tab) + theme_custom +
      geom_line(aes(x = .data$terms, y = .data$RMSE.raw.complete, color = "Complete"), linewidth = .75, na.rm = TRUE) +
      geom_point(aes(x = .data$terms, y = .data$RMSE.raw.complete), size = 2.5, color = "#33aa55", na.rm = TRUE) +
      geom_line(aes(x = .data$terms, y = .data$RMSE.raw.test, color = "Validation"), linewidth = .75, na.rm = TRUE) +
      geom_point(aes(x = .data$terms, y = .data$RMSE.raw.test), size = 2.5, color = "#1f77b4", na.rm = TRUE) +
      geom_line(aes(x = .data$terms, y = .data$RMSE.raw.train, color = "Training"), linewidth = .75, na.rm = TRUE) +
      geom_point(aes(x = .data$terms, y = .data$RMSE.raw.train), size = 2.5, color = "#d62728", na.rm = TRUE) +
      labs(title = "Raw Score RMSE (1)", x = "Number of terms", y = "Root Mean Squared Error") +
      scale_color_manual(values = c("Training" = "#d62728", "Validation" = "#1f77b4", "Complete" = "#33aa55")) +
      scale_x_continuous(breaks = breaks_step_1)
    print(p1)

    if (norms) {
      p2 <- ggplot(tab) + theme_custom +
        geom_line(aes(x = .data$terms, y = .data$R2.norm.test, color = "Validation"), linewidth = .75, na.rm = TRUE) +
        geom_point(aes(x = .data$terms, y = .data$R2.norm.test), size = 2.5, color = "#1f77b4", na.rm = TRUE) +
        geom_line(aes(x = .data$terms, y = .data$R2.norm.train, color = "Training"), linewidth = .75, na.rm = TRUE) +
        geom_point(aes(x = .data$terms, y = .data$R2.norm.train), size = 2.5, color = "#d62728", na.rm = TRUE) +
        labs(title = expression(paste("Norm Score ", R^2, " (2)")), x = "Number of terms", y = expression(R^2)) +
        scale_color_manual(values = c("Training" = "#d62728", "Validation" = "#1f77b4", "Complete" = "#33aa55")) +
        scale_x_continuous(breaks = breaks_step_1)
      print(p2)

      p3 <- ggplot(tab) + theme_custom +
        geom_line(aes(x = .data$terms, y = .data$Crossfit, color = "Crossfit"), linewidth = .75, na.rm = TRUE) +
        geom_point(aes(x = .data$terms, y = .data$Crossfit), size = 2.5, color = "#1f77b4", na.rm = TRUE) +
        geom_hline(aes(yintercept = 1.10, color = "Overfit"), linetype = "dashed", linewidth = 1, na.rm = TRUE) +
        geom_hline(aes(yintercept = 0.90, color = "Underfit"), linetype = "dashed", linewidth = 1, na.rm = TRUE) +
        labs(title = "Norm Score CROSSFIT (3)", x = "Number of terms", y = "Crossfit") +
        scale_color_manual(values = c("Underfit" = "#FF2728", "Crossfit" = "#1f77b4", "Overfit" = "#AA00AA")) +
        scale_x_continuous(breaks = breaks_step_1)
      print(p3)

      p4 <- ggplot(tab) + theme_custom +
        geom_line(aes(x = .data$terms, y = .data$Delta.R2.test, color = "Delta R2"), linewidth = .75, na.rm = TRUE) +
        geom_point(aes(x = .data$terms, y = .data$Delta.R2.test), size = 2.5, color = "#1f77b4", na.rm = TRUE) +
        geom_hline(aes(yintercept = 0.00, color = "Equal R2"), linetype = "dashed", linewidth = 1, na.rm = TRUE) +
        labs(title = expression(paste("Norm Score ", Delta, R^2, " in Validation (4)")), x = "Number of terms", y = "Delta R2") +
        scale_color_manual(values = c("Equal R2" = "#33aa55", "Delta R2" = "#1f77b4")) +
        scale_x_continuous(breaks = breaks_step_1)
      print(p4)
    } else {
      tab$R2.norm.train <- NULL
      tab$R2.norm.test <- NULL
      tab$Delta.R2.test <- NULL
      tab$Crossfit <- NULL
      tab$RMSE.norm.test <- NULL
    }

    cat("\nOccurrence of selected terms, sorted by frequency:\n")
    print(sort(table(Terms), decreasing = TRUE))

    cat("\nThe simulation yielded the following optimal settings:\n")
    if (norms) {
      cat(paste0("\nNumber of terms with best crossfit: ",
                 which.min((1 - tab$Crossfit)^2)))
      best.norm <- which.max(r2.test)
      FirstNegative <- which(tab$Delta.R2.test <= 0)[1]

      cat(paste0("\nNumber of terms with best norm validation R2: ",
                 best.norm, "\n"))
      cat(paste0("First negative norm score R2 delta in validation: ",
                 FirstNegative))
      cat(paste0("\nNumber of terms with best norm validation RMSE: ",
                 which.min(tab$RMSE.norm.test)))

      cat(paste0("\nChoosing a model with ", (FirstNegative - 1),
                 " terms might be a good choice. For this, use the parameter ",
                 "'terms = ", (FirstNegative - 1), "' in the cnorm-function.\n"))
      cat("\nPlease investigate the plots and the summary table, as the results might vary within a narrow range.")
      cat("\nEspecially pay attention to where RMSE.norm.test and delta R2 stop to progress.")
    }

    cat("\n\n")
    return(tab[min:max, ])

  } else {
    cat("\n\n")
    cat(paste0("Repeated cross validation with prespecified formula and ",
               repetitions, " repetitions yielded the following results:\n\n"))
    tab$Delta.R2.test <- NULL
    return(tab[stats::complete.cases(tab), ])
  }
}


#' Calculates the standard error (SE) or root mean square error (RMSE) of the
#' norm scores. In case of large datasets, both results should be almost
#' identical.
#'
#' @param model a cnorm object
#' @param type either '1' for the standard error sensu Oosterhuis et al. (2016)
#'   or '2' for the RMSE (default)
#'
#' @return The standard error (SE) of the norm scores sensu Oosterhuis et al.
#'   (2016) or the RMSE
#' @export
#'
#' @references Oosterhuis, H. E. M., van der Ark, L. A., & Sijtsma, K. (2016).
#'   Sample Size Requirements for Traditional and Regression-Based Norms.
#'   Assessment, 23(2), 191-202. https://doi.org/10.1177/1073191115580638
getNormScoreSE <- function(model, type = 2) {
  if (!isTaylor(model)) {
    stop("Please provide cnorm object as the model parameter")
  }

  if (type != 1 && type != 2) type <- 2

  data <- model$data
  model <- model$model
  minNorm <- model$minL1
  maxNorm <- model$maxL1
  raw <- data[[model$raw]]
  age <- data[[model$age]]

  fitted <- predictNorm(raw, age, model, minNorm = minNorm, maxNorm = maxNorm)

  diff <- fitted - data$normValue
  diff <- diff[!is.na(diff)]

  if (type == 1)
    return(sqrt(sum(diff^2) / (length(diff) - 2)))
  else
    return(sqrt(mean(diff^2)))
}


#' Build regression function for bestModel
#'
#' @param raw name of the raw score variable
#' @param k the power degree for location
#' @param t the power degree for age
#' @param age use age
#'
#' @return regression function
#' @keywords internal
buildFunction <- function(raw, k, t, age) {
  terms <- paste0("L", 1:k)
  if (age) {
    terms <- c(terms, paste0("A", 1:t))
    terms <- c(terms, as.vector(outer(1:k, 1:t, function(i, j)
      paste0("L", i, "A", j))))
  }
  stats::formula(paste(raw, paste(terms, collapse = " + "), sep = " ~ "))
}


# ---------------------------------------------------------------------------
# Internal helpers for analytic monotonicity checks
# ---------------------------------------------------------------------------

#' Parse Taylor coefficients into an (L power x A power) matrix
#'
#' Maps named regression coefficients following the Taylor naming scheme
#' (\code{(Intercept)}, \code{L#}, \code{A#}, \code{L#A#}) onto a matrix
#' \code{B} with \code{B[l + 1, a + 1]} holding the coefficient of
#' \eqn{L^l A^a}. The intercept is deliberately placed at \code{B[1, 1]}:
#' while a plain monotonicity check would be invariant to the constant term,
#' the clipping-aware check in \code{polyViolatesMonotonicity()} operates on
#' the absolute raw score scale, so the intercept determines which reversals
#' fall inside \code{[minRaw, maxRaw]}.
#'
#' Returns \code{NULL} if the coefficient names do not exclusively follow the
#' Taylor naming scheme (e.g. with custom predictors), or if two names map to
#' the same (L, A) power cell (duplicated names, or degenerate aliases such
#' as \code{L0}), which would otherwise be overwritten silently.
#' @keywords internal
#' @noRd
taylorCoefficientMatrix <- function(coefficients) {
  nm <- names(coefficients)
  if (is.null(nm) ||
      !all(grepl("^(\\(Intercept\\)|L\\d+|A\\d+|L\\d+A\\d+)$", nm)))
    return(NULL)

  lp <- integer(length(nm))
  ap <- integer(length(nm))

  hasL <- grepl("L\\d+", nm)
  hasA <- grepl("A\\d+", nm)

  # conversion restricted to matching names -> no NA coercion warnings
  lp[hasL] <- as.integer(sub("^.*?L(\\d+).*$", "\\1", nm[hasL]))
  ap[hasA] <- as.integer(sub("^.*?A(\\d+).*$", "\\1", nm[hasA]))

  # guard against colliding cells (duplicated names, "L0"/"A0" aliases)
  if (anyDuplicated(cbind(lp, ap)) > 0L) return(NULL)

  B <- matrix(0, nrow = max(lp) + 1L, ncol = max(ap) + 1L)
  B[cbind(lp + 1L, ap + 1L)] <- as.numeric(coefficients)
  B
}


#' Evaluate polynomial (ascending coefficients) via Horner scheme
#' @keywords internal
#' @noRd
evalPolynomial <- function(cf, x) {
  r <- rep(0, length(x))
  for (coefI in rev(cf)) r <- r * x + coefI
  r
}


#' Exact, clipping-aware monotonicity violation check
#'
#' Checks whether p(L) violates monotonicity on \code{[minL, maxL]} in the
#' direction implied by \code{descend}. The interval is partitioned at the
#' real roots of p'(L); within each subinterval p is strictly monotone, so
#' \code{|p(a) - p(b)|} is the exact local reversal depth. A wrong-direction
#' segment only counts as a violation if the part of its raw score span that
#' is *visible after clipping* to \code{[minRaw, maxRaw]} (the effective dip)
#' exceeds \code{minDip}. Reversals entirely outside the clipping bounds thus
#' have an effective dip <= 0 and are ignored.
#'
#' @param pcoef Polynomial coefficients in L, ascending order (constant
#'   first). The constant term must be included for the clipping logic to be
#'   meaningful.
#' @param minL,maxL Interval of the latent location L to check.
#' @param descend If \code{TRUE}, p must be non-increasing; otherwise
#'   non-decreasing.
#' @param minRaw,maxRaw Raw score clipping bounds.
#' @param minDip Minimum raw-score reversal depth (in raw score units) for a
#'   sign violation to be considered a genuine, reportable inconsistency.
#'   Defaults to 1\% of the raw score range (intentional tolerance factor);
#'   falls back to an absolute epsilon if the range is unknown/degenerate.
#' @param dTol Numeric tolerance factor for the derivative sign test
#'   (intentional; scaled by the local magnitude of p).
#' @keywords internal
#' @noRd
polyViolatesMonotonicity <- function(pcoef, minL, maxL, descend = FALSE,
                                     minRaw = -Inf, maxRaw = Inf,
                                     minDip = NULL, dTol = 1e-8) {

  deg <- length(pcoef) - 1L
  if (deg < 1L) return(FALSE)

  dcoef <- pcoef[-1L] * seq_len(deg)

  # strip numerically-zero leading terms; threshold is scale-relative so the
  # behaviour does not depend on the raw score / predictor scaling
  zeroTol <- 1e-14 * max(1, abs(dcoef))
  while (length(dcoef) > 0L && abs(dcoef[length(dcoef)]) < zeroTol)
    dcoef <- dcoef[-length(dcoef)]
  if (length(dcoef) == 0L) return(FALSE)   # flat in L -> nothing to violate

  # tolerance factor (intentional): reversals smaller than 1% of the raw
  # score range are treated as practically irrelevant
  if (is.null(minDip)) {
    rawRange <- maxRaw - minRaw
    minDip <- if (is.finite(rawRange) && rawRange > 0) 1e-2 * rawRange else 1e-6
  }

  breaks <- c(minL, maxL)
  if (length(dcoef) > 1L) {
    rts <- polyroot(dcoef)
    re <- Re(rts)[abs(Im(rts)) < 1e-8 * (1 + abs(Re(rts)))]
    re <- re[re > minL & re < maxL]

    # merge roots that are numerically indistinguishable (root-finding
    # jitter); dips hidden by this merging are far below minDip by design
    if (length(re) > 1L) {
      re <- sort(re)
      minSep <- 1e-6 * (maxL - minL)
      keep <- c(TRUE, diff(re) > minSep)
      re <- re[keep]
    }
    breaks <- sort(unique(c(minL, re, maxL)))
  }

  for (s in seq_len(length(breaks) - 1L)) {
    a <- breaks[s]
    b <- breaks[s + 1L]
    dv <- evalPolynomial(dcoef, (a + b) / 2)
    pa <- evalPolynomial(pcoef, a)
    pb <- evalPolynomial(pcoef, b)

    # numeric tolerance on the derivative sign test (intentional factor),
    # scaled by the local magnitude of p
    tol <- dTol * max(1, abs(pa), abs(pb))

    wrong <- if (descend) dv > tol else dv < -tol
    if (wrong) {
      lo <- min(pa, pb)
      hi <- max(pa, pb)
      # clipping-aware: only the part of the reversal visible inside
      # [minRaw, maxRaw] can matter; entirely clipped reversals yield
      # effDip <= 0 and are skipped automatically
      effDip <- min(hi, maxRaw) - max(lo, minRaw)
      if (effDip > minDip) return(TRUE)
    }
  }
  FALSE
}


#' Subset or reorder rows of a summary.regsubsets object
#'
#' \code{keep} may be a logical vector (filtering) or an integer permutation
#' (reordering). Fields absent from the object are skipped.
#' @keywords internal
#' @noRd
filterSubsetRows <- function(results, keep) {
  results$which  <- results$which[keep, , drop = FALSE]
  results$outmat <- results$outmat[keep, , drop = FALSE]
  for (f in c("adjr2", "cp", "bic", "rss", "rsq"))
    if (!is.null(results[[f]])) results[[f]] <- results[[f]][keep]
  results
}


#' Screen `regsubsets` output for monotonic consistency
#'
#' For each model size, candidate models are checked in order of decreasing
#' R^2 (rows are explicitly re-sorted by size and ascending RSS, so this
#' holds regardless of input order). A model is consistent if its prediction
#' is monotone in L, in the direction implied by \code{descend}, across all
#' evaluated ages -- verified analytically in L via
#' \code{polyViolatesMonotonicity()} (the same routine used by
#' \code{checkConsistency(method = "analytic")}), which is clipping-aware
#' (only the reversal depth visible inside \code{[minRaw, maxRaw]} counts)
#' and tolerant of practically negligible reversals via \code{minDip}.
#'
#' Note that the check is exact in L but *sampled* in age: the age grid is
#' automatically densified to at least \code{2 * maxApow + 1} points (where
#' \code{maxApow} is the highest age power in the candidate terms) as a
#' heuristic safeguard against interaction-driven violations between grid
#' points. The final \code{checkConsistency()} call remains the
#' authoritative verdict.
#'
#' Models that do not depend on L at all are treated as
#' degenerate/inconsistent (they cannot serve for norming), as are
#' rank-deficient fits. If no consistent model exists for a size, the best
#' model of that size is retained as fallback (flagged inconsistent). At
#' most one model per size is retained. \code{highestConsistent} reports the
#' *number of terms* of the largest consistent model (not a row index), or
#' is absent if no model is consistent.
#'
#' @param data1 Data frame containing the Taylor basis columns (including
#'   \code{L1} and \code{A1}); its \code{"descend"} attribute supplies the
#'   default direction.
#' @param results A \code{summary.regsubsets} object.
#' @param raw Numeric vector of raw scores (response used in the subset
#'   selection).
#' @param nAgePoints Minimum number of age grid points; automatically raised
#'   to \code{2 * maxApow + 1}.
#' @param weights Optional observation weights (must match \code{raw} in
#'   length); used to refit each candidate via \code{lm.wfit}.
#' @param minRaw,maxRaw Raw score clipping bounds used for the
#'   clipping-awareness of the monotonicity check. Default to the observed
#'   raw score range.
#' @param descend Expected direction of the raw~L relationship. Defaults to
#'   \code{attr(data1, "descend")}; if that attribute is absent, ascending
#'   is assumed.
#' @param minDip Minimum practically relevant raw-score reversal depth passed
#'   through to \code{polyViolatesMonotonicity()}. \code{NULL} (default) lets
#'   that function pick a self-scaling default based on the raw score range.
#' @keywords internal
#' @noRd
screenSubset <- function(data1, results, raw, nAgePoints = 4,
                         weights = NULL, minRaw = NULL, maxRaw = NULL,
                         descend = NULL, minDip = NULL) {
  all_vars <- colnames(results$outmat)
  y <- as.numeric(raw)

  if (!is.null(weights) && length(weights) != length(y))
    stop("'weights' must have the same length as 'raw'.")

  nTerms <- as.integer(rowSums(results$outmat == "*"))

  # the screening logic requires rows sorted by size, best (lowest RSS)
  # model first within each size -- enforce rather than assume
  ord <- order(nTerms, results$rss)
  if (!identical(ord, seq_along(ord))) {
    results <- filterSubsetRows(results, ord)
    nTerms <- nTerms[ord]
  }
  nModels <- length(nTerms)

  # screening requires pure, numeric Taylor terms and the L1/A1 columns
  taylorOnly <- all(grepl("^(L\\d+|A\\d+|L\\d+A\\d+)$", all_vars)) &&
    all(all_vars %in% colnames(data1)) &&
    all(vapply(data1[all_vars], is.numeric, logical(1))) &&
    !is.null(data1$L1) && !is.null(data1$A1)

  if (!taylorOnly) {
    # cannot screen custom predictors: keep the best model per size
    keepFirst <- !duplicated(nTerms)
    results1 <- filterSubsetRows(results, keepFirst)
    results1$consistent <- rep(NA, sum(keepFirst))
    results1$highestConsistent <- NULL
    return(results1)
  }

  X_full <- cbind(`(Intercept)` = 1,
                  as.matrix(data1[, all_vars, drop = FALSE]))
  Lvals <- data1$L1
  Avals <- data1$A1

  # lm.fit()/lm.wfit() cannot handle missing values -- restrict to
  # complete cases (consistently for X, y, weights, and the L/A ranges)
  cc <- stats::complete.cases(X_full, y)
  if (!all(cc)) {
    X_full <- X_full[cc, , drop = FALSE]
    y      <- y[cc]
    Lvals  <- Lvals[cc]
    Avals  <- Avals[cc]
    if (!is.null(weights)) weights <- weights[cc]
  }

  minL <- min(Lvals)
  maxL <- max(Lvals)
  ages <- seq(min(Avals), max(Avals), length.out = nAgePoints)

  if (is.null(minRaw)) minRaw <- min(y)
  if (is.null(maxRaw)) maxRaw <- max(y)
  if (is.null(descend)) descend <- isTRUE(attr(data1, "descend"))

  consistent      <- rep(FALSE, nModels)
  currentNumber   <- 0L
  consistentFound <- FALSE

  for (i in seq_len(nModels)) {
    if (nTerms[i] > currentNumber) {
      currentNumber   <- nTerms[i]
      consistentFound <- FALSE
    }
    if (consistentFound) next

    sel <- c(TRUE, results$outmat[i, ] == "*")
    fit <- if (is.null(weights)) stats::lm.fit(X_full[, sel, drop = FALSE], y)
    else stats::lm.wfit(X_full[, sel, drop = FALSE], y, w = weights)
    cf <- fit$coefficients

    if (anyNA(cf)) next                       # rank deficient -> inconsistent

    B <- taylorCoefficientMatrix(cf)
    if (is.null(B) || nrow(B) < 2L) next      # no L-dependency -> degenerate

    # invariant: the intercept must survive into B[1, 1] -- it anchors the
    # absolute raw score scale on which clipping and minDip operate
    stopifnot(isTRUE(all.equal(unname(B[1L, 1L]),
                               unname(cf[["(Intercept)"]]))))

    tPow <- ncol(B) - 1L
    violated <- FALSE
    for (a in ages) {
      pcoef <- as.vector(B %*% a^(0:tPow))
      if (polyViolatesMonotonicity(pcoef, minL, maxL, descend = descend,
                                   minRaw = minRaw, maxRaw = maxRaw,
                                   minDip = minDip)) {
        violated <- TRUE
        break
      }
    }

    ok <- !violated
    consistent[i]   <- ok
    consistentFound <- ok
  }

  # fallback: if no consistent model exists for a size, keep the best one
  keepFlag <- consistent
  for (term in unique(nTerms)) {
    idx <- which(nTerms == term)
    if (!any(consistent[idx])) keepFlag[idx[1]] <- TRUE
  }

  results1 <- filterSubsetRows(results, keepFlag)
  results1$consistent <- consistent[keepFlag]

  # number of terms of the largest consistent model (not a row position)
  keptTerms <- nTerms[keepFlag]
  results1$highestConsistent <- if (any(results1$consistent))
    max(keptTerms[results1$consistent]) else NULL

  results1
}
