#' Determine Regression Model
#'
#' Computes Taylor polynomial regression models by evaluating a series of models with increasing predictors.
#' It aims to find a consistent model that effectively captures the variance in the data. It draws on the
#' regsubsets function from the leaps package and builds up to 20 models for each number of predictors, evaluates
#' these models regarding model consistency and selects consistent model with the highest R^2.
#' This automatic model selection should usually be accompanied with visual inspection of the percentile plots
#' and assessment of fit statistics. Set R^2 or number of terms manually to retrieve a more parsimonious model,
#' if desired.
#'
#' The functions \code{rankBySlidingWindow}, \code{rankByGroup}, \code{bestModel},
#' \code{computePowers} and \code{prepareData} are usually not called directly, but accessed
#' through other functions like \code{cnorm}.
#'
#' Additional functions like \code{plotSubset(model)} and \code{cnorm.cv} can aid in model evaluation.
#'
#' @param data Preprocessed dataset with 'raw' scores, powers, interactions, and usually an explanatory variable (like age).
#' @param raw Name of the raw score variable (default: 'raw').
#' @param terms Desired number of terms in the model.
#' @param R2 Adjusted R^2 stopping criterion for model building.
#' @param k Power constant influencing model complexity (default: 4, max: 6).
#' @param t Age power parameter. If unset, defaults to `k`.
#' @param predictors List of predictors or regression formula for model selection. Overrides 'k' and can include additional variables.
#' @param force.in Variables forcibly included in the regression.
#' @param weights Optional case weights. If set to FALSE, default weights (if any) are ignored.
#' @param plot If TRUE (default), displays a percentile plot of the model and information about the
#'             regression object. FALSE turns off plotting and report.
#' @param extensive If TRUE (default), screen models for consistency and - if possible, exclude inconsistent ones
#' @param subsampling If TRUE (default), model coefficients are calculated using 10-folds and averaged across the folds.
#'                    This produces more robust estimates with a slight increase in bias.
#' @return The model. Further exploration can be done using \code{plotSubset(model)} and \code{plotPercentiles(data, model)}.
#' @examples
#'
#' # Example with sample data
#' \dontrun{
#' # It is not recommende to use this function. Rather use 'cnorm' instead.
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
                      subsampling = TRUE) {
  # retrieve attributes
  if (is.null(raw)) {
    raw <- attr(data, "raw")
  }

  if (!is.null(weights)) {
    if (is.numeric(weights) && length(weights) == nrow(data)) {
      data$weights <- weights
      attr(data, "weights") <- "weights"
    } else{
      weights <- NULL
    }
  }

  if (is.null(k)) {
    k <- attr(data, "k")
  } else if (k > attr(data, "k")) {
    warning(
      paste0(
        "k parameter exceeds the power degrees in the dataset. Setting to default of k = ",
        attr(data, "k")
      )
    )
    k <- attr(data, "k")
  }



  if (is.null(t)) {
    if (is.null(attr(data, "t"))) {
      t <- 3
    } else if (!is.null(attr(data, "t"))) {
      t <- attr(data, "t")
    }
  }

  if ((k < 1 || k > 6) & is.null(predictors)) {
    warning(
      "k parameter out of bounds. Please specify a value between 1 and 6. Setting to default = 5."
    )
    k <- 5
  }

  nvmax <- (t + 1) * (k + 1) - 1 + length(predictors)

  # check variable range
  if (!is.null(R2) && (R2 <= 0 || R2 >= 1)) {
    warning("R2 parameter out of bounds. Setting to default R2 = .99")
    R2 <- .99
  }

  if (terms < 0 || terms > nvmax) {
    warning("terms parameter out of bounds. Setting to 5.")
    terms <- 5
  }

  if (!(raw %in% colnames(data)) &&
      (!inherits(predictors, "formula"))) {
    stop(paste(
      c(
        "ERROR: Raw value variable '",
        raw,
        "' does not exist in data object."
      ),
      collapse = ""
    ))
  }

  if ((!is.null(predictors)) &&
      (!inherits(predictors, "formula")) &&
      (!(all(predictors %in% colnames(data))))) {
    stop("ERROR: Missing variables from predictors variable. Please check variable list.")
  }

  # set up regression function
  useAge <- isTRUE(attr(data, "useAge"))
  if (is.null(predictors)) {
    useAge <- attr(data, "useAge")
    lmX <-
      buildFunction(
        raw = raw,
        k = k,
        t = t,
        age = useAge
      )
  } else {
    if (inherits(predictors, "formula")) {
      lmX <- predictors
    } else {
      lmX <-
        formula(paste(raw, paste(predictors, collapse = " + "), sep = " ~ "))
    }
  }

  big <- FALSE
  if (nvmax > 50) {
    big <- TRUE
    if(plot)
      message("The computation might take some time ...")
  }


  if (!is.null(force.in)) {
    c <- strsplit(format(paste0(lmX))[[3]], " \\+ ")
    index <- match(force.in, c[[1]])
  } else {
    index <- NULL
  }


  # rename variable, otherwise it would be automatically used
  if (is.null(weights) && !is.null(data$weights)) {
    data$weights.old <- data$weights
    data$weights <- NULL
  }

  nbest <- 1
  if(extensive && useAge)
    nbest <- 20

  # determine best subset
  if (is.null(weights))
    subsets <-
    regsubsets(
      lmX,
      data = data,
      nbest = nbest,
      nvmax = nvmax,
      force.in = index,
      really.big = big,
      method = "exhaustive"
    )
  else
    subsets <-
    regsubsets(
      lmX,
      data = data,
      nbest = nbest,
      nvmax = nvmax,
      force.in = index,
      really.big = big,
      weights = weights,
      method = "exhaustive"
    )

  results <- summary(subsets)
  if(extensive && useAge){
    results <- screenSubset(data, results, data[, raw], k, t)
    highestConsistent <- results$highestConsistent
  }else
    highestConsistent <- NULL

  # model selection strategy:
  # 1. If no criterion is specified, take largest consistent model, if available
  # 2. else take the first model exceeding R2 > .99
  # 3. else if nothing worked, take model with 5 terms
  # 4. else if terms are specified, take terms
  # 5. Selection based on R2
  selectionStrategy <- 0

  # take highest consistent model
  if (is.null(R2) && (terms == 0)) {
    if(!is.null(results$highestConsistent)){
      i <- results$highestConsistent
      selectionStrategy <- 1
      report <- paste0("Final solution: ", i, " terms (highest consistent model)")
    }
    # no consistent model available, take model with R2 > .99
    else{
      i <- which(results$adjr2 > 0.99)[1]
      selectionStrategy <- 2
      # not available, take last model
      if (is.na(i)) {
        i <- 5
        selectionStrategy <- 3
        report <- paste0("Final solution: ", i, " terms (model with highest R2)")
      }else{
        report <- paste0("Final solution: ", i, " terms (model exceeding R2 > .99)")
      }
    }
  }else if(terms > 0){
    i <- terms
    selectionStrategy <- 4
    report <- paste0("User specified solution: ", i, " terms")
  } else{
    i <- which(results$adjr2 > R2)[1]
    selectionStrategy <- 5
    # not available, take last model
    if (is.na(i)) {
      i <- nvmax
      selectionStrategy <- 3
      report <- paste0("User specified solution: R2 > ", R2, ", but value not reached. Using the highest model instead.")
    }else{
      report <- paste0("User specified solution: R2 > ", R2, " resulting in ", i, " terms")
    }
  }


  report[2] <-
    paste0("R-Square Adj. = ", round(results$adjr2[i], digits = 6))


  # build regression formula
  text <- paste0(raw, " ~ ",
                 paste(colnames(results$outmat)[results$outmat[i,] == "*"],
                       collapse = " + "))

  report[3] <- paste0("Final regression model: ", text)

  # determine final lm object; use resampling if specified
  if(subsampling){
    if (is.null(attr(data, "weights")))
      bestformula <- subsample_lm(text, data, NULL)
    else
      bestformula <- subsample_lm(text, data, weights = data$weights)
  }else{
    if (is.null(attr(data, "weights")))
      bestformula <- lm(text, data)
    else
      bestformula <- lm(text, data, weights = data$weights)
  }


  # compute rmse
  tab <-
    data.frame(raw = data[, raw], fitted = bestformula$fitted.values)
  tab <- tab[complete.cases(tab), ]
  rmse <- sqrt(sum((tab$raw - tab$fitted) ^ 2) / length(tab$raw))

  # Model information
  bestformula$ideal.model <- i
  bestformula$cutoff <- R2
  bestformula$subsets <- results

  # add information for horizontal and vertical extrapolation
  if (attr(data, "useAge")) {
    bestformula$useAge <- TRUE
  } else{
    bestformula$useAge <- FALSE
  }

  # conventional norming
  if (is.null(data$A1)) {
    bestformula$minA1 <- 0
    bestformula$maxA1 <- 0

  }
  # continuous norming
  else{
    bestformula$minA1 <- min(data$A1)
    bestformula$maxA1 <- max(data$A1)
  }

  bestformula$minL1 <- min(data$L1)
  bestformula$maxL1 <- max(data$L1)
  bestformula$minRaw <- min(data[, raw])
  bestformula$maxRaw <- max(data[, raw])
  bestformula$raw <- raw
  bestformula$rmse <- rmse
  bestformula$scaleSD <- attributes(data)$scaleSD
  bestformula$scaleM <- attributes(data)$scaleM
  bestformula$descend <- attributes(data)$descend
  bestformula$group <- attributes(data)$group
  bestformula$age <- attributes(data)$age
  bestformula$k <- attributes(data)$k
  bestformula$A <- attributes(data)$A
  bestformula$highestConsistent <- highestConsistent
  bestformula$selectionStrategy <- selectionStrategy

  # Print output
  report[4] <-
    paste0("Regression function: ",
           regressionFunction(bestformula, digits = 10))
  report[5] <- paste0("Raw Score RMSE = ", round(rmse, digits = 5))
  if (!is.null(weights)) {
    report[6] <-
      paste0(
        "Post stratification was applied. The weights range from ",
        round(min(weights), digits = 3),
        " to ",
        round(max(weights), digits = 3),
        " (m = ",
        round(mean(weights), digits = 3),
        ", sd = ",
        round(sd(weights), digits = 3),
        ")."
      )
  }

  bestformula$report <- report

  if (plot) {
    cat(report, sep = "\n")
  }


  if (anyNA(bestformula$coefficients)) {
    warning(
      "The regression contains missing coefficients. No fitting model could be found. Please try a different number of terms."
    )
  }

  if (terms > 15) {
    message(
      "\nThe model includes a high number of terms. Simpler models are usually more robust. Cross validation with 'cv(model$data)' or an inspection of information functions with 'plot.subset' might help to identify a balanced number of terms. Consider fixing this parameter to a smaller number."
    )
  }

  if(plot){
  if (!is.null(data$A1)) {
    cat(
      "\nUse 'printSubset(model)' to get detailed information on the different solutions, 'plotPercentiles(model) to display percentile plot, plotSubset(model)' to inspect model fit."
    )
  } else{
    cat(
      "\nConventional norming was applied. Use 'normTable(0, model)' or 'rawTable(0, model)' to retrieve norm scores. If you would like to achieve a closer fit, increase the terms parameter."
    )
  }
  }

  class(bestformula) <- "cnormModel"

  if(plot&&bestformula$useAge){
    tmp <- list(data = data, model = bestformula)
    class(tmp) <- "cnormTemp"
    plotPercentiles(tmp)
  }


  return(bestformula)
}


#' Print Model Selection Information
#'
#' Displays R^2 and other metrics for models with varying predictors, aiding in choosing the best-fitting model
#' after model fitting.
#' @param x Model output from 'bestModel' or a cnorm object.
#' @param ... Additional parameters.
#' @return Table with model information criteria.
#' @export
#'
#' @examples
#' # Using cnorm object from sample data
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' printSubset(result)
#' @family model
printSubset <- function(x, ...) {
  if (isTaylor(x)) {
    x <- x$model
  }

  # compute F and significance
  RSS1 <- c(NA, x$subsets$rss)
  RSS2 <- c(x$subsets$rss, NA)
  k1 <- seq(from = 1, to = length(x$subsets$rss) + 1)
  k2 <- seq(from = 2, to = length(x$subsets$rss) + 2)
  df1 <- k2 - k1
  df2 <- length(x$fitted.values) - k2
  F <- ((RSS1 - RSS2) / df1) / (RSS2 / df2)
  p <- 1 - pf(F, df1, df2)
  table <- data.frame(
    R2adj = x$subsets$adjr2,
    BIC = x$subsets$bic,
    CP = x$subsets$cp,
    RSS = x$subsets$rss,
    RMSE = sqrt(x$subsets$rss / length(x$fitted.values)),
    DeltaR2adj = head(c(x$subsets$adjr2, NA) - c(NA, x$subsets$adjr2), -1),
    F = head(F, -1),
    p = head(p, -1),
    nr = seq(1, length(x$subsets$adjr2), by = 1)
  )

  if(!is.null(x$subsets$consistent))
    table$consistent <- x$subsets$consistent

  return(table)
}

#' Check the consistency of the norm data model
#'
#' While abilities increase and decline over age, within one age group, the
#' norm scores always have to show a monotonic increase or decrease with increasing raw
#' scores. Violations of this assumption are an indication for problems
#' in modeling the relationship between raw and norm scores. There are
#' several reasons, why this might occur:
#' \enumerate{
#'   \item Vertical extrapolation: Choosing extreme norm scores, e. g. values
#'   -3 <= x and x >= 3 In order to model these extreme values, a large sample
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
#' @param model The model from the bestModel function or a cnorm object
#' @param minAge Age to start with checking
#' @param maxAge Upper end of the age check
#' @param stepAge Stepping parameter for the age check.
#' values indicate higher precision / closer checks
#' @param minNorm Lower end of the norm value range
#' @param maxNorm Upper end of the norm value range
#' @param minRaw clipping parameter for the lower bound of raw scores
#' @param maxRaw clipping parameter for the upper bound of raw scores
#' @param stepNorm Stepping parameter for the norm table check within age with lower
#' scores indicating a higher precision. The choice depends of the norm scale
#' used. With T scores a stepping parameter of 1 is suitable
#' @param warn If set to TRUE, already minor violations of the model assumptions
#' are displayed (default = FALSE)
#' @param silent turn off messages
#' @return Boolean, indicating model violations (TRUE) or no problems (FALSE)
#' @examples
#' model <- cnorm(raw = elfe$raw, group = elfe$group, plot = FALSE)
#' modelViolations <- checkConsistency(model, minNorm = 25, maxNorm = 75)
#' plotDerivative(model, minNorm = 25, maxNorm = 75)
#'
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
                             warn = FALSE,
                             silent = FALSE) {
  if (isTaylor(model)) {
    model <- model$model
  }

  if (!inherits(model, "cnormModel")) {
    stop("Please provide a cnorm model.")
  }

  if (is.null(minAge)) {
    minAge <- model$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- model$maxA1
  }

  if (is.null(minNorm)) {
    minNorm <- model$minL1
  }

  if (is.null(maxNorm)) {
    maxNorm <- model$maxL1
  }

  if (is.null(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  if (is.null(stepAge)) {
    stepAge <- (maxAge - minAge) / 3
  }

  descend <- model$descend

  i <- minAge
  results <- c()
  norm <- seq(minNorm, maxNorm, by = stepNorm)

  while (i <= maxAge) {
    raw <- predictRaw(norm, rep(i, length(norm)), model$coefficients, minRaw = minRaw, maxRaw = maxRaw)
    correct <- TRUE

    if (descend)
      correct <- all(diff(raw) <= 0)
    else
      correct <- all(diff(raw) >= 0)

    if (!correct) {
      results <- c(results, round(i, digits = 1))
    }

    i <- i + stepAge
  }

  if (length(results) == 0) {
    if (!silent) {
      cat("No relevant violations of model consistency found.\n")
    }
    return(FALSE)
  } else {
    if (!silent) {
      message(
        paste0(
          "Violations of monotonicity found within the specified range of age and norm score at age points: ", paste(results, sep=" "),
          "\n\nUse 'plotPercentiles' to visually inspect the norm curve or 'plotDerivative' to identify regions violating the consistency. Rerun the modeling with adjusted parameters or restrict the valid value range accordingly.\n")
      )

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
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' regressionFunction(result)
#' @export
#' @family model
regressionFunction <- function(model, raw = NULL, digits = NULL) {
  if (isTaylor(model)) {
    raw <- "raw"
    model <- model$model
  } else{
    if (is.null(raw)) {
      raw <- model$raw
    }
  }

  i <- 2
  if (is.null(digits)) {
    formulA <- paste(raw, model$coefficients[[1]], sep = " ~ ")
    while (i <= length(model$coefficients)) {
      formulA <- paste0(formulA,
                        " + (",
                        model$coefficients[[i]],
                        "*",
                        names(model$coefficients[i]),
                        ")")
      i <- i + 1
    }
  } else {
    formulA <-
      paste(raw, format(model$coefficients[[1]], digits = digits), sep = " ~ ")
    while (i <= length(model$coefficients)) {
      formulA <- paste0(
        formulA,
        " + (",
        format(model$coefficients[[i]], digits = digits),
        "*",
        names(model$coefficients[i]),
        ")"
      )
      i <- i + 1
    }
  }
  return(formulA)
}

#' Derivative of regression model
#'
#' Calculates the derivative of the location / norm value from the regression model with the first
#' derivative as the default. This is useful for finding violations of model assumptions and problematic
#' distribution features as f. e. bottom and ceiling effects, non-progressive norm scores within an
#' age group or in general #' intersecting percentile curves.
#' @param model The regression model or a cnorm object
#' @param order The degree of the derivate, default: 1
#' @return The derived coefficients
#' @examples
#' m <- cnorm(raw = elfe$raw, group = elfe$group)
#' derivedCoefficients <- derive(m)
#' @export
#' @family model
derive <- function(model,
                   order = 1) {
  if (isTaylor(model)) {
    model <- model$model
  }


  coeff <- model$coefficients[grep("L", names(model$coefficients))]

  for (o in 1:order) {
    if (o > 1) {
      coeff <- coeff[grep("L", names(coeff))]
    }
    i <- 1
    name <- names(coeff)
    # easy, straight forward derivation of betas and variable names
    while (i <= length(coeff)) {
      nam <- strsplit(name[[i]], "")

      if (nam[[1]][1] == "L") {
        coeff[[i]][1] <- coeff[[i]][1] * as.numeric(nam[[1]][2])
      }
      nam[[1]][2] <- as.numeric(nam[[1]][2]) - 1

      newString <- ""

      if (nchar(name[[i]]) == 2) {
        if (nam[[1]][2] > 0) {
          newString <- paste0(nam[[1]][1], nam[[1]][2])
        }
      } else {
        if (nam[[1]][2] > 0) {
          newString <-
            paste0(nam[[1]][1], nam[[1]][2], nam[[1]][3], nam[[1]][4])
        } else {
          newString <- paste0(nam[[1]][3], nam[[1]][4])
        }
      }

      if(nchar(newString)==0)
        newString <- "Intercept"

      name[[i]] <- newString

      i <- i + 1
    }

    names(coeff) <- name
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
  strat <- c("largest consistent model", "first model exceeding R2 > .99", "fall back to model with 5 terms",
    "terms specified manually", "selection based on R2")
  # Extract relevant information
  terms <- length(object$coefficients) - 1  # Subtract 1 for intercept
  adj_r_squared <- object$subsets$adjr2[object$ideal.model]
  rmse <- object$rmse
  selection_strategy <- object$selectionStrategy
  highest_consistent <- object$highestConsistent

  # Create summary list
  summary_list <- list(
    terms = terms,
    adj_r_squared = adj_r_squared,
    rmse = rmse,
    selection_strategy = selection_strategy,
    highest_consistent = highest_consistent,
    raw_variable = object$raw,
    use_age = object$useAge,
    min_raw = object$minRaw,
    max_raw = object$maxRaw,
    regression_function = regressionFunction(object)
  )

  # Add age-related information if applicable
  if (object$useAge) {
    summary_list$min_age <- object$minA1
    summary_list$max_age <- object$maxA1
  }

  cat("cNORM Model Summary\n")
  cat("-------------------\n")
  cat("Number of terms:", summary_list$terms, "\n")
  cat("Adjusted R-squared:", round(summary_list$adj_r_squared, 4), "\n")
  cat("RMSE:", round(summary_list$rmse, 4), "\n")
  cat("Selection strategy:", summary_list$selection_strategy)
  if(summary_list$selection_strategy > 0 && summary_list$selection_strategy < 6){
    cat(", ", strat[summary_list$selection_strategy])
  }
  cat("\nHighest consistent model:", summary_list$highest_consistent, "\n")
  cat("Raw score variable:", summary_list$raw_variable, "\n")
  cat("Raw score range:", summary_list$min_raw, "to", summary_list$max_raw, "\n")
  if (summary_list$use_age) {
    cat("Age range:", summary_list$min_age, "to", summary_list$max_age, "\n")
  }
  cat("\nRegression function:\n")
  cat(summary_list$regression_function, "\n")

  cat(object$report, sep = "\n")
}

#' Check for horizontal and vertical extrapolation
#'
#' Regression model only work in a specific range and extrapolation horizontally (outside
#' the original range) or vertically (extreme norm scores) might lead to inconsistent
#' results. The function generates a message, indicating extrapolation and the range of the original data.
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
#' m <- cnorm(raw = elfe$raw, group = elfe$group)
#' rangeCheck(m)
#' @family model
rangeCheck <-
  function(object,
           minAge = NULL,
           maxAge = NULL,
           minNorm = NULL,
           maxNorm = NULL,
           digits = 3,
           ...) {
    if (isTaylor(object)) {
      object <- object$model
    }

    summary <-
      paste0(
        "The original data for the regression model spanned from age ",
        round(object$minA1, digits),
        " to ",
        round(object$maxA1, digits),
        ", with a norm score range from ",
        round(object$minL1, digits),
        " to ",
        round(object$maxL1, digits),
        ". The raw scores range from ",
        object$minRaw,
        " to ",
        object$maxRaw,
        "."
      )
    if (object$descend) {
      summary <-
        paste0(summary, " The ranking was done in descending order.")
    }
    reportOnly <-
      (is.null(minAge) ||
         is.null(maxAge) || is.null(minNorm) || is.null(maxNorm))
    if (!reportOnly &&
        (minAge < object$minA1 ||
         maxAge > object$maxA1) &&
        (minNorm < object$minL1 || maxNorm > object$maxL1)) {
      summary <-
        paste(
          "Horizontal and vertical extrapolation detected. Be careful using age groups and extreme norm scores outside the original sample.",
          summary,
          sep = "\n"
        )
    } else if (!reportOnly &&
               (minAge < object$minA1 || maxAge > object$maxA1)) {
      summary <-
        paste(
          "Horizontal extrapolation detected. Be careful using age groups outside the original sample.",
          summary,
          sep = "\n"
        )
    } else if (!reportOnly &&
               (minNorm < object$minL1 || maxNorm > object$maxL1)) {
      summary <-
        paste(
          "Vertical extrapolation detected. Be careful using extreme norm scores exceeding the scores of the original sample.",
          summary,
          sep = "\n"
        )
    }

    return(summary)
  }

#' Cross-validation for Term Selection in cNORM
#'
#' Assists in determining the optimal number of terms for the regression model using repeated Monte Carlo
#' cross-validation. It leverages an 80-20 split between training and validation data, with stratification by norm group
#' or random sample in case of using sliding window ranking.
#'
#' Successive models, with an increasing number of terms, are evaluated, and the RMSE for raw scores plotted. This
#' encompasses the training, validation, and entire dataset. If `norms` is set to TRUE (default), the function will also
#' calculate the mean norm score reliability and crossfit measures. Note that due to the computational requirements
#' of norm score calculations, execution can be slow, especially with numerous repetitions or terms.
#'
#' When `cv` is set to "full" (default), both test and validation datasets are ranked separately, providing comprehensive
#' cross-validation. For a more streamlined validation process focused only on modeling, a pre-ranked dataset can be used.
#' The output comprises RMSE for raw score models, norm score R^2, delta R^2, crossfit, and the norm score SE according
#' to Oosterhuis, van der Ark, & Sijtsma (2016).
#'
#' This function is not yet prepared for the 'extensive' search strategy, introduced in version 3.3, but instead
#' relies on the first model per number of terms, without consistency check.
#'
#' For assessing overfitting:
#' \deqn{CROSSFIT = R(Training; Model)^2 / R(Validation; Model)^2}
#' A CROSSFIT > 1 suggests overfitting, < 1 suggests potential underfitting, and values around 1 are optimal,
#' given a low raw score RMSE and high norm score validation R^2.
#'
#' Suggestions for ideal model selection:
#' \itemize{
#'   \item Visual inspection of percentiles with `plotPercentiles` or `plotPercentileSeries`.
#'   \item Pair visual inspection with repeated cross-validation (e.g., 10 repetitions).
#'   \item Aim for low raw score RMSE and high norm score R^2, avoiding terms with significant overfit (e.g., crossfit > 1.1).
#' }
#'
#' @param data Data frame of norm sample or a cnorm object. Should have ranking, powers, and interaction of L and A.
#' @param formula Formula from an existing regression model; min/max functions ignored. If using a cnorm object, this is automatically fetched.
#' @param repetitions Number of repetitions for cross-validation.
#' @param norms If TRUE, computes norm score crossfit and R^2. Note: Computationally intensive.
#' @param min Start with a minimum number of terms (default = 1).
#' @param max Maximum terms in model, up to (k + 1) * (t + 1) - 1.
#' @param cv "full" (default) splits data into training/validation, then ranks. Otherwise, expects a pre-ranked dataset.
#' @param pCutoff Checks stratification for unbalanced data. Performs a t-test per group. Default set to 0.2 to minimize beta error.
#' @param width If provided, ranking done via `rankBySlidingWindow`. Otherwise, by group.
#' @param raw Name of the raw score variable.
#' @param age Name of the age variable.
#' @param group Name of the grouping variable.
#' @param weights Name of the weighting parameter.
#'
#' @return Table with results per term number: RMSE for raw scores, R^2 for norm scores, and crossfit measure.
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
#' @references Oosterhuis, H. E. M., van der Ark, L. A., & Sijtsma, K. (2016). Sample Size Requirements for Traditional
#' and Regression-Based Norms. Assessment, 23(2), 191–202. https://doi.org/10.1177/1073191115580638
#' @family model
cnorm.cv <- function(data,
                     formula = NULL,
                     repetitions = 5,
                     norms = TRUE,
                     min = 1,
                     max = 12,
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
    # REMOVED: cnorm.model <- data$model (dead code — data is now a data frame)
  }

  if (is.null(pCutoff)) {
    pCutoff <- if (nrow(data) < 10000) .2 else .1   # FIX: <- not =
  }

  d <- data

  if (is.null(raw))
    raw <- attr(d, "raw")

  if (is.null(raw))
    stop("Please provide a raw score variable name.")

  # FIX: data[, raw] throws on missing column; use %in% instead
  if (!is.null(raw) && !(raw %in% colnames(data)))
    stop(paste0("The specified raw score variable '", raw, "' is not present in the dataset."))

  if (is.null(group)) group <- attr(d, "group")
  if (is.null(age))   age   <- attr(d, "age")

  if (is.na(width) && !is.null(attr(d, "width")))
    width <- attr(d, "width")

  if (is.null(group) || (is.null(age) && is.na(width)))
    stop("Please provide either a grouping variable or age and width.")

  if (is.null(weights))
    weights <- attr(d, "weights")

  # FIX: data[, weights] throws on missing column; use %in%; && not &
  if (!is.null(weights) && !(weights %in% colnames(data))) {
    warning("Weighting variable not found in dataset. Continuing without weighting.\n")
    weights <- NULL
  } else if (!is.null(weights)) {
    cat("Applying weighting ...\n")
  }

  # FIX: guard against NULL attribute before is.na() to prevent length-zero condition
  scaleM  <- attr(d, "scaleMean")
  scaleSD <- attr(d, "scaleSD")
  if (is.null(scaleM)  || is.na(scaleM)  || cv == "full") scaleM  <- 50
  if (is.null(scaleSD) || is.na(scaleSD) || cv == "full") scaleSD <- 10

  k <- attr(d, "k"); if (is.null(k)) k <- 5
  t <- attr(d, "t"); if (is.null(t)) t <- 3

  # FIX: correct formula — was (t * k)^2 - 1, which gave 224 for k=5,t=3 instead of 23
  n.models <- (k + 1) * (t + 1) - 1

  if (is.na(max) || max > n.models || max < 1)
    max <- n.models

  if (is.null(formula)) {
    lmX <- buildFunction(raw = raw, k = k, t = t, age = TRUE)
  } else {
    lmX <- formula
    # FIX: length(formula) on a terms object returns 3 (call structure), not the
    # number of predictors; use term.labels attribute instead
    n_formula_terms <- length(attr(formula, "term.labels"))
    min <- n_formula_terms
    max <- n_formula_terms
  }

  val.errors      <- rep(0, max)
  train.errors    <- rep(0, max)
  complete.errors <- rep(0, max)
  r2.train      <- rep(0, max)
  r2.test       <- rep(0, max)
  delta         <- rep(NA, max)
  crossfit      <- rep(0, max)
  norm.rmse     <- rep(0, max)
  norm.se       <- rep(0, max)
  norm.rmse.min <- rep(0, max)

  # FIX: pre-allocate list rather than growing Terms vector in loop
  terms_list <- vector("list", repetitions * (max - min + 1L))
  terms_idx  <- 1L

  rankGroup <- TRUE
  if (!is.null(age) && !is.na(width)) {
    cat("Age and width parameters available, switching to rankBySlidingWindow() ...\n")
    rankGroup <- FALSE
  }

  # FIX: seq_len instead of 1:repetitions
  for (a in seq_len(repetitions)) {
    p.value <- .01
    n       <- 1L
    train   <- NA
    test    <- NA

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
        test  <- lapply(sp, function(x) x[c(TRUE,  rep(FALSE, 4)), ])

        # FIX: seq_along instead of 1:length(train)
        p <- vapply(seq_along(train), function(z)
          t.test(train[[z]][, raw], test[[z]][, raw])$p.value, numeric(1))

        p.value <- min(p)
        if (p.value < pCutoff) next

        train <- do.call(rbind, train)
        test  <- do.call(rbind, test)

        if (cv == "full") {
          train <- prepareData(train, raw = raw, group = group, age = age,
                               width = width, weights = weights, silent = TRUE)
          test  <- prepareData(test,  raw = raw, group = group, age = age,
                               width = width, weights = weights, silent = TRUE)
        }

      } else {
        d      <- d[sample(nrow(d)), ]
        # FIX: explicit floor to avoid fractional row index
        number <- floor(nrow(d) * 0.8)
        train  <- d[seq_len(number), ]
        test   <- d[(number + 1L):nrow(d), ]

        p.value <- t.test(train[, age], test[, age])$p.value
        if (p.value < pCutoff) next

        train <- rankBySlidingWindow(train, age = age, raw = raw,
                                     weights = weights, width = width, silent = TRUE)
        test  <- rankBySlidingWindow(test,  age = age, raw = raw,
                                     weights = weights, width = width, silent = TRUE)
        train <- computePowers(train, age = age, k = k, t = t, silent = TRUE)
      }
    }

    subsets <- regsubsets(lmX, data = train, nbest = 1, nvmax = max,
                          really.big = n.models > 25)

    if (norms && is.null(formula))
      cat(paste0("Cycle ", a, "\n"))

    for (i in min:max) {
      # FIX: variables[-1] is cleaner than variables[2:length(variables)]
      variables <- names(coef(subsets, id = i))[-1]
      reg       <- paste0(raw, " ~ ", paste(variables, collapse = " + "))
      model     <- lm(reg, train)

      test.fitted       <- predict.lm(model, test)
      model$k           <- k
      model$minRaw      <- min(train[, raw])
      model$maxRaw      <- max(train[, raw])
      model$scaleM      <- scaleM
      model$scaleSD     <- scaleSD
      class(model)      <- "cnormModel"

      # FIX: store in pre-allocated list
      terms_list[[terms_idx]] <- attr(model$terms, "term.labels")
      terms_idx <- terms_idx + 1L

      # FIX: accumulate RMSE directly (not MSE) so the final step is plain division
      train.errors[i] <- train.errors[i] +
        sqrt(mean((model$fitted.values - train[, raw])^2, na.rm = TRUE))
      val.errors[i]   <- val.errors[i]   +
        sqrt(mean((test.fitted          - test[, raw])^2,  na.rm = TRUE))

      if (norms) {
        train$T <- predictNorm(train[, raw], train[, age], model,
                               min(train$normValue), max(train$normValue), silent = TRUE)
        test$T  <- predictNorm(test[, raw],  test[, age],  model,
                               min(train$normValue), max(train$normValue), silent = TRUE)

        r2.train[i]  <- r2.train[i]  +
          cor(train$normValue, train$T, use = "pairwise.complete.obs")^2
        r2.test[i]   <- r2.test[i]   +
          cor(test$normValue,  test$T,  use = "pairwise.complete.obs")^2
        norm.rmse[i] <- norm.rmse[i] +
          sqrt(mean((test$T - test$normValue)^2, na.rm = TRUE))

        # FIX: (1) sqrt(sum(e^2)/(n-2)) matches Oosterhuis et al. formula,
        #      not sum(|e|)/(n-2) as in original;
        #      (2) sum(!is.na()) counts valid observations, length(!is.na()) does not
        n_valid    <- sum(!is.na(test$T))
        norm.se[i] <- norm.se[i] +
          sqrt(sum((test$T - test$normValue)^2, na.rm = TRUE) / max(n_valid - 2L, 1L))
      }
    }
  }

  norm.rmse.min[1] <- NA
  complete <- regsubsets(lmX, data = d, nbest = 1, nvmax = n.models,
                         really.big = n.models > 25)

  # FIX: seq_len instead of 1:max
  for (i in seq_len(max)) {
    variables <- names(coef(complete, id = i))[-1]
    reg       <- paste0(raw, " ~ ", paste(variables, collapse = " + "))
    model     <- lm(reg, d)

    complete.errors[i] <- sqrt(mean((model$fitted.values - d[, raw])^2, na.rm = TRUE))

    # FIX: plain division — RMSE was accumulated, not MSE, so no sqrt needed
    train.errors[i] <- train.errors[i] / repetitions
    val.errors[i]   <- val.errors[i]   / repetitions

    if (norms) {
      r2.train[i]  <- r2.train[i]  / repetitions
      r2.test[i]   <- r2.test[i]   / repetitions
      norm.rmse[i] <- norm.rmse[i] / repetitions
      norm.se[i]   <- norm.se[i]   / repetitions

      if (i > min) {
        delta[i]         <- r2.test[i] - r2.test[i - 1L]
        norm.rmse.min[i] <- if (norm.rmse[i] > 0) norm.rmse[i] - norm.rmse[i - 1L] else NA
      }
    }

    if (i < min) {
      r2.train[i] <- NA; r2.test[i] <- NA
      val.errors[i] <- NA; train.errors[i] <- NA; complete.errors[i] <- NA
      norm.rmse[i] <- NA
    }
    if (i <= min) norm.rmse.min[i] <- NA
  }

  # FIX: unlist pre-allocated list
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

  breaks_step_1 <- function(x) {
    seq(floor(min(x)), ceiling(max(x)), by = 1)
  }

  if (is.null(formula)) {
    p1 <- ggplot(tab) + theme_custom
    p1 <- p1 +
      geom_line(aes(x = .data$terms, y = .data$RMSE.raw.complete, color = "Complete"), linewidth = .75, na.rm = TRUE) +
      geom_point(aes(x = .data$terms, y = .data$RMSE.raw.complete), size = 2.5, color = "#33aa55", na.rm = TRUE) +
      geom_line(aes(x = .data$terms, y = .data$RMSE.raw.test, color = "Validation"), linewidth = .75, na.rm = TRUE) +
      geom_point(aes(x = .data$terms, y = .data$RMSE.raw.test), size = 2.5, color = "#1f77b4", na.rm = TRUE) +
      geom_line(aes(x = .data$terms, y = .data$RMSE.raw.train, color = "Training"), linewidth = .75, na.rm = TRUE) +
      geom_point(aes(x = .data$terms, y = .data$RMSE.raw.train), size = 2.5, color = "#d62728", na.rm = TRUE) +
      labs(title = "Raw Score RMSE (1)",
           x = "Number of terms",
           y = "Root Mean Squared Error") +
      scale_color_manual(values = c("Training" = "#d62728", "Validation" = "#1f77b4", "Complete" = "#33aa55")) +
      scale_x_continuous(breaks = breaks_step_1)
    print(p1)


    if (norms) {
      p2 <- ggplot(tab) + theme_custom +
        geom_line(aes(x = .data$terms, y = .data$R2.norm.test, color = "Validation"), linewidth = .75, na.rm = TRUE) +
        geom_point(aes(x = .data$terms, y = .data$R2.norm.test), size = 2.5, color = "#1f77b4", na.rm = TRUE) +
        geom_line(aes(x = .data$terms, y = .data$R2.norm.train, color = "Training"), linewidth = .75, na.rm = TRUE) +
        geom_point(aes(x = .data$terms, y = .data$R2.norm.train), size = 2.5, color = "#d62728", na.rm = TRUE) +
        labs(title = expression(paste("Norm Score ", R^2 , " (2)")),
             x = "Number of terms",
             y = expression(R^2)) +
        scale_color_manual(values = c("Training" = "#d62728", "Validation" = "#1f77b4", "Complete" = "#33aa55")) +
        scale_x_continuous(breaks = breaks_step_1)
      print(p2)

      p3 <- ggplot(tab) + theme_custom +
        geom_line(aes(x = .data$terms, y = .data$Crossfit, color = "Crossfit"), linewidth = .75, na.rm = TRUE) +
        geom_point(aes(x = .data$terms, y = .data$Crossfit), size = 2.5, color = "#1f77b4", na.rm = TRUE) +
        geom_hline(aes(yintercept = 1.10, color = "Overfit"), linetype = "dashed", linewidth = 1, na.rm = TRUE) +
        geom_hline(aes(yintercept = 0.90, color = "Underfit"), linetype = "dashed", linewidth = 1, na.rm = TRUE) +
        labs(title = "Norm Score CROSSFIT (3)",
             x = "Number of terms",
             y = "Crossfit") +
        scale_color_manual(values = c("Underfit" = "#FF2728", "Crossfit" = "#1f77b4", "Overfit" = "#AA00AA")) +
        scale_x_continuous(breaks = breaks_step_1)

      print(p3)



      # plot delta r2 test
      p4 <- ggplot(tab) + theme_custom +
        geom_line(aes(x = .data$terms, y = .data$Delta.R2.test, color = "Delta R2"), linewidth = .75, na.rm = TRUE) +
        geom_point(aes(x = .data$terms, y = .data$Delta.R2.test), size = 2.5, color = "#1f77b4", na.rm = TRUE) +
        geom_hline(aes(yintercept = 0.00, color = "Equal R2"), linetype = "dashed", linewidth = 1, na.rm = TRUE) +
        labs(title = expression(paste("Norm Score ", Delta, R^2 , " in Validation (4)")),
             x = "Number of terms",
             y = "Delta R2") +
        scale_color_manual(values = c("Equal R2" = "#33aa55", "Delta R2" = "#1f77b4")) +
        scale_x_continuous(breaks = breaks_step_1)

      print(p4)

    } else{
      tab$R2.norm.train <- NULL
      tab$R2.norm.test <- NULL
      tab$Delta.R2.test <- NULL
      tab$Crossfit <- NULL
      tab$RMSE.norm.test <- NULL
    }

    cat("\n")
    cat("Occurance of selected terms, sorted by frequency:\n")
    print(sort(table(Terms), decreasing = T))

    cat("\n")
    cat("The simulation yielded the following optimal settings:\n")
    if (norms) {
      cat(paste0("\nNumber of terms with best crossfit: ", which.min((
        1 - tab$Crossfit
      ) ^ 2)))
    }

    if (norms) {
      best.norm <- which.max(r2.test)
      FirstNegative <- which(tab$Delta.R2.test <= 0)[1]

      cat(paste0(
        "\nNumber of terms with best norm validation R2: ",
        best.norm,
        "\n"
      ))

      cat(paste0(
        "First negative norm score R2 delta in validation: ",
        FirstNegative
      ))

      cat(paste0(
        "\nNumber of terms with best norm validation RMSE: ",
        which.min(tab$RMSE.norm.test)
      ))
      cat(
        paste0(
          "\nChoosing a model with ",
          (FirstNegative - 1),
          " terms might be a good choice. For this, use the parameter 'terms = ",
          (FirstNegative - 1),
          "' in the cnorm-function.\n"
        )
      )
      cat(
        "\nPlease investigate the plots and the summary table, as the results might vary within a narrow range."
      )
      cat(
        "\nEspecially pay attention to RMSE.norm.test delta R2 stops to progress."
      )
    }

    cat("\n")
    cat("\n")
    return(tab[min:max, ])
  } else{
    cat("\n")
    cat("\n")

    cat(
      paste0(
        "Repeated cross validation with prespecified formula and ",
        repetitions,
        " repetitions yielded the following results:\n"
      )
    )
    cat("\n")
    tab$Delta.R2.test <- NULL
    return(tab[complete.cases(tab), ])
  }
}




#' Calculates the standard error (SE) or root mean square error (RMSE) of the norm scores
#' In case of large datasets, both results should be almost identical
#'
#' @param model a cnorm object
#' @param type either '1' for the standard error senso Oosterhuis et al. (2016) or '2' for
#'             the RMSE (default)
#'
#' @return The standard error (SE) of the norm scores sensu Oosterhuis et al. (2016) or the RMSE
#' @export
#'
#' @references Oosterhuis, H. E. M., van der Ark, L. A., & Sijtsma, K. (2016). Sample Size Requirements for Traditional and Regression-Based Norms. Assessment, 23(2), 191–202. https://doi.org/10.1177/1073191115580638
getNormScoreSE <- function(model, type = 2) {
  if (!isTaylor(model)) {
    stop("Please provide cnorm object as the model parameter")
  }

  if (type != 1 && type != 2) {
    type <- 2
  }

  data <- model$data
  model <- model$model
  minNorm <- model$minL1
  maxNorm <- model$maxL1
  d <- data
  raw <- data[[model$raw]]
  age <- data[[model$age]]

  d$fitted <-
    predictNorm(
      raw,
      age,
      model,
      minNorm = minNorm,
      maxNorm = maxNorm
    )

  diff <- d$fitted - data$normValue
  diff <- diff[!is.na(diff)]

  #return(sqrt(mean(diff^2)))
  if (type == 1)
    return(sqrt(sum(diff ^ 2) / (length(diff) - 2)))
  else
    return(sqrt(mean(diff ^ 2)))
}



#' Build regression function for bestModel
#'
#' @param raw name of the raw score variable
#' @param k the power degree for location
#' @param t the power degree for age
#' @param age use age
#'
#' @return regression function
buildFunction <- function(raw, k, t, age) {
  terms <- paste0("L", 1:k)
  if (age) {
    terms <- c(terms, paste0("A", 1:t))
    terms <- c(terms, as.vector(outer(1:k, 1:t, function(i, j) paste0("L", i, "A", j))))
  }
  formula(paste(raw, paste(terms, collapse = " + "), sep = " ~ "))
}




#' Check Monotonicity of Predicted Values
#'
#' This function checks if the predicted values from a linear model are
#' monotonically increasing or decreasing across a range of L values for
#' multiple age points.
#'
#' @param lm_model An object of class 'lm' representing the fitted linear model.
#' @param pred_data Matrix with prediction values
#' @param minRaw lowest raw score in prediction
#' @param maxRaw highest raw score in prediction
#'
#' @return A named character vector where each element corresponds to an age point.
#'         Possible values for each element are 1 for "Monotonically increasing"
#'         -1 for "Monotonically decreasing", or 0 for "Not monotonic".
#'
#' @details The function creates a prediction data frame using all combinations
#'          of the provided L values and age points. It then generates predictions
#'          using the provided linear model and checks if these predictions are
#'          monotonically increasing or decreasing for each age point across the
#'          range of L values.
#'
#'
check_monotonicity <- function(lm_model, pred_data, minRaw, maxRaw) {
  n_ages  <- length(unique(pred_data$A))
  n_norms <- nrow(pred_data) / n_ages

  predictions <- predict(lm_model, newdata = pred_data)
  predictions  <- pmax(pmin(predictions, maxRaw), minRaw)

  pred_matrix <- matrix(predictions, nrow = n_norms, ncol = n_ages)

  results <- sapply(seq_len(n_ages), function(col) {
    d <- diff(pred_matrix[, col])
    if (all(d >= 0)) 1L else if (all(d <= 0)) -1L else 0L
  })

  length(unique(results)) == 1 && results[1] != 0
}

predictionMatrix <- function(minL, maxL, minA, maxA, k, t){
  # Create a data frame for predictions
  pred_data <- expand.grid(L = seq(from = minL, to = maxL, length.out=50), A = c(minA, maxA))

  for (i in 1:k) {
    pred_data[paste0("L", i)] <- pred_data$L^i
  }
  for (j in 1:t) {
    pred_data[paste0("A", j)] <- pred_data$A^j
  }
  for (i in 1:k) {
    for (j in 1:t) {
      pred_data[paste0("L", i, "A", j)] <- pred_data[paste0("L", i)] * pred_data[paste0("A", j)]
    }
  }

  return(pred_data)
}

screenSubset <- function(data1, results, raw, k, t){
  minRaw <- min(data1$raw)
  maxRaw <- max(data1$raw)

  # Create a data frame for predictions
  pred_data <- predictionMatrix(min(data1$L1), max(data1$L1), min(data1$A1), max(data1$A1), k, t)

  # prepare variables
  nTerms <- as.numeric(apply(results$outmat, 1, function(row) sum(row == '*', na.rm = TRUE)))
  consistent <- rep(FALSE, length(nTerms))
  norms <- seq(from = min(data1$L1), to = max(data1$L1), length.out = 50)
  age <- c(min(data1$A1), min(data1$A1) + (max(data1$A1)-min(data1$A1))/2, max(data1$A1))
  currentNumber <- 0

  # Loop through each possible model to screen consistency
  for(i in 1:length(nTerms)){
    if(nTerms[i]>currentNumber){
      currentNumber <- nTerms[[i]]
      consistentFound <- FALSE
    }

    if(!consistentFound){
      text <- paste0("raw ~ ",
                     paste(colnames(results$outmat)[results$outmat[i,] == "*"],
                           collapse = " + "))
      linear.model <- lm(text, data = data1)
      consistentFound <- check_monotonicity(linear.model, pred_data, minRaw, maxRaw)
      consistent[i] <- consistentFound
    }
  }



  # set first occurence of model per term to true, if no consistent one found
  df_modified <- data.frame(terms = nTerms, consistent = consistent, R2 = results$adjr2)
  df <- df_modified
  df_sorted <- df[order(df$R2, decreasing = TRUE), ]
  df_sorted <- df_sorted[df$consistent, ]

  # Loop through each unique term
  unique_terms <- unique(nTerms)
  for (term in unique_terms) {
    # Get indices for the current term
    term_indices <- which(df$terms == term)

    # Check if there are any TRUE values for this term
    if (!any(df$consistent[term_indices])) {
      # If no TRUE values, set the first occurrence to TRUE
      df_modified$consistent[term_indices[1]] <- TRUE
    }
  }

  consistent <- df_modified$consistent
  results1 <- results
  results1$consistent <- df$consistent[consistent]
  results1$which <- results1$which[consistent,]
  results1$outmat <- results1$outmat[consistent,]
  results1$adjr2 <- results1$adjr2[consistent]
  results1$cp <- results1$cp[consistent]
  results1$bic <- results1$bic[consistent]
  results1$rss <- results1$rss[consistent]
  results1$rsq <- results1$rsq[consistent]

  highestConsistent <- NULL

  for(i in 1:(length(results1$consistent))){
    if(results1$consistent[i]){
      highestConsistent <- i
    }
  }

  results1$highestConsistent <- highestConsistent
  return(results1)
}

#' K-fold Resampled Coefficient Estimation for Linear Regression
#'
#' @description
#' Performs k-fold resampling to estimate averaged coefficients for linear regression.
#' The coefficients are averaged across k different subsets of the data to provide
#' more stable estimates. For small samples (n < 100), returns a standard linear model instead.
#'
#' @param text A character string or formula specifying the model to be fitted
#' @param data A data frame containing the variables in the model
#' @param weights Optional numeric vector of weights. If NULL, unweighted regression is performed
#' @param k Integer specifying the number of resampling folds (default = 10)
#'
#' @return An object of class 'lm' with averaged coefficients from k-fold resampling.
#' For small samples, returns a standard lm object.
#'
#' @details
#' The function splits the data into k subsets, fits a linear model on k-1 subsets,
#' and stores the coefficients. This process is repeated k times, and the final
#' coefficients are averaged across all iterations to provide more stable estimates.
subsample_lm <- function(text, data, weights, k = 10) {

  # Save current random seed state to get reproducible results
  if (exists(".Random.seed", envir = .GlobalEnv)) {
    old_seed <- .Random.seed
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
  }else{
    on.exit(rm(".Random.seed", envir = .GlobalEnv))
  }

  set.seed(123)

  # in case of very small samples, just return linear model
  if(nrow(data)<100){
    if(is.null(weights)){
      return(lm(text, data))
    }else{
      return(lm(text, data, weights = weights))
    }
  }

  formula <- formula(text)

  # Set up k-fold CV
  folds <- sample(rep(1:k, length.out = nrow(data)))

  # Store coefficients from each fold
  coef_matrix <- matrix(NA, nrow = k,
                        ncol = length(attr(terms(formula), "term.labels")) + 1)
  colnames(coef_matrix) <- c("(Intercept)",
                             attr(terms(formula), "term.labels"))

  # Perform k-fold CV
  for(i in 1:k) {
    # Split data and fit the model
    fold_idx <- which(folds == i)
    train_data <- data[-fold_idx, ]

    if(is.null(weights)){
      fit <- lm(formula, data = train_data)
    }else{
      train_weights <- weights[-folds[[i]]]
      fit <- lm(formula, data = train_data, weights = train_weights)
    }

    # Store coefficients
    coef_matrix[i,] <- coef(fit)
  }

  # Calculate final coefficients (mean across folds)
  final_coef <- colMeans(coef_matrix)

  # Create final model with averaged coefficients
  final_model <- lm(formula, data = data)
  final_model$coefficients <- final_coef
  final_model$fitted.values <- model.matrix(final_model) %*% final_coef
  final_model$residuals <- final_model$model[[1]] - final_model$fitted.values

  return(final_model)
}
