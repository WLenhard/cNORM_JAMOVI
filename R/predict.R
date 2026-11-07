#' Computes the curve for a specific T value
#'
#' As with this continuous norming regression approach, raw scores are modeled as a function of age and norm score
#' (location), getNormCurve is a straightforward approach to show the raw score development over
#' age, while keeping the norm value constant. This way, e. g. academic performance or intelligence development
#' of a specific ability is shown.
#' @param norm The specific norm score, e. g. T value
#' @param model The model from the regression modeling obtained with the cnorm function
#' @param minAge Age to start from
#' @param maxAge Age to stop at
#' @param step Stepping parameter for the precision when retrieving of the values, lower
#' values indicate higher precision (default 0.1).
#' @param minRaw lower bound of the range of raw scores (default = 0)
#' @param maxRaw upper bound of raw scores
#' @return data.frame of the variables raw, age and norm
#' @examples
#' \dontrun{
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#' getNormCurve(35, cnorm.elfe)
#' }
#'
#' @family predict
#' @export
getNormCurve <-
  function(norm,
           model,
           minAge = NULL,
           maxAge = NULL,
           step = 0.1,
           minRaw = NULL,
           maxRaw = NULL) {
    if (isTaylor(model)) {
      model <- model$model
    }

    if (!inherits(model, "cnorm") &&
        !inherits(model, "cnormModel")) {
      stop("Please provide a cnorm object.")
    }

    if (is.null(minAge)) {
      minAge <- model$minA1
    }

    if (is.null(maxAge)) {
      maxAge <- model$maxA1
    }

    if (is.null(minRaw)) {
      minRaw <- model$minRaw
    }

    if (is.null(maxRaw)) {
      maxRaw <- model$maxRaw
    }

    ages <- seq(minAge, maxAge, by = step)
    results <- lapply(ages, function(age) {
      r <- predictRaw(norm, age, model$coefficients, minRaw, maxRaw)
      data.frame(norm = paste(norm, "T"),
                 age = age,
                 raw = r)
    })
    curve <- do.call(rbind, results)

    return(curve)
  }


#' Predict raw values
#'
#' Most elementary function to predict raw score based on Location (L, T score),
#' Age (grouping variable) and the coefficients from a regression model.
#'
#' @param norm The norm score, e. g. a specific T score or a vector of scores
#' @param age The age value or a vector of scores
#' @param coefficients The a cnorm object or the coefficients from the regression model
#' @param minRaw Minimum score for the results; can be used for clipping unrealistic outcomes,
#' usually set to the lower bound of the range of values of the test (default: 0)
#' @param maxRaw Maximum score for the results; can be used for clipping unrealistic outcomes
#' usually set to the upper bound of the range of values of the test
#' @return the predicted raw score or a data.frame of scores in case, lists of norm scores or age is used
#' @examples
#' \dontrun{
#' # Prediction of single scores
#' model <- cnorm(raw = elfe$raw, group = elfe$group)
#' predictRaw(35, 3.5, model)
#' }
#'
#' @family predict
#' @export
predictRaw <-
  function(norm,
           age,
           coefficients,
           minRaw = -Inf,
           maxRaw = Inf) {
    if (inherits(coefficients, "cnorm")) {
      coef <- coefficients$model$coefficients
    } else if (inherits(coefficients, "cnormModel")) {
      coef <- coefficients$coefficients
    } else {
      coef <- coefficients
    }

    if (length(norm) != length(age)) {
      if (length(norm) == 1L)
        norm <- rep_len(norm, length(age))
      else if (length(age) == 1L)
        age  <- rep_len(age, length(norm))
      else
        stop("`norm` and `age` must have the same length, ",
             "or one of them must be of length 1.")
    }

    # Robust extraction of L (k) and A (t) max powers
    L_names <- grep("^L[0-9]+", names(coef), value = TRUE)
    A_names <- grep("A[0-9]+$", names(coef), value = TRUE)

    k <- if (length(L_names) > 0L)
      max(as.integer(sub("^L([0-9]+).*", "\\1", L_names)))
    else
      0L
    t <- if (length(A_names) > 0L)
      max(as.integer(sub(".*A([0-9]+)$", "\\1", A_names)))
    else
      0L

    # Belt-and-braces guard against -Inf
    k <- if (is.finite(k))
      k
    else
      0
    t <- if (is.finite(t))
      t
    else
      0

    # Prepare the matrix for new data
    X_new <- prepare_matrix(norm, age, k, t)

    # Get all variable names from the model
    all_vars <- c("Intercept", colnames(X_new))

    # Identify which variables are in the model
    model_vars <- names(coef)
    model_vars <- gsub("^.*Intercept.*$", "Intercept", model_vars)

    # Create a matrix with all possible variables, filled with zeros
    X_new_full <- matrix(0, nrow = nrow(X_new), ncol = length(all_vars))
    colnames(X_new_full) <- all_vars

    # Fill in the values for the variables that are present in X_new
    X_new_full[, colnames(X_new)] <- X_new

    # Add the intercept column
    X_new_full[, "Intercept"] <- 1

    # Subset X_new_full to include only the variables that are in the model.
    # drop = FALSE keeps it a matrix even when only one column survives.
    X_new_selected <- X_new_full[, model_vars, drop = FALSE]

    # Make predictions
    predictions <- as.vector(X_new_selected %*% coef)
    predictions[predictions < minRaw] <- minRaw
    predictions[predictions > maxRaw] <- maxRaw

    return(predictions)
  }


#' Create a norm table based on model for specific age
#'
#' This function generates a norm table for a specific age based on the regression
#' model by assigning raw scores to norm scores. Please specify the
#' range of norm scores you want to cover. For Taylor polynomial models, specifying a
#' range of T = 25 to T = 75 covers 98.4% of the population.
#' For parametric models (beta-binomial, CMP, shash), norm tables are computed directly
#' from the distribution functions.
#' In case a confidence coefficient (CI, default .9) and reliability is specified,
#' confidence intervals are computed for the true score estimates, including a correction for
#' regression to the mean (Eid & Schmidt, 2012, p. 272).
#'
#' @param A the age as single value or a vector of age values
#' @param model The regression model from cnorm, cnorm.betabinomial, cnorm.cmp, or cnorm.shash
#' @param minNorm The lower bound of the norm score range (Taylor models)
#' @param maxNorm The upper bound of the norm score range (Taylor models)
#' @param minRaw clipping parameter for the lower bound of raw scores (or start score for CMP/shash)
#' @param maxRaw clipping parameter for the upper bound of raw scores (or end score for CMP/shash)
#' @param start Minimum raw score value for the norm table (default: observed minimum)
#' @param end Maximum raw score value for the norm table (default: observed maximum)
#' @param step Stepping parameter with lower values indicating higher precision (for CMP: integer step size)
#' @param monotonuous corrects for decreasing norm scores in case of model inconsistencies (default)
#' @param CI confidence coefficient, ranging from 0 to 1, default .9
#' @param reliability coefficient, ranging between 0 to 1
#' @param pretty Format table by collapsing intervals and rounding to meaningful precision
#' @param ... Additional arguments passed to model-specific norm table methods (e.g., \code{mid_p} for CMP)
#' @return either data.frame with norm scores, predicted raw scores and percentiles in case of simple A
#' value or a list of norm tables if vector of A values was provided
#' @seealso rawTable
#' @references Eid, M. & Schmidt, K. (2012). Testtheorie und Testkonstruktion. Hogrefe.
#' @family predict
#' @export
normTable.default <- function(A,
                      model,
                      minNorm = NULL,
                      maxNorm = NULL,
                      minRaw = NULL,
                      maxRaw = NULL,
                      start = NULL,
                      end = NULL,
                      step = NULL,
                      monotonuous = TRUE,
                      CI = .9,
                      reliability = NULL,
                      pretty = TRUE,
                      ...) {

  # Harmonize start/end and minRaw/maxRaw across all model families
  if (!is.null(start) && is.null(minRaw)) minRaw <- start
  if (!is.null(end) && is.null(maxRaw))   maxRaw <- end
  if (!is.null(minRaw) && is.null(start)) start <- minRaw
  if (!is.null(maxRaw) && is.null(end))   end <- maxRaw

  if (isBeta(model)) {
    return(normTable.betabinomial(model, A, CI = CI, reliability = reliability))
  } else if (isSHASH(model)) {
    return(normTable.shash(
      model,
      A,
      start = start,
      end = end,
      step = if (is.null(step)) 1 else step,
      CI = CI,
      reliability = reliability
    ))
  } else if (isCMP(model)) {
    # For count data, step is strictly an integer (default 1)
    res <- normTable.cmp(
      model,
      ages = A,
      start = start,
      end = end,
      step = if (is.null(step) || step < 1) 1L else as.integer(round(step)),
      CI = CI,
      reliability = reliability,
      ...
    )
    if (length(A) == 1L && is.list(res) && !is.data.frame(res)) {
      return(res[[1L]])
    } else {
      return(res)
    }
  } else if (isTaylor(model)) {
    model <- model$model
  } else if (!inherits(model, "cnormModel")) {
    stop("Please provide a valid cnorm model object.")
  }

  if (model$useAge && !is.numeric(A)) {
    stop("Please specify age.")
  }

  if (is.null(model)) {
    stop("No model specified")
  }

  if (is.null(CI) || is.na(CI)) {
    reliability <- NULL
  } else if (CI > .99999 || CI < .00001) {
    stop("Confidence coefficient (CI) out of range. Please specify value between 0 and 1.")
  }

  rel <- FALSE
  if (!is.null(reliability)) {
    if (reliability > .9999 || reliability < .0001) {
      stop("Reliability coefficient out of range. Please specify value between 0 and 1.")
    } else {
      se  <- qnorm(1 - ((1 - CI) / 2)) * sqrt(reliability * (1 - reliability))
      rel <- TRUE
    }
  }

  if (is.null(minNorm) || is.na(minNorm)) {
    minNorm <- model$scaleM - 2.5 * model$scaleSD
  }

  if (is.null(maxNorm) || is.na(maxNorm)) {
    maxNorm <- model$scaleM + 2.5 * model$scaleSD
  }

  if (is.null(minNorm) || is.null(maxNorm)) {
    stop("Please specify minNorm and maxNorm.")
  }

  if (is.null(step) || is.na(step)) {
    step <- model$scaleSD / 10
  }

  descend <- model$descend

  if (is.null(minRaw) || is.na(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw) || is.na(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  tables <- vector("list", length(A))

  for (x in seq_along(A)) {
    maxn <- maxNorm
    minn <- minNorm
    n_steps <- floor((maxn - minn) / step) + 1L
    norm <- vector("list", n_steps)
    raw  <- vector("list", n_steps)
    i <- 1L

    while (i <= n_steps) {
      r <- predictRaw(minn,
                      A[[x]],
                      model$coefficients,
                      minRaw = minRaw,
                      maxRaw = maxRaw)

      norm[[i]] <- minn
      raw[[i]]  <- r

      minn <- minn + step
      i    <- i + 1L
    }

    normTable <- do.call(rbind, Map(data.frame, norm = norm, raw = raw))

    if (!is.na(model$scaleM) && !is.na(model$scaleSD)) {
      normTable$percentile <- pnorm((normTable$norm - model$scaleM) / model$scaleSD) * 100
    }

    if (monotonuous) {
      normTable$raw <- enforce_monotone_middle(normTable$raw, descend = isTRUE(model$descend))
    }

    if (rel) {
      zPredicted <- reliability * (normTable$norm - model$scaleM) / model$scaleSD
      normTable$lowerCI    <- (zPredicted - se) * model$scaleSD + model$scaleM
      normTable$upperCI    <- (zPredicted + se) * model$scaleSD + model$scaleM
      normTable$lowerCI_PR <- pnorm(zPredicted - se) * 100
      normTable$upperCI_PR <- pnorm(zPredicted + se) * 100
    }

    if (pretty)
      tables[[x]] <- prettyPrint(normTable)
    else
      tables[[x]] <- normTable
  }

  names(tables) <- A

  if (length(A) == 1) {
    return(tables[[1]])
  } else {
    return(tables)
  }
}


#' Create a table with norm scores assigned to raw scores for a specific age based on the regression model
#'
#' This function is comparable to 'normTable', despite it reverses the assignment:
#' A table with raw scores and the according norm scores for a specific age based on the regression
#' model is generated. This way, the inverse function of the regression model is solved numerically with
#' brute force. Please specify the range of raw values, you want to cover. With higher precision
#' and smaller stepping, this function becomes computational intensive.
#' In case a confidence coefficient (CI, default .9) and the reliability is specified,
#' confidence intervals are computed for the true score estimates, including a correction for
#' regression to the mean (Eid & Schmidt, 2012, p. 272).
#' @param A the age, either single value or vector with age values
#' @param model The regression model or a cnorm object
#' @param minRaw The lower bound of the raw score range
#' @param maxRaw The upper bound of the raw score range
#' @param minNorm Clipping parameter for the lower bound of norm scores (default 25)
#' @param maxNorm Clipping parameter for the upper bound of norm scores (default 25)
#' @param step Stepping parameter for the raw scores (default 1)
#' @param monotonuous corrects for decreasing norm scores in case of model inconsistencies (default)
#' @param CI confidence coefficient, ranging from 0 to 1, default .9
#' @param reliability coefficient, ranging between  0 to 1
#' @param pretty Format table by collapsing intervals and rounding to meaningful precision
#' @return either data.frame with raw scores and the predicted norm scores in case of simple A value or a list
#' of norm tables if vector of A values was provided
#' @seealso normTable
#' @references Eid, M. & Schmidt, K. (2012). Testtheorie und Testkonstruktion. Hogrefe.
#' @examples
#' \dontrun{
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#' # generate a norm table for the raw value range from 0 to 28 for the time point month 7 of grade 3
#' table <- rawTable(3 + 7 / 12, cnorm.elfe, minRaw = 0, maxRaw = 28)
#'
#' # generate several raw tables
#' table <- rawTable(c(2.5, 3.5, 4.5), cnorm.elfe, minRaw = 0, maxRaw = 28)
#'
#' # additionally compute confidence intervals
#' table <- rawTable(c(2.5, 3.5, 4.5), cnorm.elfe, minRaw = 0, maxRaw = 28, CI = .9, reliability = .94)
#'
#' # conventional norming, set age to arbitrary value
#' model <- cnorm(raw = elfe$raw)
#' rawTable(0, model)
#' }
#'
#' @family predict
#' @export
rawTable <- function(A,
                     model,
                     minRaw = NULL,
                     maxRaw = NULL,
                     minNorm = NULL,
                     maxNorm = NULL,
                     step = 1,
                     monotonuous = TRUE,
                     CI = .9,
                     reliability = NULL,
                     pretty = TRUE) {
  if (isParametric(model)) {
    stop("rawTable() is only applicable for Taylor polynomial models. ",
         "For parametric models (Beta-Binomial, CMP, SHASH), use normTable() instead, ",
         "which directly tabulates raw scores to norm scores.")
  }

  if (isTaylor(model)) {
    model <- model$model
  } else if (!inherits(model, "cnormModel")) {
    stop("Please provide a cnorm object.")
  }

  if (model$useAge && !is.numeric(A)) {
    stop("Please specify age.")
  }

  if (is.null(step) || is.na(step)) {
    step <- 1
  }

  if (is.null(model)) {
    stop("No model specified")
  }

  if (is.null(minNorm) || is.na(minNorm)) {
    minNorm <- model$minL1
  }

  if (is.null(maxNorm) || is.na(maxNorm)) {
    maxNorm <- model$maxL1
  }

  if (is.null(minRaw) || is.na(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw) || is.na(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  if (is.null(CI) || is.na(CI)) {
    reliability <- NULL
  } else if (CI > .99999 || CI < .00001) {
    stop("Confidence coefficient (CI) out of range. Please specify value between 0 and 1.")
  }

  rel <- FALSE
  if (!is.null(reliability)) {
    if (reliability > .9999 || reliability < .0001) {
      stop("Reliability coefficient out of range. Please specify value between 0 and 1.")
    } else {
      se  <- qnorm(1 - ((1 - CI) / 2)) * sqrt(reliability * (1 - reliability))
      rel <- TRUE
    }
  }

  tables <- vector("list", length(A))

  for (x in seq_along(A)) {
    maxr <- maxRaw
    minr <- minRaw
    n_steps <- floor((maxr - minr) / step) + 1L
    norm <- vector("list", n_steps)
    raw  <- vector("list", n_steps)
    i <- 0L
    while (minr <= maxr) {
      i <- i + 1L
      n <- predictNorm(minr, A[[x]], model, minNorm, maxNorm)
      norm[[i]] <- n
      raw[[i]]  <- minr

      minr <- minr + step
    }

    table <- do.call(rbind, Map(data.frame, raw = raw, norm = norm))

    if (!is.na(model$scaleM) && !is.na(model$scaleSD)) {
      table$percentile <- pnorm((table$norm - model$scaleM) / model$scaleSD) * 100
    }

    if (model$descend) {
      table <- table[order(table$raw, decreasing = TRUE), ]
    }

    if (monotonuous) {
      table$norm <- enforce_monotone_middle(table$norm, descend = FALSE)
      if (!is.null(table$percentile)) {
        table$percentile <- pnorm((table$norm - model$scaleM) / model$scaleSD) * 100
      }
    }

    if (rel) {
      zPredicted <- reliability * (table$norm - model$scaleM) / model$scaleSD
      table$lowerCI    <- (zPredicted - se) * model$scaleSD + model$scaleM
      table$upperCI    <- (zPredicted + se) * model$scaleSD + model$scaleM
      table$lowerCI_PR <- pnorm(zPredicted - se) * 100
      table$upperCI_PR <- pnorm(zPredicted + se) * 100
    }

    if (pretty)
      tables[[x]] <- prettyPrint(table)
    else
      tables[[x]] <- table
  }

  names(tables) <- A

  if (length(A) == 1) {
    return(tables[[1]])
  } else {
    return(tables)
  }
}


#' Create a table based on first order derivative of the regression model for specific age
#'
#' In order to check model assumptions, a table of the first order derivative of the model
#' coefficients is created.
#' @param A the age
#' @param model The regression model or a cnorm object
#' @param minNorm The lower bound of the norm value range
#' @param maxNorm The upper bound of the norm value range
#' @param step Stepping parameter with lower values indicating higher precision
#' @return data.frame with norm scores and the predicted scores based on the
#' derived regression function
#' @seealso plotDerivative, derive
#' @examples
#' \dontrun{
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # retrieve function for time point 6
#' d <- derivationTable(6, cnorm.elfe, step = 0.5)
#' }
#'
#' @family predict
#' @export
derivationTable <-
  function(A,
           model,
           minNorm = NULL,
           maxNorm = NULL,
           step = 0.1) {
    if (isTaylor(model)) {
      model <- model$model
    } else if (!inherits(model, "cnormModel")) {
      stop("Please provide a cnorm object.")
    }

    if (is.null(minNorm)) {
      minNorm <- model$minL1
    }

    if (is.null(maxNorm)) {
      maxNorm <- model$maxL1
    }

    n_steps <- floor((maxNorm - minNorm) / step) + 1L
    norm <- vector("list", n_steps)
    raw  <- vector("list", n_steps)
    i <- 0L
    coeff <- derive(model)
    while (minNorm <= maxNorm) {
      i <- i + 1L
      r <- predictRaw(minNorm, A, coeff)

      norm[[i]] <- minNorm
      raw[[i]]  <- r

      minNorm <- minNorm + step
    }
    normTable <- do.call(rbind, Map(data.frame, norm = norm, raw = raw))
    return(normTable)
  }


#' Retrieve norm value for raw score at a specific age
#'
#' This functions numerically determines the norm score for raw scores depending on the
#' level of the explanatory variable A, e. g. norm scores for raw scores at given ages.
#' @param raw The raw value, either single numeric or numeric vector
#' @param A the explanatory variable (e. g. age), either single numeric or numeric vector
#' @param model The regression model or a cnorm object
#' @param minNorm The lower bound of the norm score range
#' @param maxNorm The upper bound of the norm score range
#' @param force Try to resolve missing norm scores in case of inconsistent models
#' @param silent set to TRUE to suppress messages
#' @return The predicted norm score for a raw score, either single value or vector
#' @examples
#' \dontrun{
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # return norm value for raw value 21 for grade 2, month 9
#' specificNormValue <- predictNorm(raw = 21, A = 2.75, cnorm.elfe)
#' }
#'
#' @family predict
#' @export
predictNorm <-
  function(raw,
           A,
           model,
           minNorm = NULL,
           maxNorm = NULL,
           force = TRUE,
           silent = FALSE) {
    if (!inherits(model, "cnormModel") && !inherits(model, "cnorm")) {
      stop("Please provide a cnorm object.")
    }

    if (length(raw) == 1 && is.na(raw)) {
      return(rep(NA, times = length(A)))
    } else if (!is.numeric(raw)) {
      stop("Please provide a single numeric value or a numeric vector for the raw score.")
    }

    if (length(A) == 1 && is.na(A)) {
      return(rep(NA, times = length(raw)))
    } else if (!is.numeric(A)) {
      stop("Please provide a single numeric value or a numeric vector for A.")
    }

    if (length(A) > 1 &&
        length(raw) > 1 && length(raw) != length(A)) {
      stop("A and raw need to have the same length.")
    }

    if (length(A) == 0 || length(raw) == 0) {
      return(NULL)
    }

    if (isTaylor(model)) {
      if (is.null(minNorm)) {
        minNorm <- attributes(model$data)$scaleMean - (attributes(model$data)$scaleSD * 2.5)
      }

      if (is.null(maxNorm)) {
        maxNorm <- attributes(model$data)$scaleMean + (attributes(model$data)$scaleSD * 2.5)
      }

      model <- model$model
    } else if (inherits(model, "cnormModel")) {
      if (is.null(minNorm)) {
        minNorm <- attributes(model)$scaleMean - (attributes(model)$scaleSD * 2.5)
      }

      if (is.null(maxNorm)) {
        maxNorm <- attributes(model)$scaleMean + (attributes(model)$scaleSD * 2.5)
      }
    } else {
      if (is.null(minNorm) || is.null(maxNorm)) {
        stop("ERROR: Please specify minimum and maximum norm score")
      }
    }

    if (length(raw) == 1 && length(A) == 1) {
      return(predictNormByRoots(raw, A, model, minNorm, maxNorm, force = force))
    } else if (length(raw) > 1 || length(A) > 1) {
      if (length(raw) == 1) {
        raw <- rep(raw, length(A))
      } else if (length(A) == 1) {
        A <- rep(A, length(raw))
      }

      # Hash for de-duplication of redundant lookups
      hash <- paste0(raw, "_", A)

      normTable <- na.omit(data.frame(A = A, raw = raw, hash = hash))
      normTable <- normTable[!duplicated(normTable[, "hash"]), ]

      if (nrow(normTable) > 500 && !silent) {
        cat("Retrieving norm scores, please stand by ...\n")
      }
      raw2 <- normTable$raw
      A2   <- normTable$A
      n    <- length(raw2)
      values <- rep(NA_real_, n)

      for (i in seq_len(n)) {
        v <- predictNormByRoots(raw2[[i]], A2[[i]], model, minNorm, maxNorm, force = force)
        if (length(v) == 0) {
          v <- NA
        }
        values[[i]] <- v
      }

      values <- values[match(hash, normTable$hash)]
      return(values)
    } else {
      stop(
        "Please check raw and A value. Both have to be either single values or vectors of the same length."
      )
    }
  }


#' Format raw and norm tables
#'
#' Internal helper. Takes a raw or norm table, condenses repeated intervals at
#' the bottom and top, and rounds numeric columns to a meaningful precision.
#'
#' @param table the table to format
#' @return formatted table
#' @noRd
prettyPrint <- function(table) {
  if (colnames(table)[1] == "raw")
    tab <- table[match(unique(table$norm), table$norm), ]
  else
    tab <- table[match(unique(table$raw), table$raw), ]

  if (nrow(tab) == 1)
    return(tab)

  for (i in seq_len(nrow(tab))) {
    x <- which(tab[i, 2] == table[, 2])
    if (length(x) > 1) {
      label <- paste0(table[x[1], 1], " - ", table[x[length(x)], 1])
      tab[i, 1] <- label
    }
  }

  if (colnames(table)[1] == "raw")
    tab$norm <- round(tab$norm, digits = 2)
  else
    tab$raw <- round(tab$raw, digits = 2)

  tab$percentile <- round(tab$percentile, digits = 1)

  if (!is.null(tab$lowerCI_PR))
    tab$lowerCI_PR <- round(tab$lowerCI_PR, 1)

  if (!is.null(tab$upperCI_PR))
    tab$upperCI_PR <- round(tab$upperCI_PR, 1)

  if (!is.null(tab$lowerCI))
    tab$lowerCI <- round(tab$lowerCI, 2)

  if (!is.null(tab$upperCI))
    tab$upperCI <- round(tab$upperCI, 2)

  return(tab)
}


#' Internal helper to enforce monotonicity outward from the middle,
#' robust to NA values.
#'
#' @param v numerical vector
#' @param descend logical. If `FALSE` (default) the result is monotone
#'   non-decreasing; if `TRUE`, monotone non-increasing.
#' @return the input vector with monotonicity enforced outward from the
#'   median; `NA`s are preserved in place.
#' @noRd
enforce_monotone_middle <- function(v, descend = FALSE) {
  v <- as.numeric(v)
  n <- length(v)
  if (n < 2L)
    return(v)

  # Anchor at the middle of the *non-NA* observations
  non_na_idx <- which(!is.na(v))
  if (length(non_na_idx) < 2L)
    return(v)
  mid <- non_na_idx[(length(non_na_idx) + 1L) %/% 2L]

  # NA-safe running extrema
  running <- function(x, fun) {
    out <- x
    cur <- NA_real_
    for (i in seq_along(x)) {
      if (!is.na(x[i])) {
        cur <- if (is.na(cur))
          x[i]
        else
          fun(cur, x[i])
        out[i] <- cur
      }
    }
    out
  }

  if (!descend) {
    v[mid:n] <- running(v[mid:n], max)
    if (mid > 1L)
      v[1:mid] <- rev(running(rev(v[1:mid]), min))
  } else {
    v[mid:n] <- running(v[mid:n], min)
    if (mid > 1L)
      v[1:mid] <- rev(running(rev(v[1:mid]), max))
  }
  v
}
