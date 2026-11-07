#' Internal function for retrieving regression function coefficients at specific age
#'
#' The function is an inline for searching zeros in the inverse regression
#' function. It collapses the regression function at a specific age and simplifies
#' the coefficients.
#' @param raw The raw value (subtracted from the intercept)
#' @param age The age
#' @param model The cNORM regression model
#'
#' @return The coefficients
calcPolyInL <- function(raw, age, model) {
  k <- model$k
  coeff <- model$coefficients
  return(calcPolyInLBase2(raw, age, coeff, k))
}


#' Internal function for retrieving regression function coefficients at specific
#' age
#'
#' The function is an inline for searching zeros in the inverse regression
#' function. It collapses the regression function at a specific age and
#' simplifies the coefficients. Optimized version of the prior 'calcPolyInLBase'
#' @param raw The raw value (subtracted from the intercept)
#' @param age The age
#' @param coeff The cNORM regression model coefficients
#' @param k The cNORM regression model power parameter
#'
#' @return The coefficients
calcPolyInLBase2 <- function(raw, age, coeff, k) {
  nam <- names(coeff)
  coeff <- as.numeric(coeff)

  # use regex to identify powers of A
  positionsA <- regexpr("A\\d", nam)
  positionsA[positionsA == -1] <- 0
  powerA <- as.numeric(gsub("A", "", regmatches(nam, positionsA)))
  powerA[is.na(powerA)] <- 0

  # modify coefficients by powers of A
  coeff <- coeff * (age^powerA)

  # use regex to identify powers of L
  positionsL <- rep("", length(nam))
  indices <- grep("^L", nam)
  positionsL[indices] <- substr(nam[indices], start=2, stop=2)
  positionsL[positionsL==""] <- "0"
  positionsL <- as.numeric(positionsL)
  coefficients <- rep(0, k + 1)

  # iterate through coefficients
  for(j in 0:k)
    coefficients[j + 1] <- sum(coeff[positionsL==j])

  coefficients[1] <-  coefficients[1] - raw
  return(coefficients)
}


predictNormByRoots <- function(raw, age, model,
                                minNorm, maxNorm,
                                polynom = NULL,
                                force = TRUE,
                                covariate = NULL) {

  if (!is.null(covariate)) {
    if (is.null(model$coefficients)) {
      stop("Covariate specified, but model does not include covariate")
    }
    # model$coefficients <- simplifyCoefficients(model$coefficients, covariate)
  }

  # Build the polynomial in L (norm) at the requested age.
  if (is.null(polynom)) {
    polynomForPrediction <- calcPolyInLBase2(
      raw   = raw,
      age   = age,
      coeff = model$coefficients,
      k     = model$k
    )
  } else {
    polynomForPrediction    <- polynom
    polynomForPrediction[1] <- polynomForPrediction[1] - raw
  }

  # Guard against degenerate polynomials (e.g. intercept-only models).
  if (length(polynomForPrediction) < 2L ||
      all(polynomForPrediction[-1L] == 0)) {
    return(NA_real_)
  }

  # Real roots only.
  roots  <- polyroot(polynomForPrediction)
  output <- Re(roots[abs(Im(roots)) < 1e-7])

  # Single in-range real root: done.
  if (length(output) == 1L && output >= minNorm && output <= maxNorm) {
    return(output)
  }

  # Direction of search: which side of the model's prediction at scaleM
  # is `raw` on?  IMPORTANT: do NOT clip, otherwise the comparison flips
  # near the clipping boundary.
  raw_at_scaleM <- predictRaw(model$scaleM, age, model$coefficients)

  if (raw > raw_at_scaleM) {
    output <- output[output > model$scaleM & output <= maxNorm]
  } else if (raw < raw_at_scaleM) {
    output <- output[output < model$scaleM & output >= minNorm]
  } else {
    return(model$scaleM)
  }

  if (length(output) == 1L) {
    return(output)
  } else if (length(output) > 1L) {
    # Multiple plausible roots: take the one closest to scaleM.
    return(output[which.min((output - model$scaleM)^2)])
  }

  # ------------------------------------------------------------------
  # Fallback: numerical search.  optimize() always returns a value in
  # [minNorm, maxNorm], so we must judge success by the *objective*,
  # not by the position.
  # ------------------------------------------------------------------
  functionToMinimize <- function(norm) {
    currentRawValue <- predictRaw(norm = norm, age = age,
                                  coefficients = model$coefficients)
    (currentRawValue - raw)^2
  }

  optimum <- optimize(functionToMinimize,
                      lower = minNorm, upper = maxNorm,
                      tol   = .Machine$double.eps^0.5)

  # Tolerance on the residual raw score.  Anything below ~1e-3 is a
  # genuine root for any realistic raw-score scale.
  raw_residual_tol <- 1e-3

  if (sqrt(optimum$objective) <= raw_residual_tol) {
    return(optimum$minimum)
  }

  # The raw score is not reachable on [minNorm, maxNorm] at this age.
  msg <- paste0("No plausible norm score available for raw = ", raw,
                " at age ", age, "; ")

  if (!force) {
    warning(msg, "returning NA.")
    return(NA_real_)
  }

  # force = TRUE: clip to the bound on the side of the distribution
  # that the raw score lies on.
  if (raw > raw_at_scaleM) {
    return(maxNorm)
  } else {
    return(minNorm)
  }
}

