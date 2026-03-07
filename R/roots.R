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


predictNormByRoots <- function(raw, age, model, minNorm, maxNorm, polynom = NULL, force = FALSE, covariate = NULL) {

  if(!is.null(covariate)){
    if(is.null(model$coefficients)){
      stop("Covariate specified, but model does not include covariate")
    }
    #model$coefficients <- simplifyCoefficients(model$coefficients, covariate)
  }

  if (is.null(polynom)) {
    polynomForPrediction <- calcPolyInLBase2(
      raw = raw,
      age = age,
      coeff = model$coefficients,
      k = model$k
    )
  } else {
    polynomForPrediction <- polynom
    polynomForPrediction[1] <- polynomForPrediction[1] - raw
  }

  roots <- polyroot(polynomForPrediction)
  output <- Re(roots[abs(Im(roots)) < 10^(-7)])

  # only one real part as a solution within correct range
  if (length(output) == 1 && output >= minNorm && output <= maxNorm) {
    return(output)
  }

  # not exactly one plausible solution, search for alternative on correct side of distribution
    median <- predictRaw(model$scaleM, age, model$coefficients, minRaw = model$minRaw, maxRaw = model$maxRaw)
    if (raw > median) {
      output <- output[output > model$scaleM & output <= maxNorm]
    } else if (raw < median) {
      output <- output[output < model$scaleM & output >= minNorm]
    } else {
      return(model$scaleM)
    }

    if (length(output) == 1) {
      return(output)
    } else if (length(output) > 1) {
      # fetch the solution closest to median
      # warning(paste0("Multiple roots found for ", raw, " at age ", age, "; returning most plausible norm score."))
      return(output[which.min((output - model$scaleM)^2)])
    } else {
      # nothing worked, apply numerical searching strategy
      startNormScore <- minNorm
      currentRawValue <- predictRaw(norm = minNorm, age = age, coefficients = model$coefficients)

      functionToMinimize <- function(norm) {
        currentRawValue <- predictRaw(norm = norm, age = age, coefficients = model$coefficients)
        functionValue <- (currentRawValue - raw)^2
      }

      optimum <- optimize(functionToMinimize, lower = minNorm, upper = maxNorm, tol = .Machine$double.eps)

      if(optimum$minimum >= minNorm && optimum$minimum <= maxNorm){
        return(optimum$minimum)
      }else if (!force&&(optimum$minimum < minNorm || optimum$minimum > maxNorm)) {
        # everything failed, return NA
        warning(paste0("No plausible norm score available for ", raw, " at age ", age, "; returning NA"))

        return(NA)
      }else if(force && optimum$minimum < minNorm){
        warning(paste0("No plausible norm score available for ", raw, " at age ", age, "; returning lower boundary of the norms."))
        return(minNorm)
      }else if(force && optimum$minimum > maxNorm){
        warning(paste0("No plausible norm score available for ", raw, " at age ", age, "; returning upper boundary of the norms."))
        return(maxNorm)
      }
    }
}

