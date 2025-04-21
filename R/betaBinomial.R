#' Calculate the negative log-likelihood for a beta binomial regression model
#'
#' This function computes the negative log-likelihood for a beta binomial regression model
#' where both the mean and standard deviation are modeled as functions of predictors.
#'
#' @param params A numeric vector containing all model parameters. The first n_beta elements
#'               are coefficients for the mean model, and the remaining elements are
#'               coefficients for the log-standard deviation model.
#' @param X A matrix of predictors for the mean model.
#' @param Z A matrix of predictors for the log-standard deviation model.
#' @param y A numeric vector of response values.
#' @param weights A numeric vector of weights for each observation.
#'
#' @return The negative log-likelihood of the model.
#' @keywords internal
log_likelihood <- function(params, X, Z, y, weights) {
  n_beta <- ncol(X)
  beta <- params[1:n_beta]
  gamma <- params[(n_beta + 1):length(params)]

  mu <- X %*% beta
  log_sigma <- Z %*% gamma
  sigma <- exp(log_sigma)

  ll <- sum(weights * dnorm(y, mean = mu, sd = sigma, log = TRUE))
  return(-ll)  # Return negative log-likelihood for minimization
}


#' Fit a beta binomial regression model
#'
#' This function fits a beta binomial regression model where both the mean and
#' standard deviation of the response variable are modeled as polynomial functions
#' of the predictor variable. While 'cnorm-betabinomial2' fits a beta-binomial model
#' on the basis of \eqn{\gamma} and \eqn{\beta} of a beta binomial function, this function
#' fits \eqn{\mu} and \eqn{\sigma}, which are then used to estimate the beta binomial distribution
#' parameters.
#'
#' @param age A numeric vector of predictor values (e.g., age).
#' @param score A numeric vector of response values.
#' @param weights A numeric vector of weights for each observation. Default is NULL (equal weights).
#' @param mu Integer specifying the degree of the polynomial for the mean model. Default is 2.
#' @param sigma Integer specifying the degree of the polynomial for the standard deviation model. Default is 1.
#' @param control A list of control parameters to be passed to the `optim` function.
#'   If NULL, default values are used.
#' @param n Number of items in the test, resp. maximum score to be achieved
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as
#' well, be provided f. e. c(10, 3) for Wechsler scale index points
#' @param plot Logical indicating whether to plot the model. Default is TRUE.
#'
#' @return A list of class "cnormBetaBinomial" containing:
#'   \item{beta_est}{Estimated coefficients for the mean model}
#'   \item{gamma_est}{Estimated coefficients for the log-standard deviation model}
#'   \item{se}{Standard errors of the estimated coefficients}
#'   \item{mu}{Degree of the polynomial for the mean model}
#'   \item{sigma}{Degree of the polynomial for the standard deviation model}
#'   \item{result}{Full result from the optimization procedure}
#'
#' @details
#' The function standardizes the input variables, fits polynomial models for both
#' the mean and standard deviation, and uses maximum likelihood estimation to
#' find the optimal parameters. The optimization is performed using the BFGS method.
#'
#' @keywords internal
cnorm.betabinomial1 <- function(age,
                                score,
                                n = NULL,
                                weights = NULL,
                                mu = 3,
                                sigma = 3,
                                control = NULL,
                                scale = "T",
                                plot = T) {
  # If weights are not provided, use equal weights
  if (is.null(weights)) {
    weights <- rep(1, length(age))
  } else if (length(weights) != length(age)) {
    stop("Length of weights must match length of age and score")
  }

  # Standardize inputs
  age_std <- standardize(age)
  score_std <- standardize(score)

  # Set up 'data' object containing both variables
  data <- data.frame(age = age_std, score = score_std)

  # Prepare the data matrices for mu and sigma, including intercept
  X <- cbind(1, poly(data$age, degree = mu, raw = TRUE))
  Z <- cbind(1, poly(data$age, degree = sigma, raw = TRUE))
  y <- data$score

  # Initial parameters: use some sensible starting values
  initial_params <- c(mean(y), rep(0, mu), log(sd(y)), rep(0, sigma))

  # Optimize to find parameter estimates. If control is NULL, set default
  if (is.null(control))
    control = list(reltol = 1e-8, maxit = 1000)

  result <- optim(
    initial_params,
    log_likelihood,
    X = X,
    Z = Z,
    y = y,
    weights = weights,
    method = "BFGS",
    hessian = TRUE,
    control = control
  )

  # Extract results and calculate standard errors
  beta_est <- result$par[1:(mu + 1)]
  gamma_est <- result$par[(mu + 2):length(result$par)]
  se <- sqrt(diag(solve(result$hessian)))

  # Store original mean and sd for unstandardizing later
  # add attributes for usage in other functions
  scaleM <- NA
  scaleSD <- NA

  # descriptives
  if ((typeof(scale) == "double" && length(scale) == 2)) {
    scaleM <- scale[1]
    scaleSD <- scale[2]
  } else if (scale == "IQ") {
    scaleM <- 100
    scaleSD <- 15
  } else if (scale == "z") {
    scaleM <- 0
    scaleSD <- 1
  } else if (scale == "T") {
    scaleM <- 50
    scaleSD <- 10
  }

  if (is.null(n))
    n <- max(score)

  attr(result, "age_mean") <- mean(age)
  attr(result, "age_sd") <- sd(age)
  attr(result, "ageMin") <- min(age)
  attr(result, "ageMax") <- max(age)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- sd(score)
  attr(result, "max") <- n
  attr(result, "N") <- length(score)
  attr(result, "scaleMean") <- scaleM
  attr(result, "scaleSD") <- scaleSD

  model <- list(
    beta_est = beta_est,
    gamma_est = gamma_est,
    se = se,
    mu = mu,
    sigma = sigma,
    result = result
  )
  class(model) <- "cnormBetaBinomial"



  if (plot) {
    p <- plot.cnormBetaBinomial(model, age, score, weights = weights)
    print(p)
  }

  return(model)
}

#' Predict mean and standard deviation for a beta binomial regression model
#'
#' This function generates predictions from a fitted beta binomial regression model
#' for new age points.
#'
#' @param model An object of class "cnormBetaBinomial", typically the result of a call to \code{\link{cnorm.betabinomial}}.
#' @param ages A numeric vector of age points at which to make predictions.
#' @param n The maximum score to be achieved.
#'
#' @return A data frame with columns:
#'   \item{age}{The input age points}
#'   \item{mu}{Predicted mean values}
#'   \item{sigma}{Predicted standard deviation values}
#'
#' @details
#' This function takes a fitted beta binomial regression model and generates predictions
#' for new age points. It applies the same standardization used in model fitting,
#' generates predictions on the standardized scale, and then transforms these back
#' to the original scale.
#'
#' @keywords internal
predictCoefficients <- function(model, ages, n = NULL) {
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  # Create design matrices including intercept
  X_new <- cbind(1, poly(ages_std, degree = model$mu, raw = TRUE))
  Z_new <- cbind(1, poly(ages_std, degree = model$sigma, raw = TRUE))

  predicted_mu_std <- X_new %*% model$beta_est
  predicted_sigma_std <- exp(Z_new %*% model$gamma_est)

  # Unstandardize predictions
  predicted_mu <- predicted_mu_std * attr(model$result, "score_sd") + attr(model$result, "score_mean")
  predicted_sigma <- predicted_sigma_std * attr(model$result, "score_sd")

  if (is.null(n))
    n <- attr(model$result, "max")

  m <- predicted_mu
  var <- predicted_sigma ^ 2

  m2 <- m * m
  m3 <- m2 * m

  a <- (m2 * n - m3 - m * var) / (n * var - n * m + m2)
  b <- a * ((n - m) / m)

  predicted <- data.frame(
    age = ages,
    mu = as.vector(predicted_mu),
    sigma = as.vector(predicted_sigma),
    a = a,
    b = b
  )
  return(predicted)
}

#' Compute Parameters of a Beta Binomial Distribution
#'
#' This function calculates the \eqn{\alpha} (a) and \eqn{\beta} (b) parameters of a beta binomial
#' distribution, along with the mean (m), variance (var) based on the input vector `x`
#' and the maximum number `n`.
#'
#' The beta-binomial distribution is a discrete probability distribution that models the
#' number of successes in a fixed number of trials, where the probability of success varies
#' from trial to trial. This variability in success probability is modeled by a beta
#' distribution. Such a calculation is particularly relevant in scenarios where there is
#' heterogeneity in success probabilities across trials, which is common in real-world
#' situations, as for example the number of correct solutions in a psychometric test, where
#' the test has a fixed number of items.
#'
#' @param x A numeric vector of non-negative integers representing observed counts.
#' @param n The maximum number or the maximum possible value of `x`. If not specified, uses max(x) instead.
#'
#' @return A numeric vector containing the calculated parameters in the following order:
#' alpha (a), beta (b), mean (m), standard deviation (sd), and the maximum number (n).
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' n <- 5
#'
#' betaCoefficients(x, n) # or, to set n to max(x)
#' betaCoefficients(x)
#'
#' @export
betaCoefficients <- function(x, n = NULL) {
  if (is.null(n))
    n <- max(x)

  m <- mean(x)
  sd <- sd(x)
  var <- sd ^ 2

  m2 <- m * m
  m3 <- m2 * m

  a <- (m2 * n - m3 - m * var) / (n * var - n * m + m2)
  b <- a * ((n - m) / m)

  return(c(a, b, m, sd, n))
}

#' Calculate Cumulative Probabilities, Density, Percentiles, and Z-Scores for
#' Beta-Binomial Distribution
#'
#' This function generates a norm table for a specific ages based on the beta binomial
#' regression model. In case a confidence coefficient (CI, default .9) and the
#' reliability is specified, confidence intervals are computed for the true score
#' estimates, including a correction for regression to the mean (Eid & Schmidt, 2012, p. 272).
#' @param model The model, which was fitted using the `optimized.model` function.
#' @param ages A numeric vector of age points at which to make predictions.
#' @param n The number of items resp. the maximum score.
#' @param m An optional stop criterion in table generation. Positive integer lower than n.
#' @param range The range of the norm scores in standard deviations. Default is 3. Thus, scores in the
#' range of +/- 3 standard deviations are considered.
#' @param CI confidence coefficient, ranging from 0 to 1, default .9
#' @param reliability coefficient, ranging between  0 to 1

#' @return A list of data frames with columns: x, Px, Pcum, Percentile, z, norm score
#' and possibly confidence interval
#' @export
normTable.betabinomial <- function(model,
                                   ages,
                                   n = NULL,
                                   m = NULL,
                                   range = 3,
                                   CI = .9,
                                   reliability = NULL) {
  if (!(inherits(model, "cnormBetaBinomial") ||
        inherits(model, "cnormBetaBinomial2"))) {
    stop(
      "Wrong object. Please provide object from class 'cnormBetaBinomial' or 'cnormBetaBinomial2'."
    )
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
    } else{
      se <- qnorm(1 - ((1 - CI) / 2)) * sqrt(reliability * (1 - reliability))

      rel <- TRUE
    }
  }

  if (is.null(n))
    n <- attr(model$result, "max")

  if (is.null(m))
    m <- n
  else if (m > n)
    m <- n

  if (inherits(model, "cnormBetaBinomial")) {
    predictions <- predictCoefficients(model, ages, n)
  } else{
    predictions <- predictCoefficients2(model, ages, n)
  }

  a <- predictions$a
  b <- predictions$b

  result <- list()

  for (k in 1:length(a)) {
    x <- seq(from = 0, to = m)

    # Calculate probabilities using log-space to avoid overflow
    log_pmf <- lchoose(n, x) + lbeta(x + a[k], n - x + b[k]) - lbeta(a[k], b[k])
    Px <- exp(log_pmf)

    # Normalize to ensure sum to 1
    Px <- Px / sum(Px)

    # Calculate cumulative probabilities
    cum <- cumsum(Px)

    # Calculate percentiles
    perc <- (cum - 0.5 * Px)

    # Calculate z-scores
    z <- qnorm(perc)
    z[z < -range] <- -range
    z[z > range] <- range



    mScale <- attr(model$result, "scaleMean")
    sdScale <- attr(model$result, "scaleSD")
    norm <- rep(NA, length(z))

    if (!is.na(mScale) && !is.na(sdScale)) {
      norm <- mScale + sdScale * z
    }

    df <- data.frame(
      x = x,
      Px = Px,
      Pcum = cum,
      Percentile = perc * 100,
      z = z,
      norm = norm
    )

    if (rel) {
      zPredicted <- reliability * z
      df$lowerCI <- (zPredicted - se) * sdScale + mScale
      df$upperCI <- (zPredicted + se) * sdScale + mScale
      df$lowerCI_PR <- pnorm(zPredicted - se) * 100
      df$upperCI_PR <- pnorm(zPredicted + se) * 100

    }
    result[[k]] <- df
  }
  names(result) <- ages
  return(result)
}

#' Predict Norm Scores from Raw Scores
#'
#' This function calculates norm scores based on raw scores, age, and a fitted cnormBetaBinomial model.
#'
#' @param object A fitted model object of class 'cnormBetaBinomial' or 'cnormBetaBinomial2'.
#' @param ... Additional arguments passed to the prediction method:
#'   \itemize{
#'      \item age A numeric vector of ages, same length as raw.
#'      \item score A numeric vector of raw scores.
#'      \item range The range of the norm scores in standard deviations. Default is 3. Thus, scores in the range of +/- 3 standard deviations are considered.
#'    }
#'
#' @return A numeric vector of norm scores.
#'
#' @details
#' The function first predicts the alpha and beta parameters of the beta-binomial distribution
#' for each age using the provided model. It then calculates the cumulative probability for
#' each raw score given these parameters. Finally, it converts these probabilities to the
#' norm scale specified in the model.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a fitted model named 'bb_model':
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw)
#' raw <- c(100, 121, 97, 180)
#' ages <- c(7, 8, 9, 10)
#' norm_scores <- predict(model, ages, raw)
#' }
#'
#' @export
#' @family predict
predict.cnormBetaBinomial <- function(object, ...) {

  model <- object
  args <- list(...)

  if ("age" %in% names(args)) { age <- args$age } else {if(length(args)>0) age <- args[[1]] else age <- NULL}
  if ("score" %in% names(args)) { score <- args$score } else {if(length(args)>1) score <- args[[2]] else score <- NULL}
  if ("range" %in% names(args)) { range <- args$range } else { if(length(args)>2) range <- args[[3]] else range <- 3 }

  if (!(inherits(model, "cnormBetaBinomial") ||
        inherits(model, "cnormBetaBinomial2"))) {
    stop(
      "Wrong object. Please provide object from class 'cnormBetaBinomial' or 'cnormBetaBinomial2'."
    )
  }

  if (length(age) != length(score)) {
    stop("The lengths of 'ages' and 'score' must be the same.")
  }

  n <- attr(model$result, "max")

  if (inherits(model, "cnormBetaBinomial")) {
    predictions <- predictCoefficients(model, age, n)
  } else{
    predictions <- predictCoefficients2(model, age, n)
  }

  a <- predictions$a
  b <- predictions$b

  z_scores <- numeric(length(age))

  for (i in 1:length(age)) {
    x <- seq(from = 0, to = n)

    # Calculate probabilities using log-space to avoid overflow
    log_pmf <- lchoose(n, x) + lbeta(x + a[i], n - x + b[i]) - lbeta(a[i], b[i])
    Px <- exp(log_pmf)

    # Normalize to ensure sum to 1
    Px <- Px / sum(Px)

    # Calculate cumulative probabilities
    cum <- cumsum(Px)

    # Calculate percentiles
    perc <- cum - 0.5 * Px

    # Find the index of the raw score
    score_index <- which(x == score[i])

    if (length(score_index) == 0) {
      warning(paste(
        "Raw score",
        score[i],
        "not found for age",
        age[i],
        ". Returning NA."
      ))
      z_scores[i] <- NA
    } else {
      # Calculate z-score
      z_scores[i] <- qnorm(perc[score_index])
    }
  }

  z_scores[z_scores < -range] <- -range
  z_scores[z_scores > range] <- range
  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  if (!is.na(mScale) && !is.na(sdScale)) {
    # scale z scores to scale
    z_scores <- mScale + sdScale * z_scores
    return(z_scores)
  } else{
    # percentile
    return(pnorm(z_scores) * 100)
  }
}

#' Plot cnormBetaBinomial Model with Data and Percentile Lines
#'
#' This function creates a visualization of a fitted cnormBetaBinomial model,
#' including the original data points manifest percentiles and specified percentile lines.
#'
#' @param x A fitted model object of class "cnormBetaBinomial" or "cnormBetaBinomial2".
#' @param ... Additional arguments passed to the plot method.
#'   \itemize{
#'      \item age A vector the age data.
#'      \item A vector of the score data.
#'      \item weights An optional numeric vector of weights for each observation.
#'      \item percentiles An optional vector with the percentiles to plot.
#'      \item points Logical indicating whether to plot the data points. Default is TRUE.
#'    }
#'
#' @return A ggplot object.
#'
#' @examples
#'
#' \dontrun{
#' # Computing beta binomial models already displays plot
#' model.bb <- cnorm.betabinomial(elfe$group, elfe$raw)
#'
#' # Without data points
#' plot(model.bb, age = elfe$group, score = elfe$raw, weights=NULL, points=FALSE)
#'
#' }
#' @family plot
#' @export
plot.cnormBetaBinomial <- function(x, ...) {

  model <- x
  args <- list(...)

  if ("age" %in% names(args)) { age <- args$age } else {if(length(args)>0) age <- args[[1]] else age <- NULL}
  if ("score" %in% names(args)) { score <- args$score } else {if(length(args)>1) score <- args[[2]] else score <- NULL}
  if ("weights" %in% names(args)) { weights <- args$weights } else { weights <- NULL }
  if ("percentiles" %in% names(args)) { percentiles <- args$percentiles } else { percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975) }
  if ("points" %in% names(args)) { points <- args$points } else { points <- TRUE }

  if(is.null(age) || is.null(score))
    stop("Please provide 'age' and 'score' vectors.")

  if (!(inherits(model, "cnormBetaBinomial") ||
        inherits(model, "cnormBetaBinomial2"))) {
    stop(
      "Wrong object. Please provide object from class 'cnormBetaBinomial' or 'cnormBetaBinomial2'."
    )
  }

  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (!is.null(weights) && length(weights) != length(age)) {
    stop("Length of 'weights' must match length of 'age' and 'score'.")
  }

  # Generate prediction points
  n_points <- 100
  data <- data.frame(age = age, score = score)
  if (!is.null(weights)) {
    data$w <- weights
  }

  age_range <- range(age)
  pred_ages <- seq(age_range[1], age_range[2], length.out = n_points)

  # Get predictions
  if (inherits(model, "cnormBetaBinomial")) {
    preds <- predictCoefficients(model, pred_ages)
  } else {
    preds <- predictCoefficients2(model, pred_ages)
  }

  # Calculate percentile lines
  percentile_lines <- lapply(percentiles, function(p) {
    qbeta(p, shape1 = preds$a, shape2 = preds$b) * attr(model$result, "max")
  })

  percentile_data <- do.call(cbind, percentile_lines)
  colnames(percentile_data) <- paste0("P", percentiles * 100)

  plot_data <- data.frame(
    age = pred_ages,
    mu = preds$mu,
    sigma = preds$sigma,
    percentile_data
  )

  # Create the plot
  p <- ggplot()

  if (points)
    p <- p + geom_point(
      data = data,
      aes(x = age, y = score),
      alpha = 0.2,
      size = 0.6
    )

  # Calculate and add manifest percentiles
  if (length(age) / length(unique(age)) > 50 && min(table(data$age)) > 30) {
    # Distinct age groups
    data$group <- age
  } else {
    # Use getGroups function
    data$group <- getGroups(age)
  }

  # get actual percentiles
  NAMES <- paste("PR", percentiles * 100, sep = "")
  percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data$group), function(df) {
    c(age = mean(df$age),
      weighted.quantile(df$score, probs = percentiles, weights = df$w))
  })))
  colnames(percentile.actual) <- c("age", NAMES)
  manifest_data <- percentile.actual

  # Add percentile lines and points with proper legend
  for (i in seq_along(percentiles)) {
    p <- p +
      geom_line(
        data = plot_data,
        aes(
          x = .data$age,
          y = .data[[paste0("P", percentiles[i] * 100)]],
          color = !!NAMES[i]
        ),
        size = 0.6
      ) +
      geom_point(
        data = manifest_data,
        aes(
          x = .data$age,
          y = .data[[NAMES[i]]],
          color = !!NAMES[i]
        ),
        size = 2,
        shape = 18
      )
  }

  # Customize the plot
  p <- p +
    theme_minimal() +
    labs(
      title = "Percentile Plot (Beta-Binomial Model)",
      x = "Age",
      y = "Score",
      color = "Percentile"
    ) +
    scale_y_continuous(limits = c(0, attr(model$result, "max"))) +
    scale_color_manual(
      values = setNames(rainbow(length(percentiles)), NAMES),
      breaks = NAMES,
      labels = paste0(percentiles * 100, "%")
    ) +
    guides(color = guide_legend(override.aes = list(
      linetype = rep("solid", length(NAMES)),
      shape = rep(18, length(NAMES))
    )))

  p <- p +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  return(p)
}

#' Diagnostic Information for Beta-Binomial Model
#'
#' This function provides diagnostic information for a fitted beta-binomial model
#' from the cnorm.betabinomial function. It returns various metrics related to
#' model convergence, fit, and complexity. In case, age and raw scores are provided,
#' the function as well computes R2, rmse and bias for the norm scores based on
#' the manifest and predicted norm scores.
#'
#' @param model An object of class "cnormBetaBinomial", typically the result of a call to cnorm.betabinomial().
#' @param age An optional vector with age values
#' @param score An optional vector with raw values
#' @param weights An optional vector with weights
#'
#' @return A list containing the following diagnostic information:
#' \itemize{
#'   \item converged: Logical indicating whether the optimization algorithm converged.
#'   \item n_evaluations: Number of function evaluations performed during optimization.
#'   \item n_gradient: Number of gradient evaluations performed during optimization.
#'   \item final_value: Final value of the objective function (negative log-likelihood).
#'   \item message: Any message returned by the optimization algorithm.
#'   \item AIC: Akaike Information Criterion.
#'   \item BIC: Bayesian Information Criterion.
#'   \item max_gradient: Maximum absolute gradient at the solution (if available).
#' }
#'
#' @details
#' The AIC and BIC are calculated as:
#' AIC = 2k - 2ln(L)
#' BIC = ln(n)k - 2ln(L)
#' where k is the number of parameters, L is the maximum likelihood, and n is the number of observations.
#'
#' @examples
#' \dontrun{
#' # Fit a beta-binomial model
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw)
#'
#' # Get diagnostic information
#' diag_info <- diagnostics.betabinomial(model)
#'
#' # Print the diagnostic information
#' print(diag_info)
#'
#' # Summary the diagnostic information
#' summary(diag_info)
#'
#' # Check if the model converged
#' if(diag_info$converged) {
#'   cat("Model converged successfully.\n")
#' } else {
#'   cat("Warning: Model did not converge.\n")
#' }
#'
#' # Compare AIC and BIC
#' cat("AIC:", diag_info$AIC, "\n")
#' cat("BIC:", diag_info$BIC, "\n")
#' }
#'
#' @export
diagnostics.betabinomial <- function(model,
                                     age = NULL,
                                     score = NULL,
                                     weights = NULL) {
  if (!(inherits(model, "cnormBetaBinomial") ||
        inherits(model, "cnormBetaBinomial2"))) {
    stop(
      "Wrong object. Please provide object from class 'cnormBetaBinomial' or 'cnormBetaBinomial2'."
    )
  }

  opt_results <- model$result

  if (inherits(model, "cnormBetaBinomial")) {
    n_params <- length(model$beta_est) + length(model$gamma_est)
    param_names <- c(paste0("beta_", 0:(length(model$beta_est) - 1)), paste0("gamma_", 0:(length(
      model$gamma_est
    ) - 1)))
    estimates <- c(model$beta_est, model$gamma_est)
  } else {
    n_params <- length(model$alpha_est) + length(model$beta_est)
    param_names <- c(paste0("alpha_", 0:(length(
      model$alpha_est
    ) - 1)), paste0("beta_", 0:(length(model$beta_est) - 1)))
    estimates <- c(model$alpha_est, model$beta_est)
  }

  n_obs <- attr(model$result, "N")
  convergence <- opt_results$convergence == 0

  max_gradient <- NA
  if (is.numeric(opt_results$gradient) &&
      length(opt_results$gradient) > 0)
    max_gradient <- max(abs(opt_results$gradient))

  # Calculate parameter estimates and standard errors

  se <- sqrt(diag(solve(opt_results$hessian)))
  z_values <- estimates / se
  p_values <- 2 * (1 - pnorm(abs(z_values)))

  # Calculate log-likelihood, AIC, and BIC
  log_likelihood <- -opt_results$value
  AIC <- 2 * n_params - 2 * log_likelihood
  BIC <- log(n_obs) * n_params - 2 * log_likelihood

  # Calculate R-squared if age and score data are provided
  R2 <- NA
  rmse <- NA
  bias <- NA
  if (!is.null(age) && !is.null(score)) {
    if (length(age) / length(unique(age)) > 50 && min(table(age)) > 30) {
      data <- data.frame(group = age, raw = score)
      data <- rankByGroup(
        data = data,
        raw = "raw",
        group = "group",
        weights = weights
      )
      norm_scores <- predict(model, data$group, data$raw)
    } else{
      data <- data.frame(age = age, raw = score)
      data$groups <- getGroups(age)
      width <- (max(age) - min(age)) / length(unique(data$groups))
      data <- rankBySlidingWindow(
        data,
        age = "age",
        raw = "raw",
        width = width,
        weights = weights
      )
      norm_scores <- predict(model, data$age, data$raw)
    }

    norm_manifest <- data$normValue
    R2 <- cor(norm_scores, norm_manifest, use="pairwise.complete.obs") ^ 2
    rmse <- sqrt(mean((norm_scores - norm_manifest) ^ 2))
    bias <- mean(norm_scores - norm_manifest)
  } else {
    message <- "No age and raw scores provided. Cannot calculate R2, RMSE, and bias."
  }

  if (inherits(model, "cnormBetaBinomial")) {
    type = "cnormBetaBinomial"
  } else{
    type = "cnormBetaBinomial2"
  }

  list(
    type = type,
    converged = convergence,
    n_evaluations = opt_results$counts["function"],
    n_gradient = opt_results$counts["gradient"],
    final_value = opt_results$value,
    message = opt_results$message,
    AIC = AIC,
    BIC = BIC,
    log_likelihood = log_likelihood,
    max_gradient = max_gradient,
    n_params = n_params,
    n_obs = n_obs,
    param_estimates = setNames(estimates, param_names),
    param_se = setNames(se, param_names),
    z_values = setNames(z_values, param_names),
    p_values = setNames(p_values, param_names),
    R2 = R2,
    rmse = rmse,
    bias = bias
  )
}

#' Calculate the negative log-likelihood for a beta-binomial regression model
#'
#' This function computes the negative log-likelihood for a beta-binomial regression model
#' where both the alpha and beta parameters are modeled as functions of predictors.
#'
#' @param params A numeric vector containing all model parameters. The first n_alpha elements
#'               are coefficients for the alpha model, and the remaining elements are
#'               coefficients for the beta model.
#' @param X A matrix of predictors for the alpha model.
#' @param Z A matrix of predictors for the beta model.
#' @param y A numeric vector of response values.
#' @param n The maximum score (number of trials in the beta-binomial distribution).
#' @param weights A numeric vector of weights for each observation. If NULL, equal weights are used.
#'
#' @return The negative log-likelihood of the model.
#'
#' @details
#' This function uses a numerically stable implementation of the beta-binomial log-probability.
#' It allows for weighted observations, which can be useful for various modeling scenarios.
#'
#' @keywords internal
log_likelihood2 <- function(params, X, Z, y, n, weights = NULL) {
  n_alpha <- ncol(X)
  alpha_coef <- params[1:n_alpha]
  beta_coef <- params[(n_alpha + 1):length(params)]

  log_alpha <- X %*% alpha_coef
  log_beta <- Z %*% beta_coef
  alpha <- exp(log_alpha)
  beta <- exp(log_beta)

  # More numerically stable implementation of beta-binomial log-probability
  dbetabinom <- function(x, size, alpha, beta, log = FALSE) {
    logp <- lchoose(size, x) +
      lgamma(size + 1) +
      lgamma(alpha + x) +
      lgamma(beta + size - x) +
      lgamma(alpha + beta) -
      lgamma(x + 1) -
      lgamma(size - x + 1) -
      lgamma(size + alpha + beta) -
      lgamma(alpha) -
      lgamma(beta)

    if (log)
      return(logp)
    else
      return(exp(logp))
  }

  # If weights are not provided, use equal weights
  if (is.null(weights)) {
    weights <- rep(1, length(y))
  }

  ll <- sum(weights * dbetabinom(
    y,
    size = n,
    alpha = alpha,
    beta = beta,
    log = TRUE
  ))
  return(-ll)  # Return negative log-likelihood for minimization
}


#' Fit a beta-binomial regression model for continuous norming
#'
#' This function fits a beta-binomial regression model where both the alpha and beta
#' parameters of the beta-binomial distribution are modeled as polynomial functions
#' of the predictor variable (typically age). While 'cnorm-betabinomial' fits a beta-binomial model
#' on the basis of \eqn{\mu} and \eqn{\sigma}, this function fits a beta-binomial model directly on the basis
#' of \eqn{\gamma} and \eqn{\beta}.
#'
#' @param age A numeric vector of predictor values (e.g., age).
#' @param score A numeric vector of response values.
#' @param n The maximum score (number of trials in the beta-binomial distribution). If NULL, max(score) is used.
#' @param weights A numeric vector of weights for each observation. Default is NULL (equal weights).
#' @param alpha_degree Integer specifying the degree of the polynomial for the alpha model. Default is 3.
#' @param beta_degree Integer specifying the degree of the polynomial for the beta model. Default is 3.
#' @param control A list of control parameters to be passed to the `optim` function.
#'   If NULL, default values are used.
#' @param scale Type of norm scale, either "T" (default), "IQ", "z" or a double vector with the mean and standard deviation.
#' @param plot Logical indicating whether to plot the model. Default is TRUE.
#'
#' @return A list of class "cnormBetaBinomial2" containing:
#'   \item{alpha_est}{Estimated coefficients for the alpha model}
#'   \item{beta_est}{Estimated coefficients for the beta model}
#'   \item{se}{Standard errors of the estimated coefficients}
#'   \item{alpha_degree}{Degree of the polynomial for the alpha model}
#'   \item{beta_degree}{Degree of the polynomial for the beta model}
#'   \item{result}{Full result from the optimization procedure}
#'
#' @details
#' The function standardizes the input variables, fits polynomial models for both
#' the alpha and beta parameters, and uses maximum likelihood estimation to
#' find the optimal parameters. The optimization is performed using the L-BFGS-B method.
#'
#' @keywords internal
cnorm.betabinomial2 <- function(age,
                                score,
                                n = NULL,
                                weights = NULL,
                                alpha_degree = 3,
                                beta_degree = 3,
                                control = NULL,
                                scale = "T",
                                plot = T) {
  # Standardize inputs
  age_std <- standardize(age)

  # Set up 'data' object containing both variables
  data <- data.frame(age = age_std, score = score)
  if (is.null(n))
    n <- max(score)

  # Prepare the data matrices for alpha and beta, including intercept
  X <- cbind(1, poly(data$age, degree = alpha_degree, raw = TRUE))
  Z <- cbind(1, poly(data$age, degree = beta_degree, raw = TRUE))
  y <- data$score

  # Initial parameters: use some sensible starting values
  starting_values <- betaCoefficients(y)
  starting_values[starting_values<=0] <- 1e-6
  initial_alpha <- log(starting_values[1])
  initial_beta <- log(starting_values[2])

  initial_params <- c(initial_alpha,
                      rep(1e-9, alpha_degree),
                      initial_beta,
                      rep(1e-9, beta_degree))

  # Optimize to find parameter estimates. If control is NULL, set default
  if (is.null(control)){
    n_param <-alpha_degree + beta_degree + 2
    control = list(factr = 1e-6, maxit = n_param*100, lmm = n_param)
  }

  result <- optim(
    initial_params,
    log_likelihood2,
    X = X,
    Z = Z,
    y = y,
    n = n,
    weights = weights,
    method = "L-BFGS-B",
    hessian = TRUE,
    control = control
  )

  if (result$convergence != 0) {
    warning("Optimization did not converge. Consider adjusting control parameters.")
  }

  # Extract results and calculate standard errors
  alpha_est <- result$par[1:(alpha_degree + 1)]
  beta_est <- result$par[(alpha_degree + 2):length(result$par)]
  se <- sqrt(diag(solve(result$hessian)))

  # Store original mean and sd for unstandardizing later
  # add attributes for usage in other functions
  scaleM <- NA
  scaleSD <- NA

  # descriptives
  if ((typeof(scale) == "double" && length(scale) == 2)) {
    scaleM <- scale[1]
    scaleSD <- scale[2]
  } else if (scale == "IQ") {
    scaleM <- 100
    scaleSD <- 15
  } else if (scale == "z") {
    scaleM <- 0
    scaleSD <- 1
  } else if (scale == "T") {
    scaleM <- 50
    scaleSD <- 10
  }

  attr(result, "age_mean") <- mean(age)
  attr(result, "age_sd") <- sd(age)
  attr(result, "ageMin") <- min(age)
  attr(result, "ageMax") <- max(age)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- sd(score)
  attr(result, "max") <- n
  attr(result, "N") <- length(score)
  attr(result, "scaleMean") <- scaleM
  attr(result, "scaleSD") <- scaleSD

  model <- list(
    alpha_est = alpha_est,
    beta_est = beta_est,
    se = se,
    alpha_degree = alpha_degree,
    beta_degree = beta_degree,
    result = result
  )

  class(model) <- "cnormBetaBinomial2"
  if (plot) {
    p <- plot.cnormBetaBinomial(model, age, score, weights = weights)
    print(p)
  }

  return(model)
}


#' Predict alpha and beta parameters for a beta-binomial regression model
#'
#' This function generates predictions from a fitted beta-binomial regression model
#' for new age points.
#'
#' @param model An object of class "cnormBetaBinomial2", typically the result of a call to cnorm.betabinomial2().
#' @param ages A numeric vector of age points at which to make predictions.
#' @param n The maximum score to be achieved.
#'
#' @return A data frame with columns:
#'   \item{age}{The input age points}
#'   \item{a}{Predicted alpha values}
#'   \item{b}{Predicted beta values}
#'   \item{mu}{Predicted mean values}
#'   \item{sigma}{Predicted standard deviation values}
#'
#' @details
#' This function takes a fitted beta-binomial regression model and generates predictions
#' for new age points. It applies the same standardization used in model fitting,
#' generates predictions on the standardized scale, and then transforms these back
#' to the original scale.
#'
#' @keywords internal
predictCoefficients2 <- function(model, ages, n = NULL) {
  if (!inherits(model, "cnormBetaBinomial2")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial2'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  # Create design matrices including intercept
  X_new <- cbind(1, poly(ages_std, degree = model$alpha_degree, raw = TRUE))
  Z_new <- cbind(1, poly(ages_std, degree = model$beta_degree, raw = TRUE))

  log_alpha <- X_new %*% model$alpha_est
  log_beta <- Z_new %*% model$beta_est

  alpha <- exp(log_alpha)
  beta <- exp(log_beta)

  if (is.null(n))
    n <- attr(model$result, "max")

  # Calculate mean and variance of beta-binomial distribution
  mu <- n * alpha / (alpha + beta)
  var <- (n * alpha * beta * (alpha + beta + n)) / ((alpha + beta) ^ 2 * (alpha + beta + 1))
  sigma <- sqrt(var)

  predicted <- data.frame(
    age = ages,
    a = as.vector(alpha),
    b = as.vector(beta),
    mu = as.vector(mu),
    sigma = as.vector(sigma)
  )
  return(predicted)
}

#' Fit a beta-binomial regression model for continuous norming
#'
#' This function fits a beta-binomial regression model where both the \eqn{\alpha} and \eqn{\beta}
#' parameters of the beta-binomial distribution are modeled as polynomial functions
#' of the predictor variable (typically age). Setting mode to 1 fits a beta-binomial
#' model on the basis of \eqn{\mu} and \eqn{\sigma}, setting it to 2 (default) fits a beta-binomial
#' model directly on the basis of \eqn{\alpha} and \eqn{\beta}.
#'
#' @param age A numeric vector of predictor values (e.g., age).
#' @param score A numeric vector of response values.
#' @param n The maximum score (number of trials in the beta-binomial distribution). If NULL, max(score) is used.
#' @param weights A numeric vector of weights for each observation. Default is NULL (equal weights).
#' @param mode Integer specifying the mode of the model. Default is 2 (direct modelling of \eqn{\gamma} and \eqn{\beta}).
#'             If set to 1, the model is fitted on the basis of \eqn{\mu} and \eqn{\sigma}, the predicted
#'             mean and standard deviation over age.
#' @param alpha Integer specifying the degree of the polynomial for the alpha model.
#'              Default is 3. If mode is set to 1, this parameter is used to specify the degree
#'              of the polynomial for the \eqn{\mu} model.
#' @param beta Integer specifying the degree of the polynomial for the beta model. Default is 3.
#'             If mode is set to 1, this parameter is used to specify the degree of the polynomial
#'             for the \eqn{\sigma} model.
#' @param control A list of control parameters to be passed to the `optim` function.
#'   If NULL, default values are used, namely control = list(reltol = 1e-8, maxit = 1000)
#'   for mode 1 and control = list(factr = 1e-8, maxit = 1000) for mode 2.
#' @param scale Type of norm scale, either "T" (default), "IQ", "z" or a double vector with the mean and standard deviation.
#' @param plot Logical indicating whether to plot the model. Default is TRUE.
#'
#' @return A list of class "cnormBetaBinomial" or "cnormBetaBinomial2". In case of mode 2
#'         containing:
#'   \item{alpha_est}{Estimated coefficients for the alpha model}
#'   \item{beta_est}{Estimated coefficients for the beta model}
#'   \item{se}{Standard errors of the estimated coefficients}
#'   \item{alpha_degree}{Degree of the polynomial for the alpha model}
#'   \item{beta_degree}{Degree of the polynomial for the beta model}
#'   \item{result}{Full result from the optimization procedure}
#'
#' @details
#' The function standardizes the input variables, fits polynomial models for both
#' the alpha and beta parameters, and uses maximum likelihood estimation to
#' find the optimal parameters. The optimization is performed using the L-BFGS-B method.
#'
#' @examples
#' \dontrun{
#' # Fit a beta-binomial regression model to the PPVT data
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw, n = 228)
#' summary(model)
#'
#' # Use weights for post-stratification
#' marginals <- data.frame(var = c("sex", "sex", "migration", "migration"),
#'                         level = c(1,2,0,1),
#'                         prop = c(0.51, 0.49, 0.65, 0.35))
#' weights <- computeWeights(ppvt, marginals)
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw, n = 228, weights = weights)
#' }
#' @export
cnorm.betabinomial <- function(age,
                               score,
                               n = NULL,
                               weights = NULL,
                               mode = 2,
                               alpha = 3,
                               beta = 3,
                               control = NULL,
                               scale = "T",
                               plot = T) {
  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (is.null(n)) {
    n <- max(score)
    message("n parameter not specified, using the maximum score in the data instead. Consider to provide n manually.")
  }

  if(!(all(score >= 0) & all(score == floor(score)))){
    warning("The score variable needs to include only positive integers for modelling with beta-binomial distributions. Trying to use Taylor polynomials instead (function 'cnorm').")
    return(cnorm(raw=score, age=age, weights = weights, scale = scale, plot = plot))
  }

  if (mode == 2) {
    model <- cnorm.betabinomial2(age, score, n, weights, alpha, beta, control, scale, plot)
  } else{
    model <- cnorm.betabinomial1(age, score, n, weights, mu = alpha, sigma = beta, control, scale, plot)
  }

  return(model)
}

#' Summarize a Beta-Binomial Continuous Norming Model
#'
#' This function provides a summary of a fitted beta-binomial continuous norming model,
#' including model fit statistics, convergence information, and parameter estimates.
#'
#' @param object An object of class "cnormBetaBinomial" or "cnormBetaBinomial2", typically
#'   the result of a call to \code{\link{cnorm.betabinomial}}.
#' @param ... Additional arguments passed to the summary method.
#' @param ... Additional arguments passed to the summary method:
#'   \itemize{
#'      \item age An optional numeric vector of age values corresponding to the raw scores. If provided along with \code{raw}, additional fit statistics (R-squared, RMSE, bias) will be calculated.
#'      \item score An optional numeric vector of raw scores. Must be provided if \code{age} is given.
#'      \item weights An optional numeric vector of weights for each observation.
#'    }
#'
#' @return Invisibly returns a list containing detailed diagnostic information about the model.
#'   The function primarily produces printed output summarizing the model.
#'
#' @details
#' The summary includes:
#' \itemize{
#'   \item Basic model information (type, number of observations, number of parameters)
#'   \item Model fit statistics (log-likelihood, AIC, BIC)
#'   \item R-squared, RMSE, and bias (if age and raw scores are provided)
#'         in comparison to manifest norm scores
#'   \item Convergence information
#'   \item Parameter estimates with standard errors, z-values, and p-values
#' }
#'
#' @examples
#' \dontrun{
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw, n = 228)
#' summary(model)
#'
#' # Including R-squared, RMSE, and bias in the summary:
#' summary(model, age = ppvt$age, score = ppvt$raw)
#' }
#' @seealso \code{\link{cnorm.betabinomial}}, \code{\link{diagnostics.betabinomial}}
#'
#' @export
summary.cnormBetaBinomial <- function(object, ...) {
  args <- list(...)

  if ("age" %in% names(args)) { age <- args$age } else {if(length(args)>0) age <- args[[1]] else age <- NULL}
  if ("score" %in% names(args)) { score <- args$score } else {if(length(args)>1) score <- args[[2]] else score <- NULL}
  if ("weights" %in% names(args)) { weights <- args$weights } else {if(length(args)>2) weights <- args[[3]] else weights <- NULL}


  diag <- diagnostics.betabinomial(object, age, score, weights)

  cat("Beta-Binomial Continuous Norming Model\n")
  cat("---------------------------------------\n")
  cat("Model type:", diag$type, "\n")
  cat("Number of observations:", diag$n_obs, "\n")
  cat("Number of parameters:", diag$n_params, "\n")
  cat("\n")

  cat("Model Fit:\n")
  cat("  Log-likelihood:", round(diag$log_likelihood, 2), "\n")
  cat("  AIC:", round(diag$AIC, 2), "\n")
  cat("  BIC:", round(diag$BIC, 2), "\n")
  if (!is.na(diag$R2)) {
    cat("  R-squared:", round(diag$R2, 4), "\n")
    cat("  RMSE:", round(diag$rmse, 4), "\n")
    cat("  Bias:", round(diag$bias, 4), "\n")
  }
  cat("\n")

  cat("Convergence:\n")
  cat("  Converged:", diag$converged, "\n")
  cat("  Function evaluations:", diag$n_evaluations, "\n")
  cat("  Gradient evaluations:", diag$n_gradient, "\n")
  cat("  Max gradient:", round(diag$max_gradient, 6), "\n")
  cat("  Message:", diag$message, "\n")
  cat("\n")

  cat("Parameter Estimates:\n")
  param_table <- data.frame(
    Estimate = diag$param_estimates,
    `Std. Error` = diag$param_se,
    `z value` = diag$z_values,
    `Pr(>|z|)` = diag$p_values
  )
  print(param_table, digits = 4)

  invisible(diag)
}

#' Summarize a Beta-Binomial Continuous Norming Model
#'
#' This function provides a summary of a fitted beta-binomial continuous norming model,
#' including model fit statistics, convergence information, and parameter estimates.
#'
#' @param object An object of class "cnormBetaBinomial" or "cnormBetaBinomial2", typically
#'   the result of a call to \code{\link{cnorm.betabinomial}}.
#' @param ... Additional arguments passed to the summary method.
#' @param ... Additional arguments passed to the summary method:
#'   \itemize{
#'      \item age An optional numeric vector of age values corresponding to the raw scores. If provided along with \code{raw}, additional fit statistics (R-squared, RMSE, bias) will be calculated.
#'      \item score An optional numeric vector of raw scores. Must be provided if \code{age} is given.
#'      \item weights An optional numeric vector of weights for each observation.
#'    }
#'
#' @return Invisibly returns a list containing detailed diagnostic information about the model.
#'   The function primarily produces printed output summarizing the model.
#'
#' @details
#' The summary includes:
#' \itemize{
#'   \item Basic model information (type, number of observations, number of parameters)
#'   \item Model fit statistics (log-likelihood, AIC, BIC)
#'   \item R-squared, RMSE, and bias (if age and raw scores are provided)
#'         in comparison to manifest norm scores
#'   \item Convergence information
#'   \item Parameter estimates with standard errors, z-values, and p-values
#' }
#'
#' @examples
#' \dontrun{
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw, n = 228)
#' summary(model)
#'
#' # Including R-squared, RMSE, and bias in the summary:
#' summary(model, age = ppvt$age, raw = ppvt$raw)
#' }
#' @seealso \code{\link{cnorm.betabinomial}}, \code{\link{diagnostics.betabinomial}}
#'
#' @export
summary.cnormBetaBinomial2 <- summary.cnormBetaBinomial

#' Plot cnormBetaBinomial Model with Data and Percentile Lines
#'
#' This function creates a visualization of a fitted cnormBetaBinomial model,
#' including the original data points manifest percentiles and specified percentile lines.
#'
#' @param x A fitted model object of class "cnormBetaBinomial" or "cnormBetaBinomial2".
#' @param ... Additional arguments passed to the plot method.
#'   \itemize{
#'      \item age A vector the age data.
#'      \item A vector of the score data.
#'      \item weights An optional numeric vector of weights for each observation.
#'      \item percentiles An optional vector with the percentiles to plot.
#'      \item points Logical indicating whether to plot the data points. Default is TRUE.
#'    }
#'
#' @return A ggplot object.
#'
#' @family plot
#' @export
plot.cnormBetaBinomial2 <- plot.cnormBetaBinomial

#' Predict Norm Scores from Raw Scores
#'
#' This function calculates norm scores based on raw scores, age, and a fitted cnormBetaBinomial model.
#'
#' @param object A fitted model object of class 'cnormBetaBinomial' or 'cnormBetaBinomial2'.
#' @param ... Additional arguments passed to the prediction method:
#'   \itemize{
#'      \item age A numeric vector of ages, same length as raw.
#'      \item score A numeric vector of raw scores.
#'      \item range The range of the norm scores in standard deviations. Default is 3. Thus, scores in the range of +/- 3 standard deviations are considered.
#'    }
#'
#' @return A numeric vector of norm scores.
#'
#' @details
#' The function first predicts the alpha and beta parameters of the beta-binomial distribution
#' for each age using the provided model. It then calculates the cumulative probability for
#' each raw score given these parameters. Finally, it converts these probabilities to the
#' norm scale specified in the model.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a fitted model named 'bb_model':
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw)
#' raw <- c(100, 121, 97, 180)
#' ages <- c(7, 8, 9, 10)
#' norm_scores <- predict(model, ages, raw)
#' }
#'
#' @export
#' @family predict
predict.cnormBetaBinomial2 <- predict.cnormBetaBinomial
