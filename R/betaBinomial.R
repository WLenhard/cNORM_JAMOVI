# ==========================================================================
# Beta-binomial continuous norming (parametric part of cNORM)
# ==========================================================================


# --------------------------------------------------------------------------
# Internal helpers
# --------------------------------------------------------------------------

#' Build a raw polynomial design matrix with intercept
#'
#' @param x A numeric vector.
#' @param degree Degree of the raw polynomial.
#' @return A numeric matrix with columns 1, x, x^2, ..., x^degree.
#' @keywords internal
bb_design_matrix <- function(x, degree) {
  cbind(1, outer(x, seq_len(degree), `^`))
}

#' Resolve a norm scale specification
#'
#' @param scale Either a character ("T", "IQ", "z", anything else = percentile)
#'   or a numeric vector of length 2 with mean and sd.
#' @return A numeric vector c(mean, sd); c(NA, NA) indicates percentile output.
#' @keywords internal
bb_resolve_scale <- function(scale) {
  if (is.numeric(scale) && length(scale) == 2) {
    return(c(scale[1], scale[2]))
  }
  if (is.character(scale) && length(scale) == 1) {
    return(switch(scale,
                  "IQ" = c(100, 15),
                  "z"  = c(0, 1),
                  "T"  = c(50, 10),
                  c(NA_real_, NA_real_)))
  }
  c(NA_real_, NA_real_)
}

#' Validate and clean age / score / weights input
#'
#' Performs the shared input validation for the beta-binomial fitting
#' functions: length checks, removal of non-finite cases, weight validation,
#' and checks for negative or non-integer scores as well as degenerate
#' predictor variance.
#'
#' @param age Numeric vector of predictor values.
#' @param score Numeric vector of response values.
#' @param weights Optional numeric vector of weights.
#' @return A list with the (possibly filtered) elements age, score, weights.
#' @keywords internal
bb_prepare_data <- function(age, score, weights = NULL) {
  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }
  if (!is.null(weights) && length(weights) != length(age)) {
    stop("Length of 'weights' must match length of 'age' and 'score'.")
  }

  # reject invalid weights early
  if (!is.null(weights)) {
    if (any(weights < 0, na.rm = TRUE)) {
      stop("'weights' must be non-negative.")
    }
    if (all(weights == 0, na.rm = TRUE)) {
      stop("All 'weights' are zero. The model cannot be fitted.")
    }
  }

  vectors_to_check <- list(age = age, score = score)
  if (!is.null(weights)) {
    vectors_to_check$weights <- weights
  }

  needs_filtering <- any(vapply(vectors_to_check,
                                function(x) any(!is.finite(x)),
                                logical(1)))

  if (needs_filtering) {
    message("Vector(s) contained non-finite values (NA, NaN, Inf). These cases will be removed.")
    tmp <- do.call(filter_complete, c(vectors_to_check, verbose = FALSE))
    age <- tmp[[1]]
    score <- tmp[[2]]
    if (!is.null(weights)) {
      weights <- tmp[[3]]
    }
  }

  # Guard against degenerate input after filtering.
  if (length(age) < 2) {
    stop("Insufficient valid observations after removing non-finite cases.")
  }
  if (sd(age) == 0) {
    stop("'age' has zero variance. Age-dependent modelling requires ",
         "variability in the predictor.")
  }

  if (any(score < 0)) {
    stop("'score' contains negative values. ",
         "Beta-binomial modelling requires positive integers (including zero). ",
         "Please consider using Taylor polynomials (function 'cnorm') or ",
         "SinusH-ArcsinH distributions (function 'cnorm.shash') instead, ",
         "or transform your data to positive integers.")
  }

  if (any(score != floor(score))) {
    stop("'score' contains non-integer values. ",
         "Beta-binomial modelling requires positive integers (including zero). ",
         "Please consider using Taylor polynomials (function 'cnorm') or ",
         "SinusH-ArcsinH distributions (function 'cnorm.shash') instead, ",
         "or transform your data to positive integers.")
  }

  list(age = age, score = score, weights = weights)
}

#' Beta-binomial distribution on the full support 0:n
#'
#' Computes the (normalized) probability mass function, the cumulative
#' distribution function and mid-p percentiles of a beta-binomial
#' distribution in a numerically stable way (log space).
#'
#' @param a Alpha parameter (> 0).
#' @param b Beta parameter (> 0).
#' @param n Number of trials / maximum score.
#' @return A list with elements x (support 0:n), Px (pmf), cum (cdf) and
#'   perc (mid-p percentiles, i.e. P(X < x) + 0.5 * P(X = x)).
#' @keywords internal
bb_distribution <- function(a, b, n) {
  x <- 0:n
  if (!is.finite(a) || !is.finite(b) || a <= 0 || b <= 0) {
    na <- rep(NA_real_, n + 1)
    return(list(x = x, Px = na, cum = na, perc = na))
  }

  log_pmf <- lchoose(n, x) + lbeta(x + a, n - x + b) - lbeta(a, b)
  Px <- exp(log_pmf)
  s <- sum(Px)
  if (!is.finite(s) || s <= 0) {
    na <- rep(NA_real_, n + 1)
    return(list(x = x, Px = na, cum = na, perc = na))
  }
  Px <- Px / s               # normalize to guard against numerical drift
  cum <- cumsum(Px)
  cum[n + 1] <- 1            # enforce exact upper bound
  perc <- cum - 0.5 * Px     # mid-p percentiles

  list(x = x, Px = Px, cum = cum, perc = perc)
}


# --------------------------------------------------------------------------
# Likelihood functions and analytic gradients
# --------------------------------------------------------------------------

#' Calculate the negative log-likelihood for a beta binomial regression model
#'
#' This function computes the negative log-likelihood for a beta binomial
#' regression model where both the mean and standard deviation are modeled
#' as functions of predictors (mode 1, normal approximation on standardized
#' scores).
#'
#' @param params A numeric vector containing all model parameters. The first
#'   n_beta elements are coefficients for the mean model, and the remaining
#'   elements are coefficients for the log-standard deviation model.
#' @param X A matrix of predictors for the mean model.
#' @param Z A matrix of predictors for the log-standard deviation model.
#' @param y A numeric vector of response values.
#' @param weights A numeric vector of weights for each observation
#'   (NULL = equal weights).
#'
#' @return The negative log-likelihood of the model (large finite penalty
#'   if non-finite).
#' @keywords internal
log_likelihood <- function(params, X, Z, y, weights = NULL) {
  n_beta <- ncol(X)
  beta <- params[seq_len(n_beta)]
  gamma <- params[(n_beta + 1):length(params)]

  mu <- drop(X %*% beta)
  # Clamp the linear predictor of log(sigma) to keep exp() finite and to
  # stabilize BFGS line searches far away from the optimum
  log_sigma <- pmin(pmax(drop(Z %*% gamma), -20), 20)
  sigma <- exp(log_sigma)

  ll_i <- dnorm(y, mean = mu, sd = sigma, log = TRUE)
  ll_i[!is.finite(ll_i)] <- -709  # approx. log(.Machine$double.xmin)

  ll <- if (is.null(weights)) sum(ll_i) else sum(weights * ll_i)

  if (!is.finite(ll)) {
    return(1e10)  # large but finite penalty
  }
  -ll
}

#' Analytic gradient of the negative log-likelihood (mode 1)
#'
#' Computes the exact gradient of \code{\link{log_likelihood}} with respect
#' to all regression coefficients. With \eqn{r = (y - \mu)/\sigma}:
#' \deqn{\partial\ell/\partial\mu = r/\sigma, \qquad
#'       \partial\ell/\partial\log\sigma = r^2 - 1.}
#' Coefficient gradients follow via the chain rule through the design
#' matrices. Contributions of observations whose log(sigma) linear predictor
#' is clamped (see \code{log_likelihood}) are zeroed, which is the exact
#' subgradient of the clamped objective. Non-finite per-observation terms
#' (matching the penalty branch of the objective) are zeroed as well.
#'
#' @inheritParams log_likelihood
#' @return Numeric vector: gradient of the negative log-likelihood with
#'   respect to \code{params}.
#' @keywords internal
gradient_log_likelihood <- function(params, X, Z, y, weights = NULL) {
  n_beta <- ncol(X)
  beta <- params[seq_len(n_beta)]
  gamma <- params[(n_beta + 1):length(params)]

  mu <- drop(X %*% beta)
  eta_sigma <- drop(Z %*% gamma)
  sigma_free <- (eta_sigma > -20) & (eta_sigma < 20)
  sigma <- exp(pmin(pmax(eta_sigma, -20), 20))

  r <- (y - mu) / sigma
  d_mu <- r / sigma            # dl/dmu
  d_lsig <- r * r - 1          # dl/d(log sigma)

  # Zero out numerically failed observations (consistent with the
  # floored/penalized branch of the objective) and clamped predictors
  ok <- is.finite(d_mu) & is.finite(d_lsig)
  d_mu[!ok] <- 0
  d_lsig[!ok] <- 0
  d_lsig <- d_lsig * sigma_free

  w <- if (is.null(weights)) 1 else weights
  grad <- c(-drop(crossprod(X, w * d_mu)),
            -drop(crossprod(Z, w * d_lsig)))
  grad[!is.finite(grad)] <- 0
  grad
}

#' Calculate the negative log-likelihood for a beta-binomial regression model
#'
#' This function computes the negative log-likelihood for a beta-binomial
#' regression model where both the alpha and beta parameters are modeled as
#' functions of predictors (mode 2).
#'
#' @param params A numeric vector containing all model parameters. The first
#'   n_alpha elements are coefficients for the alpha model, and the remaining
#'   elements are coefficients for the beta model.
#' @param X A matrix of predictors for the alpha model.
#' @param Z A matrix of predictors for the beta model.
#' @param y A numeric vector of response values.
#' @param n The maximum score (number of trials).
#' @param weights A numeric vector of weights for each observation
#'   (NULL = equal weights).
#' @param lch Optional precomputed \code{lchoose(n, y)}. Since this term does
#'   not depend on the parameters, passing it once avoids recomputation in
#'   every optimizer iteration.
#'
#' @return The negative log-likelihood of the model (large finite penalty
#'   if non-finite).
#'
#' @details
#' Uses a numerically stable implementation of the beta-binomial
#' log-probability via \code{lbeta}. The linear predictors of
#' log(alpha) and log(beta) are clamped to [-20, 20].
#'
#' @keywords internal
log_likelihood2 <- function(params, X, Z, y, n, weights = NULL, lch = NULL) {
  n_alpha <- ncol(X)
  alpha_coef <- params[seq_len(n_alpha)]
  beta_coef <- params[(n_alpha + 1):length(params)]

  log_alpha <- pmin(pmax(drop(X %*% alpha_coef), -20), 20)
  log_beta <- pmin(pmax(drop(Z %*% beta_coef), -20), 20)

  alpha <- exp(log_alpha)
  beta <- exp(log_beta)

  if (is.null(lch)) {
    lch <- lchoose(n, y)
  }

  logp <- lch + lbeta(y + alpha, n - y + beta) - lbeta(alpha, beta)
  logp[!is.finite(logp)] <- -709  # approx. log(.Machine$double.xmin)

  ll <- if (is.null(weights)) sum(logp) else sum(weights * logp)

  if (!is.finite(ll)) {
    return(1e10)  # large but finite penalty
  }
  -ll
}

#' Analytic gradient of the negative log-likelihood (mode 2)
#'
#' Computes the exact gradient of \code{\link{log_likelihood2}} with respect
#' to all regression coefficients. With the per-observation log-probability
#' \deqn{\log p = \mathrm{lchoose}(n, y) + \mathrm{lbeta}(y+\alpha, n-y+\beta)
#'   - \mathrm{lbeta}(\alpha, \beta),}
#' the partial derivatives on the natural scale are (with digamma function
#' \eqn{\psi}):
#' \deqn{\partial \log p/\partial\alpha = \psi(y+\alpha) - \psi(\alpha)
#'   + \psi(\alpha+\beta) - \psi(n+\alpha+\beta)}
#' \deqn{\partial \log p/\partial\beta = \psi(n-y+\beta) - \psi(\beta)
#'   + \psi(\alpha+\beta) - \psi(n+\alpha+\beta)}
#' The chain rule through the log link multiplies by \eqn{\alpha} resp.
#' \eqn{\beta}; coefficient gradients follow as
#' \eqn{-X^\top(w \cdot \partial\ell/\partial\eta)} for the negative
#' log-likelihood. Contributions of observations whose linear predictor is
#' clamped (see \code{log_likelihood2}) are zeroed, which is the exact
#' subgradient of the clamped objective.
#'
#' Supplying this gradient to \code{\link[stats]{optim}} (L-BFGS-B) replaces
#' the finite-difference approximation, which required
#' \code{length(params) + 1} likelihood evaluations per gradient.
#'
#' @inheritParams log_likelihood2
#' @return Numeric vector: gradient of the negative log-likelihood with
#'   respect to \code{params}.
#' @keywords internal
gradient_log_likelihood2 <- function(params, X, Z, y, n,
                                     weights = NULL, lch = NULL) {
  # Note: 'lch' is accepted (optim passes identical ... to fn and gr)
  # but not needed, since the term is parameter-independent.
  n_alpha <- ncol(X)
  alpha_coef <- params[seq_len(n_alpha)]
  beta_coef <- params[(n_alpha + 1):length(params)]

  eta_alpha <- drop(X %*% alpha_coef)
  eta_beta <- drop(Z %*% beta_coef)
  alpha_free <- (eta_alpha > -20) & (eta_alpha < 20)
  beta_free <- (eta_beta > -20) & (eta_beta < 20)

  alpha <- exp(pmin(pmax(eta_alpha, -20), 20))
  beta <- exp(pmin(pmax(eta_beta, -20), 20))

  ab <- alpha + beta
  dig_ab <- digamma(ab)
  dig_nab <- digamma(n + ab)

  # dl/d(log alpha), dl/d(log beta) via chain rule through the log link
  d_lalpha <- alpha * (digamma(y + alpha) - digamma(alpha) + dig_ab - dig_nab)
  d_lbeta  <- beta  * (digamma(n - y + beta) - digamma(beta) + dig_ab - dig_nab)

  # Zero out numerically failed observations (consistent with the floored
  # branch of the objective) and clamped predictors (exact subgradient)
  ok <- is.finite(d_lalpha) & is.finite(d_lbeta)
  d_lalpha[!ok] <- 0
  d_lbeta[!ok] <- 0
  d_lalpha <- d_lalpha * alpha_free
  d_lbeta <- d_lbeta * beta_free

  w <- if (is.null(weights)) 1 else weights
  grad <- c(-drop(crossprod(X, w * d_lalpha)),
            -drop(crossprod(Z, w * d_lbeta)))
  grad[!is.finite(grad)] <- 0
  grad
}


# --------------------------------------------------------------------------
# Model fitting
# --------------------------------------------------------------------------

#' Fit a beta binomial regression model (mode 1, mu/sigma parameterization)
#'
#' This function fits a beta binomial regression model where both the mean and
#' standard deviation of the response variable are modeled as polynomial
#' functions of the predictor variable. While 'cnorm.betabinomial2' fits a
#' beta-binomial model directly on the basis of \eqn{\alpha} and \eqn{\beta},
#' this function fits \eqn{\mu} and \eqn{\sigma}, which are then used to
#' estimate the beta binomial distribution parameters. Kept for backwards
#' compatibility; 'cnorm.betabinomial2' is the recommended default.
#'
#' @param age A numeric vector of predictor values (e.g., age).
#' @param score A numeric vector of response values.
#' @param n Number of items in the test, resp. maximum score to be achieved.
#' @param weights A numeric vector of weights for each observation.
#'   Default is NULL (equal weights).
#' @param mu Integer specifying the degree of the polynomial for the mean
#'   model. Default is 3.
#' @param sigma Integer specifying the degree of the polynomial for the
#'   standard deviation model. Default is 3.
#' @param control A list of control parameters to be passed to the
#'   \code{optim} function. Any parameters not specified are filled with
#'   the defaults \code{list(reltol = 1e-8, maxit = 1000)}.
#' @param scale Type of norm scale, either T (default), IQ, z or percentile
#'   (= no transformation); a double vector with the mean and standard
#'   deviation can be provided as well, e.g. c(10, 3) for Wechsler scale
#'   index points.
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
#' The function standardizes the input variables, fits polynomial models for
#' both the mean and standard deviation, and uses maximum likelihood
#' estimation with analytic gradients to find the optimal parameters. The
#' optimization is performed using the BFGS method.
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
                                plot = TRUE) {
  # Shared input validation and cleaning
  cleaned <- bb_prepare_data(age, score, weights)
  age <- cleaned$age
  score <- cleaned$score
  weights <- cleaned$weights

  # Resolve and validate n before fitting.
  if (is.null(n)) {
    n <- max(score)
  }
  if (max(score) > n) {
    stop("'score' contains values larger than n = ", n, ".")
  }

  # Guard against zero score variance (log(sd) = -Inf start value)
  if (sd(score) == 0) {
    stop("'score' has zero variance. The model cannot be fitted.")
  }

  # Standardize inputs
  age_std <- standardize(age)
  score_std <- standardize(score)

  # Design matrices (raw polynomials incl. intercept)
  X <- bb_design_matrix(age_std, mu)
  Z <- bb_design_matrix(age_std, sigma)
  y <- score_std

  # Sensible starting values on the standardized scale
  initial_params <- c(mean(y), rep(0, mu), log(sd(y)), rep(0, sigma))

  # Merge user control with defaults instead of all-or-nothing
  control_defaults <- list(reltol = 1e-8, maxit = 1000)
  control <- if (is.null(control))
    control_defaults
  else
    utils::modifyList(control_defaults, control)

  result <- optim(
    initial_params,
    log_likelihood,
    gr = gradient_log_likelihood,
    X = X,
    Z = Z,
    y = y,
    weights = weights,
    method = "BFGS",
    hessian = TRUE,
    control = control
  )

  if (result$convergence != 0) {
    warning("Optimization did not converge (code: ", result$convergence,
            "). Consider adjusting control parameters.")
  }

  # Store the analytic gradient at the solution; consumed by
  # diagnostics.betabinomial as a first-order optimality check.
  result$gradient <- gradient_log_likelihood(result$par, X = X, Z = Z,
                                             y = y, weights = weights)

  # Extract results and calculate standard errors
  beta_est <- result$par[1:(mu + 1)]
  gamma_est <- result$par[(mu + 2):length(result$par)]

  se <- tryCatch({
    d <- diag(solve(result$hessian))
    if (any(d < 0, na.rm = TRUE)) {
      warning("Hessian is not positive definite; standard errors are set to NA where invalid.")
      d[d < 0] <- NA_real_
    }
    sqrt(d)
  }, error = function(e) {
    warning("Could not compute standard errors: Hessian matrix issue")
    rep(NA_real_, length(result$par))
  })

  # Resolve norm scale
  scaleMSD <- bb_resolve_scale(scale)

  # Store attributes for usage in other functions
  attr(result, "age_mean") <- mean(age)
  attr(result, "age_sd") <- sd(age)
  attr(result, "ageMin") <- min(age)
  attr(result, "ageMax") <- max(age)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- sd(score)
  attr(result, "max") <- n
  attr(result, "N") <- length(score)
  attr(result, "scaleMean") <- scaleMSD[1]
  attr(result, "scaleSD") <- scaleMSD[2]

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

  model
}

#' Fit a beta-binomial regression model for continuous norming (mode 2)
#'
#' This function fits a beta-binomial regression model where both the alpha
#' and beta parameters of the beta-binomial distribution are modeled as
#' polynomial functions of the predictor variable (typically age). While
#' 'cnorm.betabinomial1' fits a beta-binomial model on the basis of \eqn{\mu}
#' and \eqn{\sigma}, this function fits the model directly on the basis of
#' \eqn{\alpha} and \eqn{\beta}. This is the recommended default approach
#' (see \code{\link{cnorm.betabinomial}}).
#'
#' @param age A numeric vector of predictor values (e.g., age).
#' @param score A numeric vector of response values.
#' @param n The maximum score (number of trials in the beta-binomial
#'   distribution). If NULL, max(score) is used.
#' @param weights A numeric vector of weights for each observation.
#'   Default is NULL (equal weights).
#' @param alpha_degree Integer specifying the degree of the polynomial for
#'   the alpha model. Default is 3.
#' @param beta_degree Integer specifying the degree of the polynomial for
#'   the beta model. Default is 3.
#' @param control A list of control parameters to be passed to the
#'   \code{optim} function (method "L-BFGS-B"). Any parameters not specified
#'   are filled with adaptive defaults based on problem size.
#' @param scale Type of norm scale, either "T" (default), "IQ", "z" or a
#'   double vector with the mean and standard deviation.
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
#' The function standardizes the input variables, fits polynomial models for
#' both the alpha and beta parameters, and uses maximum likelihood estimation
#' with analytic (digamma-based) gradients to find the optimal parameters.
#' The optimization is performed using the L-BFGS-B method with parameter
#' bounds to prevent numerical issues.
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
                                plot = TRUE) {
  # Shared input validation and cleaning
  cleaned <- bb_prepare_data(age, score, weights)
  age <- cleaned$age
  score <- cleaned$score
  weights <- cleaned$weights

  if (is.null(n)) {
    n <- max(score)
    message("Using max(score) = ", n, " as the maximum score.")
  }

  if (max(score) > n) {
    stop("'score' contains values larger than n = ", n, ".")
  }

  # Standardize predictor
  age_std <- standardize(age)

  # Design matrices (raw polynomials incl. intercept)
  X <- bb_design_matrix(age_std, alpha_degree)
  Z <- bb_design_matrix(age_std, beta_degree)
  y <- score

  # Precompute the parameter-independent part of the log-likelihood once;
  # this saves one lchoose() call per observation per optimizer evaluation
  lch <- lchoose(n, y)

  # Robust initial parameter calculation via method of moments
  initial_values <- tryCatch({
    vals <- suppressWarnings(betaCoefficients(y, n))
    vals[vals <= 0 | !is.finite(vals)] <- 1e-4
    vals
  }, error = function(e) {
    m <- max(mean(y), 1e-4)
    a <- 1.0
    b <- max((n - m) / m * a, 1e-4)
    c(a, b, m, sd(y), n)
  })

  initial_params <- c(log(initial_values[1]),
                      rep(1e-6, alpha_degree),
                      log(initial_values[2]),
                      rep(1e-6, beta_degree))

  # Adaptive control parameters based on problem size.
  # Note on 'factr': optim() multiplies factr with .Machine$double.eps
  # (~2.2e-16) to obtain the relative reduction tolerance. Values below 1
  # would demand sub-machine precision and cause spurious convergence
  # failures. The values below correspond to tolerances of ~2e-12 to ~2e-10,
  # i.e. very strict but numerically achievable.
  n_param <- alpha_degree + beta_degree + 2
  if (n <= 50) {
    factr <- 1e4
    maxit <- n_param * 100
  } else if (n <= 150) {
    factr <- 1e5
    maxit <- n_param * 150
  } else {
    factr <- 1e6
    maxit <- n_param * 200
  }
  control_defaults <- list(factr = factr,
                           maxit = maxit,
                           lmm = min(n_param, 20))

  # Merge user control with adaptive defaults
  control <- if (is.null(control))
    control_defaults
  else
    utils::modifyList(control_defaults, control)

  # Parameter bounds consistent with the clamping in log_likelihood2
  lower_bounds <- rep(-20, length(initial_params))
  upper_bounds <- rep(20, length(initial_params))

  run_optim <- function(par, ctrl) {
    optim(
      par,
      log_likelihood2,
      gr = gradient_log_likelihood2,
      X = X,
      Z = Z,
      y = y,
      n = n,
      weights = weights,
      lch = lch,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      hessian = TRUE,
      control = ctrl
    )
  }

  # First optimization attempt; retry with neutral start values on failure
  result <- tryCatch({
    run_optim(initial_params, control)
  }, error = function(e) {
    message("First optimization attempt failed. Trying with different parameters...")
    retry_params <- c(log(1.0), rep(0, alpha_degree),
                      log(3.0), rep(0, beta_degree))
    retry_control <- control
    if (!is.null(retry_control$factr)) {
      retry_control$factr <- retry_control$factr * 100
    }
    if (!is.null(retry_control$maxit)) {
      retry_control$maxit <- retry_control$maxit * 2
    }
    run_optim(retry_params, retry_control)
  })

  if (result$convergence != 0) {
    warning("Optimization did not converge (code: ", result$convergence,
            "). Consider adjusting control parameters.")
  }

  # Warn when parameters sit at their box constraints; standard errors
  # and Wald tests are not valid there.
  at_bound <- result$par <= lower_bounds + 1e-6 |
    result$par >= upper_bounds - 1e-6
  if (any(at_bound)) {
    warning(
      "Parameter(s) at index ",
      paste(which(at_bound), collapse = ", "),
      " reached their bounds. Standard errors and Wald tests for these ",
      "parameters are not valid. Consider reducing model complexity."
    )
  }

  result$gradient <- gradient_log_likelihood2(result$par, X = X, Z = Z,
                                              y = y, n = n,
                                              weights = weights)

  # Extract results
  alpha_est <- result$par[1:(alpha_degree + 1)]
  beta_est <- result$par[(alpha_degree + 2):length(result$par)]
  se <- tryCatch({
    d <- diag(solve(result$hessian))
    if (any(d < 0, na.rm = TRUE)) {
      warning("Hessian is not positive definite; standard errors are set to NA where invalid.")
      d[d < 0] <- NA_real_
    }
    sqrt(d)
  }, error = function(e) {
    warning("Could not compute standard errors: Hessian matrix issue")
    rep(NA_real_, length(result$par))
  })

  # Resolve norm scale
  scaleMSD <- bb_resolve_scale(scale)

  # Store attributes for usage in other functions
  attr(result, "age_mean") <- mean(age)
  attr(result, "age_sd") <- sd(age)
  attr(result, "ageMin") <- min(age)
  attr(result, "ageMax") <- max(age)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- sd(score)
  attr(result, "max") <- n
  attr(result, "N") <- length(score)
  attr(result, "scaleMean") <- scaleMSD[1]
  attr(result, "scaleSD") <- scaleMSD[2]

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

  model
}

#' Fit a beta-binomial regression model for continuous norming
#'
#' This function fits a beta-binomial regression model where both the
#' \eqn{\alpha} and \eqn{\beta} parameters of the beta-binomial distribution
#' are modeled as polynomial functions of the predictor variable (typically
#' age). Setting mode to 1 fits a beta-binomial model on the basis of
#' \eqn{\mu} and \eqn{\sigma}, setting it to 2 (default and recommended)
#' fits a beta-binomial model directly on the basis of \eqn{\alpha} and
#' \eqn{\beta}.
#'
#' @param age A numeric vector of predictor values (e.g., age).
#' @param score A numeric vector of response values.
#' @param n The maximum score (number of trials in the beta-binomial
#'   distribution). If NULL, max(score) is used.
#' @param weights A numeric vector of weights for each observation.
#'   Default is NULL (equal weights).
#' @param mode Integer specifying the mode of the model, either 1 or 2.
#'   Default is 2 (direct modelling of \eqn{\alpha} and \eqn{\beta};
#'   recommended). If set to 1, the model is fitted on the basis of
#'   \eqn{\mu} and \eqn{\sigma}, the predicted mean and standard deviation
#'   over age (kept for backwards compatibility).
#' @param alpha Integer specifying the degree of the polynomial for the alpha
#'   model. Default is 3. If mode is set to 1, this parameter is used to
#'   specify the degree of the polynomial for the \eqn{\mu} model.
#' @param beta Integer specifying the degree of the polynomial for the beta
#'   model. Default is 3. If mode is set to 1, this parameter is used to
#'   specify the degree of the polynomial for the \eqn{\sigma} model.
#' @param control A list of control parameters to be passed to the
#'   \code{optim} function. Parameters not specified are filled with
#'   defaults, namely \code{list(reltol = 1e-8, maxit = 1000)} for mode 1
#'   and adaptive L-BFGS-B settings (factr between 1e4 and 1e6, maxit
#'   proportional to the number of parameters) for mode 2.
#' @param scale Type of norm scale, either "T" (default), "IQ", "z" or a
#'   double vector with the mean and standard deviation.
#' @param plot Logical indicating whether to plot the model. Default is TRUE.
#'
#' @return A list of class "cnormBetaBinomial" or "cnormBetaBinomial2".
#'   In case of mode 2 containing:
#'   \item{alpha_est}{Estimated coefficients for the alpha model}
#'   \item{beta_est}{Estimated coefficients for the beta model}
#'   \item{se}{Standard errors of the estimated coefficients}
#'   \item{alpha_degree}{Degree of the polynomial for the alpha model}
#'   \item{beta_degree}{Degree of the polynomial for the beta model}
#'   \item{result}{Full result from the optimization procedure}
#'
#' @details
#' The function standardizes the input variables, fits polynomial models for
#' both distribution parameters, and uses maximum likelihood estimation with
#' analytic gradients to find the optimal parameters. In mode 2, the
#' optimization is performed using the L-BFGS-B method.
#'
#' Please note that the model fit indices (log-likelihood, AIC, BIC) of
#' mode 1 and mode 2 models are not directly comparable, since mode 1
#' maximizes a normal likelihood on standardized scores, whereas mode 2
#' maximizes the discrete beta-binomial likelihood on the raw scores.
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
                               plot = TRUE) {
  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (!(is.numeric(mode) && length(mode) == 1 && mode %in% c(1, 2))) {
    stop("'mode' must be 1 or 2.")
  }

  if (is.null(n)) {
    n <- max(score, na.rm = TRUE)
    message("n parameter not specified, using the maximum score in the data instead. Consider to provide n manually.")
  }

  if (!(all(score >= 0, na.rm = TRUE) &&
        all(score == floor(score), na.rm = TRUE))) {
    warning("The score variable needs to include only positive integers for modelling with beta-binomial distributions. Trying to use Taylor polynomials instead (function 'cnorm').")
    return(cnorm(
      raw = score,
      age = age,
      weights = weights,
      scale = scale,
      plot = plot
    ))
  }

  if (mode == 2) {
    model <- cnorm.betabinomial2(age, score, n, weights,
                                 alpha_degree = alpha,
                                 beta_degree = beta,
                                 control = control,
                                 scale = scale,
                                 plot = plot)
  } else {
    model <- cnorm.betabinomial1(age, score, n, weights,
                                 mu = alpha,
                                 sigma = beta,
                                 control = control,
                                 scale = scale,
                                 plot = plot)
  }

  model
}


# --------------------------------------------------------------------------
# Prediction of distribution parameters
# --------------------------------------------------------------------------

#' Predict mean and standard deviation for a beta binomial regression model
#'
#' This function generates predictions from a fitted beta binomial regression
#' model (mode 1) for new age points and converts them into alpha and beta
#' parameters of the beta-binomial distribution via method of moments.
#'
#' @param model An object of class "cnormBetaBinomial".
#' @param ages A numeric vector of age points at which to make predictions.
#' @param n The maximum score to be achieved.
#'
#' @return A data frame with columns:
#'   \item{age}{The input age points}
#'   \item{mu}{Predicted mean values}
#'   \item{sigma}{Predicted standard deviation values}
#'   \item{a}{Alpha parameters}
#'   \item{b}{Beta parameters}
#'
#' @details
#' The function applies the same standardization used in model fitting,
#' generates predictions on the standardized scale, and transforms these back
#' to the original scale. If the method of moments yields invalid (i.e.
#' non-positive or non-finite) parameters, a mean-preserving low-precision
#' fallback is used instead of arbitrarily clamping both parameters.
#'
#' @keywords internal
predictCoefficients <- function(model, ages, n = NULL) {
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  # Design matrices including intercept
  X_new <- bb_design_matrix(ages_std, model$mu)
  Z_new <- bb_design_matrix(ages_std, model$sigma)

  predicted_mu_std <- drop(X_new %*% model$beta_est)
  log_sigma_std <- pmin(pmax(drop(Z_new %*% model$gamma_est), -20), 20)
  predicted_sigma_std <- exp(log_sigma_std)

  # Unstandardize predictions
  predicted_mu <- predicted_mu_std * attr(model$result, "score_sd") +
    attr(model$result, "score_mean")
  predicted_sigma <- predicted_sigma_std * attr(model$result, "score_sd")

  if (is.null(n)) {
    n <- attr(model$result, "max")
  }

  # Method of moments; guard the mean against the boundaries of the support
  m <- pmin(pmax(predicted_mu, 1e-6), n - 1e-6)
  v <- pmax(predicted_sigma^2, 1e-12)

  m2 <- m * m
  m3 <- m2 * m
  denom <- n * v - n * m + m2

  a <- (m2 * n - m3 - m * v) / denom
  b <- a * ((n - m) / m)

  # Mean-preserving fallback for degenerate cases (e.g. variance at or above
  # the admissible bound): use a small precision phi with a = p*phi,
  # b = (1-p)*phi, which keeps E(X) = n*p intact.
  bad <- !is.finite(a) | !is.finite(b) | a <= 0 | b <= 0
  if (any(bad)) {
    p <- m[bad] / n
    phi <- 1e-3
    a[bad] <- pmax(p * phi, 1e-6)
    b[bad] <- pmax((1 - p) * phi, 1e-6)
  }

  data.frame(
    age = ages,
    mu = predicted_mu,
    sigma = predicted_sigma,
    a = a,
    b = b
  )
}

#' Predict alpha and beta parameters for a beta-binomial regression model
#'
#' This function generates predictions from a fitted beta-binomial regression
#' model (mode 2) for new age points.
#'
#' @param model An object of class "cnormBetaBinomial2".
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
#' The function applies the same standardization used in model fitting and
#' the same clamping of the linear predictors (\eqn{\pm 20} on the log scale)
#' as the likelihood function, ensuring consistent behavior also under
#' (mild) extrapolation.
#'
#' @keywords internal
predictCoefficients2 <- function(model, ages, n = NULL) {
  if (!inherits(model, "cnormBetaBinomial2")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial2'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  # Design matrices including intercept
  X_new <- bb_design_matrix(ages_std, model$alpha_degree)
  Z_new <- bb_design_matrix(ages_std, model$beta_degree)

  # Clamp consistently with log_likelihood2
  log_alpha <- pmin(pmax(drop(X_new %*% model$alpha_est), -20), 20)
  log_beta <- pmin(pmax(drop(Z_new %*% model$beta_est), -20), 20)

  alpha <- exp(log_alpha)
  beta <- exp(log_beta)

  if (is.null(n)) {
    n <- attr(model$result, "max")
  }

  # Mean and standard deviation of the beta-binomial distribution
  ab <- alpha + beta
  mu <- n * alpha / ab
  v <- (n * alpha * beta * (ab + n)) / (ab^2 * (ab + 1))

  data.frame(
    age = ages,
    a = alpha,
    b = beta,
    mu = mu,
    sigma = sqrt(v)
  )
}

#' Compute Parameters of a Beta Binomial Distribution
#'
#' This function calculates the \eqn{\alpha} (a) and \eqn{\beta} (b)
#' parameters of a beta binomial distribution, along with the mean (m) and
#' standard deviation (sd) based on the input vector \code{x} and the maximum
#' number \code{n}, using the method of moments.
#'
#' The beta-binomial distribution is a discrete probability distribution that
#' models the number of successes in a fixed number of trials, where the
#' probability of success varies from trial to trial. This variability in
#' success probability is modeled by a beta distribution. Such a calculation
#' is particularly relevant in scenarios where there is heterogeneity in
#' success probabilities across trials, which is common in real-world
#' situations, as for example the number of correct solutions in a
#' psychometric test, where the test has a fixed number of items.
#'
#' @param x A numeric vector of non-negative integers representing observed
#'   counts.
#' @param n The maximum number or the maximum possible value of \code{x}.
#'   If not specified, uses max(x) instead.
#'
#' @return A numeric vector containing the calculated parameters in the
#'   following order: alpha (a), beta (b), mean (m), standard deviation (sd),
#'   and the maximum number (n). If the method of moments is degenerate
#'   (mean at the boundary of the support or a non-positive moment
#'   denominator), alpha and beta are returned as \code{NA} with a warning.
#'   Non-positive estimates (indicating dispersion incompatible with a
#'   beta-binomial distribution) are returned as-is with a warning.
#'
#' @export
betaCoefficients <- function(x, n = NULL) {
  if (length(x) < 2 || any(!is.finite(x))) {
    stop("'x' must contain at least two finite values.")
  }
  if (is.null(n)) {
    n <- max(x)
  }
  if (max(x) > n) {
    stop("'x' contains values larger than n = ", n, ".")
  }

  m <- mean(x)
  s <- sd(x)
  v <- s^2

  m2 <- m * m
  m3 <- m2 * m
  denom <- n * v - n * m + m2

  if (m <= 0 || m >= n || !is.finite(denom) || denom == 0) {
    warning("Method of moments is degenerate (mean at the boundary of the ",
            "support or zero moment denominator). Returning NA for alpha ",
            "and beta.")
    return(c(NA_real_, NA_real_, m, s, n))
  }

  a <- (m2 * n - m3 - m * v) / denom
  b <- a * ((n - m) / m)

  if (a <= 0 || b <= 0) {
    warning("Method of moments produced non-positive parameter estimates. ",
            "The dispersion of 'x' is incompatible with a beta-binomial ",
            "distribution.")
  }

  c(a, b, m, s, n)
}


# --------------------------------------------------------------------------
# Norm tables and prediction of norm scores
# --------------------------------------------------------------------------

#' Calculate Cumulative Probabilities, Density, Percentiles, and Z-Scores for
#' Beta-Binomial Distribution
#'
#' This function generates a norm table for specific ages based on the beta
#' binomial regression model. In case a confidence coefficient (CI, default
#' .9) and the reliability is specified, confidence intervals are computed
#' for the true score estimates, including a correction for regression to
#' the mean (Eid & Schmidt, 2012, p. 272).
#'
#' @param model The model, fitted with \code{\link{cnorm.betabinomial}}.
#' @param ages A numeric vector of age points at which to compute the tables.
#' @param n The number of items resp. the maximum score.
#' @param m An optional stop criterion in table generation. Positive integer
#'   lower than n. Please note: The probabilities are always computed on the
#'   full support 0 to n; m only truncates the output table.
#' @param range The range of the norm scores in standard deviations.
#'   Default is 3. Thus, scores in the range of +/- 3 standard deviations
#'   are considered.
#' @param CI Confidence coefficient, ranging from 0 to 1, default .9.
#' @param reliability Reliability coefficient, ranging between 0 to 1.
#'
#' @return A list of data frames with columns: x, Px, Pcum, Percentile, z,
#'   norm score and possibly confidence intervals.
#' @export
normTable.betabinomial <- function(model,
                                   ages,
                                   n = NULL,
                                   m = NULL,
                                   range = 3,
                                   CI = .9,
                                   reliability = NULL) {
  if (!isBeta(model)) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial' or 'cnormBetaBinomial2'.")
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
      se <- qnorm(1 - ((1 - CI) / 2)) * sqrt(reliability * (1 - reliability))
      rel <- TRUE
    }
  }

  if (is.null(n)) {
    n <- attr(model$result, "max")
  }

  if (is.null(m) || m > n) {
    m <- n
  }

  if (inherits(model, "cnormBetaBinomial")) {
    predictions <- predictCoefficients(model, ages, n)
  } else {
    predictions <- predictCoefficients2(model, ages, n)
  }

  a <- predictions$a
  b <- predictions$b

  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  result <- vector("list", length(a))

  for (k in seq_along(a)) {
    # IMPORTANT: probabilities are computed on the full support 0:n;
    # truncation at m happens afterwards without renormalization
    dist <- bb_distribution(a[k], b[k], n)

    # Clip via pmin/pmax instead of logical subscript assignment.
    z <- pmin(pmax(qnorm(dist$perc), -range), range)

    norm <- rep(NA_real_, length(z))
    if (!is.na(mScale) && !is.na(sdScale)) {
      norm <- mScale + sdScale * z
    }

    df <- data.frame(
      x = dist$x,
      Px = dist$Px,
      Pcum = dist$cum,
      Percentile = dist$perc * 100,
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

    # Truncate output table at m (no renormalization!)
    result[[k]] <- df[df$x <= m, , drop = FALSE]
  }

  names(result) <- ages
  result
}

#' Predict Norm Scores from Raw Scores
#'
#' This function calculates norm scores based on raw scores, age, and a
#' fitted cnormBetaBinomial model.
#'
#' @param object A fitted model object of class 'cnormBetaBinomial' or
#'   'cnormBetaBinomial2'.
#' @param ... Additional arguments passed to the prediction method:
#'   \itemize{
#'      \item age A numeric vector of ages, same length as score.
#'      \item score A numeric vector of raw scores.
#'      \item range The range of the norm scores in standard deviations.
#'        Default is 3. Thus, scores in the range of +/- 3 standard
#'        deviations are considered.
#'    }
#'
#' @return A numeric vector of norm scores (or percentiles, if no norm scale
#'   was specified in the model). Missing, non-integer or out-of-range raw
#'   scores yield \code{NA}.
#'
#' @details
#' The function first predicts the alpha and beta parameters of the
#' beta-binomial distribution for each unique age using the provided model.
#' It then calculates the mid-p cumulative probability for each raw score
#' given these parameters and converts these probabilities to the norm scale
#' specified in the model. The distribution is computed only once per unique
#' age value, which considerably speeds up predictions for grouped data.
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

  if ("age" %in% names(args)) {
    age <- args$age
  } else if (length(args) > 0) {
    age <- args[[1]]
  } else {
    age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else if (length(args) > 1) {
    score <- args[[2]]
  } else {
    score <- NULL
  }
  if ("range" %in% names(args)) {
    range <- args$range
  } else if (length(args) > 2) {
    range <- args[[3]]
  } else {
    range <- 3
  }

  if (!isBeta(model)) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial' or 'cnormBetaBinomial2'.")
  }

  if (is.null(age) || is.null(score)) {
    stop("Both 'age' and 'score' must be provided.")
  }

  if (length(age) != length(score)) {
    stop("The lengths of 'age' and 'score' must be the same.")
  }

  n <- attr(model$result, "max")

  # Compute distribution parameters only once per unique age
  unique_ages <- unique(age)
  idx <- match(age, unique_ages)

  if (inherits(model, "cnormBetaBinomial")) {
    predictions <- predictCoefficients(model, unique_ages, n)
  } else {
    predictions <- predictCoefficients2(model, unique_ages, n)
  }

  # Validate scores
  valid <- is.finite(score) & score >= 0 & score <= n & score == floor(score)
  if (any(!valid)) {
    warning("Some raw scores are missing, non-integer or outside the range 0 to ",
            n, ". Returning NA for these cases.")
  }

  z_scores <- rep(NA_real_, length(age))

  for (u in seq_along(unique_ages)) {
    rows <- which(idx == u & valid)
    if (length(rows) == 0) next

    dist <- bb_distribution(predictions$a[u], predictions$b[u], n)
    z_scores[rows] <- qnorm(dist$perc[score[rows] + 1])
  }

  z_scores <- pmin(pmax(z_scores, -range), range)

  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  if (!is.na(mScale) && !is.na(sdScale)) {
    return(mScale + sdScale * z_scores)
  } else {
    return(pnorm(z_scores) * 100)
  }
}

#' Predict Norm Scores from Raw Scores
#'
#' This function calculates norm scores based on raw scores, age, and a
#' fitted cnormBetaBinomial2 model. See
#' \code{\link{predict.cnormBetaBinomial}} for details.
#'
#' @param object A fitted model object of class 'cnormBetaBinomial' or
#'   'cnormBetaBinomial2'.
#' @param ... Additional arguments passed to the prediction method:
#'   \itemize{
#'      \item age A numeric vector of ages, same length as score.
#'      \item score A numeric vector of raw scores.
#'      \item range The range of the norm scores in standard deviations.
#'        Default is 3.
#'    }
#'
#' @return A numeric vector of norm scores.
#'
#' @export
#' @family predict
predict.cnormBetaBinomial2 <- predict.cnormBetaBinomial


# --------------------------------------------------------------------------
# Plotting
# --------------------------------------------------------------------------

#' Plot cnormBetaBinomial Model with Data and Percentile Lines
#'
#' This function creates a visualization of a fitted cnormBetaBinomial model,
#' including the original data points, the manifest percentiles (diamond-shaped
#' dots per age group) and the model-implied percentile lines.
#'
#' By default (\code{discrete = TRUE}), the percentile lines represent the
#' exact quantiles of the fitted discrete beta-binomial distribution and are
#' therefore step functions. Setting \code{discrete = FALSE} draws smooth
#' lines based on the quantiles of the underlying beta (mixing) distribution.
#' Note that this continuous approximation omits the binomial stage of the
#' variance and therefore displays less spread than the fitted model actually
#' implies, particularly in the outer percentiles. Use it only for visual
#' comparability with continuous modelling approaches.
#'
#' Cases with non-finite values (NA, NaN, Inf) in age, score or weights are
#' removed prior to plotting, mirroring the behavior of the fitting functions.
#'
#' @param x A fitted model object of class "cnormBetaBinomial" or
#'   "cnormBetaBinomial2".
#' @param ... Additional arguments passed to the plot method.
#'   \itemize{
#'      \item age A vector of the age data.
#'      \item score A vector of the score data.
#'      \item weights An optional numeric vector of weights for each observation.
#'      \item percentiles An optional vector with the percentiles to plot.
#'      \item points Logical indicating whether to plot the data points.
#'        Default is TRUE.
#'      \item discrete Logical indicating whether to plot the exact discrete
#'        beta-binomial quantiles (TRUE, default) or a smooth continuous
#'        approximation via the underlying beta distribution (FALSE).
#'    }
#'
#' @return A ggplot object.
#'
#' @family plot
#' @export
plot.cnormBetaBinomial <- function(x, ...) {
  model <- x
  args <- list(...)

  if ("age" %in% names(args)) {
    age <- args$age
  } else if (length(args) > 0) {
    age <- args[[1]]
  } else {
    age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else if (length(args) > 1) {
    score <- args[[2]]
  } else {
    score <- NULL
  }
  weights <- if ("weights" %in% names(args)) args$weights else NULL
  percentiles <- if ("percentiles" %in% names(args)) args$percentiles
  else c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  points <- if ("points" %in% names(args)) args$points else TRUE
  discrete <- if ("discrete" %in% names(args)) args$discrete else TRUE

  if (is.null(age) || is.null(score)) {
    stop("Please provide 'age' and 'score' vectors.")
  }

  if (!isBeta(model)) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial' or 'cnormBetaBinomial2'.")
  }

  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (!is.null(weights) && length(weights) != length(age)) {
    stop("Length of 'weights' must match length of 'age' and 'score'.")
  }

  # Remove non-finite cases (NA, NaN, Inf), mirroring the fitting functions.
  # Without this, weighted.quantile() propagates NA into the manifest
  # percentiles and the dots silently disappear from the plot.
  ok <- is.finite(age) & is.finite(score)
  if (!is.null(weights)) {
    ok <- ok & is.finite(weights)
  }
  if (!all(ok)) {
    message("Vector(s) contained non-finite values (NA, NaN, Inf). ",
            "These cases will be removed for plotting.")
    age <- age[ok]
    score <- score[ok]
    if (!is.null(weights)) {
      weights <- weights[ok]
    }
  }

  # Generate prediction points
  n_points <- 100
  data <- data.frame(age = age, score = score)
  data$w <- if (is.null(weights)) rep(1, nrow(data)) else weights

  age_range <- range(age)
  pred_ages <- seq(age_range[1], age_range[2], length.out = n_points)
  n_max <- attr(model$result, "max")

  # Get predictions
  if (inherits(model, "cnormBetaBinomial")) {
    preds <- predictCoefficients(model, pred_ages, n_max)
  } else {
    preds <- predictCoefficients2(model, pred_ages, n_max)
  }

  if (discrete) {
    # Exact quantiles of the fitted discrete beta-binomial distribution
    percentile_matrix <- vapply(seq_along(pred_ages), function(j) {
      dist <- bb_distribution(preds$a[j], preds$b[j], n_max)
      if (anyNA(dist$cum)) {
        return(rep(NA_real_, length(percentiles)))
      }
      vapply(percentiles,
             function(p) as.numeric(dist$x[which.max(dist$cum >= p)]),
             numeric(1))
    }, numeric(length(percentiles)))
    percentile_values <- t(percentile_matrix)   # rows: ages, cols: percentiles
  } else {
    # Continuous approximation via the underlying beta (mixing) distribution;
    # omits the binomial-stage variance and compresses outer percentiles
    percentile_values <- sapply(percentiles, function(p) {
      qbeta(p, shape1 = preds$a, shape2 = preds$b) * n_max
    })
  }

  # Wide-format prediction data, consistent with plot.cnormShash
  percentile_data <- as.data.frame(percentile_values)
  colnames(percentile_data) <- paste0("P", percentiles * 100)

  plot_data <- data.frame(
    age = pred_ages,
    mu = preds$mu,
    sigma = preds$sigma,
    percentile_data,
    check.names = FALSE
  )

  # Create the plot
  p <- ggplot()

  if (points) {
    p <- p + geom_point(
      data = data,
      aes(x = age, y = score),
      alpha = 0.2,
      size = 0.6
    )
  }

  # Grouping for manifest percentiles
  if (length(age) / length(unique(age)) > 50 && min(table(data$age)) > 30) {
    data$group <- age
  } else {
    data$group <- getGroups(age)
  }

  # Limit to max 30 groups for better visibility
  if (length(unique(data$group)) > 30) {
    data$group <- getGroups(age, n = 30)
  }

  # Get actual percentiles
  NAMES <- paste("PR", percentiles * 100, sep = "")
  percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data$group), function(df) {
    c(age = mean(df$age),
      weighted.quantile(df$score, probs = percentiles, weights = df$w))
  })))
  colnames(percentile.actual) <- c("age", NAMES)
  manifest_data <- percentile.actual

  # Add percentile lines and points
  # Discrete quantiles are piecewise constant in age: render them with
  # geom_step (vertical risers) instead of geom_line, which would draw
  # sheared ramps between adjacent prediction ages.
  for (i in seq_along(percentiles)) {
    if (discrete) {
      p <- p + geom_step(
        data = plot_data,
        aes(
          x = .data$age,
          y = .data[[paste0("P", percentiles[i] * 100)]],
          color = !!NAMES[i]
        ),
        direction = "mid",
        linewidth = 0.6
      )
    } else {
      p <- p + geom_line(
        data = plot_data,
        aes(
          x = .data$age,
          y = .data[[paste0("P", percentiles[i] * 100)]],
          color = !!NAMES[i]
        ),
        linewidth = 0.6
      )
    }
    p <- p + geom_point(
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
    scale_y_continuous(limits = c(0, n_max)) +
    scale_color_manual(
      values = setNames(rainbow(length(percentiles)), NAMES),
      breaks = NAMES,
      labels = paste0(percentiles * 100, "%")
    ) +
    guides(color = guide_legend(override.aes = list(
      linetype = rep("solid", length(NAMES)),
      shape = rep(18, length(NAMES))
    ))) +
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

  p
}

#' Plot cnormBetaBinomial2 Model with Data and Percentile Lines
#'
#' This function creates a visualization of a fitted cnormBetaBinomial2
#' model, including the original data points, manifest percentiles and
#' specified percentile lines. See \code{\link{plot.cnormBetaBinomial}}.
#'
#' @param x A fitted model object of class "cnormBetaBinomial" or
#'   "cnormBetaBinomial2".
#' @param ... Additional arguments passed to the plot method.
#'   \itemize{
#'      \item age A vector of the age data.
#'      \item score A vector of the score data.
#'      \item weights An optional numeric vector of weights for each observation.
#'      \item percentiles An optional vector with the percentiles to plot.
#'      \item points Logical indicating whether to plot the data points.
#'        Default is TRUE.
#'      \item discrete Logical indicating whether to plot the exact discrete
#'        beta-binomial quantiles (TRUE, default) or a smooth continuous
#'        approximation via the underlying beta distribution (FALSE).
#'    }
#'
#' @return A ggplot object.
#'
#' @family plot
#' @export
plot.cnormBetaBinomial2 <- plot.cnormBetaBinomial


# --------------------------------------------------------------------------
# Diagnostics and summary
# --------------------------------------------------------------------------

#' Diagnostic Information for Beta-Binomial Model
#'
#' This function provides diagnostic information for a fitted beta-binomial
#' model from the cnorm.betabinomial function. It returns various metrics
#' related to model convergence, fit, and complexity. In case age and raw
#' scores are provided, the function as well computes R2, RMSE and bias for
#' the norm scores based on the manifest and predicted norm scores.
#'
#' @param model An object of class "cnormBetaBinomial" or
#'   "cnormBetaBinomial2", typically the result of a call to
#'   cnorm.betabinomial().
#' @param age An optional vector with age values.
#' @param score An optional vector with raw values.
#' @param weights An optional vector with weights.
#'
#' @return A list containing diagnostic information, including convergence,
#'   number of evaluations, log-likelihood, AIC, BIC, the maximum absolute
#'   analytic gradient at the solution (first-order optimality), parameter
#'   estimates with standard errors, z and p values, and - if age and score
#'   are provided - R2, RMSE and bias of the norm scores.
#'
#' @details
#' The AIC and BIC are calculated as:
#' AIC = 2k - 2ln(L)
#' BIC = ln(n)k - 2ln(L)
#' where k is the number of parameters, L is the maximum likelihood, and n
#' is the number of observations.
#'
#' Please note that fit indices of mode 1 and mode 2 models are not directly
#' comparable, since the two modes maximize different likelihoods (normal
#' on standardized scores vs. discrete beta-binomial).
#'
#' @examples
#' \dontrun{
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw)
#' diag_info <- diagnostics.betabinomial(model)
#' print(diag_info)
#' summary(diag_info)
#'
#' if(diag_info$converged) {
#'   cat("Model converged successfully.\n")
#' } else {
#'   cat("Warning: Model did not converge.\n")
#' }
#'
#' cat("AIC:", diag_info$AIC, "\n")
#' cat("BIC:", diag_info$BIC, "\n")
#' }
#'
#' @export
diagnostics.betabinomial <- function(model,
                                     age = NULL,
                                     score = NULL,
                                     weights = NULL) {
  if (!isBeta(model)) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial' or 'cnormBetaBinomial2'.")
  }

  opt_results <- model$result

  if (inherits(model, "cnormBetaBinomial")) {
    type <- "cnormBetaBinomial"
    n_params <- length(model$beta_est) + length(model$gamma_est)
    param_names <- c(paste0("beta_", 0:(length(model$beta_est) - 1)),
                     paste0("gamma_", 0:(length(model$gamma_est) - 1)))
    estimates <- c(model$beta_est, model$gamma_est)
  } else {
    type <- "cnormBetaBinomial2"
    n_params <- length(model$alpha_est) + length(model$beta_est)
    param_names <- c(paste0("alpha_", 0:(length(model$alpha_est) - 1)),
                     paste0("beta_", 0:(length(model$beta_est) - 1)))
    estimates <- c(model$alpha_est, model$beta_est)
  }

  n_obs <- attr(model$result, "N")
  convergence <- opt_results$convergence == 0

  # First-order optimality: max absolute analytic gradient at the solution
  # (stored by the fitting functions)
  max_gradient <- NA
  if (is.numeric(opt_results$gradient) && length(opt_results$gradient) > 0) {
    max_gradient <- max(abs(opt_results$gradient))
  }

  # Robust standard errors (Hessian may be singular or indefinite)
  se <- tryCatch({
    d <- diag(solve(opt_results$hessian))
    d[d < 0] <- NA_real_
    sqrt(d)
  }, error = function(e) {
    rep(NA_real_, length(estimates))
  })
  z_values <- estimates / se
  p_values <- 2 * (1 - pnorm(abs(z_values)))

  # Log-likelihood, AIC, and BIC
  log_likelihood <- -opt_results$value
  AIC <- 2 * n_params - 2 * log_likelihood
  BIC <- log(n_obs) * n_params - 2 * log_likelihood

  # Norm score recovery statistics, if data are provided
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
        weights = weights,
        scale = c(attr(model$result, "scaleMean"),
                  attr(model$result, "scaleSD"))
      )
      norm_scores <- predict(model, data$group, data$raw)
    } else {
      data <- data.frame(age = age, raw = score)
      data$groups <- getGroups(age)
      width <- (max(age) - min(age)) / length(unique(data$groups))
      data <- rankBySlidingWindow(
        data,
        age = "age",
        raw = "raw",
        width = width,
        weights = weights,
        scale = c(attr(model$result, "scaleMean"),
                  attr(model$result, "scaleSD"))
      )
      norm_scores <- predict(model, data$age, data$raw)
    }

    norm_manifest <- data$normValue
    R2 <- cor(norm_scores, norm_manifest, use = "pairwise.complete.obs")^2
    rmse <- sqrt(mean((norm_scores - norm_manifest)^2, na.rm = TRUE))
    bias <- mean(norm_scores - norm_manifest, na.rm = TRUE)
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

#' Summarize a Beta-Binomial Continuous Norming Model
#'
#' This function provides a summary of a fitted beta-binomial continuous
#' norming model, including model fit statistics, convergence information,
#' and parameter estimates.
#'
#' @param object An object of class "cnormBetaBinomial" or
#'   "cnormBetaBinomial2", typically the result of a call to
#'   \code{\link{cnorm.betabinomial}}.
#' @param ... Additional arguments passed to the summary method:
#'   \itemize{
#'      \item age An optional numeric vector of age values corresponding to
#'        the raw scores. If provided along with \code{score}, additional fit
#'        statistics (R-squared, RMSE, bias) will be calculated.
#'      \item score An optional numeric vector of raw scores. Must be
#'        provided if \code{age} is given.
#'      \item weights An optional numeric vector of weights for each
#'        observation.
#'    }
#'
#' @return Invisibly returns a list containing detailed diagnostic
#'   information about the model. The function primarily produces printed
#'   output summarizing the model.
#'
#' @details
#' The summary includes:
#' \itemize{
#'   \item Basic model information (type, number of observations, number of parameters)
#'   \item Model fit statistics (log-likelihood, AIC, BIC)
#'   \item R-squared, RMSE, and bias (if age and raw scores are provided)
#'         in comparison to manifest norm scores
#'   \item Convergence information, including the maximum absolute gradient
#'         at the solution (first-order optimality)
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

  if ("age" %in% names(args)) {
    age <- args$age
  } else if (length(args) > 0) {
    age <- args[[1]]
  } else {
    age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else if (length(args) > 1) {
    score <- args[[2]]
  } else {
    score <- NULL
  }
  if ("weights" %in% names(args)) {
    weights <- args$weights
  } else if (length(args) > 2) {
    weights <- args[[3]]
  } else {
    weights <- NULL
  }

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
  if (!is.na(diag$max_gradient)) {
    cat("  Max gradient:", round(diag$max_gradient, 6), "\n")
  }
  if (!is.null(diag$message)) {
    cat("  Message:", diag$message, "\n")
  }
  cat("\n")

  cat("Parameter Estimates:\n")
  param_table <- data.frame(
    Estimate = diag$param_estimates,
    `Std. Error` = diag$param_se,
    `z value` = diag$z_values,
    `Pr(>|z|)` = diag$p_values,
    check.names = FALSE
  )
  print(param_table, digits = 4)

  invisible(diag)
}

#' Summarize a Beta-Binomial Continuous Norming Model
#'
#' Summary method for objects of class "cnormBetaBinomial2". See
#' \code{\link{summary.cnormBetaBinomial}} for details.
#'
#' @param object An object of class "cnormBetaBinomial" or
#'   "cnormBetaBinomial2", typically the result of a call to
#'   \code{\link{cnorm.betabinomial}}.
#' @param ... Additional arguments passed to the summary method:
#'   \itemize{
#'      \item age An optional numeric vector of age values.
#'      \item score An optional numeric vector of raw scores.
#'      \item weights An optional numeric vector of weights.
#'    }
#'
#' @return Invisibly returns a list containing detailed diagnostic
#'   information about the model.
#'
#' @seealso \code{\link{cnorm.betabinomial}}, \code{\link{diagnostics.betabinomial}}
#'
#' @export
summary.cnormBetaBinomial2 <- summary.cnormBetaBinomial


