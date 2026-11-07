# =============================================================================
# Conway-Maxwell-Poisson (CMP) regression for continuous norming of count data
# (e.g., speeded tests). Analogue to the shash and beta-binomial modules.
# =============================================================================

#' Admissible range of log(nu): nu in [exp(-3), exp(3)] = [0.05, 20.1].
#' The linear predictor of log(nu) is clamped to this range (cf. delta in shash).
#' The clamp acts on the predictor, not on individual coefficients.
#' @noRd
CMP_LOG_NU_RANGE <- c(-3, 3)

#' Fit a Conway-Maxwell-Poisson (CMP) Regression Model for Continuous Norming
#'
#' This function fits a Conway-Maxwell-Poisson regression model (Conway & Maxwell, 1962;
#' Shmueli et al., 2005) for continuous norming of count data, e.g., speeded tests where
#' the raw score is the number of correctly processed items within a time limit. Both
#' the location parameter mu(age) and the dispersion parameter nu(age) vary smoothly as
#' polynomial functions of age (or any other continuous predictor). In contrast to the
#' Poisson model, the CMP distribution does not force variance = mean: it can model
#' over-dispersion (nu < 1), equi-dispersion (nu = 1, Poisson) and under-dispersion
#' (nu > 1).
#'
#' @section Parameterization:
#' The probability mass function is
#' \deqn{P(Y = y) = \frac{1}{Z(\mu, \nu)} \left(\frac{\mu^y}{y!}\right)^\nu, \quad
#'   Z(\mu, \nu) = \sum_{j=0}^{\infty} \left(\frac{\mu^j}{j!}\right)^\nu}
#' i.e., the Poisson kernel raised to the power nu. This is the classical CMP
#' distribution with rate \eqn{\lambda = \mu^\nu}. \strong{Note:} the coefficients are
#' therefore \emph{not} those of \pkg{COMPoissonReg}, which models \eqn{\lambda} (or
#' the mean in its mean-parametrized variants) directly. Modelling \eqn{\mu} instead of
#' \eqn{\lambda} (i) makes mu interpretable as an approximate mean,
#' \eqn{E(Y) \approx \mu - (\nu - 1)/(2\nu)}, with \eqn{Var(Y) \approx \mu/\nu}
#' (the exact moments are available via \code{\link{predictMoments}}),
#' (ii) decouples the location and dispersion parameters, which stabilizes the optimization
#' and the standard errors, and (iii) yields trivial starting values (a Poisson regression).
#' The model is
#' \deqn{\log \mu(a) = \sum_{k=0}^{K_\mu} \beta_k a^k, \qquad
#'       \log \nu(a) = \sum_{k=0}^{K_\nu} \gamma_k a^k}
#' with standardized age \code{a = (age - mean(age)) / sd(age)}.
#' The coefficients refer to the log scale; only the intercepts refer to the mean age.
#'
#' @section Right-truncated model:
#' Many speeded tests have a finite number of items on the test sheet. If \code{max_score}
#' is given, the CMP distribution is truncated to \eqn{\{0, 1, \dots, M\}} with
#' \eqn{M} = \code{max_score} (the normalizing constant is summed up to \eqn{M} only), so that no
#' probability mass is assigned to impossible scores and percentile ranks are properly
#' normalized at the ceiling. In this case, mu and nu are the parameters of the
#' \emph{truncated} distribution and the approximations of mean and variance above no
#' longer hold near the ceiling; use \code{\link{predictMoments}} for exact moments.
#'
#' @section Shape flexibility:
#' For large mu, the skewness of the CMP distribution is approximately
#' \eqn{1/\sqrt{\mu\nu}}, i.e., the shape is nearly determined by mean and variance. If
#' skewness or kurtosis vary independently across age, the SHASH model
#' (\code{\link{cnorm.shash}}) or the distribution-free approach are more flexible.
#'
#' @section Name collision:
#' \pkg{COMPoissonReg} also exports functions named \code{dcmp}, \code{pcmp}, \code{qcmp}
#' and \code{rcmp}, but with the arguments \code{(lambda, nu)}. If both packages are
#' attached, the one attached last masks the other and calls such as
#' \code{dcmp(x, 8, 1)} are silently interpreted with a different parameterization. Use
#' the explicit \code{cNORM::dcmp()} or \code{COMPoissonReg::dcmp()} in this case.
#'
#' @param age A numeric vector of predictor values (typically age, but can be any continuous predictor).
#' @param score A numeric vector of raw scores. Must be the same length as age and must
#'   consist of non-negative integers (counts).
#' @param weights An optional numeric vector of non-negative weights for each observation.
#'   If NULL (default), all observations are weighted equally. Weights enter the
#'   likelihood as multipliers (e.g., post-stratification weights). AIC, BIC and the
#'   Hessian-based standard errors are strictly valid only for frequency weights; the
#'   sample size used in BIC is the number of rows.
#' @param mu_degree Integer (>= 1) specifying the degree of the polynomial for log(mu(age)).
#'   Default is 3. Note that the polynomial acts on the log scale, so lower degrees
#'   than in the Taylor or shash approaches are often sufficient.
#' @param nu_degree Integer specifying the degree of the polynomial for log(nu(age)).
#'   Default is 2. Use \code{0} for a constant but estimated dispersion, and \code{NULL}
#'   to fix the dispersion at the value given in \code{nu} (\code{nu = 1} yields a
#'   Poisson regression). Recommendation: keep \code{nu_degree} low to avoid overfitting.
#' @param nu Fixed dispersion parameter (must be > 0), used only if \code{nu_degree} is NULL.
#'   Default is 1. Values < 1: over-dispersion; > 1: under-dispersion.
#' @param control An optional list of control parameters passed to \code{optim}. Any
#'   parameters not specified are filled with defaults:
#'   \itemize{
#'     \item \code{factr}: Controls precision of optimization (default: 1e3). A less extreme
#'       value than in the shash module is used because the normalizing constant is evaluated
#'       with a finite truncation tolerance, which limits attainable function precision.
#'     \item \code{maxit}: Maximum number of iterations (default: n_parameters * 200)
#'     \item \code{lmm}: Memory limit for L-BFGS-B (default: min(n_parameters, 20))
#'   }
#' @param scale Character string or numeric vector specifying the type of norm scale for output.
#'   This affects the scaling of derived norm scores but does not influence model fitting:
#'   \itemize{
#'     \item "T": T-scores (mean = 50, SD = 10) - default
#'     \item "IQ": IQ-like scores (mean = 100, SD = 15)
#'     \item "z": z-scores (mean = 0, SD = 1)
#'     \item c(M, SD): Custom scale with specified mean M and standard deviation SD
#'   }
#' @param max_terms Maximum number of terms used to evaluate the normalizing constant
#'   Z(mu, nu). Default (NULL) is \code{max(2000, 20 * max(score))}. Increase only if
#'   a warning indicates that the series did not converge. The value is stored in the
#'   model and also applied in \code{predict}, \code{normTable} and \code{plot}.
#' @param tol Relative truncation tolerance for the normalizing constant (default 1e-12).
#'   Also stored and re-used in all downstream functions.
#' @param plot Logical indicating whether to automatically display a diagnostic plot of the
#'   fitted model. Default is TRUE.
#' @param max_score Optional positive integer: the maximum attainable raw score (e.g., the
#'   number of items on the sheet). If given, the distribution is right-truncated at this
#'   value (see section "Right-truncated model"). Default \code{NULL}: no ceiling.
#' @param start Optional list with numeric vectors \code{mu} and (optionally) \code{nu}
#'   containing starting values for the polynomial coefficients (on the scale of
#'   \code{mu_est} / \code{nu_est}, i.e., of a previously fitted model). Shorter vectors
#'   are padded with zeros, which allows warm starts from lower polynomial degrees.
#'   If the optimization fails from these values, the default starting values are used.
#'
#' @return An object of class "cnormCMP" containing the fitted model results. This is a list with:
#'   \item{mu_est}{Coefficients of the polynomial for log(mu(age)). The first coefficient is the intercept.}
#'   \item{nu_est}{Coefficients of the polynomial for log(nu(age)); NULL if nu is fixed.}
#'   \item{nu}{The fixed dispersion value (relevant if \code{nu_degree} is NULL).}
#'   \item{se}{Standard errors of all estimated coefficients (NA where they could not be computed).}
#'   \item{mu_degree, nu_degree}{The polynomial degrees used.}
#'   \item{max_score}{The ceiling used for right-truncation (NULL if none).}
#'   \item{at_bound}{Logical; TRUE if the predictor of log(nu) reached the admissible range
#'     at an observed age (standard errors and Wald tests are then not valid).}
#'   \item{vcov}{Covariance matrix of the coefficients (mu coefficients first); NULL if unavailable.}
#'   \item{result}{Output from \code{optim} and the data attributes needed for prediction.
#'     \emph{Note:} the optimizer works in an orthogonalized (QR) parameterization of the
#'     polynomials for numerical stability; \code{result$par} and \code{result$hessian}
#'     refer to this parameterization. Use \code{mu_est}, \code{nu_est}, \code{se} and
#'     \code{vcov} for coefficients on the usual polynomial scale.}
#'
#' @details
#' Parameters are estimated by maximum likelihood with L-BFGS-B and analytic gradients.
#' The CMP distribution belongs to the exponential family in (log lambda, -nu), so the
#' score equations are simple moment conditions: the gradient with respect to the linear
#' predictors only requires E(Y) and E(log Y!) under the current CMP distribution, which
#' are obtained from the same truncated series as the normalizing constant. Series
#' evaluation is vectorized, truncation is adaptive with a rigorous geometric tail bound,
#' and observations sharing the same (mu, nu) (e.g., age groups) are evaluated only once.
#'
#' \subsection{Differences to the beta-binomial and shash models}{
#' The (untruncated) CMP distribution has an unbounded support. It is appropriate for
#' speeded tests and other open-ended counts. For tests with a fixed number of items and
#' a hard ceiling (accuracy tests), the beta-binomial model respects this bound, whereas
#' the untruncated CMP model assigns (small) probability mass to scores above the ceiling.
#' Use \code{max_score} for speeded tests with a finite number of items.
#' }
#'
#' @note
#' \itemize{
#'   \item Raw scores must be non-negative integers.
#'   \item The dispersion is restricted to nu in [0.05, 20] by clamping the predictor of
#'     log(nu).
#'   \item Polynomial models can exhibit edge effects; predict outside the observed age range
#'     with caution.
#'   \item Because the distribution is discrete, norm tables are step-like in the raw score.
#'   \item AIC/BIC are not comparable between the discrete CMP model and continuous
#'     (shash) or distribution-free models.
#' }
#'
#' @seealso \code{\link{cnorm.shash}}, \code{\link{autoselect.cmp}},
#'   \code{\link{normTable.cmp}}, \code{\link{dcmp}}, \code{\link{predictMoments}}
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' model <- cnorm.cmp(age = speed$age, score = speed$fluency)
#'
#' # Constant but estimated dispersion
#' model0 <- cnorm.cmp(speed$age, speed$fluency, mu_degree = 3, nu_degree = 0)
#'
#' # Poisson regression (dispersion fixed to 1)
#' model_pois <- cnorm.cmp(speed$age, speed$fluency, nu_degree = NULL, nu = 1)
#'
#' # Right-truncated at the number of items on the sheet
#' model_t <- cnorm.cmp(speed$age, speed$fluency, max_score = 80)
#'
#' summary(model, age = speed$age, score = speed$fluency)
#' AIC(model); BIC(model)
#' normTable.cmp(model, ages = c(8, 9), start = 0, end = 80)
#' }
#'
#' @author Wolfgang Lenhard
#' @references
#' Conway, R. W., & Maxwell, W. L. (1962). A queuing model with state dependent service rates.
#' *Journal of Industrial Engineering*, 12, 132-136.
#'
#' Shmueli, G., Minka, T. P., Kadane, J. B., Borle, S., & Boatwright, P. (2005). A useful
#' distribution for fitting discrete data: revival of the Conway-Maxwell-Poisson distribution.
#' *Journal of the Royal Statistical Society C*, 54(1), 127-142.
#'
#' Sellers, K. F., & Shmueli, G. (2010). A flexible regression model for count data.
#' *The Annals of Applied Statistics*, 4(2), 943-961.
#'
#' Huang, A. (2017). Mean-parametrized Conway-Maxwell-Poisson regression models for dispersed
#' counts. *Statistical Modelling*, 17(6), 359-380.
#'
#' @export
cnorm.cmp <- function(age,
                      score,
                      weights = NULL,
                      mu_degree = 3,
                      nu_degree = 2,
                      nu = 1,
                      control = NULL,
                      scale = "T",
                      max_terms = NULL,
                      tol = 1e-12,
                      plot = TRUE,
                      max_score = NULL,
                      start = NULL) {
  # ---- Input validation -------------------------------------------------
  if (!is.numeric(age) || !is.numeric(score)) {
    stop("'age' and 'score' must be numeric vectors.")
  }

  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      stop("'weights' must be a numeric vector.")
    }
    if (length(weights) != length(age)) {
      stop("Length of 'weights' must match length of 'age' and 'score'.")
    }
  }

  if (!is.numeric(mu_degree) || length(mu_degree) != 1L || !is.finite(mu_degree) ||
      mu_degree < 1 || mu_degree != round(mu_degree)) {
    stop("'mu_degree' must be a positive integer.")
  }

  if (!is.null(nu_degree) &&
      (!is.numeric(nu_degree) || length(nu_degree) != 1L || !is.finite(nu_degree) ||
       nu_degree < 0 || nu_degree != round(nu_degree))) {
    stop("'nu_degree' must be NULL (fixed nu) or a non-negative integer (0 = constant nu).")
  }

  if (!is.numeric(nu) || length(nu) != 1L || !is.finite(nu) || nu <= 0) {
    stop("'nu' must be a single positive number.")
  }

  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0 || tol >= 1) {
    stop("'tol' must be a single number in (0, 1).")
  }

  max_score <- cmp_check_max_score(max_score)

  if (!is.null(start) && !is.list(start)) {
    stop("'start' must be NULL or a list with elements 'mu' and (optionally) 'nu'.")
  }

  # validate 'scale' early with explicit errors. Accepts integer vectors as well.
  if (is.numeric(scale) && length(scale) == 2 && all(is.finite(scale)) && scale[2] > 0) {
    scaleM <- scale[1]
    scaleSD <- scale[2]
  } else if (is.character(scale) && length(scale) == 1) {
    scaleM <- switch(scale,
                     "T" = 50,
                     "IQ" = 100,
                     "z" = 0,
                     stop("Unknown scale '", scale,
                          "'. Use 'T', 'IQ', 'z', or a numeric vector c(M, SD)."))
    scaleSD <- switch(scale, "T" = 10, "IQ" = 15, "z" = 1)
  } else {
    stop("'scale' must be 'T', 'IQ', 'z', or a numeric vector c(M, SD) with SD > 0.")
  }

  # Prepare vectors
  vectors_to_check <- list(age = age, score = score)
  if (!is.null(weights)) {
    vectors_to_check$weights <- weights
  }

  # Check if filtering needed
  needs_filtering <- any(vapply(vectors_to_check, function(x) any(!is.finite(x)), logical(1)))

  if (needs_filtering) {
    message("Vector(s) contained non-finite values (NA, NaN, Inf). These cases will be removed.")
    tmp <- do.call(filter_complete, c(vectors_to_check, verbose = FALSE))
    age <- tmp[[1]]
    score <- tmp[[2]]
    if (!is.null(weights))
      weights <- tmp[[3]]
  }

  # The CMP model is a model for counts
  if (any(score < 0)) {
    stop("'score' contains negative values. The CMP model requires non-negative integer counts.")
  }
  if (any(abs(score - round(score)) > 1e-8)) {
    stop("'score' contains non-integer values. The CMP model requires integer counts. ",
         "For continuous raw scores, consider cnorm.shash().")
  }
  score <- round(score)

  if (!is.null(weights)) {
    if (any(weights < 0)) {
      stop("'weights' must be non-negative.")
    }
    if (sum(weights) <= 0) {
      stop("'weights' must have a positive sum.")
    }
  }

  if(is.null(max_score)){
    message("'max_score' not set. Assuming positively unbounded (open-ended) range of score values. Please set 'max_score' if there is an upper limit.")
  }

  if (!is.null(max_score) && any(score > max_score)) {
    stop("'score' contains values above 'max_score' (maximum observed: ", max(score),
         ", max_score: ", max_score, ").")
  }

  # Guard against degenerate input (zero/undefined variance)
  if (length(score) < 2L || !is.finite(stats::sd(score)) || stats::sd(score) <= 0) {
    stop("'score' has zero or undefined variance. The model cannot be fitted.")
  }
  age_sd <- stats::sd(age)
  if (!is.finite(age_sd) || age_sd <= 0) {
    stop("'age' has zero or undefined variance. The model cannot be fitted.")
  }
  k_max <- max(mu_degree, if (is.null(nu_degree)) 0 else nu_degree)
  if (length(unique(age)) <= k_max) {
    stop("Too few distinct age values (", length(unique(age)), ") for a polynomial of degree ",
         k_max, ". Reduce 'mu_degree' / 'nu_degree'.")
  }

  if (is.null(max_terms)) {
    max_terms <- max(2000L, as.integer(ceiling(20 * max(score))))
  }

  # ---- Standardize age (inline: identical formula is used in prediction) ---
  age_mean <- mean(age)
  age_std <- (age - age_mean) / age_sd

  # ---- Design matrices in an orthogonalized (QR) basis --------------------
  # Raw polynomials of degree >= 3 are strongly collinear. The optimizer therefore
  # works with Z = X T^{-1} (orthogonal columns); coefficients are mapped back below.
  X_mu <- cmp_design(age_std, mu_degree)
  b_mu <- cmp_basis(X_mu)
  Z_mu <- b_mu$Z
  if (!is.null(nu_degree)) {
    X_nu <- cmp_design(age_std, nu_degree)
    b_nu <- cmp_basis(X_nu)
    Z_nu <- b_nu$Z
    use_varying_nu <- TRUE
  } else {
    b_nu <- NULL
    Z_nu <- NULL
    use_varying_nu <- FALSE
  }
  fixed_nu <- if (use_varying_nu) NULL else nu
  n_mu <- ncol(Z_mu)

  # ---- Starting values -----------------------------------------------------
  # Default: Poisson regression for mu, moment-based start for nu
  initial_params <- cmp_start_values(Z_mu, Z_nu, score, weights)
  n_param <- length(initial_params)

  starts <- list()
  if (!is.null(start)) {
    s <- initial_params
    if (!is.null(start$mu)) {
      s[seq_len(n_mu)] <- drop(b_mu$T %*% cmp_pad(start$mu, n_mu))
    }
    if (use_varying_nu && !is.null(start$nu)) {
      s[n_mu + seq_len(ncol(Z_nu))] <- drop(b_nu$T %*% cmp_pad(start$nu, ncol(Z_nu)))
    }
    if (all(is.finite(s))) {
      starts[[length(starts) + 1L]] <- s
    }
  }
  starts[[length(starts) + 1L]] <- initial_params

  # Conservative last-resort start: constant location, nu = 1
  conservative <- initial_params
  conservative[seq_len(n_mu)] <- c(log(max(stats::median(score), 0.5)), rep(0, n_mu - 1L))
  if (use_varying_nu) {
    conservative[n_mu + seq_len(ncol(Z_nu))] <- 0
  }
  starts[[length(starts) + 1L]] <- conservative

  control_defaults <- list(factr = 1e3,
                           maxit = n_param * 200,
                           lmm = min(n_param, 20))
  control <- if (is.null(control))
    control_defaults
  else
    utils::modifyList(control_defaults, control)

  # Objective with analytic gradient; function value and gradient share one
  # evaluation of the series (cached for identical parameter vectors).
  # No box constraints: the admissible range of nu is enforced by clamping the
  # predictor of log(nu) inside the likelihood.
  obj <- cmp_make_objective(Z_mu, Z_nu, score, weights, fixed_nu, tol, max_terms, max_score)

  run_optim <- function(par0, ctrl) {
    tryCatch(
      optim(par0, obj$fn, gr = obj$gr, method = "L-BFGS-B", hessian = TRUE, control = ctrl),
      error = function(e) NULL
    )
  }
  invalid <- function(r) is.null(r) || !is.finite(r$value) || r$value >= 1e9

  result <- NULL
  for (i in seq_along(starts)) {
    ctrl <- control
    if (i > 1L && i == length(starts)) {
      # last resort: relaxed precision, more iterations
      ctrl$factr <- if (is.null(ctrl$factr)) 1e4 else ctrl$factr * 10
      ctrl$maxit <- if (is.null(ctrl$maxit)) 1000 else ctrl$maxit * 2
    }
    result <- run_optim(starts[[i]], ctrl)
    if (!invalid(result)) break
    if (i < length(starts)) {
      message("Optimization attempt ", i, " failed. Trying with different starting values...")
    }
  }

  if (invalid(result)) {
    stop("Optimization failed: the CMP likelihood could not be evaluated. ",
         "Try reducing the polynomial degrees, fixing nu (nu_degree = NULL) or increasing 'max_terms'.")
  }

  # Check convergence
  if (result$convergence != 0) {
    warning(
      "Optimization did not converge (code: ",
      result$convergence,
      "). Consider adjusting control parameters or degrees of the polynomials. Check percentile curves for plausibility."
    )
  }

  # ---- Back-transform coefficients to the polynomial scale ----------------
  par_Z <- result$par
  mu_Z <- par_Z[seq_len(n_mu)]
  nu_Z <- if (use_varying_nu) par_Z[n_mu + seq_len(ncol(Z_nu))] else NULL
  mu_est <- drop(backsolve(b_mu$T, mu_Z))
  nu_est <- if (use_varying_nu) drop(backsolve(b_nu$T, nu_Z)) else NULL

  # ---- Admissible range of nu: check the predictor at the observed ages ---
  nu_at_bound <- FALSE
  if (use_varying_nu) {
    eta_nu_obs <- drop(Z_nu %*% nu_Z)
    nu_at_bound <- any(eta_nu_obs <= CMP_LOG_NU_RANGE[1] + 1e-6 |
                         eta_nu_obs >= CMP_LOG_NU_RANGE[2] - 1e-6)
    if (nu_at_bound) {
      warning(
        "The predictor of log(nu) reached the admissible range [",
        round(exp(CMP_LOG_NU_RANGE[1]), 2), ", ", round(exp(CMP_LOG_NU_RANGE[2]), 1),
        "] at observed ages. The likelihood is flat there; standard errors and Wald tests ",
        "are not valid. Consider reducing the degree of the dispersion polynomial."
      )
    }
  }

  # ---- Standard errors on the polynomial scale -----------------------------
  # Hessian is symmetrized; covariance in the QR basis is mapped back via
  # Cov_raw = B Cov_Z B', B = blockdiag(T_mu^-1, T_nu^-1).
  H <- result$hessian
  H <- (H + t(H)) / 2
  result$hessian <- H

  se <- rep(NA_real_, n_param)
  vcov_raw <- NULL
  cov_Z <- tryCatch(solve(H), error = function(e) NULL)
  if (is.null(cov_Z) || any(!is.finite(cov_Z))) {
    warning("Could not compute standard errors: Hessian matrix issue")
  } else {
    B <- matrix(0, n_param, n_param)
    B[seq_len(n_mu), seq_len(n_mu)] <- backsolve(b_mu$T, diag(n_mu))
    if (use_varying_nu) {
      idx_nu <- n_mu + seq_len(ncol(Z_nu))
      B[idx_nu, idx_nu] <- backsolve(b_nu$T, diag(ncol(Z_nu)))
    }
    vcov_raw <- B %*% cov_Z %*% t(B)
    vcov_raw <- (vcov_raw + t(vcov_raw)) / 2
    d <- diag(vcov_raw)
    if (any(d < 0, na.rm = TRUE)) {
      warning("Hessian is not positive definite; standard errors are set to NA where invalid.")
      d[d < 0] <- NA
    }
    se <- sqrt(d)
  }

  # ---- Store attributes ------------------------------------------------------
  grad_final <- tryCatch(obj$gr(result$par), error = function(e) NA_real_)
  attr(result, "age_mean") <- age_mean
  attr(result, "age_sd") <- age_sd
  attr(result, "ageMin") <- min(age)
  attr(result, "ageMax") <- max(age)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- stats::sd(score)
  attr(result, "max") <- max(score)
  attr(result, "min") <- min(score)
  attr(result, "N") <- length(score)
  attr(result, "scaleMean") <- scaleM
  attr(result, "scaleSD") <- scaleSD
  attr(result, "nu") <- nu
  attr(result, "max_terms") <- max_terms
  attr(result, "tol") <- tol
  attr(result, "max_score") <- max_score
  attr(result, "weighted") <- !is.null(weights)
  attr(result, "max_abs_gradient") <- if (all(is.na(grad_final))) NA_real_ else max(abs(grad_final), na.rm = TRUE)

  # Create model object
  model <- list(
    mu_est = mu_est,
    nu_est = nu_est,
    nu = nu,
    se = se,
    mu_degree = mu_degree,
    nu_degree = nu_degree,
    max_score = max_score,
    at_bound = nu_at_bound,
    vcov = vcov_raw,
    result = result
  )

  class(model) <- "cnormCMP"

  if (plot) {
    p <- plot.cnormCMP(model, age, score, weights = weights)
    print(p)
  }

  return(model)
}

#' Conway-Maxwell-Poisson (CMP) Distribution
#'
#' Probability mass function, distribution function, quantile function and random
#' generation for the Conway-Maxwell-Poisson distribution in the (mu, nu)
#' parameterization used by \code{\link{cnorm.cmp}}:
#' \deqn{P(Y = y) \propto (\mu^y / y!)^\nu .}
#' The classical rate parameter is \eqn{\lambda = \mu^\nu}. For \code{nu = 1}, the
#' distribution is the Poisson distribution with mean \code{mu}.
#'
#' @section Name collision:
#' \pkg{COMPoissonReg} exports functions with the same names but the arguments
#' \code{(lambda, nu)}. If both packages are attached, the one attached last masks the
#' other, and the same call is silently interpreted with a different parameterization.
#' Use \code{cNORM::dcmp()} etc. explicitly, or convert with \code{lambda = mu^nu}.
#'
#' @name cmp
#' @aliases dcmp pcmp qcmp rcmp
#'
#' @param x,q vector of quantiles (non-negative integers for \code{dcmp}).
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param mu location parameter (> 0, default 1); approximately the mean,
#'   \eqn{E(Y) \approx \mu - (\nu - 1)/(2\nu)} (untruncated case).
#' @param nu dispersion parameter (> 0, default 1). \code{nu < 1}: over-dispersion
#'   (variance > mean); \code{nu > 1}: under-dispersion (variance < mean).
#' @param log,log.p logical; if TRUE, probabilities are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x].
#' @param max_score optional ceiling \eqn{M}. If finite, the distribution is right-truncated to
#'   \eqn{\{0, 1, \dots, M\}} (default \code{Inf}: no truncation).
#'
#' @details
#' The normalizing constant is evaluated by a vectorized, adaptively truncated series
#' with a rigorous geometric tail bound (relative error < 1e-12). The quantile function
#' is computed by inversion of the cumulative sums (the cumulative distribution is
#' computed only once per distinct pair (mu, nu)), and random numbers are generated by
#' inversion.
#'
#' @return \code{dcmp} gives the mass, \code{pcmp} the distribution function,
#' \code{qcmp} the quantile function, and \code{rcmp} generates random deviates.
#'
#' @references
#' Shmueli, G., Minka, T. P., Kadane, J. B., Borle, S., & Boatwright, P. (2005). A useful
#' distribution for fitting discrete data: revival of the Conway-Maxwell-Poisson distribution.
#' \emph{Journal of the Royal Statistical Society C}, 54(1), 127-142.
#'
#' @examples
#' # Equals the Poisson distribution for nu = 1
#' all.equal(dcmp(0:20, mu = 8, nu = 1), dpois(0:20, 8))
#'
#' # Under-dispersed (nu = 3) and over-dispersed (nu = 0.5) counts with similar mean
#' x <- rcmp(1000, mu = 20, nu = 3)
#' c(mean(x), var(x))
#' y <- rcmp(1000, mu = 20, nu = 0.5)
#' c(mean(y), var(y))
#'
#' qcmp(c(0.025, 0.5, 0.975), mu = 20, nu = 3)
#' pcmp(20, mu = 20, nu = 3)
#'
#' # Right-truncated at 25
#' sum(dcmp(0:25, mu = 20, nu = 1, max_score = 25))
#'
#' @export
#' @rdname cmp
dcmp <- function(x, mu = 1, nu = 1, log = FALSE, max_score = Inf) {
  if (length(x) == 0L || length(mu) == 0L || length(nu) == 0L)
    return(numeric(0))
  ms <- cmp_check_max_score(max_score)
  n <- max(length(x), length(mu), length(nu))
  x <- rep_len(x, n)
  mu <- rep_len(mu, n)
  nu <- rep_len(nu, n)

  out <- rep(NA_real_, n)
  bad <- is.na(x) | is.na(mu) | is.na(nu)
  nan <- !bad & (mu < 0 | nu <= 0)
  zero <- !bad & !nan & mu == 0
  calc <- !bad & !nan & mu > 0
  out[nan] <- NaN

  if (any(zero)) {
    out[zero] <- as.numeric(x[zero] == 0)
    if (log) out[zero] <- ifelse(x[zero] == 0, 0, -Inf)
  }

  if (any(calc)) {
    xc <- x[calc]
    eta <- base::log(mu[calc])
    nuc <- nu[calc]
    mom <- cmp_engine(eta, nuc, max_terms = cmp_max_terms(mu[calc]), max_score = ms)
    ld <- nuc * (xc * eta - lgamma(xc + 1)) - mom$logZ
    nonint <- abs(xc - round(xc)) > 1e-7 * pmax(1, abs(xc))
    if (any(nonint)) warning("non-integer x in dcmp")
    ld[nonint | xc < 0] <- -Inf
    if (!is.null(ms)) ld[xc > ms] <- -Inf
    if (any(!mom$ok)) {
      warning("CMP series did not converge for some parameter combinations; NaN produced.")
      ld[!mom$ok] <- NaN
    }
    out[calc] <- if (log) ld else exp(ld)
  }
  out
}

#' @export
#' @rdname cmp
pcmp <- function(q, mu = 1, nu = 1, lower.tail = TRUE, log.p = FALSE, max_score = Inf) {
  if (length(q) == 0L || length(mu) == 0L || length(nu) == 0L)
    return(numeric(0))
  ms <- cmp_check_max_score(max_score)
  n <- max(length(q), length(mu), length(nu))
  q <- rep_len(q, n)
  mu <- rep_len(mu, n)
  nu <- rep_len(nu, n)

  out <- rep(NA_real_, n)
  bad <- is.na(q) | is.na(mu) | is.na(nu)
  nan <- !bad & (mu < 0 | nu <= 0)
  zero <- !bad & !nan & mu == 0
  calc <- !bad & !nan & mu > 0
  out[nan] <- NaN

  if (any(zero)) {
    p0 <- as.numeric(q[zero] >= 0)
    out[zero] <- if (lower.tail) p0 else 1 - p0
  }

  if (any(calc)) {
    mom <- cmp_engine(base::log(mu[calc]), nu[calc], q = floor(q[calc]),
                      max_terms = cmp_max_terms(mu[calc]), max_score = ms)
    pr <- if (lower.tail) mom$lower else mom$upper
    if (any(!mom$ok)) {
      warning("CMP series did not converge for some parameter combinations; NaN produced.")
      pr[!mom$ok] <- NaN
    }
    out[calc] <- pr
  }

  if (log.p) base::log(out) else out
}

#' @export
#' @rdname cmp
qcmp <- function(p, mu = 1, nu = 1, lower.tail = TRUE, log.p = FALSE, max_score = Inf) {
  if (length(p) == 0L || length(mu) == 0L || length(nu) == 0L)
    return(numeric(0))
  ms <- cmp_check_max_score(max_score)
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  n <- max(length(p), length(mu), length(nu))
  p <- rep_len(p, n)
  mu <- rep_len(mu, n)
  nu <- rep_len(nu, n)

  out <- rep(NA_real_, n)
  bad <- is.na(p) | is.na(mu) | is.na(nu)
  nan <- !bad & (mu < 0 | nu <= 0 | p < 0 | p > 1)
  out[nan] <- NaN

  ok_in <- !bad & !nan
  out[ok_in & (mu == 0 | p == 0)] <- 0
  out[ok_in & mu > 0 & p == 1] <- if (is.null(ms)) Inf else ms

  calc <- ok_in & mu > 0 & p > 0 & p < 1
  if (any(calc)) {
    out[calc] <- cmp_quantile(p[calc], mu[calc], nu[calc], max_score = ms)
  }
  out
}

#' @export
#' @rdname cmp
rcmp <- function(n, mu = 1, nu = 1, max_score = Inf) {
  if (length(n) > 1L) n <- length(n)
  if (n == 0L) return(numeric(0))
  u <- stats::runif(n)
  qcmp(u, mu = rep_len(mu, n), nu = rep_len(nu, n), max_score = max_score)
}


# -----------------------------------------------------------------------------
# Internal computing engine
# -----------------------------------------------------------------------------

#' Validate the optional ceiling; returns NULL (no ceiling) or an integer
#' @keywords internal
#' @noRd
cmp_check_max_score <- function(max_score) {
  if (is.null(max_score)) return(NULL)
  if (!is.numeric(max_score) || length(max_score) != 1L || is.na(max_score) ||
      max_score < 1 || (is.finite(max_score) && max_score != round(max_score))) {
    stop("'max_score' must be NULL, Inf or a single positive integer.")
  }
  if (!is.finite(max_score)) return(NULL)
  as.integer(max_score)
}

#' Polynomial design matrix with intercept (degree 0 = intercept only)
#' @keywords internal
#' @noRd
cmp_design <- function(x, degree) {
  if (degree < 1L) {
    matrix(1, nrow = length(x), ncol = 1L)
  } else {
    cbind(1, outer(x, seq_len(degree), "^"))   # raw polynomial terms (NA-safe)
  }
}

#' Orthogonalized basis of a design matrix
#'
#' Returns Z and T with X = Z T, where Z has orthogonal columns scaled to unit mean
#' square (the first column is the constant 1) and T is upper triangular with a positive
#' diagonal. Coefficients in the two parameterizations are related by
#' beta_Z = T beta_raw, i.e., beta_raw = T^{-1} beta_Z.
#' @keywords internal
#' @noRd
cmp_basis <- function(X) {
  n <- nrow(X)
  p <- ncol(X)
  qrX <- qr(X, tol = 1e-10)
  if (qrX$rank < p || any(qrX$pivot != seq_len(p))) {
    stop("The polynomial design matrix is rank deficient. Reduce the polynomial degree.")
  }
  R <- qr.R(qrX)
  Q <- qr.Q(qrX)
  s <- sign(diag(R))
  s[s == 0] <- 1
  list(Z = sweep(Q, 2L, s, "*") * sqrt(n),
       T = (s * R) / sqrt(n))
}

#' Pad (or cut) a coefficient vector to a given length
#' @keywords internal
#' @noRd
cmp_pad <- function(v, len) {
  v <- as.numeric(v)
  if (length(v) < len) v <- c(v, rep(0, len - length(v)))
  v[seq_len(len)]
}

#' Default truncation limit for the series, depending on the largest mu
#' @param floor optional lower limit (e.g., the value stored in a fitted model)
#' @keywords internal
#' @noRd
cmp_max_terms <- function(mu, floor = NULL) {
  mu <- mu[is.finite(mu)]
  top <- if (length(mu)) max(mu) else 0
  mt <- max(2000, min(1e5, ceiling(20 * top)))
  if (!is.null(floor) && is.finite(floor)) mt <- max(mt, floor)
  as.integer(mt)
}

#' Group identical rows of one or more numeric key vectors
#'
#' @param keys List of numeric vectors of equal length.
#' @return List with \code{rep} (index of one representative per group) and
#'   \code{inv} (group index of every element).
#' @keywords internal
#' @noRd
cmp_group <- function(keys) {
  n <- length(keys[[1L]])
  if (n < 2L) {
    return(list(rep = seq_len(n), inv = seq_len(n)))
  }
  ord <- do.call(order, unname(keys))
  newgrp <- c(TRUE, rep(FALSE, n - 1L))
  for (k in keys) {
    ks <- k[ord]
    d <- ks[-1L] != ks[-n]
    d[is.na(d)] <- TRUE
    newgrp[-1L] <- newgrp[-1L] | d
  }
  inv <- integer(n)
  inv[ord] <- cumsum(newgrp)
  list(rep = ord[newgrp], inv = inv)
}

#' Row-wise cumulative sums of a matrix
#' @keywords internal
#' @noRd
cmp_row_cumsum <- function(E) {
  if (nrow(E) == 1L) {
    return(matrix(cumsum(E[1L, ]), nrow = 1L))
  }
  if (nrow(E) <= ncol(E)) {
    return(t(apply(E, 1L, cumsum)))
  }
  # many rows, few columns: one vectorized step per column
  for (j in seq_len(ncol(E))[-1L]) {
    E[, j] <- E[, j] + E[, j - 1L]
  }
  E
}

#' Evaluate the CMP series for a block of parameter pairs with a common truncation J
#'
#' With eta = log(mu), the log-terms are log t_j = nu * (j * eta - log(j!)), j = 0..J.
#' The terms are log-concave in j with maximum at floor(mu); they are therefore
#' normalized by the exact maximum (stable log-sum-exp). Since
#' t_{j+1} / t_j = (mu / (j + 1))^nu is decreasing in j, the neglected tail after J is
#' bounded by t_J * r / (1 - r) with r = (mu / (J + 1))^nu (valid for r < 1).
#' If \code{exact = TRUE} (right-truncated distribution with J = max_score), the sum is
#' finite and exact, so no tail bound is needed.
#'
#' @param eta,nu Vectors (length m) of log(mu) and nu.
#' @param J Highest summation index.
#' @param q Optional vector (length m): returns P(Y <= q) and P(Y > q).
#' @param p Optional vector (length m): returns the smallest x with P(Y <= x) >= p.
#' @param second Logical; also return E(Y^2).
#' @param tol Relative tolerance of the tail bound.
#' @param exact Logical; the series is the complete (finite) support.
#' @param full Logical; also return the complete pmf and cdf (m x (J + 1) matrices).
#'
#' @return A list with logZ, EY, ElgF (= E[log Y!]), ok and optionally EY2, lower, upper,
#'   quant, pmf, cdf.
#' @keywords internal
#' @noRd
cmp_block <- function(eta, nu, J, q = NULL, p = NULL, second = FALSE, tol = 1e-12,
                      exact = FALSE, full = FALSE) {
  m <- length(eta)
  js <- 0:J
  lg <- lgamma(js + 1)

  A <- outer(eta, js)                     # m x (J + 1): j * eta
  A <- (A - rep(lg, each = m)) * nu       # nu * (j * eta - log j!)

  jstar <- pmin(floor(exp(pmin(eta, 700))), J)
  mx <- nu * (jstar * eta - lgamma(jstar + 1))
  E <- exp(A - mx)                        # relative terms, maximum equals 1
  S <- rowSums(E)

  out <- list(
    logZ = mx + log(S),
    EY   = drop(E %*% js) / S,
    ElgF = drop(E %*% lg) / S
  )
  if (second) out$EY2 <- drop(E %*% (js^2)) / S

  if (exact) {
    out$ok <- is.finite(out$logZ)
  } else {
    # Rigorous bound of the truncated tail (relative to the sum)
    r <- exp(nu * (eta - log(J + 1)))
    tail_rel <- E[, J + 1L] * r / pmax(1 - r, 1e-300) / S
    tail_rel[!(r < 1)] <- Inf
    out$ok <- is.finite(out$logZ) & (tail_rel < tol)
  }

  if (!is.null(q)) {
    mask <- matrix(js, nrow = m, ncol = J + 1L, byrow = TRUE) <= q
    out$lower <- rowSums(E * mask) / S
    out$upper <- rowSums(E * (!mask)) / S
  }
  if (!is.null(p) || full) {
    Cm <- cmp_row_cumsum(E) / S
    if (full) {
      out$pmf <- E / S
      out$cdf <- Cm
    }
    if (!is.null(p)) {
      out$quant <- rowSums(Cm < p * (1 - 64 * .Machine$double.eps))
    }
  }
  out
}

#' Vectorized CMP series engine with adaptive truncation
#'
#' Rows are de-duplicated (identical mu, nu[, q or p] are evaluated once), binned by their
#' estimated required truncation (powers of 2), and processed in memory-bounded blocks.
#' Rows whose tail bound is not met are re-evaluated with doubled truncation up to
#' \code{max_terms}; rows failing there are flagged via \code{ok = FALSE}.
#' For a right-truncated distribution (\code{max_score} given), all rows are summed
#' exactly up to \code{max_score}.
#'
#' @param eta Numeric vector, log(mu).
#' @param nu Numeric vector (or scalar), dispersion.
#' @param q,p,second See \code{cmp_block}.
#' @param tol Relative tolerance of the tail bound.
#' @param max_terms Maximum number of series terms.
#' @param block_size Maximum number of matrix cells per block.
#' @param max_score Optional integer ceiling (right-truncation), or NULL.
#'
#' @return A list of vectors (length of \code{eta}): logZ, EY, ElgF, ok and optionally
#'   EY2, lower, upper, quant.
#' @keywords internal
#' @noRd
cmp_engine <- function(eta, nu, q = NULL, p = NULL, second = FALSE,
                       tol = 1e-12, max_terms = 2000L, block_size = 1e6,
                       max_score = NULL) {
  n <- length(eta)
  if (length(nu) == 1L) nu <- rep(nu, n)
  exact <- !is.null(max_score)
  if (exact) max_terms <- as.integer(max_score)

  keys <- list(eta, nu)
  if (!is.null(q)) keys <- c(keys, list(q))
  if (!is.null(p)) keys <- c(keys, list(p))
  g <- cmp_group(keys)

  e <- eta[g$rep]
  v <- nu[g$rep]
  qq <- if (!is.null(q)) q[g$rep] else NULL
  pp <- if (!is.null(p)) p[g$rep] else NULL
  m <- length(e)

  fields <- c("logZ", "EY", "ElgF")
  if (second) fields <- c(fields, "EY2")
  if (!is.null(q)) fields <- c(fields, "lower", "upper")
  if (!is.null(p)) fields <- c(fields, "quant")
  res <- lapply(stats::setNames(fields, fields), function(f) rep(NA_real_, m))
  res$ok <- rep(FALSE, m)

  valid <- is.finite(e) & is.finite(v) & v > 0
  if (!is.null(qq)) valid <- valid & !is.na(qq)
  if (!is.null(pp)) valid <- valid & !is.na(pp)
  pending <- valid

  if (exact) {
    J_bin <- rep(max_terms, m)
  } else {
    # Initial truncation: the terms peak at about mu with spread about sqrt(mu / nu)
    mu_a <- exp(pmin(e, 700))
    J_est <- ceiling(mu_a + 10 * sqrt(mu_a / v) + 30)
    J_bin <- pmin(2^ceiling(log2(pmax(J_est, 64))), max_terms)
  }

  while (any(pending)) {
    for (b in sort(unique(J_bin[pending]))) {
      rows <- which(pending & J_bin == b)
      if (length(rows) == 0L) next
      step <- max(1L, floor(block_size / (b + 1)))
      for (s in seq(1L, length(rows), by = step)) {
        idx <- rows[s:min(s + step - 1L, length(rows))]
        blk <- cmp_block(e[idx], v[idx], b, q = qq[idx], p = pp[idx],
                         second = second, tol = tol, exact = exact)
        for (f in fields) res[[f]][idx] <- blk[[f]]
        good <- blk$ok
        res$ok[idx[good]] <- TRUE
        finished <- good | (b >= max_terms)
        pending[idx[finished]] <- FALSE
        if (any(!finished)) J_bin[idx[!finished]] <- min(2 * b, max_terms)
      }
    }
  }

  lapply(res, function(x) x[g$inv])
}

#' Complete pmf and cdf of ONE CMP distribution (adaptive truncation)
#'
#' @return List with pmf and cdf (vectors over 0..J), ok and J.
#' @keywords internal
#' @noRd
cmp_table <- function(eta, nu, tol = 1e-12, max_terms = 2000L, max_score = NULL) {
  exact <- !is.null(max_score)
  if (exact) {
    J <- as.integer(max_score)
    max_terms <- J
  } else {
    mu_a <- exp(min(eta, 700))
    J <- min(2^ceiling(log2(max(ceiling(mu_a + 10 * sqrt(mu_a / nu) + 30), 64))), max_terms)
  }
  repeat {
    blk <- cmp_block(eta, nu, J, tol = tol, exact = exact, full = TRUE)
    if (isTRUE(blk$ok[1L]) || J >= max_terms) break
    J <- min(2 * J, max_terms)
  }
  list(pmf = blk$pmf[1L, ], cdf = blk$cdf[1L, ], ok = isTRUE(blk$ok[1L]), J = J)
}

#' Quantile function for vectors of (p, mu, nu) with 0 < p < 1
#'
#' Pairs (mu, nu) that occur at least three times (e.g., many random deviates for one
#' age) obtain their cumulative distribution once, and all quantiles are found by
#' \code{findInterval}. Remaining rows are processed by the batched engine.
#' @keywords internal
#' @noRd
cmp_quantile <- function(p, mu, nu, tol = 1e-12, max_terms = NULL, max_score = NULL) {
  n <- length(p)
  res <- rep(NA_real_, n)
  if (n == 0L) return(res)

  grp <- cmp_group(list(mu, nu))
  members <- split(seq_len(n), grp$inv)
  big <- which(lengths(members) >= 3L)
  done <- rep(FALSE, n)
  failed <- FALSE
  p_adj <- p * (1 - 64 * .Machine$double.eps)

  for (k in big) {
    idx <- members[[k]]
    r <- grp$rep[k]
    tab <- cmp_table(base::log(mu[r]), nu[r], tol = tol,
                     max_terms = cmp_max_terms(mu[r], max_terms), max_score = max_score)
    done[idx] <- TRUE
    if (tab$ok) {
      res[idx] <- findInterval(p_adj[idx], tab$cdf, left.open = TRUE)
    } else {
      res[idx] <- NaN
      failed <- TRUE
    }
  }

  rest <- which(!done)
  if (length(rest) > 0L) {
    mom <- cmp_engine(base::log(mu[rest]), nu[rest], p = p[rest], tol = tol,
                      max_terms = cmp_max_terms(mu[rest], max_terms), max_score = max_score)
    qq <- mom$quant
    if (any(!mom$ok)) {
      qq[!mom$ok] <- NaN
      failed <- TRUE
    }
    res[rest] <- qq
  }

  if (failed) {
    warning("CMP series did not converge for some parameter combinations; NaN produced.")
  }
  res
}

#' Continuity-corrected ("mid-p") quantile function
#'
#' Linear interpolation of the cdf on [x - 0.5, x + 0.5], i.e., the inverse of the mid-p
#' percentile rank G(x) = P(Y < x) + 0.5 P(Y = x) at integers. This is the quantile
#' consistent with the mid-p norm scores and with interpolated manifest quantiles.
#' @keywords internal
#' @noRd
cmp_quantile_mid <- function(p, mu, nu, tol = 1e-12, max_terms = NULL, max_score = NULL) {
  x <- cmp_quantile(p, mu, nu, tol = tol, max_terms = max_terms, max_score = max_score)
  cp <- cmp_cum_pmf(x, mu, nu, tol = tol, max_terms = max_terms, max_score = max_score)
  pmf <- pmax(cp$pmf, .Machine$double.xmin)
  q <- x - 0.5 + (p - cp$cdf_prev) / pmf
  pmin(pmax(q, x - 0.5), x + 0.5)
}

#' Cumulative and point probabilities in one pass (for norm tables and prediction)
#'
#' @return List with pmf = P(Y = x), cdf_prev = P(Y <= x - 1), cdf = P(Y <= x), ok.
#' @keywords internal
#' @noRd
cmp_cum_pmf <- function(x, mu, nu, tol = 1e-12, max_terms = NULL, max_score = NULL) {
  eta <- base::log(mu)
  mom <- cmp_engine(eta, nu, q = x - 1, tol = tol,
                    max_terms = cmp_max_terms(mu, max_terms), max_score = max_score)
  pmf <- exp(nu * (x * eta - lgamma(x + 1)) - mom$logZ)
  if (!is.null(max_score)) pmf[x > max_score] <- 0
  cdf_prev <- mom$lower
  list(pmf = pmf, cdf_prev = cdf_prev, cdf = pmin(cdf_prev + pmf, 1), ok = mom$ok)
}

#' Options stored in a fitted model (tolerance, truncation limit, ceiling)
#' @keywords internal
#' @noRd
cmp_opts <- function(model) {
  r <- model$result
  tol <- attr(r, "tol")
  if (is.null(tol)) tol <- 1e-12
  list(tol = tol,
       max_terms = attr(r, "max_terms"),
       max_score = attr(r, "max_score"))
}

#' Age groups for manifest percentiles and calibration checks
#' @keywords internal
#' @noRd
cmp_age_groups <- function(age) {
  if (length(age) / length(unique(age)) > 50 && min(table(age)) > 30) {
    grp <- age
  } else {
    grp <- getGroups(age)
  }
  # Limit to max 30 groups for better visibility
  if (length(unique(grp)) > 30) {
    grp <- getGroups(age, n = 30)
  }
  grp
}

#' Initial values for the CMP regression
#'
#' mu: Poisson regression (weighted); nu: from the Pearson dispersion of that fit,
#' using Var(Y) approx mu / nu. The design matrices are the (orthogonalized) matrices
#' used by the optimizer, so the returned values refer to that parameterization.
#' @keywords internal
#' @noRd
cmp_start_values <- function(X_mu, X_nu, y, weights) {
  n_mu <- ncol(X_mu)
  w <- if (is.null(weights)) rep(1, length(y)) else weights

  beta <- NULL
  phi <- NA_real_
  fit <- tryCatch(
    suppressWarnings(stats::glm.fit(x = X_mu, y = y, weights = w,
                                    family = stats::poisson(),
                                    control = list(maxit = 50))),
    error = function(e) NULL
  )
  if (!is.null(fit) && isTRUE(fit$converged) && all(is.finite(fit$coefficients))) {
    beta <- unname(fit$coefficients)
    fv <- fit$fitted.values
    pos <- is.finite(fv) & fv > 0
    phi <- sum(w[pos] * (y[pos] - fv[pos])^2 / fv[pos]) / max(sum(w[pos]) - n_mu, 1)
  }
  if (is.null(beta)) {
    beta <- c(log(max(stats::weighted.mean(y, w), 0.1)), rep(0, n_mu - 1L))
  }

  start <- beta
  if (!is.null(X_nu)) {
    nu0 <- if (is.finite(phi) && phi > 0) -log(phi) else 0
    nu0 <- min(max(nu0, -2), 2)
    start <- c(start, nu0, rep(0, ncol(X_nu) - 1L))
  }
  start
}

#' Negative log-likelihood and gradient of the CMP regression model (joint evaluation)
#'
#' Per observation, with eta = log(mu), nu = exp(eta_nu) and
#' \eqn{\ell = \nu (y \eta - \log y!) - \log Z(\mu, \nu)}:
#' \deqn{\partial \ell / \partial \eta = \nu (y - E Y)}
#' \deqn{\partial \ell / \partial \log\nu = \nu [\eta (y - E Y) - (\log y! - E \log Y!)]}
#' where expectations are taken under the current CMP distribution (right-truncated at
#' \code{max_score}, if given; the formulas are unchanged because the truncated
#' distribution is again an exponential family). Contributions of observations whose
#' log(nu) predictor is clamped are zeroed (exact subgradient of the clamped objective).
#'
#' @return A list with \code{nll} and \code{grad}. If the series cannot be evaluated
#'   (non-finite parameters or truncation limit reached), a large penalty (1e10) and a
#'   zero gradient are returned, as in the shash module.
#' @keywords internal
#' @noRd
cmp_evaluate <- function(params,
                         X_mu,
                         X_nu = NULL,
                         y,
                         weights = NULL,
                         fixed_nu = NULL,
                         tol = 1e-12,
                         max_terms = 2000L,
                         max_score = NULL) {
  n_mu <- ncol(X_mu)
  penalty <- list(nll = 1e10, grad = rep(0, length(params)))

  eta <- drop(X_mu %*% params[seq_len(n_mu)])
  if (!is.null(X_nu)) {
    eta_nu <- drop(X_nu %*% params[n_mu + seq_len(ncol(X_nu))])
    nu_free <- (eta_nu > CMP_LOG_NU_RANGE[1]) & (eta_nu < CMP_LOG_NU_RANGE[2])
    nu <- exp(pmin.int(pmax.int(eta_nu, CMP_LOG_NU_RANGE[1]), CMP_LOG_NU_RANGE[2]))
  } else {
    nu <- rep(fixed_nu, length(y))
  }

  if (is.null(weights)) {
    weights <- 1  # Will broadcast in multiplication
  }

  if (any(!is.finite(eta))) return(penalty)

  mom <- cmp_engine(eta, nu, tol = tol, max_terms = max_terms, max_score = max_score)
  if (!all(mom$ok)) return(penalty)

  lgy <- lgamma(y + 1)
  logdens <- nu * (y * eta - lgy) - mom$logZ
  nll <- -sum(weights * logdens)
  if (!is.finite(nll)) return(penalty)

  resid <- y - mom$EY
  grad <- -drop(crossprod(X_mu, weights * (nu * resid)))
  if (!is.null(X_nu)) {
    d_lognu <- nu * (eta * resid - (lgy - mom$ElgF)) * nu_free
    grad <- c(grad, -drop(crossprod(X_nu, weights * d_lognu)))
  }
  grad[!is.finite(grad)] <- 0

  list(nll = nll, grad = grad)
}

#' Calculate the negative log-likelihood for a CMP regression model
#'
#' @param params A numeric vector containing all model parameters (mu coefficients, then nu coefficients)
#' @param X_mu Design matrix for log(mu)
#' @param X_nu Design matrix for log(nu) (or NULL for fixed nu)
#' @param y Response vector (counts)
#' @param weights Observation weights
#' @param fixed_nu If X_nu is NULL, the dispersion is fixed to this value
#' @param tol,max_terms Tolerance and truncation limit of the series
#' @param max_score Optional ceiling (right-truncation), or NULL
#'
#' @return The negative log-likelihood of the model
#' @keywords internal
log_likelihood_cmp <- function(params, X_mu, X_nu = NULL, y, weights = NULL,
                               fixed_nu = NULL, tol = 1e-12, max_terms = 2000L,
                               max_score = NULL) {
  cmp_evaluate(params, X_mu, X_nu, y, weights, fixed_nu, tol, max_terms, max_score)$nll
}

#' Analytic gradient of the negative log-likelihood for a CMP regression model
#'
#' @inheritParams log_likelihood_cmp
#' @return Numeric vector: gradient of the negative log-likelihood with respect to \code{params}.
#' @keywords internal
gradient_cmp <- function(params, X_mu, X_nu = NULL, y, weights = NULL,
                         fixed_nu = NULL, tol = 1e-12, max_terms = 2000L,
                         max_score = NULL) {
  cmp_evaluate(params, X_mu, X_nu, y, weights, fixed_nu, tol, max_terms, max_score)$grad
}

#' Build objective and gradient closures that share one series evaluation
#' @keywords internal
#' @noRd
cmp_make_objective <- function(X_mu, X_nu, y, weights, fixed_nu, tol, max_terms,
                               max_score = NULL) {
  last_par <- NULL
  last_val <- NULL
  evaluate <- function(par) {
    if (!is.null(last_par) && identical(par, last_par)) return(last_val)
    val <- cmp_evaluate(par, X_mu, X_nu, y, weights, fixed_nu, tol, max_terms, max_score)
    last_par <<- par
    last_val <<- val
    val
  }
  list(fn = function(par) evaluate(par)$nll,
       gr = function(par) evaluate(par)$grad)
}

#' Is the object a fitted CMP model?
#' @keywords internal
#' @noRd
isCMP <- function(model) {
  inherits(model, "cnormCMP")
}

#' Predict parameters for a CMP regression model
#'
#' @param model An object of class "cnormCMP"
#' @param ages A numeric vector of age points for prediction
#'
#' @return A data frame with predicted mu, nu and lambda = mu^nu (and optionally mean, variance, sd)
#'
#' @keywords internal
predictCoefficients_cmp <- function(model, ages) {
  if (!isCMP(model)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
  }

  # Standardize new ages (same formula as in cnorm.cmp)
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  X_mu_new <- cmp_design(ages_std, model$mu_degree)
  log_mu <- drop(X_mu_new %*% model$mu_est)
  mu <- exp(log_mu)

  if (!is.null(model$nu_degree)) {
    X_nu_new <- cmp_design(ages_std, model$nu_degree)
    log_nu <- pmin(pmax(drop(X_nu_new %*% model$nu_est), CMP_LOG_NU_RANGE[1]),
                   CMP_LOG_NU_RANGE[2])
    nu <- exp(log_nu)
  } else {
    nu <- rep(model$nu, length(ages))
  }

  predicted <- data.frame(
    age = ages,
    mu = mu,
    nu = nu,
    lambda = mu^nu
  )

  return(predicted)
}

#' Residuals of a CMP continuous norming model
#'
#' Computes residuals on the z-scale for a diagnostic check of the fitted model.
#' With \code{type = "quantile"}, randomized quantile residuals (Dunn & Smyth, 1996) are
#' returned: if the model is correct, they are exactly standard normal, also for discrete
#' data. With \code{type = "z"}, the deterministic mid-p z-scores
#' \eqn{\Phi^{-1}(P(Y < y) + 0.5 P(Y = y))} are returned (the z-scores underlying the
#' norm scores); they are approximately standard normal for larger counts, but their
#' variance is slightly below 1 for small counts.
#'
#' @param object A fitted model of class "cnormCMP".
#' @param age Numeric vector of ages.
#' @param score Numeric vector of raw scores (counts), same length as \code{age}.
#' @param type \code{"quantile"} (randomized, default) or \code{"z"} (mid-p).
#' @param seed Optional seed; if given, \code{set.seed(seed)} is called before the
#'   randomization (note that this changes the state of the random number generator).
#' @param ... Not used.
#'
#' @return A numeric vector of residuals.
#'
#' @references
#' Dunn, P. K., & Smyth, G. K. (1996). Randomized quantile residuals. \emph{Journal of
#' Computational and Graphical Statistics}, 5(3), 236-244.
#'
#' @examples
#' \dontrun{
#' model <- cnorm.cmp(speed$age, speed$fluency)
#' r <- residuals(model, speed$age, speed$fluency, seed = 1)
#' c(mean(r), sd(r))
#' qqnorm(r)
#' }
#' @export
residuals.cnormCMP <- function(object, age, score, type = c("quantile", "z"),
                               seed = NULL, ...) {
  if (!isCMP(object)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
  }
  type <- match.arg(type)
  if (!is.numeric(age) || !is.numeric(score)) {
    stop("'age' and 'score' must be numeric vectors.")
  }
  if (length(age) != length(score)) {
    stop("The lengths of 'age' and 'score' must be the same.")
  }
  score <- round(score)

  pr <- predictCoefficients_cmp(object, age)
  o <- cmp_opts(object)
  cp <- cmp_cum_pmf(score, pr$mu, pr$nu, tol = o$tol, max_terms = o$max_terms,
                    max_score = o$max_score)

  if (type == "quantile") {
    if (!is.null(seed)) set.seed(seed)
    u <- cp$cdf_prev + stats::runif(length(score)) * cp$pmf
  } else {
    u <- cp$cdf_prev + 0.5 * cp$pmf
  }
  u[!cp$ok] <- NA
  stats::qnorm(pmin(pmax(u, 1e-12), 1 - 1e-12))
}

#' Log-likelihood of a CMP model (enables \code{AIC()} and \code{BIC()})
#'
#' @param object A fitted model of class "cnormCMP".
#' @param ... Not used.
#' @return An object of class "logLik".
#' @export
logLik.cnormCMP <- function(object, ...) {
  structure(-object$result$value,
            df = length(object$result$par),
            nobs = attr(object$result, "N"),
            class = "logLik")
}

#' Number of observations used to fit a CMP model
#'
#' @param object A fitted model of class "cnormCMP".
#' @param ... Not used.
#' @return Integer.
#' @export
nobs.cnormCMP <- function(object, ...) {
  attr(object$result, "N")
}

#' Plot CMP Model with Data and Percentile Lines
#'
#' The model-implied percentile curves are continuity-corrected quantiles, i.e., the
#' inverse of the mid-p percentile rank used for the norm scores, so that they are
#' directly comparable to the (interpolated) manifest percentiles.
#'
#' @param x A fitted model object of class "cnormCMP"
#' @param ... Additional arguments including age, score, weights, percentiles, points
#'
#' @return A ggplot object
#'
#' @export
plot.cnormCMP <- function(x, ...) {
  model <- x
  args <- list(...)

  if ("age" %in% names(args)) {
    age <- args$age
  } else {
    if (length(args) > 0)
      age <- args[[1]]
    else
      age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else {
    if (length(args) > 1)
      score <- args[[2]]
    else
      score <- NULL
  }
  if ("weights" %in% names(args)) {
    weights <- args$weights
  } else {
    weights <- NULL
  }
  if ("percentiles" %in% names(args)) {
    percentiles <- args$percentiles
  } else {
    percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  }
  if ("points" %in% names(args)) {
    points <- args$points
  } else {
    points <- TRUE
  }

  if (is.null(age) || is.null(score))
    stop("Please provide 'age' and 'score' vectors.")

  if (!isCMP(model)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
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
  } else {
    data$w <- rep(1, nrow(data))
  }
  data <- data[is.finite(data$age) & is.finite(data$score) & is.finite(data$w), , drop = FALSE]
  age <- data$age

  age_range <- range(age)
  pred_ages <- seq(age_range[1], age_range[2], length.out = n_points)

  # Get predictions
  preds <- predictCoefficients_cmp(model, pred_ages)
  o <- cmp_opts(model)

  # Percentile lines: continuity-corrected CMP quantiles (consistent with mid-p norm scores)
  percentile_lines <- lapply(percentiles, function(p) {
    cmp_quantile_mid(rep(p, n_points), mu = preds$mu, nu = preds$nu,
                     tol = o$tol, max_terms = o$max_terms, max_score = o$max_score)
  })

  percentile_data <- do.call(cbind, percentile_lines)
  colnames(percentile_data) <- paste0("P", percentiles * 100)

  plot_data <- data.frame(
    age = pred_ages,
    mu = preds$mu,
    nu = preds$nu,
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

  # Calculate manifest percentiles
  data$group <- cmp_age_groups(age)

  # Get actual percentiles
  NAMES <- paste("PR", percentiles * 100, sep = "")
  percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data$group), function(df) {
    c(age = mean(df$age),
      weighted.quantile(df$score, probs = percentiles, weights = df$w))
  })))
  colnames(percentile.actual) <- c("age", NAMES)
  manifest_data <- percentile.actual

  # Add percentile lines and points
  for (i in seq_along(percentiles)) {
    p <- p +
      geom_line(
        data = plot_data,
        aes(
          x = .data$age,
          y = .data[[paste0("P", percentiles[i] * 100)]],
          color = !!NAMES[i]
        ),
        linewidth = 0.6
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
      title = "Percentile Plot (Conway-Maxwell-Poisson Model)",
      x = "Age",
      y = "Score",
      color = "Percentile"
    ) +
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
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
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

#' Print method for CMP objects
#'
#' @param x A cnormCMP object
#' @param ... Additional arguments
#' @export
print.cnormCMP <- function(x, ...) {
  cat("Conway-Maxwell-Poisson Regression Model for Continuous Norming\n")
  cat("==============================================================\n\n")

  cat("Model Parameters:\n")
  cat("- Location (mu, log link) polynomial degree:", x$mu_degree, "\n")
  if (!is.null(x$nu_degree)) {
    cat("- Dispersion (nu, log link): Polynomial degree",
        x$nu_degree,
        ifelse(x$nu_degree == 0, "(constant)", ""),
        "\n")
  } else {
    cat("- Dispersion (nu): Fixed at", x$nu, "\n")
  }
  if (!is.null(x$max_score)) {
    cat("- Right-truncated at max_score =", x$max_score, "\n")
  }
  cat("\n")

  cat("Sample size:", attr(x$result, "N"), "\n")
  cat("Age range:",
      attr(x$result, "ageMin"),
      "to",
      attr(x$result, "ageMax"),
      "\n")
  cat("Score range:",
      attr(x$result, "min"),
      "to",
      attr(x$result, "max"),
      "\n\n")

  cat("Optimization:\n")
  cat("- Convergence:",
      ifelse(x$result$convergence == 0, "Successful", "Failed"),
      "\n")
  cat("- Log-likelihood:", -x$result$value, "\n")
  cat("- AIC:", 2 * length(x$result$par) + 2 * x$result$value, "\n")
  if (isTRUE(x$at_bound)) {
    cat("- WARNING: log(nu) reached its admissible range at observed ages.\n")
  }
  invisible(x)
}

#' Build the worker that fits one chain of candidate models (internal)
#'
#' Defined at top level (not inside \code{autoselect.cmp}) so that, when it is shipped to
#' parallel workers, only the data arguments are serialized with it. A chain is a fixed
#' degree of nu with ascending degrees of mu; with \code{warm_start = TRUE} each fit
#' starts from the solution of the previous (lower) degree.
#'
#' Warnings of each fit are captured (not discarded) and every record carries the flag
#' \code{at_bound} (predictor of log(nu) at its admissible range).
#' @keywords internal
#' @noRd
cmp_select_fitter <- function(age, score, weights, nu, control, scale,
                              max_terms, tol, max_score, warm_start) {
  force(age); force(score); force(weights); force(nu); force(control)
  force(scale); force(max_terms); force(tol); force(max_score); force(warm_start)

  fit_once <- function(mu_deg, nu_deg, start) {
    out <- list(mu = mu_deg, nu = nu_deg, model = NULL,
                BIC = Inf, AIC = NA_real_, logLik = NA_real_,
                converged = FALSE, at_bound = NA, status = "error",
                message = NA_character_, warnings = character(0))
    warns <- character(0)
    res <- tryCatch({
      m <- withCallingHandlers(
        suppressMessages(
          cnorm.cmp(age = age, score = score, weights = weights,
                    mu_degree = mu_deg,
                    nu_degree = if (nu_deg < 0) NULL else nu_deg,
                    nu = nu, control = control, scale = scale,
                    max_terms = max_terms, tol = tol, plot = FALSE,
                    max_score = max_score, start = start)
        ),
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      list(model = m, err = NA_character_)
    }, error = function(e) list(model = NULL, err = conditionMessage(e)))

    out$warnings <- warns
    if (is.null(res$model)) {
      out$message <- res$err
      return(out)
    }
    d <- diagnostics.cmp(res$model)
    out$model <- res$model
    out$BIC <- if (is.finite(d$BIC)) d$BIC else Inf
    out$AIC <- d$AIC
    out$logLik <- d$log_likelihood
    out$converged <- isTRUE(d$converged)
    out$at_bound <- isTRUE(res$model$at_bound)
    out$status <- if (!is.finite(d$BIC)) "error"
    else if (!isTRUE(d$converged)) "not_converged"
    else "ok"
    out
  }

  # 0 = clean (converged, nu inside its range), 1 = usable with caveats, 2 = failed
  quality <- function(r) {
    if (r$status == "error") 2L
    else if (r$status == "ok" && !isTRUE(r$at_bound)) 0L
    else 1L
  }
  better <- function(a, b) {
    qa <- quality(a); qb <- quality(b)
    if (qa != qb) qa < qb else a$BIC < b$BIC
  }

  function(task) {
    out <- vector("list", length(task$mu_degs))
    prev <- NULL
    for (i in seq_along(task$mu_degs)) {
      mu_deg <- task$mu_degs[i]
      nu_deg <- task$nu_deg
      start <- NULL
      if (warm_start && !is.null(prev)) {
        start <- list(mu = prev$mu_est, nu = prev$nu_est)
      }
      r <- fit_once(mu_deg, nu_deg, start)
      # A warm start that did not end cleanly is compared with a cold start
      if (!is.null(start) && quality(r) > 0L) {
        r0 <- fit_once(mu_deg, nu_deg, NULL)
        if (better(r0, r)) r <- r0
      }
      out[[i]] <- r
      if (!is.null(r$model)) prev <- r$model
    }
    out
  }
}


#' Calculate Norm Tables for the Conway-Maxwell-Poisson Distribution
#'
#' Generates norm tables for specific ages based on a fitted CMP continuous norming model.
#' Computes point probabilities, cumulative probabilities, mid-p percentile ranks, z-scores,
#' norm scores, and optional true-score confidence intervals (Kelley's formula) for count raw scores.
#'
#' @param model Fitted CMP model object of class "cnormCMP". Can also be passed as the second
#'   argument if \code{ages} is given first (matching the \code{\link{normTable}} generic).
#' @param ages Numeric vector of age points for norm table generation.
#' @param start Minimum raw score value for the norm table. Default is \code{0} (the natural
#'   floor for count data).
#' @param end Maximum raw score value for the norm table. Default is \code{max_score} for a
#'   right-truncated model, or the maximum observed score if untruncated.
#' @param step Step size between consecutive raw scores (integer >= 1, default: 1).
#' @param CI Confidence coefficient (0-1, default: 0.90) for confidence intervals.
#' @param reliability Reliability coefficient (0-1) for Kelley's true score confidence intervals.
#' @param mid_p Logical; if TRUE (default), uses mid-p adjusted percentiles for discrete scores:
#'   \eqn{P(Y < x) + 0.5 P(Y = x)}.
#' @param minRaw,maxRaw Optional aliases for \code{start} and \code{end}.
#' @param range Range of the norm scores in standard deviations (default 3), identical to
#'   \code{\link{predict.cnormCMP}}: z-scores and norm scores are limited to +/- \code{range}.
#'   Use \code{Inf} to switch off truncation. The column \code{Percentile} is never limited.
#' @param ... Additional arguments.
#'
#' @return A list of data frames (one per age) containing:
#'   \item{x}{Raw score count}
#'   \item{Px}{Point probability \eqn{P(Y = x)}}
#'   \item{Pcum}{Cumulative probability \eqn{P(Y \le x)}}
#'   \item{Percentile}{Mid-p percentile rank (0 to 100)}
#'   \item{z}{Standardized z-score (bounded by \code{+/- range})}
#'   \item{norm}{Norm score on the model's scale (e.g. T-score)}
#'   \item{lowerCI, upperCI}{True-score confidence interval limits (if reliability is given)}
#'   \item{lowerCI_PR, upperCI_PR}{Percentile rank confidence limits (if reliability is given)}
#'
#' @export
#' @family normTable
normTable.cmp <- function(model,
                          ages,
                          start = NULL,
                          end = NULL,
                          step = 1,
                          CI = .9,
                          reliability = NULL,
                          mid_p = TRUE,
                          minRaw = NULL,
                          maxRaw = NULL,
                          range = 3,
                          ...) {

  # Support flexible argument ordering: normTable(ages, model) vs normTable(model, ages)
  if (is.numeric(model) && inherits(ages, "cnormCMP")) {
    tmp <- model
    model <- ages
    ages <- tmp
  }

  if (!isCMP(model)) {
    stop("Wrong object. Please provide an object of class 'cnormCMP'.")
  }

  if (!is.numeric(ages) || length(ages) == 0L || any(!is.finite(ages))) {
    stop("'ages' must be a non-empty vector of finite numbers.")
  }

  if (is.null(range)) range <- Inf
  if (!is.numeric(range) || length(range) != 1L || is.na(range) || range <= 0) {
    stop("'range' must be a single positive number (or Inf).")
  }

  o <- cmp_opts(model)

  # Support minRaw and maxRaw as aliases
  if (is.null(start) && !is.null(minRaw)) start <- minRaw
  if (is.null(end)   && !is.null(maxRaw)) end   <- maxRaw

  # For count data, natural floor is 0 unless explicitly specified otherwise
  if (is.null(start)) {
    start <- 0L
  } else {
    start <- max(0L, as.integer(ceiling(start)))
  }

  # For right-truncated models, natural ceiling is max_score
  if (is.null(end)) {
    end <- if (!is.null(o$max_score)) o$max_score else as.integer(ceiling(attr(model$result, "max") %||% 100))
  } else {
    end <- as.integer(floor(end))
    if (!is.null(o$max_score)) {
      end <- min(end, o$max_score)
    }
  }

  if (start >= end) {
    stop("'start' value (", start, ") must be strictly less than 'end' value (", end, ").")
  }

  # For discrete counts, step must be an integer >= 1
  if (is.null(step) || step < 1) {
    step <- 1L
  } else {
    step <- max(1L, as.integer(round(step)))
  }

  # Setup reliability and confidence intervals (Kelley's true-score formula)
  rel <- FALSE
  if (!is.null(reliability) && !is.null(CI) && !is.na(CI)) {
    if (CI > .99999 || CI < .00001) {
      stop("Confidence coefficient (CI) out of range. Please specify a value between 0 and 1.")
    }
    if (reliability > .9999 || reliability < .0001) {
      stop("Reliability coefficient out of range. Please specify a value between 0 and 1.")
    }
    # Margin of error on z scale: z_{1 - alpha/2} * sqrt(r_xx * (1 - r_xx))
    se <- stats::qnorm(1 - ((1 - CI) / 2)) * sqrt(reliability * (1 - reliability))
    rel <- TRUE
  }

  # Get predicted CMP parameters for all requested ages
  predictions <- predictCoefficients_cmp(model, ages)

  # Discrete raw count sequence
  x <- seq(from = start, to = end, by = step)

  # Scale metrics (defaults to T-scores: M = 50, SD = 10)
  mScale  <- attr(model$result, "scaleMean") %||% 50
  sdScale <- attr(model$result, "scaleSD")   %||% 10
  if (!is.finite(mScale))  mScale  <- 50
  if (!is.finite(sdScale)) sdScale <- 10

  result <- vector("list", length(ages))

  # Generate norm table for each age using fast one-pass distribution evaluation
  for (k in seq_along(ages)) {
    mu_k <- predictions$mu[k]
    nu_k <- predictions$nu[k]

    tab <- cmp_table(
      eta = base::log(mu_k),
      nu = nu_k,
      tol = o$tol,
      max_terms = cmp_max_terms(mu_k, o$max_terms),
      max_score = o$max_score
    )

    if (!tab$ok) {
      warning("CMP series did not converge for age ", ages[k],
              "; results may be unreliable. Consider increasing 'max_terms'.")
    }

    # Extract point probabilities and cumulative distribution
    # (Note: R indices are 1-based, so score x corresponds to index x + 1)
    J_max <- tab$J
    idx <- x + 1L

    Px <- rep(0, length(x))
    in_range <- idx <= length(tab$pmf)
    Px[in_range] <- tab$pmf[idx[in_range]]

    Pcum <- rep(1, length(x))
    Pcum[in_range] <- tab$cdf[idx[in_range]]

    # Previous cumulative: P(Y <= x - 1)
    Pprev <- rep(0, length(x))
    prev_idx <- x  # which is (x - 1) + 1
    has_prev <- x > 0L & prev_idx <= length(tab$cdf)
    Pprev[has_prev] <- tab$cdf[prev_idx[has_prev]]
    Pprev[x > J_max] <- 1

    # Mid-p percentile ranks: P(Y < x) + 0.5 * P(Y = x)
    perc <- if (mid_p) Pprev + 0.5 * Px else Pcum
    if (!tab$ok) perc[] <- NA_real_

    # Standardized z-score limited to +/- range
    z <- stats::qnorm(pmin(pmax(perc, 1e-12), 1 - 1e-12))
    z <- pmin(pmax(z, -range), range)

    # Norm score (e.g., T-score or IQ-score)
    norm <- mScale + sdScale * z

    df <- data.frame(
      x = x,
      Px = Px,
      Pcum = Pcum,
      Percentile = perc * 100,
      z = z,
      norm = norm
    )

    # Add Kelley's true score confidence intervals if reliability is provided
    if (rel) {
      zPredicted <- reliability * z
      df$lowerCI <- (zPredicted - se) * sdScale + mScale
      df$upperCI <- (zPredicted + se) * sdScale + mScale
      df$lowerCI_PR <- pmax(0, pmin(100, stats::pnorm(zPredicted - se) * 100))
      df$upperCI_PR <- pmax(0, pmin(100, stats::pnorm(zPredicted + se) * 100))
    }

    result[[k]] <- df
  }

  names(result) <- as.character(ages)
  return(result)
}

#' Summarize a CMP Continuous Norming Model
#'
#' This function provides a summary of a fitted Conway-Maxwell-Poisson continuous norming
#' model, including model fit statistics, convergence information, and parameter estimates.
#'
#' @param object An object of class "cnormCMP", typically the result of a call to
#'   \code{\link{cnorm.cmp}}.
#' @param ... Additional arguments passed to the summary method:
#'   \itemize{
#'      \item age An optional numeric vector of age values corresponding to the raw scores.
#'        If provided along with \code{score}, additional fit statistics (R-squared, RMSE, bias)
#'        and a calibration table (mean and SD of the z-residuals per age group) are calculated.
#'      \item score An optional numeric vector of raw scores. Must be provided if \code{age} is given.
#'      \item weights An optional numeric vector of weights for each observation.
#'    }
#'
#' @return Invisibly returns a list containing detailed diagnostic information about the model.
#'   The function primarily produces printed output summarizing the model.
#'
#' @details
#' The summary includes basic model information, model fit statistics (log-likelihood, AIC, BIC),
#' R-squared, RMSE and bias (if age and raw scores are provided), convergence information,
#' and parameter estimates with standard errors, z-values and p-values. Coefficients refer to
#' the log scale: log(mu) and log(nu); only the intercepts refer to the mean age. The
#' calibration table should show means near 0 and SDs near 1 in every age group (the SD of
#' mid-p z-scores is slightly below 1 for small counts; see \code{\link{residuals.cnormCMP}}).
#'
#' @examples
#' \dontrun{
#' model <- cnorm.cmp(speeded$age, speeded$raw)
#' summary(model)
#' summary(model, age = speeded$age, score = speeded$raw)
#' }
#'
#' @seealso \code{\link{cnorm.cmp}}, \code{\link{diagnostics.cmp}}
#' @export
summary.cnormCMP <- function(object, ...) {
  args <- list(...)

  if ("age" %in% names(args)) {
    age <- args$age
  } else {
    if (length(args) > 0)
      age <- args[[1]]
    else
      age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else {
    if (length(args) > 1)
      score <- args[[2]]
    else
      score <- NULL
  }
  if ("weights" %in% names(args)) {
    weights <- args$weights
  } else {
    if (length(args) > 2)
      weights <- args[[3]]
    else
      weights <- NULL
  }

  diag <- diagnostics.cmp(object, age, score, weights)

  cat("Conway-Maxwell-Poisson Continuous Norming Model\n")
  cat("-----------------------------------------------\n")
  cat("Polynomial degrees:\n")
  cat("  Location (log mu):", diag$mu_degree, "\n")
  if (!is.null(diag$nu_degree)) {
    cat("  Dispersion (log nu):", diag$nu_degree,
        ifelse(diag$nu_degree == 0, "(constant)", ""), "\n")
  } else {
    cat("  Dispersion (nu): Fixed at", diag$nu, "\n")
  }
  if (!is.null(diag$max_score)) {
    cat("  Right-truncated at max_score =", diag$max_score, "\n")
  }
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
  cat("  Max |gradient|:", format(diag$max_abs_gradient, digits = 3), "\n")
  cat("  Hessian condition number:", format(diag$hessian_cond, digits = 3), "\n")
  cat("  Message:", diag$message, "\n")
  if (!is.null(diag$optim_message) && nzchar(diag$optim_message)) {
    cat("  Optimizer message:", diag$optim_message, "\n")
  }
  if (isTRUE(diag$nu_at_bound)) {
    cat("  WARNING: log(nu) reached its admissible range at observed ages;\n",
        "          standard errors and Wald tests are not valid.\n")
  }
  if (!is.null(diag$note)) {
    cat("  Note:", diag$note, "\n")
  }
  cat("\n")

  cat("Parameter Estimates:\n")
  cat("Location (mu) parameters (log scale):\n")
  mu_table <- data.frame(
    Estimate = diag$mu_estimates,
    `Std. Error` = diag$mu_se,
    `z value` = diag$mu_z_values,
    `Pr(>|z|)` = diag$mu_p_values,
    check.names = FALSE
  )
  rownames(mu_table) <- paste0("log(mu)_", 0:(length(diag$mu_estimates) - 1))
  print(mu_table, digits = 4)

  if (!is.null(diag$nu_estimates)) {
    cat("\n")
    cat("Dispersion (nu) parameters (log scale):\n")
    nu_table <- data.frame(
      Estimate = diag$nu_estimates,
      `Std. Error` = diag$nu_se,
      `z value` = diag$nu_z_values,
      `Pr(>|z|)` = diag$nu_p_values,
      check.names = FALSE
    )
    rownames(nu_table) <- paste0("log(nu)_", 0:(length(diag$nu_estimates) - 1))
    print(nu_table, digits = 4)
  }

  if (!is.null(diag$calibration)) {
    cat("\n")
    cat("Calibration by age group (z-residuals; expected: mean 0, SD 1):\n")
    print(diag$calibration, digits = 3, row.names = FALSE)
  }

  invisible(diag)
}

#' Diagnostic Statistics for CMP Continuous Norming Model
#'
#' This function computes detailed diagnostic statistics for a fitted CMP model,
#' including fit statistics, parameter estimates, and convergence information.
#'
#' @param object An object of class "cnormCMP"
#' @param age An optional numeric vector of age values for computing fit statistics
#' @param score An optional numeric vector of raw scores for computing fit statistics
#' @param weights An optional numeric vector of observation weights
#'
#' @return A list containing comprehensive diagnostic information. Convergence is
#'   reported in \code{converged}, \code{message} (interpretation of the \code{optim}
#'   code), \code{optim_message} (message of the optimizer), \code{max_abs_gradient}
#'   (largest absolute gradient component at the solution) and \code{hessian_cond}
#'   (condition number of the Hessian in the orthogonalized parameterization).
#'   Remarks that are not related to convergence are returned in \code{note}.
#'   If \code{age} and \code{score} are given, \code{calibration} contains the mean and
#'   SD of the mid-p z-residuals per age group.
#'
#' @export
diagnostics.cmp <- function(object,
                            age = NULL,
                            score = NULL,
                            weights = NULL) {
  if (!isCMP(object)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
  }

  # Basic model information
  n_obs <- attr(object$result, "N")
  n_params <- length(object$result$par)
  log_likelihood <- -object$result$value
  AIC <- 2 * n_params - 2 * log_likelihood
  BIC <- n_params * log(n_obs) - 2 * log_likelihood

  # Parameter estimates and standard errors (element-wise: one NA does not blank the rest)
  mu_estimates <- object$mu_est
  nu_estimates <- object$nu_est
  n_mu <- length(mu_estimates)
  n_nu <- length(nu_estimates)

  se <- object$se
  if (is.null(se) || length(se) != n_mu + n_nu) {
    se <- rep(NA_real_, n_mu + n_nu)
  }
  mu_se <- se[seq_len(n_mu)]
  nu_se <- if (n_nu > 0L) se[n_mu + seq_len(n_nu)] else NULL

  # z-values and p-values (two-sided; pnorm(-|z|) keeps precision for tiny p-values)
  zfun <- function(est, s) ifelse(is.na(s) | s == 0, NA_real_, est / s)
  pfun <- function(z) ifelse(is.na(z), NA_real_, 2 * stats::pnorm(-abs(z)))

  mu_z_values <- zfun(mu_estimates, mu_se)
  mu_p_values <- pfun(mu_z_values)
  if (n_nu > 0L) {
    nu_z_values <- zfun(nu_estimates, nu_se)
    nu_p_values <- pfun(nu_z_values)
  } else {
    nu_z_values <- NULL
    nu_p_values <- NULL
  }

  # Convergence information
  converged <- object$result$convergence == 0
  n_evaluations <- object$result$counts["function"]
  message <- switch(
    as.character(object$result$convergence),
    "0" = "Successful convergence",
    "1" = "Maximum iterations reached",
    "10" = "Degeneracy in Nelder-Mead simplex",
    "51" = "Warning from L-BFGS-B",
    "52" = "Error from L-BFGS-B",
    paste("Convergence code:", object$result$convergence)
  )
  optim_message <- object$result$message
  if (is.null(optim_message)) optim_message <- NA_character_

  # Largest absolute gradient component at the solution
  max_abs_gradient <- attr(object$result, "max_abs_gradient")
  if (is.null(max_abs_gradient)) max_abs_gradient <- NA_real_

  # Condition number of the Hessian (orthogonalized parameterization)
  hessian_cond <- NA_real_
  if (!is.null(object$result$hessian)) {
    hessian_cond <- tryCatch({
      ev <- eigen(object$result$hessian, symmetric = TRUE, only.values = TRUE)$values
      if (min(ev) > 0) max(ev) / min(ev) else Inf
    }, error = function(e) {
      NA_real_
    })
  }

  # Calculate R-squared and calibration if age and score data are provided
  R2 <- NA
  rmse <- NA
  bias <- NA
  note <- NULL
  calibration <- NULL
  if (!is.null(age) && !is.null(score)) {
    if (length(age) != length(score)) {
      stop("The lengths of 'age' and 'score' must be the same.")
    }
    keep <- is.finite(age) & is.finite(score)
    if (!is.null(weights)) keep <- keep & is.finite(weights)
    age <- age[keep]
    score <- score[keep]
    if (!is.null(weights)) weights <- weights[keep]

    if (length(age) / length(unique(age)) > 50 &&
        min(table(age)) > 30) {
      data <- data.frame(group = age, raw = score)
      data <- rankByGroup(
        data = data,
        raw = "raw",
        group = "group",
        weights = weights,
        scale = c(
          attr(object$result, "scaleMean"),
          attr(object$result, "scaleSD")
        )
      )
      norm_scores <- predict(object, data$group, data$raw)
    } else{
      data <- data.frame(age = age, raw = score)
      data$groups <- getGroups(age)
      width <- (max(age) - min(age)) / length(unique(data$groups))
      data <- rankBySlidingWindow(
        data,
        age = "age",
        raw = "raw",
        width = width,
        weights = weights,
        scale = c(
          attr(object$result, "scaleMean"),
          attr(object$result, "scaleSD")
        )
      )
      norm_scores <- predict(object, data$age, data$raw)
    }

    norm_manifest <- data$normValue
    R2 <- cor(norm_scores, norm_manifest, use = "pairwise.complete.obs")^2
    rmse <- sqrt(mean((norm_scores - norm_manifest)^2, na.rm = TRUE))
    bias <- mean(norm_scores - norm_manifest, na.rm = TRUE)

    # Calibration: mid-p z-residuals should have mean 0 and SD close to 1 in every age group
    res <- residuals.cnormCMP(object, age, score, type = "z")
    grp <- cmp_age_groups(age)
    calibration <- do.call(rbind, lapply(split(seq_along(res), grp), function(ix) {
      data.frame(age = mean(age[ix]),
                 n = length(ix),
                 mean_z = mean(res[ix], na.rm = TRUE),
                 sd_z = stats::sd(res[ix], na.rm = TRUE))
    }))
    rownames(calibration) <- NULL
  } else {
    note <- "No age and raw scores provided. Cannot calculate R2, RMSE, bias and calibration."
  }

  # Return comprehensive diagnostic information
  list(
    # Basic model info
    mu_degree = object$mu_degree,
    nu_degree = object$nu_degree,
    nu = object$nu,
    max_score = object$max_score,
    n_obs = n_obs,
    n_params = n_params,

    # Fit statistics
    log_likelihood = log_likelihood,
    AIC = AIC,
    BIC = BIC,
    R2 = R2,
    rmse = rmse,
    bias = bias,
    calibration = calibration,

    # Parameter estimates
    mu_estimates = mu_estimates,
    nu_estimates = nu_estimates,

    # Standard errors
    mu_se = mu_se,
    nu_se = nu_se,

    # Test statistics
    mu_z_values = mu_z_values,
    nu_z_values = nu_z_values,
    mu_p_values = mu_p_values,
    nu_p_values = nu_p_values,

    # Convergence info
    converged = converged,
    n_evaluations = n_evaluations,
    max_abs_gradient = max_abs_gradient,
    hessian_cond = hessian_cond,
    nu_at_bound = isTRUE(object$at_bound),
    message = message,
    optim_message = optim_message,
    note = note
  )
}

#' Predict Norm Scores from Raw Scores (CMP model)
#'
#' This function calculates norm scores based on raw scores, age, and a fitted cnormCMP model.
#'
#' @param object A fitted model object of class 'cnormCMP'.
#' @param ... Additional arguments passed to the prediction method:
#'   \itemize{
#'      \item age A numeric vector of ages, same length as score.
#'      \item score A numeric vector of raw scores (integers).
#'      \item range The range of the norm scores in standard deviations. Default is 3.
#'        Thus, scores in the range of +/- 3 standard deviations are considered.
#'        Applies to \code{type = "norm"} and \code{type = "z"}; use \code{Inf} to switch
#'        the limit off.
#'      \item mid_p Logical; use mid-p percentile ranks, P(Y < x) + 0.5 * P(Y = x)
#'        (default TRUE), instead of P(Y <= x).
#'      \item type What to return: \code{"norm"} (default; norm scores on the scale of the
#'        model), \code{"z"} (z-scores) or \code{"percentile"} (percentile ranks in
#'        0 to 100, never limited by \code{range}).
#'    }
#'
#' @return A numeric vector of norm scores, z-scores or percentile ranks.
#'
#' @details
#' The function predicts the CMP parameters (mu, nu) for each age using the provided
#' model, calculates the (mid-)percentile rank of each raw score under the corresponding
#' discrete distribution, and converts it to the norm scale specified in the model.
#' Extreme percentiles are mapped to the boundary of the norm score range
#' (mean +/- \code{range} standard deviations). A warning is issued for scores above \code{max_score} of a right-truncated model,
#' and if the series could not be evaluated for some cases (NA is returned for them).
#'
#' @examples
#' \dontrun{
#' model <- cnorm.cmp(children$age, children$score)
#' norm_scores <- predict(model, c(7, 8, 9, 10), c(25, 30, 18, 45))
#'
#' # Mid-p percentile ranks instead of norm scores
#' predict(model, c(7, 8, 9, 10), c(25, 30, 18, 45), type = "percentile")
#' }
#'
#' @export
#' @family predict
predict.cnormCMP <- function(object, ...) {
  model <- object
  args <- list(...)


  if ("age" %in% names(args)) {
    age <- args$age
  } else {
    if (length(args) > 0)
      age <- args[[1]]
    else
      age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else {
    if (length(args) > 1)
      score <- args[[2]]
    else
      score <- NULL
  }

  if (!isCMP(model)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
  }

  if (is.null(age) || is.null(score)) {
    stop("Both 'age' and 'score' must be provided.")
  }

  if (!is.numeric(age) || !is.numeric(score)) {
    stop("'age' and 'score' must be numeric vectors.")
  }

  if (length(age) != length(score)) {
    stop("The lengths of 'age' and 'score' must be the same.")
  }

  if (any(score < 0, na.rm = TRUE)) {
    stop("'score' contains negative values. Raw scores must be non-negative counts.")
  }

  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  if ("range" %in% nms) {
    range <- args$range
  } else if (length(args) > 2 && !nzchar(nms[3])) {
    range <- args[[3]]      # positional third argument (backward compatible)
  } else {
    range <- 3
  }
  if (is.null(range) || !is.numeric(range) || length(range) != 1L || is.na(range) || range <= 0) {
    stop("'range' must be a single positive number (or Inf).")
  }
  mid_p <- if ("mid_p" %in% names(args)) isTRUE(args$mid_p) else TRUE
  type <- if ("type" %in% names(args)) {
    match.arg(args$type, c("norm", "z", "percentile"))
  } else {
    "norm"
  }

  if (any(abs(score - round(score)) > 1e-8, na.rm = TRUE)) {
    warning("Non-integer raw scores were rounded; the CMP model is defined for counts.")
  }
  score <- round(score)

  o <- cmp_opts(model)
  if (!is.null(o$max_score) && any(score > o$max_score, na.rm = TRUE)) {
    warning("Some raw scores exceed 'max_score' (", o$max_score,
            ") of the right-truncated model; they receive the highest percentile.")
  }

  # Get predicted distribution parameters for each age
  predictions <- predictCoefficients_cmp(model, age)

  # (Mid-)percentile ranks under the discrete distribution
  cp <- cmp_cum_pmf(score, mu = predictions$mu, nu = predictions$nu,
                    tol = o$tol, max_terms = o$max_terms, max_score = o$max_score)
  percentiles <- if (mid_p) cp$cdf_prev + 0.5 * cp$pmf else cp$cdf
  percentiles[!cp$ok] <- NA

  failed <- !cp$ok & is.finite(age) & is.finite(score)
  if (any(failed)) {
    warning("The CMP series did not converge for ", sum(failed),
            " case(s); NA returned. Consider increasing 'max_terms' in cnorm.cmp().")
  }

  if (type == "percentile") {
    return(percentiles * 100)
  }

  percentiles_c <- pmin(pmax(percentiles, 1e-12), 1 - 1e-12)
  z_scores <- qnorm(percentiles_c)

  # Apply range constraints
  z_scores <- pmin(pmax(z_scores, -range), range)
  if (type == "z") {
    return(z_scores)
  }

  # Get scale information
  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  # Scale z-scores to specified norm scale
  mScale + sdScale * z_scores
}
