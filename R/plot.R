#' Plot manifest and fitted raw scores
#'
#' The function plots the raw data against the fitted scores from
#' the regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' regression line.
#' @param model The regression model from the 'cnorm' function
#' @param group Should the fit be displayed by group?
#' @param type Type of display: 0 = plot manifest against fitted values, 1 = plot
#' manifest against difference values
#' @examples
#' \dontrun{
#'   # Compute model with example dataset and plot results
#'   result <- cnorm(raw = elfe$raw, group = elfe$group)
#'   plotRaw(result)
#' }
#' @import ggplot2
#' @export
#' @family plot
plotRaw <- function(model, group = FALSE, type = 0) {
  if (isParametric(model)) {
    stop("This function is not applicable for parametric models.")
  }

  if (!isTaylor(model)) {
    stop("Please provide a cnorm object.")
  }

  d <- model$data
  model <- model$model

  d$fitted <- model$fitted.values
  d$diff <- d$fitted - d$raw
  mse <- round(model$rmse, digits = 4)
  r <- round(cor(d$fitted, d$raw, use = "pairwise.complete.obs"), digits = 4)
  d <- as.data.frame(d)

  if (group) {
    if ("group" %in% colnames(d)) {
      d$group <- as.factor(d$group)
    } else {
      d$group <- as.factor(getGroups(d$age))
    }
  }

  if (type == 0) {
    p <- ggplot(d, aes(x = .data$raw, y = .data$fitted)) +
      geom_point(alpha = 0.2, color = "#0033AA") +
      geom_abline(
        intercept = 0,
        slope = 1,
        color = "red",
        linetype = "dashed"
      ) +
      labs(
        title = if (isTRUE(group))
          "Observed vs. Fitted Raw Scores by Group"
        else
          "Observed vs. Fitted Raw Scores",
        subtitle = paste("r =", r, ", RMSE =", mse),
        x = "Observed Score",
        y = "Fitted Scores"
      )
  } else {
    p <- ggplot(d, aes(x = .data$raw, y = .data$diff)) +
      geom_point(alpha = 0.2, color = "#0033AA") +
      geom_hline(yintercept = 0,
                 color = "red",
                 linetype = "dashed") +
      labs(
        title = if (isTRUE(group))
          "Observed Raw Scores vs. Difference Scores by Group"
        else
          "Observed Raw Scores vs. Difference Scores",
        subtitle = paste("r =", r, ", RMSE =", mse),
        x = "Observed Score",
        y = "Difference Scores"
      )
  }

  if (group) {
    p <- p + facet_wrap( ~ group)
  }

  p <- p + theme_minimal() +
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

#' @title Plot manifest and fitted norm scores
#'
#' @description
#' This function plots the manifest norm score against the fitted norm score from
#' the inverse regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' the regression line. Applicable for Taylor polynomial, beta-binomial, CMP, and shash models.
#'
#' @param model The regression model, usually from the 'cnorm', 'cnorm.betabinomial', 'cnorm.cmp', or 'cnorm.shash' function
#' @param age In case of parametric models, please provide the age vector
#' @param score In case of parametric models, please provide the score vector
#' @param width In case of parametric models, please provide the width for the sliding window.
#'              If null, the function tries to determine a sensible setting.
#' @param weights Vector or variable name in the dataset with weights for each
#' individual case. If NULL, no weights are used.
#' @param group An optional grouping variable, use empty string for no group, the variable name
#'              for Taylor polynomial models or a vector with the groups for parametric models
#' @param minNorm lower bound of fitted norm scores
#' @param maxNorm upper bound of fitted norm scores
#' @param type Type of display: 0 = plot manifest against fitted values, 1 = plot
#' manifest against difference values
#'
#' @return A ggplot object representing the norm scores plot.
#'
#' @examples
#' \dontrun{
#' # Taylor polynomial model
#' model <- cnorm(raw = elfe$raw, group = elfe$group)
#' plot(model, "norm")
#'
#' # Beta-binomial model
#' model.bb <- cnorm.betabinomial(elfe$group, elfe$raw, n = 28)
#' plotNorm(model.bb, age = elfe$group, score = elfe$raw)
#'
#' # Conway-Maxwell-Poisson model
#' model.cmp <- cnorm.cmp(speeded$age, speeded$raw)
#' plotNorm(model.cmp, age = speeded$age, score = speeded$raw)
#' }
#'
#' @import ggplot2
#' @export
#' @family plot
plotNorm <- function(model,
                     age = NULL,
                     score = NULL,
                     width = NULL,
                     weights = NULL,
                     group = FALSE,
                     minNorm = NULL,
                     maxNorm = NULL,
                     type = 0) {
  is_taylor <- isTaylor(model)
  if (is_taylor) {
    data <- model$data
    model <- model$model

    if (is.null(minNorm)) {
      minNorm <- model$minL1
    }

    if (is.null(maxNorm)) {
      maxNorm <- model$maxL1
    }

    d <- data
    raw <- data[[model$raw]]
    if (attr(data, "useAge"))
      age <- data[[model$age]]
    else
      age <- rep(0, length = nrow(data))

    d$fitted <- predictNorm(raw, age, model, minNorm = minNorm, maxNorm = maxNorm)

    if (group) {
      if ("group" %in% colnames(d)) {
        d$group <- as.factor(d$group)
      } else {
        d$group <- as.factor(getGroups(d$age))
      }
    }

  } else if (isParametric(model)) {
    if (is.null(age) || is.null(score)) {
      stop(
        "Please provide age and score vectors for parametric models and the width for the sliding window."
      )
    }

    # Extract model scale ensuring manifest norm scores match fitted norm scores
    scaleMean <- attr(model$result, "scaleMean")
    scaleSD <- attr(model$result, "scaleSD")
    model_scale <- c(scaleMean, scaleSD)

    d <- data.frame(age = age, score = score)
    if (is.null(width)) {
      if (length(age) / length(unique(age)) < 50)
        stop("Please provide a width for the sliding window.")

      d$group <- d$age
      if (is.null(weights))
        d <- rankByGroup(data = d,
                         group = "age",
                         raw = "score",
                         scale = model_scale)
      else
        d <- rankByGroup(
          data = d,
          group = "age",
          raw = "score",
          weights = weights,
          scale = model_scale
        )
    } else {
      if (is.null(weights))
        d <- rankBySlidingWindow(
          data = d,
          age = "age",
          raw = "score",
          width = width,
          scale = model_scale
        )
      else
        d <- rankBySlidingWindow(
          data = d,
          age = "age",
          raw = "score",
          weights = weights,
          width = width,
          scale = model_scale
        )
    }

    # S3 dispatch to predict.cnormBetaBinomial, predict.cnormCMP, or predict.cnormShash
    d$fitted <- predict(model, d$age, d$score)

  } else {
    stop("Please provide an object of type cnorm, cnormBetaBinomial, cnormBetaBinomial2, cnormCMP, or cnormShash.")
  }

  if (!"normValue" %in% colnames(d)) {
    stop(
      "The 'normValue' column is missing from the data. Please ensure it's present for both cnorm and parametric models."
    )
  }

  d$diff <- d$fitted - d$normValue
  d <- d[!is.na(d$fitted) & !is.na(d$diff), ]

  rmse <- round(sqrt(mean(d$diff^2)), digits = 4)
  r <- round(cor(d$fitted, d$normValue, use = "pairwise.complete.obs"),
             digits = 4)

  if (type == 0) {
    if (is_taylor) {
      title <- if (isTRUE(group) ||
                   (is.character(group) &&
                    nzchar(group)))
        paste("Observed vs. Fitted Norm Scores by", group)
      else
        "Observed vs. Fitted Norm Scores"
    } else {
      title <- if (is.numeric(group))
        paste("Observed vs. Fitted Norm Scores by group")
      else
        "Observed vs. Fitted Norm Scores"
    }

    p <- ggplot(d, aes(x = .data$normValue, y = .data$fitted)) +
      geom_point(alpha = 0.2, color = "#0033AA") +
      geom_abline(
        intercept = 0,
        slope = 1,
        color = "red",
        linetype = "dashed"
      ) +
      labs(
        title = title,
        subtitle = paste("r =", r, ", RMSE =", rmse),
        x = "Observed Scores",
        y = "Fitted Scores"
      )
  } else {
    if (is_taylor) {
      title <- if (group != "" &&
                   !is.null(group))
        paste("Observed Norm Scores vs. Difference Scores by", group)
      else
        "Observed Norm Scores vs. Difference Scores"
    } else {
      title <- if (is.numeric(group))
        paste("Observed Norm Scores vs. Difference Scores by group")
      else
        "Observed Norm Scores vs. Difference Scores"
    }

    p <- ggplot(d, aes(x = .data$normValue, y = .data$diff)) +
      geom_point(alpha = 0.5, color = "#0033AA") +
      geom_hline(yintercept = 0,
                 color = "red",
                 linetype = "dashed") +
      labs(
        title = title,
        subtitle = paste("r =", r, ", RMSE =", rmse),
        x = "Observed Scores",
        y = "Difference"
      )
  }

  if (isTRUE(group) || (is.character(group) && nzchar(group))) {
    p <- p + facet_wrap( ~ group)
  }

  p <- p + theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  return(p)
}

#' @import ggplot2
#' @export
#' @family plot
#'
#' @title Plot norm curves
#'
#' @description
#' This function plots the norm curves based on the regression model. It supports
#' Taylor polynomial models, beta-binomial models, Conway-Maxwell-Poisson (CMP) models,
#' and shash models.
#'
#' @param model The model from the bestModel function, a cnorm object, or a cnormBetaBinomial / cnormBetaBinomial2 / cnormCMP / cnormShash object.
#' @param normList Vector with norm scores to display. If NULL, default values are used.
#' @param minAge Age to start with checking. If NULL, it's automatically determined from the model.
#' @param maxAge Upper end of the age check. If NULL, it's automatically determined from the model.
#' @param step Stepping parameter for the age check, usually 1 or 0.1; lower scores indicate higher precision.
#' @param minRaw Lower end of the raw score range, used for clipping implausible results. If NULL, it's automatically determined from the model.
#' @param maxRaw Upper end of the raw score range, used for clipping implausible results. If NULL, it's automatically determined from the model.
#'
#' @details
#' Please check the function for inconsistent curves: The different curves should not intersect.
#' Violations of this assumption are a strong indication of violations of model assumptions in
#' modeling the relationship between raw and norm scores.
#'
#' Common reasons for inconsistencies include:
#' 1. Vertical extrapolation: Choosing extreme norm scores (e.g., scores <= -3 or >= 3).
#' 2. Horizontal extrapolation: Using the model scores outside the original dataset.
#' 3. The data cannot be modeled with the current approach, or you need another power parameter (k) or R2 for the model.
#'
#' @return A ggplot object representing the norm curves.
#'
#' @seealso \code{\link{checkConsistency}}, \code{\link{plotDerivative}}, \code{\link{plotPercentiles}}
#'
#' @examples
#' \dontrun{
#' # For Taylor continuous norming model
#' m <- cnorm(raw = ppvt$raw, group = ppvt$group)
#' plotNormCurves(m, minAge=2, maxAge=5)
#'
#' # For beta-binomial model
#' bb_model <- cnorm.betabinomial(age = ppvt$age, score = ppvt$raw, n = 228)
#' plotNormCurves(bb_model)
#'
#' # For CMP model
#' cmp_model <- cnorm.cmp(age = speeded$age, score = speeded$raw)
#' plotNormCurves(cmp_model)
#' }
plotNormCurves <- function(model,
                           normList = NULL,
                           minAge = NULL,
                           maxAge = NULL,
                           step = 0.1,
                           minRaw = NULL,
                           maxRaw = NULL) {
  if (isTaylor(model)) {
    model <- model$model
  }

  parametric <- isParametric(model)
  is_beta_binomial <- isBeta(model)
  is_shash <- isSHASH(model)
  is_cmp <- isCMP(model)

  if (!parametric && !model$useAge) {
    stop("Age or group variable explicitly set to FALSE in dataset. No plotting available.")
  }

  # Get scale information
  if (parametric) {
    scaleMean <- attr(model$result, "scaleMean")
    scaleSD <- attr(model$result, "scaleSD")
  } else {
    scaleMean <- model$scaleM
    scaleSD <- model$scaleSD
  }

  if (is.null(normList)) {
    normList <- c(-2, -1, 0, 1, 2) * scaleSD + scaleMean
  }

  if (is.null(minAge)) {
    minAge <- if (parametric)
      attr(model$result, "age_mean") - 2 * attr(model$result, "age_sd")
    else
      model$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- if (parametric)
      attr(model$result, "age_mean") + 2 * attr(model$result, "age_sd")
    else
      model$maxA1
  }

  if (is.null(minRaw)) {
    minRaw <- if (is_beta_binomial || is_cmp)
      0
    else if (is_shash)
      attr(model$result, "min")
    else
      model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- if (parametric)
      attr(model$result, "max")
    else
      model$maxRaw
  }

  frame_list <- lapply(normList, function(norm) {
    if (parametric) {
      ages <- seq(minAge, maxAge, by = step)
      p_val <- pnorm((norm - scaleMean) / scaleSD)

      if (is_beta_binomial) {
        raws <- sapply(ages, function(a) {
          if (inherits(model, "cnormBetaBinomial")) {
            pred <- predictCoefficients(model, a)
          } else {
            pred <- predictCoefficients2(model, a, attr(model$result, "max"))
          }
          qbeta(p_val, pred$a, pred$b) * attr(model$result, "max")
        })
      } else if (is_shash) {
        preds <- predictCoefficients_shash(model, ages)
        raws <- qshash(p_val, mu = preds$mu, sigma = preds$sigma, epsilon = preds$epsilon, delta = preds$delta)
      } else if (is_cmp) {
        preds <- predictCoefficients_cmp(model, ages)
        raws <- qcmp(p_val, mu = preds$mu, nu = preds$nu)
      }

      data.frame(n = norm, raw = raws, age = ages)
    } else {
      normCurve <- getNormCurve(
        norm,
        model,
        minAge = minAge,
        maxAge = maxAge,
        step = step,
        minRaw = minRaw,
        maxRaw = maxRaw
      )
      data.frame(n = norm,
                 raw = normCurve$raw,
                 age = normCurve$age)
    }
  })
  valueList <- do.call(rbind, frame_list)

  # Create rainbow color palette
  n_colors <- length(unique(valueList$n))
  color_palette <- rainbow(n_colors)

  # Create ggplot
  p <- ggplot(valueList, aes(
    x = .data$age,
    y = .data$raw,
    color = factor(.data$n)
  )) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      name = "Norm Score",
      values = color_palette,
      labels = paste("Norm", normList)
    ) +
    labs(title = "Norm Curves", x = "Explanatory Variable (Age)", y = "Raw Score") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
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

#' Cumulative norm distribution plot for conventional-norming models
#'
#' Used internally when `plotPercentiles()` is called on a cnorm object that
#' was built without age/group (i.e. `useAge == FALSE`). Plots raw score on
#' the x-axis against the model-implied percentile on the y-axis, with the
#' empirical cumulative distribution of the manifest sample overlaid.
#'
#' @param x A cnorm object.
#' @param minRaw,maxRaw Plot range on the raw-score axis. Defaults to the
#'   model's stored raw range.
#' @param title,subtitle Plot annotations; defaults match `plotPercentiles`.
#' @noRd
plotCumulative <- function(x,
                           minRaw   = NULL,
                           maxRaw   = NULL,
                           title    = NULL,
                           subtitle = NULL) {
  if (!isTaylor(x))
    stop("plotCumulative requires a cnorm object.")

  data    <- x$data
  m       <- x$model
  raw_var <- m$raw

  if (is.null(minRaw))
    minRaw <- m$minRaw
  if (is.null(maxRaw))
    maxRaw <- m$maxRaw

  curve_df <- rawTable(0, x, minRaw, maxRaw, pretty = FALSE)
  raw_obs <- data[[raw_var]]
  raw_obs <- raw_obs[!is.na(raw_obs)]
  n_obs   <- length(raw_obs)

  unique_raw <- sort(unique(raw_obs))
  manifest_df <- data.frame(raw        = unique_raw,
                            percentile = vapply(unique_raw, function(r) {
                              (sum(raw_obs < r) + 0.5 * sum(raw_obs == r)) / n_obs * 100
                            }, numeric(1)))

  if (is.null(title)) {
    title <- "Cumulative Norm Distribution (Conventional Norming)"
    subtitle <- bquote(paste("Model: ", .(m$ideal.model), ", R"^2, " = ", .(round(
      m$subsets$adjr2[[m$ideal.model]], digits = 4
    ))))
  }

  p <- ggplot() +
    geom_point(
      data = manifest_df,
      aes(x = .data$raw, y = .data$percentile),
      colour = "black",
      alpha = 0.6,
      size = 1.4
    ) +
    geom_line(
      data = curve_df,
      aes(x = .data$raw, y = .data$percentile),
      colour = "#1f77b4",
      linewidth = 0.9
    ) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = paste0("Raw Score (", raw_var, ")"),
      y        = "Percentile"
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    theme_minimal() +
    theme(
      plot.title       = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      plot.subtitle    = element_text(hjust = 0.5, size = 12),
      axis.title       = element_text(size = 12, face = "bold"),
      axis.title.x     = element_text(margin = margin(t = 10)),
      axis.title.y     = element_text(margin = margin(r = 10)),
      axis.text        = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  print(p)
  invisible(p)
}

#' Plot norm curves against actual percentiles
#'
#' The function plots the norm curves based on the regression model against
#' the actual percentiles from the raw data. Applicable only for Taylor polynomial models.
#' For parametric models (Beta-Binomial, CMP, or SHASH), please use \code{plot(model, age, score)} instead.
#'
#' @param model The Taylor polynomial regression model object from the cNORM
#' @param minRaw Lower bound of the raw score (default = 0)
#' @param maxRaw Upper bound of the raw score
#' @param minAge Variable to restrict the lower bound of the plot to a specific age
#' @param maxAge Variable to restrict the upper bound of the plot to a specific age
#' @param raw The name of the raw variable
#' @param group The name of the grouping variable; the distinct groups are automatically
#' determined
#' @param percentiles Vector with percentile scores, ranging from 0 to 1 (exclusive)
#' @param scale The norm scale, either 'T', 'IQ', 'z', 'percentile' or
#' self defined with a double vector with the mean and standard deviation
#' @param title custom title for plot
#' @param subtitle custom title for plot
#' @param points Logical indicating whether to plot the data points. Default is TRUE.
#' @seealso plotNormCurves, plotPercentileSeries
#' @export
#' @family plot
plotPercentiles <- function(model,
                            minRaw = NULL,
                            maxRaw = NULL,
                            minAge = NULL,
                            maxAge = NULL,
                            raw = NULL,
                            group = NULL,
                            percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                            scale = NULL,
                            title = NULL,
                            subtitle = NULL,
                            points = FALSE) {
  if (isParametric(model)) {
    stop(
      "This function is not applicable for parametric models (Beta-Binomial, CMP, or SHASH). ",
      "Please use 'plot(model, age, raw)' instead."
    )
  }

  if (isTaylor(model)) {
    data <- model$data
    m    <- model$model
  } else {
    stop("Please provide a cnorm object.")
  }

  if (!isTRUE(m$useAge)) {
    return(
      plotCumulative(
        model,
        minRaw      = minRaw,
        maxRaw      = maxRaw,
        title       = title,
        subtitle    = subtitle
      )
    )
  }

  if (is.null(group)) {
    group <- attr(data, "group")
  }

  age <- NULL
  if (is.null(data[[group]])) {
    age <- data[, attributes(data)$age]
    data$group <- getGroups(data[, attributes(data)$age])
    data$age <- data[, attributes(data)$age]
    group <- "group"
  }

  if (is.null(minAge)) minAge <- m$minA1
  if (is.null(maxAge)) maxAge <- m$maxA1
  if (is.null(minRaw)) minRaw <- m$minRaw
  if (is.null(maxRaw)) maxRaw <- m$maxRaw
  if (is.null(raw))    raw <- m$raw

  if (!(raw %in% colnames(data))) {
    stop(paste0("ERROR: Raw score variable '", raw, "' does not exist in data object."))
  }

  if (!(group %in% colnames(data))) {
    stop(paste0("ERROR: Grouping variable '", group, "' does not exist in data object."))
  }

  if (typeof(group) == "logical" && !group) {
    stop("The plotPercentiles-function does not work without a grouping variable.")
  }

  if (is.null(scale)) {
    T <- qnorm(percentiles, m$scaleM, m$scaleSD)
  } else if ((typeof(scale) == "double" && length(scale) == 2)) {
    T <- qnorm(percentiles, scale[1], scale[2])
  } else if (scale == "IQ") {
    T <- qnorm(percentiles, 100, 15)
  } else if (scale == "z") {
    T <- qnorm(percentiles)
  } else if (scale == "T") {
    T <- qnorm(percentiles, 50, 10)
  } else {
    T <- percentiles
  }

  NAMES <- paste("PR", percentiles * 100, sep = "")
  NAMESP <- paste("PredPR", percentiles * 100, sep = "")

  data[, group] <- round(data[, group], digits = 3)
  AGEP <- unique(data[, group])

  if (!is.null(attr(data, "descend")) && attr(data, "descend")) {
    percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data[, group]), function(df) {
      weighted.quantile(df[, raw], probs = 1 - percentiles, weights = df$w)
    })))
  } else {
    percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data[, group]), function(df) {
      weighted.quantile(df[, raw], probs = percentiles, weights = df$w)
    })))
  }
  percentile.actual$group <- as.numeric(rownames(percentile.actual))
  colnames(percentile.actual) <- c(NAMES, c(group))
  rownames(percentile.actual) <- AGEP

  share <- seq(from = minAge, to = maxAge, length.out = 100)
  AGEP <- c(AGEP, share)

  norm_rep <- rep(T, times = length(AGEP))
  age_rep  <- rep(AGEP, each  = length(T))

  preds <- predictRaw(norm_rep, age_rep, m$coefficients, minRaw = minRaw, maxRaw = maxRaw)

  percentile.fitted <- as.data.frame(matrix(preds, nrow = length(AGEP), ncol = length(T), byrow = TRUE))
  percentile.fitted$group <- AGEP
  percentile.fitted <- percentile.fitted[!duplicated(percentile.fitted$group), ]
  colnames(percentile.fitted)  <- c(NAMESP, group)
  rownames(percentile.fitted)  <- percentile.fitted$group

  percentile <- merge(percentile.actual, percentile.fitted, by = group, all = TRUE)

  END <- .8
  COL1 <- rainbow(length(percentiles), end = END)

  if (is.null(title)) {
    title <- "Observed and Predicted Percentile Curves"
    subtitle <- bquote(paste("Model: ", .(m$ideal.model), ", R"^2, "=", .(round(
      m$subsets$adjr2[[m$ideal.model]], digits = 4
    ))))
  }

  plot_data <- data.frame(
    group = rep(percentile$group, 2 * length(percentiles)),
    value = c(as.matrix(percentile[, NAMES]), as.matrix(percentile[, NAMESP])),
    type = rep(c("Observed", "Predicted"), each = nrow(percentile) * length(percentiles)),
    percentile = factor(rep(rep(NAMES, each = nrow(percentile)), 2), levels = NAMES)
  )

  plot_data_predicted <- plot_data[plot_data$type == "Predicted", ]
  plot_data_observed  <- plot_data[plot_data$type == "Observed", ]

  p <- ggplot()

  if (points) {
    if (is.null(age)) {
      p <- p + geom_point(
        data = data,
        aes(x = .data[[group]], y = .data[[raw]]),
        color = "black", alpha = 0.2, size = 0.6
      )
    } else {
      p <- p + geom_point(
        data = data,
        aes(x = .data$age, y = .data[[raw]]),
        color = "black", alpha = 0.2, size = 0.6
      )
    }
  }

  p <- p +
    geom_line(
      data = plot_data_predicted,
      aes(x = .data$group, y = .data$value, color = .data$percentile),
      linewidth = 0.6
    ) +
    geom_point(
      data = plot_data_observed,
      aes(x = .data$group, y = .data$value, color = .data$percentile),
      na.rm = TRUE, size = 2, shape = 18
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = paste0("Explanatory Variable (", group, ")"),
      y = paste0("Raw Score (", raw, ")"),
      color = "Percentile"
    ) +
    scale_color_manual(
      values = setNames(COL1, NAMES),
      labels = paste0(percentiles * 100, "%")
    ) +
    guides(color = guide_legend(override.aes = list(
      linetype = rep("solid", length(NAMES)),
      shape = rep(18, length(NAMES))
    )))

  p <- p + theme_minimal() +
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

  print(p)
  invisible(p)
}


#' Plot the density function per group by raw score
#'
#' This function plots density curves based on the regression model against the raw scores.
#' It supports traditional continuous norming models, beta-binomial models, Conway-Maxwell-Poisson (CMP)
#' models, and SHASH models.
#'
#' @param model The model from the bestModel function, a cnorm object, a cnormBetaBinomial, a cnormBetaBinomial2,
#'    a cnormCMP, or a cnormShash object.
#' @param minRaw Lower bound of the raw score. If NULL, it's automatically determined based on the model type.
#' @param maxRaw Upper bound of the raw score. If NULL, it's automatically determined based on the model type.
#' @param minNorm Lower bound of the norm score. If NULL, it's automatically determined based on the model type.
#' @param maxNorm Upper bound of the norm score. If NULL, it's automatically determined based on the model type.
#' @param group Numeric vector specifying the age groups to plot. If NULL, groups are automatically selected.
#'
#' @return A ggplot object representing the density functions.
#'
#' @details
#' The function generates density curves (or PMFs for discrete models) for specified age groups,
#' allowing for easy comparison of score distributions across different ages.
#'
#' For beta-binomial and CMP models, the display reflects the probability mass function, while for
#' continuous models (Taylor and SHASH), it displays the probability density function.
#'
#' @import ggplot2
#' @export
#' @family plot
plotDensity <- function(model,
                        minRaw = NULL,
                        maxRaw = NULL,
                        minNorm = NULL,
                        maxNorm = NULL,
                        group = NULL) {
  if (isTaylor(model)) {
    model <- model$model
  }

  is_beta_binomial <- isBeta(model)
  is_shash <- isSHASH(model)
  is_cmp <- isCMP(model)

  if (is.null(minNorm)) {
    minNorm <- if (is_beta_binomial || is_shash || is_cmp)
      -3
    else
      model$minL1
  }

  if (is.null(maxNorm)) {
    maxNorm <- if (is_beta_binomial || is_shash || is_cmp)
      3
    else
      model$maxL1
  }

  if (is.null(minRaw)) {
    minRaw <- if (is_beta_binomial || is_cmp)
      0
    else if (is_shash)
      attr(model$result, "min")
    else
      model$minRaw
  }
  if (is.null(maxRaw)) {
    maxRaw <- if (is_beta_binomial || is_shash || is_cmp)
      attr(model$result, "max")
    else
      model$maxRaw
  }

  if (is.null(group)) {
    if (is_beta_binomial || is_shash || is_cmp) {
      age_min <- attr(model$result, "ageMin")
      age_max <- attr(model$result, "ageMax")
      group <- round(seq(
        from = age_min,
        to = age_max,
        length.out = 4
      ), digits = 3)
    } else if (model$useAge) {
      group <- round(seq(
        from = model$minA1,
        to = model$maxA1,
        length.out = 4
      ),
      digits = 3)
    } else {
      group <- c(1)
    }
  }

  step <- (maxNorm - minNorm) / 100

  step_shash <- NULL
  if (is_shash) {
    step_shash <- (maxRaw - minRaw) / 100
    if (step_shash <= 0) step_shash <- 1
  }

  matrix_list <- lapply(group, function(g) {
    if (is_beta_binomial) {
      norm <- normTable.betabinomial(model, ages = g, n = attr(model$result, "max"))[[1]]
      norm$group <- rep(g, length.out = nrow(norm))
      colnames(norm)[colnames(norm) == "x"] <- "raw"
      colnames(norm)[colnames(norm) == "norm"] <- "norm1"
      colnames(norm)[colnames(norm) == "z"] <- "norm"
    } else if (is_cmp) {
      norm <- normTable.cmp(model, ages = g, start = minRaw, end = maxRaw)[[1]]
      norm$group <- rep(g, length.out = nrow(norm))
      colnames(norm)[colnames(norm) == "x"] <- "raw"
      colnames(norm)[colnames(norm) == "norm"] <- "norm1"
      colnames(norm)[colnames(norm) == "z"] <- "norm"
    } else if (is_shash) {
      norm <- normTable.shash(model, ages = g, start = minRaw, end = maxRaw, step = step_shash)[[1]]
      norm$group <- rep(g, length.out = nrow(norm))
      colnames(norm)[colnames(norm) == "x"] <- "raw"
      colnames(norm)[colnames(norm) == "norm"] <- "norm1"
      colnames(norm)[colnames(norm) == "z"] <- "norm"
    } else {
      norm <- normTable(
        g,
        model = model,
        minNorm = minNorm,
        maxNorm = maxNorm,
        minRaw = minRaw,
        maxRaw = maxRaw,
        step = step,
        pretty = FALSE
      )
      norm$group <- rep(g, length.out = nrow(norm))
    }
    return(norm)
  })

  matrix <- do.call(rbind, matrix_list)
  matrix <- matrix[matrix$norm > minNorm & matrix$norm < maxNorm, ]
  matrix <- matrix[matrix$raw >= minRaw & matrix$raw <= maxRaw, ]

  if (is_beta_binomial || is_cmp) {
    matrix$density <- matrix$Px
  } else if (is_shash) {
    matrix$density <- matrix$Px / step_shash
  } else {
    matrix$density <- dnorm(matrix$norm, mean = model$scaleM, sd = model$scaleSD)
  }

  title <- if (is_beta_binomial) {
    "Probability Mass Functions (Beta-Binomial)"
  } else if (is_cmp) {
    "Probability Mass Functions (Conway-Maxwell-Poisson)"
  } else if (is_shash) {
    "Density Functions (SHASH)"
  } else {
    "Density Functions (Taylor Polynomial)"
  }

  matrix <- matrix[complete.cases(matrix), ]
  p <- ggplot(matrix, aes(
    x = .data$raw,
    y = .data$density,
    color = factor(group)
  )) +
    geom_line(linewidth = 1, na.rm = TRUE) +
    scale_color_viridis_d(name = "Group",
                          labels = paste("Group", group),
                          option = "plasma") +
    labs(title = title, x = "Raw Score", y = if (is_beta_binomial || is_cmp) "Probability" else "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  return(p)
}


#' Generates a series of plots with percentile curves for different models
#'
#' This function makes use of 'plotPercentiles' to generate a series of plots
#' for models with an increasing number of terms.
#'
#' @param model The Taylor polynomial regression model object or a cnorm object
#' @param start Number of terms to start with (default 1)
#' @param end Number of terms to end with
#' @param group The name of the grouping variable
#' @param percentiles Vector with percentile scores
#' @param filename Prefix of the filename if saving to png
#' @seealso plotPercentiles
#' @export
#' @family plot
plotPercentileSeries <- function(model,
                                 start = 1,
                                 end = NULL,
                                 group = NULL,
                                 percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                                 filename = NULL) {
  if (isParametric(model)) {
    stop("This function is not applicable for parametric models (Beta-Binomial, CMP, or SHASH). ",
         "Please use the plotDensity function instead.")
  }

  if (isTaylor(model)) {
    d <- model$data
    model <- model$model
  } else {
    stop("Please provide a cnorm object.")
  }

  if (!isTRUE(attr(d, "useAge"))) {
    stop("Age or group variable explicitly set to FALSE in dataset. No plotting available.")
  }

  subsets <- model$subsets
  outmat  <- subsets$outmat
  nTerms  <- rowSums(outmat == "*")
  maxTerms <- max(nTerms)

  if (is.null(end) || end > maxTerms) end <- maxTerms
  if (start < 1) start <- 1
  if (start > end) start <- end

  rows <- which(nTerms >= start & nTerms <= end)
  rows <- rows[!duplicated(nTerms[rows])]
  if (length(rows) == 0L) {
    stop("No models with ", start, " to ", end, " terms available.")
  }

  w <- if (!is.null(attr(d, "weights")) && !is.null(d$weights)) d$weights else NULL

  minR <- min(d[[model$raw]])
  maxR <- max(d[[model$raw]])

  fields <- list(
    ideal.model = model$ideal.model,
    cutoff  = model$cutoff,
    useAge  = model$useAge,
    minA1   = model$minA1,
    maxA1   = model$maxA1,
    minL1   = model$minL1,
    maxL1   = model$maxL1,
    minRaw  = minR,
    maxRaw  = maxR,
    raw     = model$raw,
    scaleSD = attr(d, "scaleSD"),
    scaleM  = attr(d, "scaleM"),
    descend = attr(d, "descend"),
    group   = attr(d, "group"),
    age     = attr(d, "age"),
    k       = attr(d, "k"),
    A       = attr(d, "A")
  )

  termNames <- colnames(outmat)
  l <- vector("list", length(rows))
  names(l) <- as.character(nTerms[rows])

  for (idx in seq_along(rows)) {
    row <- rows[idx]
    size <- nTerms[row]
    message("Plotting model with ", size, " terms ...")

    selected <- termNames[outmat[row, ] == "*"]
    f <- stats::reformulate(selected, response = model$raw)
    bestformula <- if (is.null(w)) stats::lm(f, data = d) else stats::lm(f, data = d, weights = w)

    bestformula[names(fields)] <- fields

    result <- list(data = d, model = bestformula)
    class(result) <- "cnormTemp"

    r2 <- round(subsets$adjr2[row], digits = 4)
    consInfo <- if (!is.null(subsets$consistent) && !is.na(subsets$consistent[row])) {
      if (subsets$consistent[row]) ", consistent" else ", inconsistent"
    } else {
      ""
    }

    l[[idx]] <- plotPercentiles(
      result,
      minAge = model$minA1,
      maxAge = model$maxA1,
      minRaw = minR,
      maxRaw = maxR,
      percentiles = percentiles,
      scale = NULL,
      group = group,
      title = "Observed and Predicted Percentiles",
      subtitle = bquote(paste("Model with ", .(size), " terms, ",
                              R^2, " = ", .(r2), .(consInfo)))
    )

    if (!is.null(filename)) {
      ggsave(
        filename = paste0(filename, size, ".png"),
        plot = l[[idx]],
        device = "png",
        width = 10,
        height = 7,
        dpi = 300
      )
    }
  }

  invisible(l)
}


#' Evaluate information criteria for regression model
#'
#' This function plots various information criteria and model fit statistics for Taylor polynomial models.
#'
#' @param model The regression model from the bestModel function or a cnorm object.
#' @param type Integer specifying the type of plot to generate (0 to 6).
#'
#' @return A ggplot object representing the selected information criterion plot.
#' @export
#' @family plot
plotSubset <- function(model, type = 0) {
  if (isParametric(model)) {
    stop("This function is not applicable for parametric models (Beta-Binomial, CMP, or SHASH).")
  }

  if (isTaylor(model)) {
    model <- model$model
  }

  if (is.null(model$subsets)) {
    stop("The model object does not contain model selection information ('subsets').")
  }

  if (!(type %in% 0:6)) {
    warning("Unknown plot type; using type = 0 (adjusted R2).")
    type <- 0
  }

  subsets <- model$subsets
  nModels <- length(subsets$rss)
  n       <- length(model$fitted.values)

  nTerms  <- rowSums(subsets$outmat == "*")
  nParams <- nTerms + 1L

  Fvals <- rep(NA_real_, nModels)
  pvals <- rep(NA_real_, nModels)
  if (nModels > 1L) {
    df1 <- diff(nParams)
    df1[df1 < 1L] <- NA
    df2 <- n - nParams[-1L]
    Fs  <- (-diff(subsets$rss) / df1) / (subsets$rss[-1L] / df2)
    Fvals[-1L] <- Fs
    pvals[-1L] <- stats::pf(Fs, df1, df2, lower.tail = FALSE)
  }

  consistent <- subsets$consistent
  if (is.null(consistent)) consistent <- rep(TRUE, nModels)
  consistent[is.na(consistent)] <- TRUE

  cutoff <- if (!is.null(model$cutoff)) model$cutoff else .99
  sel    <- if (!is.null(model$ideal.model)) model$ideal.model else NA_integer_

  dat <- data.frame(
    adjr2 = subsets$adjr2,
    bic   = subsets$bic,
    cp    = subsets$cp,
    RSS   = subsets$rss,
    RMSE  = sqrt(subsets$rss / n),
    Fstat = Fvals,
    pval  = pvals,
    nr    = nTerms,
    consistency = factor(ifelse(consistent, "consistent", "inconsistent"),
                         levels = c("consistent", "inconsistent"))
  )

  xlab_terms <- "Number of terms"
  xlab_r2    <- expression(paste("Adjusted ", R^2))

  cfg <- switch(type + 1L,
                list(x = "nr",    y = "adjr2",
                     title = expression(paste("Information Function: Adjusted ", R^2)),
                     xlab = xlab_terms,
                     ylab = expression(paste("Adjusted ", R^2))),
                list(x = "adjr2", y = "cp",
                     title = "Information Function: Mallows's Cp",
                     xlab = xlab_r2,
                     ylab = "Mallows's Cp"),
                list(x = "adjr2", y = "bic",
                     title = "Information Function: BIC",
                     xlab = xlab_r2,
                     ylab = "Bayesian Information Criterion (BIC)"),
                list(x = "nr",    y = "RMSE",
                     title = "Information Function: RMSE",
                     xlab = xlab_terms,
                     ylab = "Root Mean Square Error (Raw Score)"),
                list(x = "nr",    y = "RSS",
                     title = "Information Function: RSS",
                     xlab = xlab_terms,
                     ylab = "Residual Sum of Squares (RSS)"),
                list(x = "nr",    y = "Fstat",
                     title = "Information Function: F-test Statistics",
                     xlab = xlab_terms,
                     ylab = "F-test Statistics for Consecutive Models"),
                list(x = "nr",    y = "pval",
                     title = "Information Function: p-values",
                     xlab = xlab_terms,
                     ylab = expression(paste("p-values for Tests on ", R^2,
                                             " adj. of Consecutive Models")))
  )

  theme_custom <- theme_minimal() +
    theme(
      plot.title   = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title   = element_text(face = "bold", size = 12),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text    = element_text(size = 10),
      legend.title = element_blank(),
      legend.text  = element_text(size = 10),
      legend.position = if (any(dat$consistency == "inconsistent")) "bottom" else "none",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  plt <- ggplot(dat, aes(x = .data[[cfg$x]], y = .data[[cfg$y]])) +
    theme_custom +
    geom_line(color = "#1f77b4", linewidth = .75, na.rm = TRUE) +
    geom_point(aes(shape = .data$consistency),
               color = "#1f77b4", size = 2.5, na.rm = TRUE) +
    scale_shape_manual(values = c(consistent = 16, inconsistent = 1), drop = FALSE) +
    labs(title = cfg$title, x = cfg$xlab, y = cfg$ylab, shape = NULL)

  if (!is.na(sel) && sel >= 1L && sel <= nModels) {
    plt <- plt +
      geom_point(data = dat[sel, , drop = FALSE],
                 aes(shape = .data$consistency),
                 color = "#3322AA", size = 2.5, stroke = 1.1, na.rm = TRUE)
  }

  if (type == 0) {
    plt <- plt +
      geom_hline(yintercept = cutoff, linetype = "dashed", linewidth = .8, color = "#d62728")
  } else if (type == 1) {
    if (all(dat$cp > 0, na.rm = TRUE)) {
      plt <- plt + scale_y_log10() + labs(y = "log-transformed Mallows's Cp")
    } else {
      message("Mallows's Cp contains non-positive values; using a linear scale instead of log10.")
    }
  } else if (type == 6) {
    plt <- plt +
      geom_hline(yintercept = 0.05, linetype = "dashed", linewidth = 1, color = "#d62728") +
      coord_cartesian(ylim = c(-0.005, 0.11))
  }

  if (isTRUE(model$averaged)) {
    plt <- plt + labs(caption = paste0(
      "Final coefficients: BIC-weighted average over ",
      length(model$averagingWeights), " consistent candidate models."))
  }

  return(plt)
}

#' Plot first order derivative of regression model
#'
#' This function plots the scores obtained via the first order derivative of the regression model.
#' Applicable only for Taylor polynomial models.
#'
#' @param model The model from the bestModel function, a cnorm object.
#' @param minAge Minimum age to start checking.
#' @param maxAge Maximum age for checking.
#' @param minNorm Lower end of the norm score range.
#' @param maxNorm Upper end of the norm score range.
#' @param stepAge Stepping parameter for age.
#' @param stepNorm Stepping parameter for norm scores.
#' @param order Degree of the derivative (default = 1).
#'
#' @export
#' @family plot
plotDerivative <- function(model,
                           minAge = NULL,
                           maxAge = NULL,
                           minNorm = NULL,
                           maxNorm = NULL,
                           stepAge = NULL,
                           stepNorm = NULL,
                           order = 1) {
  if (isTaylor(model)) {
    model <- model$model
  } else if (isParametric(model)) {
    stop("This function is not applicable for parametric models (Beta-Binomial, CMP, or SHASH). ",
         "Please use the plotDensity function instead.")
  }

  if (!model$useAge) {
    stop("Age or group variable explicitly set to FALSE in dataset. No plotting available.")
  }

  if (is.null(minAge)) minAge <- model$minA1
  if (is.null(maxAge)) maxAge <- model$maxA1
  if (is.null(minNorm)) minNorm <- model$minL1
  if (is.null(maxNorm)) maxNorm <- model$maxL1

  if (is.null(stepAge))  stepAge <- (maxAge - minAge) / 100
  if (is.null(stepNorm)) stepNorm <- (maxNorm - minNorm) / 100

  if (order <= 0) stop("Order of derivative must be a positive integer.")

  rowS <- seq(minNorm, maxNorm, by = stepNorm)
  colS <- seq(minAge, maxAge, by = stepAge)

  coeff <- derive(model, order)
  if (length(coeff) == 0) {
    stop("Derivative of order ", order, " not available for this model.")
  }

  cat(
    paste0(
      rangeCheck(model, minAge, maxAge, minNorm, maxNorm),
      " Coefficients from the ", order, " order derivative function:\n\n"
    )
  )
  print(coeff)

  dev2 <- expand.grid(X = rowS, Y = colS)
  dev2$Z <- mapply(function(norm, age) predictRaw(norm, age, coeff), dev2$X, dev2$Y)

  ordinal <- if (order <= 3) c("st", "nd", "rd")[order] else "th"
  desc <- paste0(order, ordinal, " Order Derivative")
  custom_palette <- c(
    "#FF0000", "#FF4000", "#FF8000", "#FFBF00", "#FFFF00",
    "#80FF00", "#00FF00", "#00FF80", "#00FFFF", "#0080FF",
    "#0000FF", "#4B0082", "#8B00FF"
  )
  theme_custom <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 8),
      legend.position = "right",
      legend.text = element_text(size = 8),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  p <- ggplot(dev2, aes(x = .data$Y, y = .data$X, z = .data$Z)) +
    geom_tile(aes(fill = .data$Z)) +
    geom_contour(color = "white", alpha = 0.5) +
    scale_fill_gradientn(colors = custom_palette) +
    labs(
      title = "Slope of the Regression Function",
      x = "Explanatory Variable (Age)",
      y = paste("Norm Score - ", desc),
      fill = "Derivative"
    ) +
    theme_custom +
    theme(legend.position = "right")

  if (min(dev2$Z) < 0 && max(dev2$Z) > 0)
    p <- p + geom_contour(
      aes(z = .data$Z),
      color = "black",
      linewidth = 0.5,
      breaks = 0,
      linetype = "dashed"
    )

  return(p)
}


#' Compare Two Norm Models Visually
#'
#' This function creates a visualization comparing two norm models by displaying
#' their percentile curves. The first model is shown with solid lines, the second
#' with dashed lines. If age and score vectors are provided, manifest percentiles
#' are displayed as dots. The function works with regular cnorm models (Taylor polynomials),
#' beta-binomial models, Conway-Maxwell-Poisson (CMP) models, and shash models,
#' allowing comparison between different model types.
#'
#' For discrete count and accuracy models (beta-binomial and CMP), smooth continuous curves
#' are displayed by default (\code{discrete = FALSE}). For CMP models, these are the
#' continuity-corrected (mid-p) quantiles consistent with \code{\link{plot.cnormCMP}} and
#' the derived norm scores. For beta-binomial models, smooth lines are based on the
#' underlying beta (mixing) distribution. Setting \code{discrete = TRUE} displays the exact
#' discrete quantiles as step functions. The parameter has no effect on continuous models
#' (Taylor polynomials or shash).
#'
#' Observation weights (e.g. post-stratification or frequency weights) are fully respected
#' in the computation of manifest percentiles, age group centers, and all fit indices
#' (R-squared, Bias, RMSE, and MAD).
#'
#' @param model1 First model object (distribution-free, beta-binomial, CMP, or shash)
#' @param model2 Second model object (distribution-free, beta-binomial, CMP, or shash)
#' @param percentiles Vector with percentile scores, ranging from 0 to 1 (exclusive)
#' @param age Optional vector with manifest age or group values
#' @param score Optional vector with manifest raw score values
#' @param weights Optional vector with manifest weights
#' @param title Custom title for plot (optional)
#' @param subtitle Custom subtitle for plot (optional)
#' @param discrete Logical indicating whether discrete models (beta-binomial and CMP)
#'   are displayed with their exact discrete quantiles as step functions (\code{TRUE})
#'   or with smooth continuous curves (\code{FALSE}, default). Ignored for Taylor and shash models.
#'
#' @return A ggplot object showing the comparison of both models
#'
#' @export
#' @family plot
compare <- function(model1,
                    model2,
                    percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                    age = NULL,
                    score = NULL,
                    weights = NULL,
                    title = NULL,
                    subtitle = NULL,
                    discrete = FALSE) {

  # Helper: verify if model is CMP
  is_cmp <- function(m) inherits(m, "cnormCMP")

  # Helper: robust parametric check (including CMP)
  is_param <- function(m) {
    if (exists("isParametric", mode = "function")) {
      isParametric(m) || is_cmp(m)
    } else {
      inherits(m, c("cnormBetaBinomial", "cnormBetaBinomial2", "cnormShash", "cnormCMP"))
    }
  }

  # Retrieve score and age from model if missing and model is a cnorm (Taylor) object
  if (is.null(score) && isTaylor(model1)) {
    score <- model1$data[[attributes(model1$data)$raw]]
    age <- model1$data[[attributes(model1$data)$age]]
  }

  if (is.null(score) && isTaylor(model2)) {
    score <- model2$data[[attributes(model2$data)$raw]]
    age <- model2$data[[attributes(model2$data)$age]]
  }

  # Function to get predictions for beta-binomial models
  get_bb_predictions <- function(model, pred_ages) {
    if (inherits(model, "cnormBetaBinomial")) {
      preds <- predictCoefficients(model, pred_ages)
    } else {
      preds <- predictCoefficients2(model, pred_ages)
    }

    n_max <- attr(model$result, "max")
    pred_matrix <- matrix(NA_real_,
                          nrow = length(pred_ages),
                          ncol = length(percentiles))

    if (discrete) {
      for (j in seq_along(pred_ages)) {
        dist <- bb_distribution(preds$a[j], preds$b[j], n_max)
        if (!anyNA(dist$cum)) {
          pred_matrix[j, ] <- vapply(percentiles, function(p) {
            as.numeric(dist$x[which.max(dist$cum >= p)])
          }, numeric(1))
        }
      }
    } else {
      for (i in seq_along(percentiles)) {
        pred_matrix[, i] <- stats::qbeta(percentiles[i],
                                         shape1 = preds$a,
                                         shape2 = preds$b) * n_max
      }
    }

    pred_data <- data.frame(age = pred_ages, pred_matrix)
    names(pred_data)[-1] <- paste0("P", percentiles * 100)
    return(pred_data)
  }

  # Function to get predictions for shash models
  get_shash_predictions <- function(model, pred_ages) {
    preds <- predictCoefficients_shash(model, pred_ages)

    pred_matrix <- matrix(NA_real_,
                          nrow = length(pred_ages),
                          ncol = length(percentiles))
    for (i in seq_along(percentiles)) {
      pred_matrix[, i] <- qshash(
        percentiles[i],
        mu = preds$mu,
        sigma = preds$sigma,
        epsilon = preds$epsilon,
        delta = preds$delta
      )
    }

    pred_data <- data.frame(age = pred_ages, pred_matrix)
    names(pred_data)[-1] <- paste0("P", percentiles * 100)
    return(pred_data)
  }

  # Function to get predictions for CMP models
  get_cmp_predictions <- function(model, pred_ages) {
    preds <- predictCoefficients_cmp(model, pred_ages)
    o <- cmp_opts(model)
    n_pts <- length(pred_ages)

    pred_matrix <- matrix(NA_real_,
                          nrow = n_pts,
                          ncol = length(percentiles))

    q_mid <- if (exists("cmp_quantile_mid", mode = "function")) {
      cmp_quantile_mid
    } else {
      get("cmp_quantile_mid", envir = asNamespace("cNORM"))
    }

    for (i in seq_along(percentiles)) {
      p_val <- percentiles[i]
      if (discrete) {
        pred_matrix[, i] <- qcmp(
          p_val,
          mu = preds$mu,
          nu = preds$nu,
          max_score = if (is.null(o$max_score)) Inf else o$max_score
        )
      } else {
        pred_matrix[, i] <- q_mid(
          rep(p_val, n_pts),
          mu = preds$mu,
          nu = preds$nu,
          tol = o$tol,
          max_terms = o$max_terms,
          max_score = o$max_score
        )
      }
    }

    pred_data <- data.frame(age = pred_ages, pred_matrix)
    names(pred_data)[-1] <- paste0("P", percentiles * 100)
    return(pred_data)
  }

  # Function to get predictions for cnorm (Taylor) models
  get_cnorm_predictions <- function(model, pred_ages) {
    m <- model$model
    T <- stats::qnorm(percentiles, m$scaleM, m$scaleSD)

    pred_matrix <- matrix(NA_real_,
                          nrow = length(pred_ages),
                          ncol = length(percentiles))
    for (i in seq_along(pred_ages)) {
      pred_matrix[i, ] <- predictRaw(T, pred_ages[i], m$coefficients)
    }

    pred_data <- data.frame(age = pred_ages, pred_matrix)
    names(pred_data)[-1] <- paste0("P", percentiles * 100)
    return(pred_data)
  }

  # Determine age range
  get_age_range <- function(model) {
    if (is_param(model)) {
      return(c(
        attr(model$result, "ageMin"),
        attr(model$result, "ageMax")
      ))
    } else {
      m <- model$model
      return(c(m$minA1, m$maxA1))
    }
  }

  range1 <- get_age_range(model1)
  range2 <- get_age_range(model2)

  pred_ages <- seq(min(range1[1], range2[1]), max(range1[2], range2[2]), length.out = 100)

  step1 <- (isBeta(model1) || is_cmp(model1)) && isTRUE(discrete)
  step2 <- (isBeta(model2) || is_cmp(model2)) && isTRUE(discrete)

  plot_data1 <- if (isBeta(model1)) {
    get_bb_predictions(model1, pred_ages)
  } else if (isSHASH(model1)) {
    get_shash_predictions(model1, pred_ages)
  } else if (is_cmp(model1)) {
    get_cmp_predictions(model1, pred_ages)
  } else {
    get_cnorm_predictions(model1, pred_ages)
  }

  plot_data2 <- if (isBeta(model2)) {
    get_bb_predictions(model2, pred_ages)
  } else if (isSHASH(model2)) {
    get_shash_predictions(model2, pred_ages)
  } else if (is_cmp(model2)) {
    get_cmp_predictions(model2, pred_ages)
  } else {
    get_cnorm_predictions(model2, pred_ages)
  }

  # Prepare plotting data
  plot_data_long <- data.frame(
    age = numeric(),
    value = numeric(),
    percentile = character(),
    model = character()
  )

  for (i in 2:ncol(plot_data1)) {
    plot_data_long <- rbind(
      plot_data_long,
      data.frame(
        age = plot_data1$age,
        value = plot_data1[[i]],
        percentile = names(plot_data1)[i],
        model = "Model 1"
      )
    )
  }

  for (i in 2:ncol(plot_data2)) {
    plot_data_long <- rbind(
      plot_data_long,
      data.frame(
        age = plot_data2$age,
        value = plot_data2[[i]],
        percentile = names(plot_data2)[i],
        model = "Model 2"
      )
    )
  }

  if (!is.null(score)) {
    plot_data_long$value[plot_data_long$value < min(score)] <- min(score)
    plot_data_long$value[plot_data_long$value > max(score)] <- max(score)
  }

  perc_names <- paste0("P", percentiles * 100)
  plot_data_long$percentile <- factor(plot_data_long$percentile, levels = perc_names)

  if (is.null(title))    title <- "Visual Model Comparison"
  if (is.null(subtitle)) subtitle <- "Model 1: solid lines, Model 2: dashed lines"

  model_layer <- function(dat, lty, use_step) {
    if (use_step) {
      geom_step(
        data = dat,
        aes(x = .data$age, y = .data$value, color = .data$percentile),
        direction = "mid",
        linetype = lty,
        linewidth = 0.6
      )
    } else {
      geom_line(
        data = dat,
        aes(x = .data$age, y = .data$value, color = .data$percentile),
        linetype = lty,
        linewidth = 0.6
      )
    }
  }

  color_values <- stats::setNames(rainbow(length(percentiles)), perc_names)

  p <- ggplot() +
    model_layer(plot_data_long[plot_data_long$model == "Model 1", ], "solid", step1) +
    model_layer(plot_data_long[plot_data_long$model == "Model 2", ], "dashed", step2) +
    scale_color_manual(
      values = color_values,
      breaks = perc_names,
      labels = paste0(percentiles * 100, "%")
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Age",
      y = "Score",
      color = "Percentile"
    ) +
    theme_minimal() +
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

  # Information criteria
  calc_ic <- function(m) {
    if (isTaylor(m)) {
      ideal.model <- m$model$ideal.model
      rss <- m$model$subsets$rss[ideal.model]
      n <- nrow(m$data)
      sigma2 <- rss / (n - ideal.model - 1)
      loglik <- -0.5 * n * (log(2 * pi) + log(sigma2) + 1)
      c(AIC = -2 * loglik + 2 * ideal.model,
        BIC = m$model$subsets$bic[ideal.model])
    } else {
      n_obs <- attr(m$result, "N")
      n_params <- length(m$result$par)
      loglik <- -m$result$value
      c(AIC = 2 * n_params - 2 * loglik,
        BIC = n_params * log(n_obs) - 2 * loglik)
    }
  }

  ic1 <- calc_ic(model1)
  ic2 <- calc_ic(model2)
  AIC1 <- ic1["AIC"]; BIC1 <- ic1["BIC"]
  AIC2 <- ic2["AIC"]; BIC2 <- ic2["BIC"]

  if (!is.null(score) && !is.null(age)) {
    # Filter valid complete cases across age, score, and weights
    keep <- is.finite(age) & is.finite(score)
    if (!is.null(weights)) {
      keep <- keep & is.finite(weights) & (weights > 0)
    }
    age <- age[keep]
    score <- score[keep]
    if (!is.null(weights)) {
      weights <- weights[keep]
    }

    data <- data.frame(age = age, score = score)
    data$w <- if (!is.null(weights)) weights else rep(1, length(age))

    # Age groups for manifest percentiles
    if (length(age) / length(unique(age)) > 50 && min(table(data$age)) > 30) {
      data$group <- age
    } else {
      data$group <- getGroups(age)
    }

    # Manifest percentiles using observation weights and weighted mean age
    percentile.actual <- as.data.frame(do.call("rbind", lapply(split(
      data, data$group
    ), function(df) {
      c(age = stats::weighted.mean(df$age, w = df$w),
        weighted.quantile(df$score, probs = percentiles, weights = df$w))
    })))
    colnames(percentile.actual) <- c("age", perc_names)

    # Reshape manifest data
    manifest_data_long <- data.frame(age = numeric(),
                                     value = numeric(),
                                     percentile = character())

    for (i in 2:ncol(percentile.actual)) {
      manifest_data_long <- rbind(
        manifest_data_long,
        data.frame(
          age = percentile.actual$age,
          value = percentile.actual[[i]],
          percentile = names(percentile.actual)[i]
        )
      )
    }

    manifest_data_long$percentile <- factor(manifest_data_long$percentile, levels = perc_names)

    # Add manifest diamonds to plot
    p <- p + geom_point(
      data = manifest_data_long,
      aes(
        x = .data$age,
        y = .data$value,
        color = .data$percentile
      ),
      size = 2,
      shape = 18
    )

    # Group ranking with weights
    data <- rankByGroup(data, raw = "score", group = "group", weights = if (!is.null(weights)) "w" else NULL)
    data$normValue <- 10 * (data$normValue - attributes(data)$scaleMean) / attributes(data)$scaleSD

    # Helper for fitted values
    get_fitted <- function(m) {
      if (isTaylor(m)) {
        fit <- predictNorm(data$score, data$age, m,
                           minNorm = m$model$minL1, maxNorm = m$model$maxL1)
        10 * (fit - attributes(m$data)$scaleMean) / attributes(m$data)$scaleSD
      } else {
        fit <- predict(m, age = data$age, score = data$score)
        scM <- attr(m$result, "scaleMean")
        scSD <- attr(m$result, "scaleSD")
        if (is.null(scM) || !is.finite(scM)) scM <- 50
        if (is.null(scSD) || !is.finite(scSD)) scSD <- 10
        10 * (fit - scM) / scSD
      }
    }

    data$fitted1 <- get_fitted(model1)
    data$fitted2 <- get_fitted(model2)

    # --- Weighted statistical helpers for fit metrics ---
    w_mean <- function(x, w) {
      sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)])
    }

    w_cor2 <- function(x, y, w) {
      ok <- is.finite(x) & is.finite(y) & is.finite(w) & (w > 0)
      if (sum(ok) < 3L) return(NA_real_)
      x <- x[ok]; y <- y[ok]; w <- w[ok]
      w_sum <- sum(w)
      mx <- sum(w * x) / w_sum
      my <- sum(w * y) / w_sum
      cov_xy <- sum(w * (x - mx) * (y - my)) / w_sum
      var_x  <- sum(w * (x - mx)^2) / w_sum
      var_y  <- sum(w * (y - my)^2) / w_sum
      if (var_x > 0 && var_y > 0) (cov_xy / sqrt(var_x * var_y))^2 else NA_real_
    }

    # Fully weighted fit statistics
    R2a <- w_cor2(data$fitted1, data$normValue, data$w)
    R2b <- w_cor2(data$fitted2, data$normValue, data$w)

    bias1 <- w_mean(data$fitted1 - data$normValue, data$w)
    bias2 <- w_mean(data$fitted2 - data$normValue, data$w)

    RMSE1 <- sqrt(w_mean((data$fitted1 - data$normValue)^2, data$w))
    RMSE2 <- sqrt(w_mean((data$fitted2 - data$normValue)^2, data$w))

    MAD1 <- w_mean(abs(data$fitted1 - data$normValue), data$w)
    MAD2 <- w_mean(abs(data$fitted2 - data$normValue), data$w)

    fit_table <- data.frame(
      Metric = c("R2", "Bias", "RMSE", "MAD", "AIC", "BIC"),
      Model1 = c(R2a, bias1, RMSE1, MAD1, AIC1, BIC1),
      Model2 = c(R2b, bias2, RMSE2, MAD2, AIC2, BIC2),
      Difference = c(
        R2b - R2a,
        bias2 - bias1,
        RMSE2 - RMSE1,
        MAD2 - MAD1,
        AIC2 - AIC1,
        BIC2 - BIC1
      )
    )

    fit_table[, 2:4] <- round(fit_table[, 2:4], 4)

    cat("\nModel Comparison Summary:\n")
    cat("------------------------\n")
    print(format(fit_table, justify = "right"), row.names = FALSE)
    cat("\nNote: Difference = Model2 - Model1\n")
    cat("      Fit indices are based on manifest and fitted norm scores (weighted if weights provided).\n")
    cat("      Scale metrics are T scores (scaleSD = 10)\n")
    cat("      AIC and BIC should only be used when comparing models of the same type.\n")
  } else {
    fit_table <- data.frame(
      Metric = c("AIC", "BIC"),
      Model1 = c(AIC1, BIC1),
      Model2 = c(AIC2, BIC2),
      Difference = c(AIC2 - AIC1, BIC2 - BIC1)
    )

    cat("\nModel Comparison Summary:\n")
    cat("------------------------\n")
    print(format(fit_table, justify = "right"), row.names = FALSE)
    cat("\nNote: Difference = Model2 - Model1\n")
    cat("      AIC and BIC should only be used when comparing models of the same type.\n")
  }

  return(p)
}
