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
#' # Compute model with example dataset and plot results
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotRaw(result)
#' @import ggplot2
#' @export
#' @family plot
plotRaw <- function(model, group = FALSE, type = 0) {

  if(isParametric(model)) {
    stop("This function is not applicable for parametric models.")
  }

  if(!isTaylor(model)) {
    stop("Please provide a cnorm object.")
  }

  d <- model$data
  model <- model$model

  d$fitted <- model$fitted.values
  d$diff <- d$fitted - d$raw
  mse <- round(model$rmse, digits=4)
  r <- round(cor(d$fitted, d$raw, use = "pairwise.complete.obs"), digits = 4)
  d <- as.data.frame(d)

  if (group) {
    if("group" %in% colnames(d)){
      d$group <- as.factor(d$group)
    } else {
      d$group <- as.factor(getGroups(d$age))
    }
  }

  if (type == 0) {
    p <- ggplot(d, aes(x = .data$raw, y = .data$fitted)) +
      geom_point(alpha = 0.2, color = "#0033AA") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(
        title = if(isTRUE(group)) "Observed vs. Fitted Raw Scores by Group" else "Observed vs. Fitted Raw Scores",
        subtitle = paste("r =", r, ", RMSE =", mse),
        x = "Observed Score",
        y = "Fitted Scores"
      )
  } else {
    p <- ggplot(d, aes(x = .data$raw, y = .data$diff)) +
      geom_point(alpha = 0.2, color = "#0033AA") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = if(isTRUE(group)) "Observed Raw Scores vs. Difference Scores by Group" else "Observed Raw Scores vs. Difference Scores",
        subtitle = paste("r =", r, ", RMSE =", mse),
        x = "Observed Score",
        y = "Difference Scores"
      )
  }

  if (group) {
    p <- p + facet_wrap(~ group)
  }

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

  return(p)
}

#' @title Plot manifest and fitted norm scores
#'
#' @description
#' This function plots the manifest norm score against the fitted norm score from
#' the inverse regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' the regression line. Applicable for Taylor polynomial models.
#'
#' @param model The regression model, usually from the 'cnorm' or 'cnorm.betabinomial' function
#' @param age In case of beta binomial model, please provide the age vector
#' @param score In case of beta binomial model, please provide the score vector
#' @param width In case of beta binomial model, please provide the width for the sliding window.
#'              If null, the function tries to determine a sensible setting.
#' @param weights Vector or variable name in the dataset with weights for each
#' individual case. If NULL, no weights are used.
#' @param group On optional grouping variable, use empty string for no group, the variable name
#'              for Taylor polynomial models or a vector with the groups for beta binomial models
#' @param minNorm lower bound of fitted norm scores
#' @param maxNorm upper bound of fitted norm scores
#' @param type Type of display: 0 = plot manifest against fitted values, 1 = plot
#' manifest against difference values
#'
#' @return A ggplot object representing the norm scores plot.
#'
#' @examples
#' \dontrun{
#' # Load example data set, compute model and plot results
#'
#' # Taylor polynomial model
#' model <- cnorm(raw = elfe$raw, group = elfe$group)
#' plot(model, "norm")
#'
#' # Beta binomial models; maximum number of items in elfe is n = 28
#' model.bb <- cnorm.betabinomial(elfe$group, elfe$raw, n = 28)
#' plotNorm(model.bb, age = elfe$group, score = elfe$raw)
#' }
#'
#' @import ggplot2
#' @export
#' @family plot
plotNorm <- function(model, age = NULL, score = NULL, width = NULL, weights = NULL, group = FALSE, minNorm = NULL, maxNorm = NULL, type = 0) {

  is_taylor <- isTaylor(model)
  if(is_taylor) {
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
      age <- rep(0, length=nrow(data))

    d$fitted <- predictNorm(raw, age, model, minNorm = minNorm, maxNorm = maxNorm)

    if (group) {
      if("group" %in% colnames(d)){
        d$group <- as.factor(d$group)
      } else {
        d$group <- as.factor(getGroups(d$age))
      }
    }

  } else if(isParametric(model)) {
    if(is.null(age) || is.null(score)) {
      stop("Please provide age and score vectors for beta-binomial or shash models and the width for the sliding window.")
    }

    d <- data.frame(age = age, score = score)
    if(is.null(width)){
      if(length(age)/length(unique(age))<50)
        stop("Please provide a width for the sliding window.")

      d$group <- d$age
      if(is.null(weights))
        d <- rankByGroup(data = d, group = "age", raw = "score")
      else
        d <- rankByGroup(data = d, group = "age", raw = "score", weights = weights)
    }else{
      if(is.null(weights))
        d <- rankBySlidingWindow(data = d, age = "age", raw = "score", width = width)
      else
        d <- rankBySlidingWindow(data = d, age = "age", raw = "score", weights = weights, width = width)
    }


    d$fitted <- predict.cnormBetaBinomial(model, d$age, d$score)


  } else {
    stop("Please provide an object of type cnorm, cnormBetaBinomial or cnormBetaBinomial2.")
  }

  if(!"normValue" %in% colnames(d)) {
    stop("The 'normValue' column is missing from the data. Please ensure it's present for both cnorm and beta-binomial models.")
  }

  d$diff <- d$fitted - d$normValue
  d <- d[!is.na(d$fitted) & !is.na(d$diff), ]

  rmse <- round(sqrt(mean(d$diff^2)), digits = 4)
  r <- round(cor(d$fitted, d$normValue, use = "pairwise.complete.obs"), digits = 4)

  if (type == 0) {
    if(isTaylor(model)) {
      title <- if (isTRUE(group) || (is.character(group) && nzchar(group))) paste("Observed vs. Fitted Norm Scores by", group) else "Observed vs. Fitted Norm Scores"
    }else{
      title <- if(is.numeric(group)) paste("Observed vs. Fitted Norm Scores by group") else "Observed vs. Fitted Norm Scores"
    }

    p <- ggplot(d, aes(x = .data$normValue, y = .data$fitted)) +
      geom_point(alpha = 0.2, color = "#0033AA") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(
        title = title,
        subtitle = paste("r =", r, ", RMSE =", rmse),
        x = "Observed Scores",
        y = "Fitted Scores"
      )
  } else {
    if(is_taylor) {
      title <- if(group != "" && !is.null(group)) paste("Observed Norm Scores vs. Difference Scores by", group) else "Observed Norm Scores vs. Difference Scores"
    }else{
      title <- if(is.numeric(group)) paste("Observed Norm Scores vs. Difference Scores by group") else "Observed Norm Scores vs. Difference Scores"
    }

    p <- ggplot(d, aes(x = .data$normValue, y = .data$diff)) +
      geom_point(alpha = 0.5, color = "#0033AA") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = title,
        subtitle = paste("r =", r, ", RMSE =", rmse),
        x = "Observed Scores",
        y = "Difference"
      )
  }

  if (isTRUE(group) || (is.character(group) && nzchar(group))) {
    p <- p + facet_wrap(~ group)
  }



  p <- p + theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
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
#' This function plots the norm curves based on the regression model. It supports both
#' Taylor polynomial models and beta-binomial models.
#'
#' @param model The model from the bestModel function, a cnorm object, or a cnormBetaBinomial / cnormBetaBinomial2 object.
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
#' }
plotNormCurves <- function(model,
                           normList = NULL,
                           minAge = NULL,
                           maxAge = NULL,
                           step = 0.1,
                           minRaw = NULL,
                           maxRaw = NULL) {

  if(isTaylor(model)){
    model <- model$model
  }
  parametric <- isParametric(model)

  if(!isParametric(model) && !model$useAge){
    stop("Age or group variable explicitly set to FALSE in dataset. No plotting available.")
  }

  # Get scale information
  if(parametric) {
    scaleMean <- attr(model$result, "scaleMean")
    scaleSD <- attr(model$result, "scaleSD")
  } else {
    scaleMean <- model$scaleM
    scaleSD <- model$scaleSD
  }

  if(is.null(normList)){
    normList <- c(-2, -1, 0, 1, 2) * scaleSD + scaleMean
  }

  if (is.null(minAge)) {
    minAge <- if(parametric) attr(model$result, "age_mean") - 2 * attr(model$result, "age_sd") else model$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- if(parametric) attr(model$result, "age_mean") + 2 * attr(model$result, "age_sd") else model$maxA1
  }

  if (is.null(minRaw)) {
    minRaw <- if(parametric) 0 else model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- if(parametric) attr(model$result, "max") else model$maxRaw
  }

  frame_list <- lapply(normList, function(norm) {
    if (parametric) {
      ages <- seq(minAge, maxAge, by = step)
      raws <- sapply(ages, function(a) {
        pred <- predictCoefficients2(model, a, attr(model$result, "max"))
        qbeta(pnorm((norm - scaleMean) / scaleSD), pred$a, pred$b) *
          attr(model$result, "max")
      })
      data.frame(n = norm, raw = raws, age = ages)
    } else {
      normCurve <- getNormCurve(norm, model, minAge = minAge, maxAge = maxAge,
                                step = step, minRaw = minRaw, maxRaw = maxRaw)
      data.frame(n = norm, raw = normCurve$raw, age = normCurve$age)
    }
  })
  valueList <- do.call(rbind, frame_list)

  # Create rainbow color palette
  n_colors <- length(unique(valueList$n))
  color_palette <- rainbow(n_colors)

  # Create ggplot
  p <- ggplot(valueList, aes(x = .data$age, y = .data$raw, color = factor(.data$n))) +
    geom_line(linewidth = 1) +
    scale_color_manual(name = "Norm Score",
                       values = color_palette,
                       labels = paste("Norm", normList)) +
    labs(title = "Norm Curves",
         x = "Explanatory Variable (Age)",
         y = "Raw Score") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
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


#' Plot norm curves against actual percentiles
#'
#' The function plots the norm curves based on the regression model against
#' the actual percentiles from the raw data. As in 'plotNormCurves',
#' please check for inconsistent curves, especially intersections.
#' Violations of this assumption are a strong indication for problems
#' in modeling the relationship between raw and norm scores.
#' In general, extrapolation (point 1 and 2) can carefully be done to a
#' certain degree outside the original sample, but it should in general
#' be handled with caution.
#' The original percentiles are displayed as distinct points in the according
#' color, the model based projection of percentiles are drawn as lines.
#' Please note, that the estimation of the percentiles of the raw data is done with
#' the quantile function with the default settings.
#' In case, you get 'jagged' or disorganized percentile curve, try to reduce the 'k'
#' and/or 't' parameter in modeling.
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
#' self defined with a double vector with the mean and standard deviation,
#' f. e. c(10, 3) for Wechsler scale index points; if NULL, scale information from the
#' data preparation is used (default)
#' @param title custom title for plot
#' @param subtitle custom title for plot
#' @param points Logical indicating whether to plot the data points. Default is TRUE.
#' @seealso plotNormCurves, plotPercentileSeries
#' @examples
#' # Load example data set, compute model and plot results
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotPercentiles(result)
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

  if(isParametric(model)) {
    stop("This function is not applicable for parametric models (Beta Binomial or Sinh-Arcsinh). Please use 'plot(model, age, raw)' instead.")
  }

  if(isTaylor(model)){
    data <- model$data
    m <- model$model
  }else{
    stop("Please provide a cnorm object.")
  }


  if (is.null(group)) {
    group <- attr(data, "group")
  }

  age <- NULL
  if(is.null(data[[group]])){
    age <- data[, attributes(data)$age]
    data$group <- getGroups(data[, attributes(data)$age])
    data$age <- data[, attributes(data)$age]
    group <- "group"
  }

  if (is.null(minAge)) {
    minAge <- m$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- m$maxA1
  }

  if (is.null(minRaw)) {
    minRaw <- m$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- m$maxRaw
  }

  if (is.null(raw)) {
    raw <- m$raw
  }

  if (!(raw %in% colnames(data))) {
    stop(paste(c("ERROR: Raw score variable '", raw, "' does not exist in data object."), collapse = ""))
  }

  if (!(group %in% colnames(data))) {
    stop(paste(c("ERROR: Grouping variable '", group, "' does not exist in data object."), collapse = ""))
  }

  if (typeof(group) == "logical" && !group) {
    stop("The plotPercentiles-function does not work without a grouping variable.")
  }


  # compute norm scores from percentile vector
  if (is.null(scale)) {
    # fetch scale information from model
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
    # no transformation
    T <- percentiles
  }

  # generate variable names
  NAMES <- paste("PR", percentiles * 100, sep = "")
  NAMESP <- paste("PredPR", percentiles * 100, sep = "")

  # build function for xyplot and aggregate actual percentiles per group
  xyFunction <- paste(paste(NAMES, collapse = " + "),
                      paste(NAMESP, collapse = " + "),
                      sep = " + ", collapse = " + "
  )
  xyFunction <- paste(xyFunction, group, sep = " ~ ")

  w <- attributes(data)$weights
  data[, group] <- round(data[, group], digits=3)
  AGEP <- unique(data[, group])

  # get actual percentiles
  if(!is.null(attr(data, "descend"))&&attr(data, "descend")){
    percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data[, group]), function(df){weighted.quantile(df[, raw], probs = 1 - percentiles, weights = df$w)})))
  }else{
    percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data[, group]), function(df){weighted.quantile(df[, raw], probs = percentiles, weights = df$w)})))
  }
  percentile.actual$group <- as.numeric(rownames(percentile.actual))
  colnames(percentile.actual) <- c(NAMES, c(group))
  rownames(percentile.actual) <- AGEP

  # build finer grained grouping variable for prediction and fit predicted percentiles
  share <- seq(from = minAge, to = maxAge, length.out = 100)
  AGEP <- c(AGEP, share)
  percentile.fitted <- data.frame(matrix(NA,
                                         nrow = length(AGEP),
                                         ncol = length(T)
  ))

  norm_rep <- rep(T,    times = length(AGEP))   # T1,T2,...,Tk, T1,T2,...,Tk, ...
  age_rep  <- rep(AGEP, each  = length(T))       # A1,A1,...,A1, A2,A2,...,A2, ...

  preds <- predictRaw(norm_rep, age_rep, m$coefficients,
                      minRaw = minRaw, maxRaw = maxRaw)

  percentile.fitted <- as.data.frame(
    matrix(preds, nrow = length(AGEP), ncol = length(T), byrow = TRUE)
  )
  percentile.fitted$group <- AGEP
  percentile.fitted <- percentile.fitted[!duplicated(percentile.fitted$group), ]
  colnames(percentile.fitted)  <- c(NAMESP, group)
  rownames(percentile.fitted)  <- percentile.fitted$group

  # Merge actual and predicted scores and plot them show lines
  # for predicted scores and dots for actual scores
  percentile <- merge(percentile.actual, percentile.fitted,
                      by = group, all = TRUE
  )

  END <- .8
  COL1 <- rainbow(length(percentiles), end = END)
  COL2 <- c(rainbow(length(percentiles), end = END), rainbow(length(percentiles), end = END))


  if (is.null(title)) {
    title <- "Observed and Predicted Percentile Curves"
    subtitle <- bquote(paste("Model: ", .(m$ideal.model), ", R"^2, "=", .(round(m$subsets$adjr2[[m$ideal.model]], digits = 4))))
  }

  # Prepare data for ggplot
  plot_data <- data.frame(
    group = rep(percentile$group, 2 * length(percentiles)),
    value = c(as.matrix(percentile[, NAMES]), as.matrix(percentile[, NAMESP])),
    type = rep(c("Observed", "Predicted"), each = nrow(percentile) * length(percentiles)),
    percentile = factor(rep(rep(NAMES, each = nrow(percentile)), 2), levels = NAMES)
  )

  plot_data_predicted <- plot_data[plot_data$type == "Predicted", ]
  plot_data_observed <- plot_data[plot_data$type == "Observed", ]

  # Create the ggplot - ALIGNED ORDER: Points first, then lines
  p <- ggplot()

  # Add raw scores FIRST if points is TRUE (matches plot.cnormShash)
  if (points) {
    if(is.null(age)){
      p <- p + geom_point(data = data, aes(x = .data[[group]], y = .data[[raw]]),
                          color = "black", alpha = 0.2, size = 0.6)  # Matched size
    }else{
      p <- p + geom_point(data = data, aes(x = .data$age, y = .data[[raw]]),
                          color = "black", alpha = 0.2, size = 0.6)  # Matched size
    }
  }

  # Then add percentile lines and observed points
  p <- p +
    geom_line(data = plot_data_predicted,
              aes(x = .data$group, y = .data$value, color = .data$percentile),
              linewidth = 0.6) +  # Matched linewidth
    geom_point(data = plot_data_observed,
               aes(x = .data$group, y = .data$value, color = .data$percentile),
               na.rm = TRUE,
               size = 2,
               shape = 18) +
    labs(title = title,
         subtitle = subtitle,
         x = paste0("Explanatory Variable (", group, ")"),
         y = paste0("Raw Score (", raw, ")"),
         color = "Percentile") +  # Added legend title
    scale_color_manual(
      values = setNames(COL1, NAMES),
      labels = paste0(percentiles * 100, "%")  # Matched label format
    ) +
    guides(color = guide_legend(override.aes = list(
      linetype = rep("solid", length(NAMES)),
      shape = rep(18, length(NAMES))
    )))  # Matched legend override

  # Apply consistent theme
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
#' It supports both traditional continuous norming models and beta-binomial models.
#' The function allows for customization of the plot range and groups to be displayed.
#'
#' @param model The model from the bestModel function, a cnorm object, a cnormBetaBinomial, a cnormBetaBinomial2 or
#'    cnormShash object.
#' @param minRaw Lower bound of the raw score. If NULL, it's automatically determined based on the model type.
#' @param maxRaw Upper bound of the raw score. If NULL, it's automatically determined based on the model type.
#' @param minNorm Lower bound of the norm score. If NULL, it's automatically determined based on the model type.
#' @param maxNorm Upper bound of the norm score. If NULL, it's automatically determined based on the model type.
#' @param group Numeric vector specifying the age groups to plot. If NULL, groups are automatically selected.
#'
#' @return A ggplot object representing the density functions.
#'
#' @details
#' The function generates density curves for specified age groups, allowing for easy comparison of score distributions
#' across different ages.
#'
#' For beta-binomial models, the density is based on the probability mass function, while for
#' traditional models, it uses a normal distribution based on the norm scores.
#'
#' @note
#' Please check for inconsistent curves, especially those showing implausible shapes
#' such as violations of biuniqueness in the cnorm models.
#'
#' @seealso \code{\link{plotNormCurves}}, \code{\link{plotPercentiles}}
#'
#' @examples
#' \dontrun{
#' # For traditional continuous norming model
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotDensity(result, group = c(2, 4, 6))
#'
#' # For beta-binomial model
#' bb_model <- cnorm.betabinomial(age = ppvt$age, score = ppvt$raw, n = 228)
#' plotDensity(bb_model)
#' }
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

  if(isTaylor(model)){
    model <- model$model
  }

  is_beta_binomial <- isBeta(model)
  is_shash <- isSHASH(model)

  if (is.null(minNorm)) {
    minNorm <- if(is_beta_binomial) -3 else model$minL1
  }

  if (is.null(maxNorm)) {
    maxNorm <- if(is_beta_binomial) 3 else model$maxL1
  }

  if (is.null(minRaw)) {
    minRaw <- if (is_beta_binomial || is_shash) 0 else model$minRaw
  }
  if (is.null(maxRaw)) {
    maxRaw <- if (is_beta_binomial || is_shash) attr(model$result, "max") else model$maxRaw
  }

  title <- if (is_beta_binomial) "Density Functions (Beta-Binomial)"
  else if (is_shash) "Density Functions (SHASH)"
  else "Density Functions (Taylor Polynomial)"



  if (is.null(group)) {
    if(is_beta_binomial||is_shash) {
      age_min <- attr(model$result, "ageMin")
      age_max <- attr(model$result, "ageMax")
      group <- round(seq(from = age_min, to = age_max, length.out = 4), digits = 3)
    } else if(model$useAge) {
      group <- round(seq(from = model$minA1, to = model$maxA1, length.out = 4), digits = 3)
    } else {
      group <- c(1)
    }
  }

  step <- (maxNorm - minNorm) / 100

  matrix_list <- lapply(group, function(g) {
    if(is_beta_binomial) {
      norm <- normTable.betabinomial(model, g, attr(model$result, "max"))[[1]]
      norm$group <- rep(g, length.out = nrow(norm))
      colnames(norm)[colnames(norm) == "x"] <- "raw"
      colnames(norm)[colnames(norm) == "norm"] <- "norm1"
      colnames(norm)[colnames(norm) == "z"] <- "norm"
    } else if(is_shash) {
      norm <- normTable.shash(model, g, attr(model$result, "max"))[[1]]
      norm$group <- rep(g, length.out = nrow(norm))
      colnames(norm)[colnames(norm) == "x"] <- "raw"
      colnames(norm)[colnames(norm) == "norm"] <- "norm1"
      colnames(norm)[colnames(norm) == "z"] <- "norm"
    } else {
      norm <- normTable(g, model = model, minNorm = minNorm, maxNorm = maxNorm, minRaw = minRaw, maxRaw = maxRaw, step = step, pretty = FALSE)
      norm$group <- rep(g, length.out = nrow(norm))
    }
    return(norm)
  })

  matrix <- do.call(rbind, matrix_list)
  matrix <- matrix[matrix$norm > minNorm & matrix$norm < maxNorm, ]
  matrix <- matrix[matrix$raw > minRaw & matrix$raw < maxRaw, ]

  if(is_beta_binomial) {
    matrix$density <- matrix$Px
  } else {
    matrix$density <- dnorm(matrix$norm, mean = model$scaleM, sd = model$scaleSD)
  }

  # Create ggplot
  title <- ""
  if(is_beta_binomial) {
    title <- "Density Functions (Beta-Binomial)"
  } else {
    title <- "Density Functions (Taylor Polynomial)"
  }

  matrix <- matrix[complete.cases(matrix), ]
  p <- ggplot(matrix, aes(x = .data$raw, y = .data$density, color = factor(group))) +
    geom_line(linewidth = 1, na.rm = TRUE) +
    scale_color_viridis_d(name = "Group",
                          labels = paste("Group", group),
                          option = "plasma") +
    labs(title = title,
         x = "Raw Score",
         y = "Density") +
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


#' Generates a series of plots with number curves by percentile for different models
#'
#' This functions makes use of 'plotPercentiles' to generate a series of plots
#' with different number of predictors. It draws on the information provided by the model object
#' to determine the bounds of the modeling (age and standard score range). It can be used as an
#' additional model check to determine the best fitting model. Please have a look at the
#'' plotPercentiles' function for further information.
#' @param model The Taylor polynomial regression model object from the cNORM
#' @param start Number of predictors to start with
#' @param end Number of predictors to end with
#' @param group The name of the grouping variable; the distinct groups are automatically
#' determined
#' @param percentiles Vector with percentile scores, ranging from 0 to 1 (exclusive)
#' @param filename Prefix of the filename. If specified, the plots are saves as
#' png files in the directory of the workspace, instead of displaying them
#' @seealso plotPercentiles
#' @return the complete list of plots
#' @export
#'
#' @examples
#' # Load example data set, compute model and plot results
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotPercentileSeries(result, start=4, end=6)
#'
#' @family plot
plotPercentileSeries <- function(model, start = 1, end = NULL, group = NULL,
                                 percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                                 filename = NULL) {

  if(isParametric(model)) {
    stop("This function is not applicable for parametric models (Beta Binomial or Sinh-Arcsinh). Please use the plotDensity function instead.")
  }

  if(isTaylor(model)){
    d <- model$data
    model <- model$model
  }else{
    stop("Please provide a cnorm object.")
  }

  if (!attr(d, "useAge")){
    stop("Age or group variable explicitely set to FALSE in dataset. No plotting available.")
  }

  if ((is.null(end)) || (end > length(model$subsets$rss))) {
    end <- length(model$subsets$rss)
  }

  if (start < 1) {
    start <- 1
  }

  if (start > end) {
    start <- end
  }

  minR <- min(d[, model$raw])
  maxR <- max(d[, model$raw])
  l <- list()

  while (start <= end) {
    message(paste0("Plotting model ", start))
    # compute model
    text <- paste0(model$raw, " ~ ")
    names <- colnames(model$subsets$outmat)

    j <- 1
    nr <- 0
    while (j <= length(names)) {
      if (model$subsets$outmat[start, j] == "*") {
        text1 <- names[j]
        if (nr == 0) {
          text <- paste(text, text1, sep = "")
        } else {
          text <- paste(text, text1, sep = " + ")
        }

        nr <- nr + 1
      }
      j <- j + 1
    }

    bestformula <- lm(text, d)
    bestformula$ideal.model <- model$ideal.model
    bestformula$cutoff <- model$cutoff
    bestformula$subsets <- model$subsets
    bestformula$useAge <- model$useAge
    bestformula$maxA1 <- model$maxA1
    bestformula$minA1 <- model$minA1
    bestformula$minL1 <- model$minL1
    bestformula$maxL1 <- model$maxL1
    bestformula$minRaw <- minR
    bestformula$maxRaw <- maxR
    bestformula$raw <- model$raw
    bestformula$scaleSD <- attributes(d)$scaleSD
    bestformula$scaleM <- attributes(d)$scaleM
    bestformula$descend <- attributes(d)$descend
    bestformula$group <- attributes(d)$group
    bestformula$age <- attributes(d)$age
    bestformula$k <- attributes(d)$k

    result <- list(data = d, model = bestformula)
    class(result) <- "cnormTemp"

    l[[length(l) + 1]] <- plotPercentiles(result,
                                          minAge = model$minA1, maxAge = model$maxA1,
                                          minRaw = minR,
                                          maxRaw = maxR,
                                          percentiles = percentiles,
                                          scale = NULL,
                                          group = group,
                                          title = "Observed and Predicted Percentiles",
                                          subtitle = bquote(paste("Model with ", .(start), " predictors, ", R^2, "=",
                                                               .(round(bestformula$subsets$adjr2[[start]], digits = 4))))
    )

    if (!is.null(filename)) {
      ggsave(
        filename = paste0(filename, start, ".png"),
        plot = l[[length(l)]],  # Assuming 'chart' is your ggplot object
        device = "png",
        width = 10,  # Specify width in inches
        height = 7,  # Specify height in inches
        dpi = 300  # Specify resolution
      )
    }
    start <- start + 1
  }
  return(l)
}


#' Evaluate information criteria for regression model
#'
#' This function plots various information criteria and model fit statistics against
#' the number of predictors or adjusted R-squared, depending on the type of plot selected.
#' It helps in model selection by visualizing different aspects of model performance. Models,
#' which did not pass the initial consistency check are depicted with an empty circle.
#'
#' @param model The regression model from the bestModel function or a cnorm object.
#' @param type Integer specifying the type of plot to generate:
#'   \itemize{
#'     \item 0: Adjusted R2 by number of predictors (default)
#'     \item 1: Log-transformed Mallow's Cp by adjusted R2
#'     \item 2: Bayesian Information Criterion (BIC) by adjusted R2
#'     \item 3: Root Mean Square Error (RMSE) by number of predictors
#'     \item 4: Residual Sum of Squares (RSS) by number of predictors
#'     \item 5: F-test statistic for consecutive models by number of predictors
#'     \item 6: p-value for model tests by number of predictors
#'   }
#'
#' @return A ggplot object representing the selected information criterion plot.
#'
#' @details
#' The function generates different plots to help in model selection:
#'
#' - For types 1 and 2 (Mallow's Cp and BIC), look for the "elbow" in the curve where
#'   the information criterion begins to drop. This often indicates a good balance
#'   between model fit and complexity.
#' - For type 0 (Adjusted R2), higher values indicate better fit, but be cautious
#'   of overfitting with values approaching 1.
#' - For types 3 and 4 (RMSE and RSS), lower values indicate better fit.
#' - For type 5 (F-test), higher values suggest significant improvement with added predictors.
#' - For type 6 (p-values), values below the significance level (typically 0.05)
#'   suggest significant improvement with added predictors.
#'
#' @note
#' It's important to balance statistical measures with practical considerations and
#' to visually inspect the model fit using functions like \code{plotPercentiles}.
#'
#' @seealso \code{\link{bestModel}}, \code{\link{plotPercentiles}}, \code{\link{printSubset}}
#'
#' @examples
#' # Compute model with example data and plot information function
#' cnorm.model <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotSubset(cnorm.model)
#'
#' # Plot BIC against adjusted R-squared
#' plotSubset(cnorm.model, type = 2)
#'
#' # Plot RMSE against number of predictors
#' plotSubset(cnorm.model, type = 3)
#'
#' @import ggplot2
#' @export
#' @family plot
plotSubset <- function(model, type = 0) {

  if(isParametric(model)){
    stop("This function is not applicable for parametric models (Beta Binomial or Sinh-Arcsinh).")
  }

  if(isTaylor(model)){
    model <- model$model
  }

  # Compute F and significance
  RSS1 <- c(NA, model$subsets$rss)
  RSS2 <- c(model$subsets$rss, NA)
  k1 <- seq(from = 1, to = length(RSS1))
  k2 <- seq(from = 2, to = length(RSS1) + 1)
  df1 <- k2 - k1
  df2 <- length(model$fitted.values) - k2
  F <- ((RSS1-RSS2)/df1)/(RSS2/df2)
  p <- 1 - pf(F, df1, df2)

  filled <- rep(TRUE, length(model$subsets$rss))
  if(!is.null(model$subsets$consistent))
    filled <- model$subsets$consistent
  cutoff <- .99
  if(!is.null(model$cutoff))
    cutoff <- model$cutoff

  dataFrameTMP <- data.frame(
    adjr2 = model$subsets$adjr2,
    bic = model$subsets$bic,
    cp = model$subsets$cp,
    RSS = model$subsets$rss,
    RMSE = sqrt(model$subsets$rss / length(model$fitted.values)),
    F = head(F, -1),
    p = head(p, -1),
    nr = seq(1, length(model$subsets$adjr2), by = 1),
    filled = filled
  )

  # Improved base theme
  theme_custom <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  # Custom color palette
  custom_colors <- c("Model in Ascending Order" = "#1f77b4", "Cutoff Value" = "#d62728", "p = .05" = "#d62728")

  # Base plot
  p <- ggplot(dataFrameTMP) + theme_custom

  # Define plot based on type
  if (type == 1) {
    p <- p +
      geom_line(aes(x = .data$adjr2, y = .data$cp, color = "Model in Ascending Order"), linewidth = .75) +
      geom_point(aes(x = .data$adjr2, y = .data$cp, shape = .data$filled), size = 2.5, color = "#1f77b4") +
      scale_y_log10() +
      labs(title = "Information Function: Mallows's Cp",
           x = expression(paste("Adjusted ", R^2)),
           y = "log-transformed Mallows's Cp") +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = c(1, 16))
  } else if (type == 2) {
    p <- p +
      geom_line(aes(x = .data$adjr2, y = .data$bic, color = "Model in Ascending Order"), linewidth = .75) +
      geom_point(aes(x = .data$adjr2, y = .data$bic, shape = .data$filled), size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: BIC",
           x = expression(paste("Adjusted ", R^2)),
           y = "Bayesian Information Criterion (BIC)") +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = c(1, 16))
  } else if (type == 3) {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$RMSE, color = "Model in Ascending Order"), linewidth = .75) +
      geom_point(aes(x = .data$nr, y = .data$RMSE, shape = .data$filled), size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: RMSE",
           x = "Number of Predictors",
           y = "Root Mean Square Error (Raw Score)") +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = c(1, 16))
  } else if (type == 4) {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$RSS, color = "Model in Ascending Order"), linewidth = .75) +
      geom_point(aes(x = .data$nr, y = .data$RSS, shape = .data$filled), size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: RSS",
           x = "Number of Predictors",
           y = "Residual Sum of Squares (RSS)") +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = c(1, 16))
  } else if (type == 5) {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$F, color = "Model in Ascending Order"), na.rm = TRUE, linewidth = .75) +
      geom_point(aes(x = .data$nr, y = .data$F, shape = .data$filled), na.rm = TRUE, size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: F-test Statistics",
           x = "Number of Predictors",
           y = "F-test Statistics for Consecutive Models") +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = c(1, 16))
  } else if (type == 6) {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$p, color = "Model in Ascending Order"), na.rm = TRUE, linewidth = .75) +
      geom_point(aes(x = .data$nr, y = .data$p, shape = .data$filled), na.rm = TRUE, size = 2.5, color = "#1f77b4") +
      ylim(-0.005, 0.11) +
      labs(title = "Information Function: p-values",
           x = "Number of Predictors",
           y = expression(paste("p-values for Tests on ", R^2, " adj. of Consecutive Models"))) +
      geom_hline(aes(yintercept = 0.05, color = "p = .05"), linetype = "dashed", linewidth = 1) +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = c(1, 16))
  } else {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$adjr2, color = "Model in Ascending Order"), na.rm = TRUE, linewidth = .75) +
      geom_point(aes(x = .data$nr, y = .data$adjr2, shape = .data$filled), na.rm = TRUE, size = 2.5, color = "#1f77b4") +
      labs(title = expression(paste("Information Function: Adjusted ", R^2)),
           x = "Number of Predictors",
           y = expression(paste("Adjusted ", R^2))) +
      geom_hline(
        yintercept = cutoff,
        linetype = "dashed",
        linewidth = .8
      ) +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = c(1, 16))
  }

  # Add legend title
   p <- p + labs(color = "")

  return(p)
}

#'
#' @title Plot first order derivative of regression model
#'
#' @description
#' This function plots the scores obtained via the first order derivative of the regression model
#' in dependence of the norm score.
#'
#' @param model The model from the bestModel function, a cnorm object.
#' @param minAge Minimum age to start checking. If NULL, it's automatically determined from the model.
#' @param maxAge Maximum age for checking. If NULL, it's automatically determined from the model.
#' @param minNorm Lower end of the norm score range. If NULL, it's automatically determined from the model.
#' @param maxNorm Upper end of the norm score range. If NULL, it's automatically determined from the model.
#' @param stepAge Stepping parameter for the age check, usually 1 or 0.1; lower values indicate higher precision.
#' @param stepNorm Stepping parameter for norm scores.
#' @param order Degree of the derivative (default = 1).
#'
#' @details
#' The results indicate the progression of the norm scores within each age group. The regression-based
#' modeling approach relies on the assumption of a linear progression of the norm scores. Negative scores
#' in the first order derivative indicate a violation of this assumption. Scores near zero are typical
#' for bottom and ceiling effects in the raw data.
#'
#' The regression models usually converge within the range of the original values. In case of vertical
#' and horizontal extrapolation, with increasing distance to the original data, the risk of assumption
#' violation increases as well.
#'
#' @note
#' This function is currently incompatible with reversed raw score scales ('descent' option).
#'
#' @return A ggplot object representing the derivative of the regression function.
#'
#' @seealso \code{\link{checkConsistency}}, \code{\link{bestModel}}, \code{\link{derive}}
#'
#' @examples
#' # For traditional continuous norming model
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotDerivative(result, minAge=2, maxAge=5, stepAge=.2, minNorm=25, maxNorm=75, stepNorm=1)
#'
#'
#' @import ggplot2
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

  if(isTaylor(model)){
    model <- model$model
  }else if(isParametric(model)){
    stop("This function is not applicable for parametric models (Beta Binomial or Sinh-Arcsinh). Please use the plotDensity function instead.")
  }

  if (!model$useAge){
    stop("Age or group variable explicitly set to FALSE in dataset. No plotting available.")
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

  if (is.null(stepAge)) {
    stepAge <- (maxAge - minAge)/100
  }

  if (is.null(stepNorm)) {
    stepNorm <- (maxNorm - minNorm)/100
  }

  if(order <=0 )
    stop("Order of derivative must be a positive integer.")

  rowS <- seq(minNorm, maxNorm, by = stepNorm)
  colS <- seq(minAge, maxAge, by = stepAge)

  coeff <- derive(model, order)
  if(length(coeff) == 0){
    stop("Derivative of order ", order, " not available for this model.")
  }

  cat(paste0(rangeCheck(model, minAge, maxAge, minNorm, maxNorm), " Coefficients from the ", order, " order derivative function:\n\n"))
  print(coeff)


  dev2 <- expand.grid(X = rowS, Y = colS)
  dev2$Z <- mapply(function(norm, age) predictRaw(norm, age, coeff), dev2$X, dev2$Y)

  ordinal <- if (order <= 3) c("st", "nd", "rd")[order] else "th"
  desc <- paste0(order, ordinal, " Order Derivative")
  custom_palette <- c("#FF0000", "#FF4000", "#FF8000", "#FFBF00", "#FFFF00",
                      "#80FF00", "#00FF00", "#00FF80", "#00FFFF",
                      "#0080FF", "#0000FF", "#4B0082", "#8B00FF")
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
    labs(title = "Slope of the Regression Function",
         x = "Explanatory Variable (Age)",
         y = paste("Norm Score - ", desc),
         fill = "Derivative") +
    theme_custom +
    theme(legend.position = "right")

  if(min(dev2$Z)<0 && max(dev2$Z)>0)
    p <- p + geom_contour(aes(z = .data$Z), color = "black", linewidth = 0.5, breaks = 0, linetype = "dashed")

  return(p)
}

#' General convenience plotting function
#'
#' @param x a cnorm object
#' @param y the type of plot as a string, can be one of
#' 'raw' (1), 'norm' (2), 'curves' (3), 'percentiles' (4), 'series' (5), 'subset' (6),
#' or 'derivative' (7), either as a string or the according index
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
  if      (y == "raw"         || y == 1) plotRaw(x, ...)
  else if (y == "norm"        || y == 2) plotNorm(x, ...)
  else if (y == "curves"      || y == 3) plotNormCurves(x, ...)
  else if (y == "percentiles" || y == 4) plotPercentiles(x, ...)
  else if (y == "density"     || y == 5) plotDensity(x, ...)
  else if (y == "series"      || y == 6) plotPercentileSeries(x, ...)
  else if (y == "subset"      || y == 7) plotSubset(x, ...)
  else if (y == "derivative"  || y == 8) plotDerivative(x, ...)
  else plotPercentiles(x, ...)
}

#' Compare Two Norm Models Visually
#'
#' This function creates a visualization comparing two norm models by displaying
#' their percentile curves. The first model is shown with solid lines, the second
#' with dashed lines. If age and score vectors are provided, manifest percentiles
#' are displayed as dots. The function works with regular cnorm models, beta-binomial
#' models, and shash models, allowing comparison between different model types.
#'
#' @param model1 First model object (distribution free, beta-binomial, or shash)
#' @param model2 Second model object (distribution free, beta-binomial, or shash)
#' @param age Optional vector with manifest age or group values
#' @param score Optional vector with manifest raw score values
#' @param weights Optional vector with manifest weights
#' @param percentiles Vector with percentile scores, ranging from 0 to 1 (exclusive)
#' @param title Custom title for plot (optional)
#' @param subtitle Custom subtitle for plot (optional)
#'
#' @return A ggplot object showing the comparison of both models
#'
#' @examples
#' \dontrun{
#' # Compare different types of models
#' model1 <- cnorm(group = elfe$group, raw = elfe$raw)
#' model2 <- cnorm.betabinomial(elfe$group, elfe$raw)
#' model3 <- cnorm.shash(elfe$group, elfe$raw)
#'
#' # Compare traditional cnorm with shash
#' compare(model1, model3, age = elfe$group, score = elfe$raw)
#'
#' # Compare beta-binomial with shash
#' compare(model2, model3, age = elfe$group, score = elfe$raw)
#' }
#'
#' @export
#' @family plot
compare <- function(model1, model2,
                    percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                    age = NULL,
                    score = NULL,
                    weights = NULL,
                    title = NULL,
                    subtitle = NULL) {

  # retrieve score from model if score is null and one of the
  # models is a cnorm object
  if(is.null(score) && isTaylor(model1)){
    score <- model1$data[[attributes(model1$data)$raw]]
    age <- model1$data[[attributes(model1$data)$age]]
  }

  if(is.null(score) && isTaylor(model2)){
    score <- model2$data[[attributes(model2$data)$raw]]
    age <- model2$data[[attributes(model2$data)$age]]
  }

  # Function to get predictions for beta-binomial models
  get_bb_predictions <- function(model, pred_ages) {
    if(inherits(model, "cnormBetaBinomial")) {
      preds <- predictCoefficients(model, pred_ages)
    } else {
      preds <- predictCoefficients2(model, pred_ages)
    }

    pred_matrix <- matrix(NA, nrow = length(pred_ages), ncol = length(percentiles))
    for(i in seq_along(percentiles)) {
      pred_matrix[,i] <- qbeta(percentiles[i],
                               shape1 = preds$a,
                               shape2 = preds$b) * attr(model$result, "max")
    }

    pred_data <- data.frame(age = pred_ages, pred_matrix)
    names(pred_data)[-1] <- paste0("P", percentiles * 100)
    return(pred_data)
  }

  # Function to get predictions for shash models
  get_shash_predictions <- function(model, pred_ages) {
    preds <- predictCoefficients_shash(model, pred_ages)

    pred_matrix <- matrix(NA, nrow = length(pred_ages), ncol = length(percentiles))
    for(i in seq_along(percentiles)) {
      pred_matrix[,i] <- qshash(percentiles[i],
                                mu = preds$mu,
                                sigma = preds$sigma,
                                epsilon = preds$epsilon,
                                delta = preds$delta)
    }

    pred_data <- data.frame(age = pred_ages, pred_matrix)
    names(pred_data)[-1] <- paste0("P", percentiles * 100)
    return(pred_data)
  }

  # Function to get predictions for cnorm models
  get_cnorm_predictions <- function(model, pred_ages) {
    m <- model$model
    T <- qnorm(percentiles, m$scaleM, m$scaleSD)

    pred_matrix <- matrix(NA, nrow = length(pred_ages), ncol = length(percentiles))
    for(i in 1:length(pred_ages)) {
      pred_matrix[i,] <- predictRaw(T, pred_ages[i], m$coefficients)
    }

    pred_data <- data.frame(age = pred_ages, pred_matrix)
    names(pred_data)[-1] <- paste0("P", percentiles * 100)
    return(pred_data)
  }

  # Determine age range
  get_age_range <- function(model) {
    if(isParametric(model)) {
      return(c(attr(model$result, "ageMin"), attr(model$result, "ageMax")))
    } else {
      m <- model$model
      return(c(m$minA1, m$maxA1))
    }
  }

  # Get age ranges for both models
  range1 <- get_age_range(model1)
  range2 <- get_age_range(model2)

  # Create common age sequence
  pred_ages <- seq(min(range1[1], range2[1]),
                   max(range1[2], range2[2]),
                   length.out = 100)

  # Get predictions for both models
  plot_data1 <- if(isBeta(model1)) {
    get_bb_predictions(model1, pred_ages)
  } else if(isSHASH(model1)) {
    get_shash_predictions(model1, pred_ages)
  } else {
    get_cnorm_predictions(model1, pred_ages)
  }

  plot_data2 <- if(isBeta(model2)) {
    get_bb_predictions(model2, pred_ages)
  } else if(isSHASH(model2)) {
    get_shash_predictions(model2, pred_ages)
  } else {
    get_cnorm_predictions(model2, pred_ages)
  }

  # Prepare data for plotting (reshape to long format using base R)
  plot_data_long <- data.frame(
    age = numeric(),
    value = numeric(),
    percentile = character(),
    model = character()
  )

  # Reshape data for model 1
  for(i in 2:ncol(plot_data1)) {
    plot_data_long <- rbind(plot_data_long,
                            data.frame(
                              age = plot_data1$age,
                              value = plot_data1[[i]],
                              percentile = names(plot_data1)[i],
                              model = "Model 1"
                            ))
  }

  # Reshape data for model 2
  for(i in 2:ncol(plot_data2)) {
    plot_data_long <- rbind(plot_data_long,
                            data.frame(
                              age = plot_data2$age,
                              value = plot_data2[[i]],
                              percentile = names(plot_data2)[i],
                              model = "Model 2"
                            ))
  }

  if(!is.null(score)) {
    plot_data_long$value[plot_data_long$value < min(score)] <- min(score)
    plot_data_long$value[plot_data_long$value > max(score)] <- max(score)
  }

  # Set factor levels for correct ordering
  plot_data_long$percentile <- factor(plot_data_long$percentile,
                                      levels = paste0("P", percentiles * 100))

  # Set default title if none provided
  if (is.null(title)) {
    title <- "Visual Model Comparison"
  }

  if (is.null(subtitle)) {
    subtitle <- "Model 1: solid lines, Model 2: dashed lines"
  }

  # Create plot
  p <- ggplot() +
    geom_line(data = plot_data_long[plot_data_long$model == "Model 1",],
              aes(x = .data$age, y = .data$value, color = .data$percentile),
              linetype = "solid", linewidth = 0.6) +
    geom_line(data = plot_data_long[plot_data_long$model == "Model 2",],
              aes(x = .data$age, y = .data$value, color = .data$percentile),
              linetype = "dashed", linewidth = 0.6) +
    scale_color_manual(values = rainbow(length(percentiles)),
                       labels = paste0(percentiles * 100, "%")) +
    labs(title = title,
         subtitle = subtitle,
         x = "Age",
         y = "Score",
         color = "Percentile") +
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
  if(isTaylor(model1)) {
    ideal.model <- model1$model$ideal.model
    rss <- model1$model$subsets$rss[ideal.model]
    n <- nrow(model1$data)

    sigma2 <- rss / (nrow(model1$data) - ideal.model - 1)  # residual variance
    loglik <- -0.5 * n * (log(2 * pi) + log(sigma2) + 1)

    AIC1 <- -2 * loglik + 2 * ideal.model
    BIC1 <- model1$model$subsets$bic[ideal.model]
  } else{
    n_obs <- attr(model1$result, "N")
    n_params <- length(model1$result$par)
    log_likelihood <- -model1$result$value
    AIC1 <- 2 * n_params - 2 * log_likelihood
    BIC1 <- n_params * log(n_obs) - 2 * log_likelihood
  }

  if(isTaylor(model2)) {
    ideal.model <- model2$model$ideal.model
    rss <- model2$model$subsets$rss[ideal.model]
    n <- nrow(model2$data)

    sigma2 <- rss / (nrow(model2$data) - ideal.model - 1)  # residual variance
    loglik <- -0.5 * n * (log(2 * pi) + log(sigma2) + 1)

    AIC2 <- -2 * loglik + 2 * ideal.model
    BIC2 <- model2$model$subsets$bic[ideal.model]
  } else{
    n_obs <- attr(model2$result, "N")
    n_params <- length(model2$result$par)
    log_likelihood <- -model2$result$value
    AIC2 <- 2 * n_params - 2 * log_likelihood
    BIC2 <- n_params * log(n_obs) - 2 * log_likelihood
  }

  if (!is.null(score) & !is.null(age)) {
    # Prepare data for manifest percentiles and fit statistics
    data <- data.frame(age = age, score = score)
    if(!is.null(weights)){
      data$w <- weights
    }else{
      data$w <- rep(1, length(age))
    }

    # Calculate groups for manifest percentiles
    if (length(age) / length(unique(age)) > 50 && min(table(data$age)) > 30) {
      data$group <- age
    } else {
      data$group <- getGroups(age)
    }

    # Calculate manifest percentiles
    percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data$group), function(df) {
      c(age = mean(df$age),
        weighted.quantile(df$score, probs = percentiles, weights = df$w))
    })))
    colnames(percentile.actual) <- c("age", paste0("P", percentiles * 100))

    # Reshape manifest data
    manifest_data_long <- data.frame(
      age = numeric(),
      value = numeric(),
      percentile = character()
    )

    for(i in 2:ncol(percentile.actual)) {
      manifest_data_long <- rbind(manifest_data_long,
                                  data.frame(
                                    age = percentile.actual$age,
                                    value = percentile.actual[[i]],
                                    percentile = names(percentile.actual)[i]
                                  ))
    }

    manifest_data_long$percentile <- factor(manifest_data_long$percentile,
                                            levels = paste0("P", percentiles * 100))

    # Add manifest percentiles to plot
    p <- p + geom_point(
      data = manifest_data_long,
      aes(x = .data$age, y = .data$value, color = .data$percentile),
      size = 2,
      shape = 18
    )

    # Calculate fit statistics
    if(is.null(weights)){
      data <- rankByGroup(data, raw="score", group="group")
    }else{
      data <- rankByGroup(data, raw="score", group="group", weights="w")
    }
    data$normValue <- 10*(data$normValue - attributes(data)$scaleMean) / attributes(data)$scaleSD

    # Get predictions for both models
    if(isTaylor(model1)){
      data$fitted1 <- predictNorm(data$score, data$age, model1,
                                  minNorm = model1$model$minL1,
                                  maxNorm = model1$model$maxL1)
      data$fitted1 <- 10*(data$fitted1 - attributes(model1$data)$scaleMean) / attributes(model1$data)$scaleSD
    }else if(isParametric(model1)) {
      data$fitted1 <- predict(model1, data$age, data$score)
      scaleMean <- attr(model1$result, "scaleMean")
      scaleSD <- attr(model1$result, "scaleSD")
      data$fitted1 <- 10*(data$fitted1 - scaleMean) / scaleSD
    }

    if(isTaylor(model2)){
      data$fitted2 <- predictNorm(data$score, data$age, model2,
                                  minNorm = model2$model$minL1,
                                  maxNorm = model2$model$maxL1)
      data$fitted2 <- 10*(data$fitted2 - attributes(model2$data)$scaleMean) / attributes(model2$data)$scaleSD
    }else if(isParametric(model2)) {
      data$fitted2 <- predict(model2, data$age, data$score)
      scaleMean <- attr(model2$result, "scaleMean")
      scaleSD <- attr(model2$result, "scaleSD")
      data$fitted2 <- 10*(data$fitted2 - scaleMean) / scaleSD
    }

    # Calculate fit statistics
    R2a <- cor(data$fitted1, data$normValue, use = "pairwise.complete.obs")^2
    R2b <- cor(data$fitted2, data$normValue, use = "pairwise.complete.obs")^2

    bias1 <- mean(data$fitted1 - data$normValue, na.rm = TRUE)
    bias2 <- mean(data$fitted2 - data$normValue, na.rm = TRUE)

    RMSE1 <- sqrt(mean((data$fitted1 - data$normValue)^2, na.rm = TRUE))
    RMSE2 <- sqrt(mean((data$fitted2 - data$normValue)^2, na.rm = TRUE))

    MAD1 <- mean(abs(data$fitted1 - data$normValue), na.rm = TRUE)
    MAD2 <- mean(abs(data$fitted2 - data$normValue), na.rm = TRUE)


    # Create and print summary table
    fit_table <- data.frame(
      Metric = c("R2", "Bias", "RMSE", "MAD", "AIC", "BIC"),
      Model1 = c(R2a, bias1, RMSE1, MAD1, AIC1, BIC1),
      Model2 = c(R2b, bias2, RMSE2, MAD2, AIC2, BIC2),
      Difference = c(R2b - R2a,
                     bias2 - bias1,
                     RMSE2 - RMSE1,
                     MAD2 - MAD1,
                     AIC2 - AIC1,
                     BIC2 - BIC1)
    )

    # Round values
    fit_table[, 2:4] <- round(fit_table[, 2:4], 4)

    cat("\nModel Comparison Summary:\n")
    cat("------------------------\n")
    print(format(fit_table, justify = "right"), row.names = FALSE)
    cat("\nNote: Difference = Model2 - Model1\n")
    cat("      Fit indices are based on the manifest and fitted norm scores of both models.\n")
    cat("      Scale metrics are T scores (scaleSD = 10)\n")
    cat("      AIC and BIC should only be used when comparing models of the same type.\n")
  }else{
    # Create and print summary table
    fit_table <- data.frame(
      Metric = c("AIC", "BIC"),
      Model1 = c(AIC1, BIC1),
      Model2 = c(AIC2, BIC2),
      Difference = c(AIC2 - AIC1,
                     BIC2 - BIC1)
    )

    cat("\nModel Comparison Summary:\n")
    cat("------------------------\n")
    print(format(fit_table, justify = "right"), row.names = FALSE)
    cat("\nNote: Difference = Model2 - Model1\n")
    cat("      AIC and BIC should only be used when comparing models of the same type.\n")
  }

  return(p)
}
