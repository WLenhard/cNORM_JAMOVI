continuousClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "continuousClass",
  inherit = continuousBase,
  private = list(
    .manifestNorms = NA,
    .manifestPerc = NA,
    .predictedNorms = NA,
    .predictedPerc = NA,
    .rowNames = NA,
    .raw = NA,
    
    .init = function() {
      private$.manifestNorms <- NULL
      private$.manifestPerc <- NULL
      private$.predictedNorms <- NULL
      private$.predictedPerc <- NULL
      private$.raw <- NULL
      private$.rowNames <- NULL
      
      self$results$instructions$setContent(
        "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>The module estimates continuous norm scores by modeling the functional relationship between raw scores (raw), 
            norm scores (L) and the grouping variable (A; e. g. age, schooling duration ...) using the cNORM package 
            (A. Lenhard, Lenhard & Gary, 2024; v.3.4.0). The modeling procedure minimizes the error variance contained in the norm 
            scores. It requires smaller samples sizes compared to conventional norming, closes gaps within and between the norm 
            tables and smoothes sampling errors. The approach is distibution free and does not rely on assumptions on the nature 
            of the data.</p>
            <p>The functions tries to determine a closely fitting, yet consistent model. Avoid intersecting 
            percentile curves. Please proceed as follows:</p>
            <ol>
              <li>Select your <b>raw score</b> variable</li>
              <li>Place the <b>grouping variable</b> (e. g. grade) in the 'Grouping Variable' slot. Each group in the dataset should contain at least 50 cases, preferably 100.</li>
              <li>Adjust model parameters for retrieving a favorable model via visual inspection of the percentile plot. Pay attention to the following parameters:
                  <ul>
                    <li><b>Invert ranking order</b>: Please tick, e. g. if higher values depict lower performance as it is the case in error rates, reaction times ...</li>
                    <li><b>Degree of location</b>: To model the distribution of raw scores per group, it is advisable to set this value to 5 (indicating modelling up to power 5 in the polynomials)</li>
                    <li><b>Degree of age</b>: Indicates the complexity of the age trajectory. Setting this value to three models the age trajectories up to cubic relationships.</li>
                    <li><b>Number of terms</b>: cNORM tries to find a model that is both well fitting and parsimonious. You might want to change this manually to find other suiting solutions. Beware of high values since these entail the risk of overfitting.</li>
                  </ul>
              </li>
              <li>Specify the level of the grouping variable to generate norm table.</li>
            </ol>
            <p>Further information on the procedure is available via <a href=\"https://www.psychometrica.de/cNorm_jamovi_en.html\" target=\"_blank\">Psychometrica</a>.</p>
            </div>
            </body>
            </html>")
      self$results$norms$setNote("empty", "Please specify value of grouping variable for norm data computation", init=TRUE)
    },
    
    .safeModelFit = function(data, terms = NULL, k = 5, t = 3, weights = NULL) {
      tryCatch({
        if (!is.null(terms)) {
          if (terms <= 0 || terms >= ((k + 1)*(t + 1) - 1)) {
            return(list(
              success = FALSE,
              message = paste0("Number of terms (", terms, ") is out of valid range (1 to ", ((k + 1)*(t + 1) - 2), ")."),
              model = NULL
            ))
          }
          
          if (!is.null(weights)) {
            model <- bestModel(data, terms = terms, k = k, t = t, weights = weights, plot = FALSE)
          } else {
            model <- bestModel(data, terms = terms, k = k, t = t, plot = FALSE)
          }
        } else {
          if (!is.null(weights)) {
            model <- bestModel(data, k = k, t = t, weights = weights, plot = FALSE)
          } else {
            model <- bestModel(data, k = k, t = t, plot = FALSE)
          }
        }
        
        if (is.null(model)) {
          return(list(
            success = FALSE,
            message = "Model fitting failed. Try different parameters or check your data.",
            model = NULL
          ))
        }
        
        return(list(
          success = TRUE,
          message = NULL,
          model = model
        ))
      }, error = function(e) {
        return(list(
          success = FALSE,
          message = paste0("Error fitting model: ", e$message),
          model = NULL
        ))
      })
    },
    
    .resolveScale = function(scale, sd) {
      result <- list(m1 = 0, sd1 = 1, minNorm = 20, maxNorm = 80, scaleParam = scale)
      
      if(scale == "Wechsler") {
        result$m1 <- 10
        result$sd1 <- 3
        result$scaleParam <- c(10, 3)
        result$minNorm <- 10 - (sd * 3)
        result$maxNorm <- 10 + (sd * 3)
      } else if(scale == "PISA") {
        result$m1 <- 500
        result$sd1 <- 100
        result$scaleParam <- c(500, 100)
        result$minNorm <- 500 - (sd * 100)
        result$maxNorm <- 500 + (sd * 100)
      } else if(scale == "T") {
        result$m1 <- 50
        result$sd1 <- 10
        result$scaleParam <- c(50, 10)
        result$minNorm <- 50 - (sd * 10)
        result$maxNorm <- 50 + (sd * 10)
      } else if(scale == "IQ") {
        result$m1 <- 100
        result$sd1 <- 15
        result$scaleParam <- c(100, 15)
        result$minNorm <- 100 - (sd * 15)
        result$maxNorm <- 100 + (sd * 15)
      } else if(scale == "z") {
        result$m1 <- 0
        result$sd1 <- 1
        result$scaleParam <- c(0, 1)
        result$minNorm <- -sd
        result$maxNorm <- sd
      }
      
      return(result)
    },
    
    .resolveK = function(kOption) {
      k <- 5
      if(kOption == '1') k <- 1
      else if(kOption == '2') k <- 2
      else if(kOption == '3') k <- 3
      else if(kOption == '4') k <- 4
      else if(kOption == '6') k <- 6
      return(k)
    },
    
    .resolveT = function(tOption) {
      t <- 3
      if(tOption == '1') t <- 1
      else if(tOption == '2') t <- 2
      else if(tOption == '4') t <- 4
      else if(tOption == '5') t <- 5
      else if(tOption == '6') t <- 6
      return(t)
    },
    
    .run = function() {
      if (is.null(self$options$raw) || is.null(self$options$group)){
        self$results$instructions$setVisible(visible = TRUE)
        return()
      } else {
        self$results$instructions$setContent("<html/>")
        self$results$instructions$setVisible(visible = FALSE)
      }
      
      x <- jmvcore::toNumeric(self$data[[self$options$raw]])
      g <- jmvcore::toNumeric(self$data[[self$options$group]])
      
      if(!is.null(self$options$weights)){
        w <- jmvcore::toNumeric(self$data[[self$options$weights]])
        data <- data.frame(raw = x, group = g, weights = w)
      } else {
        data <- data.frame(raw = x, group = g)
      }
      
      # CRITICAL: Preserve original Jamovi row indices so that
      # setRowNums() maps values to the correct spreadsheet rows.
      rownames(data) <- rownames(self$data)
      
      # Filter complete cases — rownames carry through
      dataComplete <- data[complete.cases(data), ]      
      if(nrow(dataComplete) == 0) {
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent("<html><head></head><body>
                                              <div class='instructions'>
                                              <p>No complete cases found. Please check your data for missing values.</p>
                                              </div></body></html>")
        return()
      }
      
      if(!is.null(self$options$weights) && min(dataComplete$weights, na.rm = TRUE) < 1){
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent("<html><head></head><body>
                                              <div class='instructions'>
                                              <p>The weighting variable must only contain values >= 1.</p>
                                              </div></body></html>")
        return()
      }
      
      k <- private$.resolveK(self$options$k)
      t <- private$.resolveT(self$options$t)
      
      scaleInfo <- private$.resolveScale(self$options$scale, self$options$range)
      m1       <- scaleInfo$m1
      sd1      <- scaleInfo$sd1
      minNorm  <- scaleInfo$minNorm
      maxNorm  <- scaleInfo$maxNorm
      scale    <- scaleInfo$scaleParam
      
      descend <- self$options$descend
      
      if(!is.null(self$options$weights)){
        dataComplete <- rankByGroup(dataComplete, raw = dataComplete$raw, group = dataComplete$group,
                                    weights = dataComplete$weights, scale = scale, descend = descend)
      } else {
        dataComplete <- rankByGroup(dataComplete, raw = dataComplete$raw, group = dataComplete$group,
                                    scale = scale, descend = descend)
      }
      
      dataComplete <- computePowers(dataComplete, k = k, t = t)
      attr(dataComplete, "k") <- k
      attr(dataComplete, "t") <- t
      
      # Capture row names AFTER rankByGroup/computePowers which may
      # reorder rows. If these functions preserve rownames (standard R
      # behavior), they still map to original Jamovi positions.
      # If they strip rownames, fall back to integer indices.
      if(is.null(rownames(dataComplete))) {
        warning("rownames lost after rankByGroup/computePowers — output columns may misalign")
        rownames(dataComplete) <- seq_len(nrow(dataComplete))
      }
      private$.rowNames <- rownames(dataComplete)
      
      if(self$options$selectionType == "automaticSelection"){
        modelResult <- private$.safeModelFit(
          data    = dataComplete,
          k       = k,
          t       = t,
          weights = if(!is.null(self$options$weights)) dataComplete$weights else NULL
        )
      } else {
        terms <- as.integer(self$options$terms)
        modelResult <- private$.safeModelFit(
          data    = dataComplete,
          terms   = terms,
          k       = k,
          t       = t,
          weights = if(!is.null(self$options$weights)) dataComplete$weights else NULL
        )
      }
      
      if (!modelResult$success) {
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>",
                                                    "<b>Error during model fitting:</b> ", modelResult$message,
                                                    "</div></body></html>"))
        return()
      }
      
      model <- modelResult$model
      
      if(self$options$model){
        self$results$modelTab$setRow(rowNo = 1, values = list(
          Variable = "Model summary",
          Weight   = "",
          Terms    = model$ideal.model,
          RMSE     = model$rmse,
          R2adj    = model$subset$adjr2[model$ideal.model],
          BIC      = model$subset$bic[model$ideal.model]
        ))
        for(i in seq_along(model$coefficients)){
          self$results$modelTab$addRow(rowKey = (i + 1), values = list(
            Variable = names(model$coefficients)[i],
            Weight   = model$coefficients[[i]],
            Terms    = "",
            RMSE     = "",
            R2adj    = "",
            BIC      = ""
          ))
        }
      }
      
      if(!is.null(self$options$normAge) && nchar(self$options$normAge) > 0){
        normAge <- as.numeric(self$options$normAge)
        
        if(!is.na(normAge)){
          minRaw <- self$options$minRaw
          maxRaw <- self$options$maxRaw
          
          if(!is.null(minRaw) && !is.null(maxRaw) && maxRaw <= minRaw){
            minRaw <- NULL
            maxRaw <- NULL
          }
          
          rawTableResult <- tryCatch({
            tab <- rawTable(normAge, model,
                            minNorm = minNorm, maxNorm = maxNorm,
                            minRaw  = minRaw,  maxRaw  = maxRaw,
                            step    = as.numeric(self$options$stepping))
            list(success = TRUE, table = tab)
          }, error = function(e) {
            list(success = FALSE, message = paste0("Error generating norm table: ", e$message))
          })
          
          if (!rawTableResult$success) {
            self$results$instructions$setVisible(visible = TRUE)
            self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>",
                                                        rawTableResult$message,
                                                        "</div></body></html>"))
            return()
          }
          
          tab   <- rawTableResult$table
          table <- self$results$norms
          table$setRow(rowNo = 1, values = list(
            Raw        = tab$raw[[1]],
            Norm       = tab$norm[[1]],
            Percentile = tab$percentile[[1]]
          ))
          
          for(i in 2:nrow(tab)){
            if(tab$norm[[i]] < tab$norm[[i - 1]]){
              tab$norm[[i]]       <- tab$norm[[i - 1]]
              tab$percentile[[i]] <- tab$percentile[[i - 1]]
            }
            table$addRow(rowKey = i, values = list(
              Raw        = tab$raw[[i]],
              Norm       = tab$norm[[i]],
              Percentile = tab$percentile[[i]]
            ))
          }
          
          foot <- paste0("Norm score table for value '", normAge, "' of the grouping variable.")
          if(!is.null(minRaw)){
            foot <- paste0(foot, " The range of raw scores was manually set from ", minRaw, " to ", maxRaw, ".")
          } else {
            foot <- paste0(foot, " The range of raw scores was automatically retrieved from the dataset.")
          }
          self$results$norms$setNote("empty", foot, init = FALSE)
        }
      }
      
      # Store manifest values - row names already captured above
      private$.raw           <- dataComplete$raw
      private$.manifestNorms <- dataComplete$normValue
      private$.manifestPerc  <- dataComplete$percentile * 100
      
      dataComplete$predictedNorm       <- predictNorm(dataComplete$raw, dataComplete$group, model,
                                                      minNorm = minNorm, maxNorm = maxNorm)
      dataComplete$predictedPercentile <- pnorm((dataComplete$predictedNorm - m1) / sd1) * 100
      
      private$.predictedNorms <- dataComplete$predictedNorm
      private$.predictedPerc  <- dataComplete$predictedPercentile
      
      private$.populateOutputs()
      
      image <- self$results$plot
      image$setState(dataComplete)
      
      self$results$instructions$setVisible(FALSE)
    },
    
    .populateOutputs = function() {
      if (self$options$saveManifest && self$results$saveManifest$isNotFilled()) {
        self$results$saveManifest$setRowNums(private$.rowNames)
        self$results$saveManifest$setValues(private$.manifestNorms)
      }
      
      if (self$options$saveManifestPerc && self$results$saveManifestPerc$isNotFilled()) {
        self$results$saveManifestPerc$setRowNums(private$.rowNames)
        self$results$saveManifestPerc$setValues(private$.manifestPerc)
      }
      
      if (self$options$savePredicted && self$results$savePredicted$isNotFilled()) {
        self$results$savePredicted$setRowNums(private$.rowNames)
        self$results$savePredicted$setValues(private$.predictedNorms)
      }
      
      if (self$options$savePredictedPerc && self$results$savePredictedPerc$isNotFilled()) {
        self$results$savePredictedPerc$setRowNums(private$.rowNames)
        self$results$savePredictedPerc$setValues(private$.predictedPerc)
      }
    },
    
    .plot = function(image, ...) {
      if (is.null(self$options$raw) || is.null(self$options$group) || is.null(image$state))
        return(FALSE)
      
      data <- image$state
      
      k <- private$.resolveK(self$options$k)
      t <- private$.resolveT(self$options$t)
      
      scaleInfo <- private$.resolveScale(self$options$scale, self$options$range)
      
      if(self$options$selectionType == "automaticSelection") {
        model <- tryCatch({
          if(!is.null(self$options$weights)) {
            bestModel(data, k = k, t = t, weights = data$weights, plot = FALSE)
          } else {
            bestModel(data, k = k, t = t, plot = FALSE)
          }
        }, error = function(e) NULL)
      } else {
        terms <- as.integer(self$options$terms)
        if(terms <= 0 || terms >= ((k + 1) * (t + 1) - 1)) {
          p <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = paste("Invalid number of terms:", terms),
                              size = 6) +
            ggplot2::theme_void() +
            ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
          print(p)
          return(TRUE)
        }
        model <- tryCatch({
          if(!is.null(self$options$weights)) {
            bestModel(data, terms = terms, k = k, t = t, weights = data$weights, plot = FALSE)
          } else {
            bestModel(data, terms = terms, k = k, t = t, plot = FALSE)
          }
        }, error = function(e) NULL)
      }
      
      if (is.null(model)) {
        p <- ggplot2::ggplot(data = data, ggplot2::aes(x = group, y = raw)) +
          ggplot2::geom_point(alpha = 0.3) +
          ggplot2::labs(title = "Data Distribution (Model fitting failed)",
                        x = "Group",
                        y = "Raw Score") +
          ggplot2::theme_bw()
        print(p)
        return(TRUE)
      }
      
      # Define percentiles to plot
      percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
      
      # Derive norm score values at each percentile from the chosen scale
      scaleParam <- scaleInfo$scaleParam
      if(is.numeric(scaleParam) && length(scaleParam) == 2) {
        T_vals <- qnorm(percentiles, mean = scaleParam[1], sd = scaleParam[2])
      } else {
        # Fallback: z-scores
        T_vals <- qnorm(percentiles)
      }
      
      NAMES  <- paste0("PR",     percentiles * 100)
      NAMESP <- paste0("PredPR", percentiles * 100)
      
      data$group <- round(data$group, digits = 3)
      AGEP <- sort(unique(data$group))
      
      # Weighted quantile helper
      weighted.quantile <- function(x, probs, weights = NULL) {
        if (is.null(weights)) weights <- rep(1, length(x))
        idx <- !is.na(x) & !is.na(weights) & is.finite(x) & is.finite(weights)
        x       <- x[idx]
        weights <- weights[idx]
        if(length(x) == 0) return(rep(NA_real_, length(probs)))
        ord         <- order(x)
        x           <- x[ord]
        weights     <- weights[ord]
        cum_weights <- cumsum(weights) / sum(weights)
        result <- numeric(length(probs))
        for (i in seq_along(probs)) {
          if (probs[i] <= 0) {
            result[i] <- x[1]
          } else if (probs[i] >= 1) {
            result[i] <- x[length(x)]
          } else {
            hi <- which(cum_weights >= probs[i])[1]
            if (hi == 1) {
              result[i] <- x[1]
            } else {
              w1 <- cum_weights[hi - 1]
              w2 <- cum_weights[hi]
              result[i] <- x[hi - 1] + (x[hi] - x[hi - 1]) * (probs[i] - w1) / (w2 - w1)
            }
          }
        }
        return(result)
      }
      
      descend <- self$options$descend
      
      # Observed percentiles per group
      percentile.actual <- as.data.frame(matrix(NA_real_, nrow = length(AGEP), ncol = length(percentiles)))
      colnames(percentile.actual) <- NAMES
      percentile.actual$group <- AGEP
      
      for (i in seq_along(AGEP)) {
        gd      <- data[data$group == AGEP[i], ]
        weights <- if (!is.null(gd$weights)) gd$weights else rep(1, nrow(gd))
        probs_i <- if(descend) rev(percentiles) else percentiles
        percentile.actual[i, NAMES] <- weighted.quantile(gd$raw, probs = probs_i, weights = weights)
      }
      
      # Fine-grained age grid for smooth predicted curves
      minAge <- min(data$group, na.rm = TRUE)
      maxAge <- max(data$group, na.rm = TRUE)
      share  <- seq(from = minAge, to = maxAge, length.out = 100)
      AGEP_pred <- sort(unique(c(AGEP, share)))
      
      rawMin <- min(data$raw, na.rm = TRUE)
      rawMax <- max(data$raw, na.rm = TRUE)
      
      percentile.fitted <- as.data.frame(matrix(NA_real_, nrow = length(AGEP_pred), ncol = length(T_vals)))
      colnames(percentile.fitted) <- NAMESP
      percentile.fitted$group <- AGEP_pred
      
      for (i in seq_along(AGEP_pred)) {
        percentile.fitted[i, NAMESP] <- predictRaw(T_vals, AGEP_pred[i], model$coefficients,
                                                   minRaw = rawMin, maxRaw = rawMax)
      }
      
      percentile.fitted <- percentile.fitted[!duplicated(percentile.fitted$group), ]
      
      percentile <- merge(percentile.actual, percentile.fitted, by = "group", all = TRUE)
      
      COL1  <- rainbow(length(percentiles), end = 0.8)
      title    <- "Observed and Predicted Percentile Curves"
      subtitle <- paste0("Model: ", model$ideal.model,
                         ", R\u00b2 = ", round(model$subset$adjr2[model$ideal.model], digits = 4))
      
      # Build long-format data for ggplot
      n_rows <- nrow(percentile)
      
      plot_data <- data.frame(
        group      = rep(percentile$group, times = 2 * length(percentiles)),
        value      = c(
          as.vector(as.matrix(percentile[, NAMES,  drop = FALSE])),
          as.vector(as.matrix(percentile[, NAMESP, drop = FALSE]))
        ),
        type       = rep(c("Observed", "Predicted"),
                         each = n_rows * length(percentiles)),
        percentile = factor(
          rep(rep(NAMES, each = n_rows), times = 2),
          levels = NAMES
        )
      )
      
      plot_data_predicted <- plot_data[plot_data$type == "Predicted", ]
      plot_data_observed  <- plot_data[plot_data$type == "Observed" & !is.na(plot_data$value), ]
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = value, color = percentile)) +
        ggplot2::geom_line(data = plot_data_predicted, linewidth = 0.75) +
        ggplot2::geom_point(data = plot_data_observed, size = 2.5) +
        ggplot2::labs(
          title    = title,
          subtitle = subtitle,
          x        = "Explanatory Variable (group)",
          y        = "Raw Score"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_manual(
          values = setNames(COL1, NAMES),
          name   = "Percentiles"
        ) +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle    = ggplot2::element_text(hjust = 0.5, size = 12),
          axis.title       = ggplot2::element_text(size = 12, face = "bold"),
          axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
          axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
          axis.text        = ggplot2::element_text(size = 10),
          legend.position  = "right",
          legend.title     = ggplot2::element_text(size = 12, face = "bold"),
          legend.text      = ggplot2::element_text(size = 10),
          panel.grid.major = ggplot2::element_line(color = "gray90"),
          panel.grid.minor = ggplot2::element_line(color = "gray95")
        ) +
        ggplot2::geom_point(
          data        = data,
          ggplot2::aes(x = group, y = raw),
          color       = "black",
          alpha       = 0.2,
          size        = 0.6,
          inherit.aes = FALSE
        )
      
      print(p)
      return(TRUE)
    }
  )
)