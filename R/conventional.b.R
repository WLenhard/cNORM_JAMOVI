conventionalClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "conventionalClass",
  inherit = conventionalBase,
  private = list(
    .init = function() {
      if (is.null(self$options$raw)) {
        self$results$instructions$setVisible(TRUE)
        self$results$instructions$setContent(
          "<html>
            <head></head>
            <body>
            <div class='instructions'>
              <p>This analysis estimates norm scores (location, <i>L</i>) and percentile ranks for raw scores within a <b>single group</b> (without age or grade dependency) using the <b>cNORM</b> package (v3.7.0).</p>
              <p>Manifest percentiles are determined via the Rankit procedure and inverse normal transformation. Optionally, the functional relationship between raw scores and norm scores is smoothed via polynomial regression up to power <i>k</i> to close gaps in the norm table and reduce sampling error.</p>
              <ol>
                <li>Select the <b>Raw Score Variable</b>.</li>
                <li>Choose the desired <b>Norm scale</b> (T, IQ, z, Wechsler, or PISA) and whether to <b>Smooth with polynomial regression</b>.</li>
                <li>Optionally add confidence intervals under <b>Output</b> or save manifest/fitted scores to the spreadsheet under <b>Save</b>.</li>
              </ol>
              <p>Further information is available at <a href=\"https://www.psychometrica.de/cNorm_jamovi_en.html\" target=\"_blank\">Psychometrica</a>.</p>
            </div>
            </body>
          </html>"
        )
      } else {
        self$results$instructions$setVisible(FALSE)
      }
    },
    
    .resolveScale = function(scale, sdRange) {
      if (scale == "Wechsler") {
        m1 <- 10; sd1 <- 3; scaleParam <- c(10, 3)
      } else if (scale == "PISA") {
        m1 <- 500; sd1 <- 100; scaleParam <- c(500, 100)
      } else if (scale == "T") {
        m1 <- 50; sd1 <- 10; scaleParam <- c(50, 10)
      } else if (scale == "IQ") {
        m1 <- 100; sd1 <- 15; scaleParam <- c(100, 15)
      } else {
        m1 <- 0; sd1 <- 1; scaleParam <- c(0, 1)
      }
      list(
        m1 = m1,
        sd1 = sd1,
        scaleParam = scaleParam,
        minNorm = m1 - sdRange * sd1,
        maxNorm = m1 + sdRange * sd1
      )
    },
    
    .prepareData = function() {
      x <- jmvcore::toNumeric(self$data[[self$options$raw]])
      hasExplicitWeights <- !is.null(self$options$weights)
      w <- NULL
      
      if (hasExplicitWeights) {
        w <- jmvcore::toNumeric(self$data[[self$options$weights]])
      } else {
        dsWeights <- attr(self$data, "jmv-weights")
        if (!is.null(dsWeights)) {
          w <- as.numeric(dsWeights)
        }
      }
      
      if (!is.null(w)) {
        df <- data.frame(raw = x, weights = w)
      } else {
        df <- data.frame(raw = x)
      }
      rownames(df) <- rownames(self$data)
      
      keep <- stats::complete.cases(df)
      if (any(keep)) {
        mat <- as.matrix(df[keep, , drop = FALSE])
        keep[keep] <- apply(mat, 1, function(r) all(is.finite(r)))
      }
      dfComplete <- df[keep, , drop = FALSE]
      
      if (nrow(dfComplete) < 5) {
        jmvcore::reject("Too few complete cases found (at least 5 valid observations are required).")
      }
      
      if (length(unique(dfComplete$raw)) < 2) {
        jmvcore::reject("The raw score variable must contain at least two distinct values.")
      }
      
      if (!is.null(w)) {
        if (any(dfComplete$weights <= 0, na.rm = TRUE)) {
          jmvcore::reject("The weighting variable must contain positive values (> 0).")
        }
      }
      
      dfComplete
    },
    
    .addNotice = function(name, type, content, position = 1) {
      notice <- jmvcore::Notice$new(
        options = self$options,
        name = name,
        type = type
      )
      notice$setContent(content)
      self$results$insert(position, notice)
    },
    
    .appendCI = function(df, m1, sd1, ciVal, relVal) {
      if (!is.null(ciVal) && !is.null(relVal)) {
        se <- stats::qnorm(1 - ((1 - ciVal) / 2)) * sqrt(relVal * (1 - relVal))
        z  <- (df$norm - m1) / sd1
        zPred <- relVal * z
        df$lowerCI    <- (zPred - se) * sd1 + m1
        df$upperCI    <- (zPred + se) * sd1 + m1
        df$lowerCI_PR <- stats::pnorm(zPred - se) * 100
        df$upperCI_PR <- stats::pnorm(zPred + se) * 100
      } else {
        df$lowerCI    <- NA_real_
        df$upperCI    <- NA_real_
        df$lowerCI_PR <- NA_real_
        df$upperCI_PR <- NA_real_
      }
      df
    },
    
    .run = function() {
      if (is.null(self$options$raw)) {
        return()
      }
      self$results$instructions$setVisible(FALSE)
      
      dataComplete <- private$.prepareData()
      hasWeights <- "weights" %in% names(dataComplete)
      
      scaleInfo <- private$.resolveScale(self$options$scale, self$options$range)
      m1      <- scaleInfo$m1
      sd1     <- scaleInfo$sd1
      minNorm <- scaleInfo$minNorm
      maxNorm <- scaleInfo$maxNorm
      scale   <- scaleInfo$scaleParam
      descend <- isTRUE(self$options$descend)
      
      # Rank within single group (group = FALSE)
      if (hasWeights) {
        dataComplete <- rankByGroup(
          dataComplete,
          raw = dataComplete$raw,
          group = FALSE,
          scale = scale,
          descend = descend,
          weights = dataComplete$weights
        )
      } else {
        dataComplete <- rankByGroup(
          dataComplete,
          raw = dataComplete$raw,
          group = FALSE,
          scale = scale,
          descend = descend
        )
      }
      
      if (is.null(rownames(dataComplete))) {
        rownames(dataComplete) <- seq_len(nrow(dataComplete))
      }
      rowNames <- rownames(dataComplete)
      
      ciVal  <- if (isTRUE(self$options$includeCI)) as.numeric(self$options$ciLevel) else NULL
      relVal <- if (isTRUE(self$options$includeCI)) as.numeric(self$options$reliability) else NULL
      
      # Manifest table (unique observed raw scores)
      tab1 <- data.frame(
        raw = dataComplete$raw,
        norm = dataComplete$normValue,
        percentile = dataComplete$percentile * 100
      )
      tab1 <- unique(tab1)
      tab1 <- tab1[order(tab1$raw), , drop = FALSE]
      tab1$type <- 0
      tab1 <- private$.appendCI(tab1, m1, sd1, ciVal, relVal)
      
      regression <- isTRUE(self$options$k)
      model <- NULL
      curveDF <- NULL
      
      if (regression) {
        kPower <- as.integer(self$options$kPower)
        useManual <- identical(self$options$selectionType, "manualSelection")
        terms <- if (useManual) as.integer(self$options$terms) else NULL
        
        if (useManual && (terms < 1L || terms > kPower)) {
          jmvcore::reject(
            paste0("The number of terms (", terms, ") must be between 1 and k (", kPower, ").")
          )
        }
        
        dataComplete <- computePowers(dataComplete, k = kPower, norm = "normValue")
        
        model <- tryCatch({
          wArg <- if (hasWeights) dataComplete$weights else NULL
          if (useManual) {
            bestModel(dataComplete, raw = "raw", k = kPower, terms = terms, weights = wArg, plot = FALSE)
          } else {
            bestModel(dataComplete, raw = "raw", k = kPower, weights = wArg, plot = FALSE)
          }
        }, error = function(e) {
          jmvcore::reject(paste0("Model fitting failed: ", conditionMessage(e)))
        })
        
        if (is.null(model)) {
          jmvcore::reject("Polynomial regression model could not be fitted. Check your data or reduce the degree k.")
        }
        
        # Populate Model Fit & Coefficients tables
        self$results$modelFit$setRow(rowNo = 1, values = list(
          nobs  = nrow(dataComplete),
          terms = as.integer(model$ideal.model),
          rmse  = model$rmse,
          r2adj = model$subset$adjr2[model$ideal.model],
          bic   = model$subset$bic[model$ideal.model]
        ))
        
        coefTable <- self$results$modelCoef
        for (i in seq_along(model$coefficients)) {
          coefTable$addRow(rowKey = i, values = list(
            term     = names(model$coefficients)[i],
            estimate = as.numeric(model$coefficients[[i]])
          ))
        }
        
        minRaw <- self$options$minRaw
        maxRaw <- self$options$maxRaw
        if (is.null(minRaw) || is.null(maxRaw) || maxRaw <= minRaw) {
          minRaw <- NULL
          maxRaw <- NULL
        }
        
        stepVal <- as.numeric(self$options$stepping)
        if (is.na(stepVal) || stepVal <= 0) {
          jmvcore::reject("Step size must be greater than 0.")
        }
        
        tab <- tryCatch({
          rawTable(
            0,
            model,
            minNorm = minNorm,
            maxNorm = maxNorm,
            minRaw = minRaw,
            maxRaw = maxRaw,
            step = stepVal,
            CI = ciVal,
            reliability = relVal
          )
        }, error = function(e) {
          jmvcore::reject(paste0("Could not generate norm table: ", conditionMessage(e)))
        })
        
        tab$type <- 1
        
        # Build smooth 200-point model curve for plotting (raw = f(norm))
        L_min <- max(minNorm, model$minL1)
        L_max <- min(maxNorm, model$maxL1)
        if (!is.finite(L_min) || !is.finite(L_max) || L_min >= L_max) {
          L_min <- model$minL1
          L_max <- model$maxL1
        }
        L_seq <- seq(L_min, L_max, length.out = 200)
        r_min <- if (!is.null(minRaw)) minRaw else model$minRaw
        r_max <- if (!is.null(maxRaw)) maxRaw else model$maxRaw
        raw_seq <- vapply(L_seq, function(l_val) {
          as.numeric(predictRaw(l_val, 0, model$coefficients, minRaw = r_min, maxRaw = r_max))
        }, numeric(1))
        perc_seq <- stats::pnorm((L_seq - m1) / sd1) * 100
        
        curveDF <- data.frame(
          raw = raw_seq,
          norm = L_seq,
          percentile = perc_seq,
          type = 1
        )
        curveDF <- private$.appendCI(curveDF, m1, sd1, ciVal, relVal)
        curveDF <- curveDF[order(curveDF$raw, curveDF$norm), , drop = FALSE]
        
        foot <- if (!is.null(minRaw)) {
          paste0("Smoothed polynomial norm table (raw score range manually set from ", minRaw, " to ", maxRaw, ").")
        } else {
          "Smoothed polynomial norm table (raw score range retrieved from dataset)."
        }
      } else {
        tab <- tab1
        foot <- "Norm score table based on the manifest frequency distribution (Rankit)."
      }
      
      # Check monotonicity and populate Norm Score Table
      nAdjusted <- 0L
      if (nrow(tab) > 1L) {
        for (i in seq_len(nrow(tab))[-1]) {
          if (!descend && tab$norm[[i]] < tab$norm[[i - 1]]) {
            tab$norm[[i]]       <- tab$norm[[i - 1]]
            tab$percentile[[i]] <- tab$percentile[[i - 1]]
            nAdjusted <- nAdjusted + 1L
          } else if (descend && tab$norm[[i]] > tab$norm[[i - 1]]) {
            tab$norm[[i]]       <- tab$norm[[i - 1]]
            tab$percentile[[i]] <- tab$percentile[[i - 1]]
            nAdjusted <- nAdjusted + 1L
          }
        }
      }
      
      if (nAdjusted > 0L) {
        private$.addNotice(
          name = "nonMonotonicNotice",
          type = jmvcore::NoticeType$WARNING,
          content = paste0(
            "The fitted regression curve was non-monotonic at ", nAdjusted,
            " raw score step(s); values were carried forward. Consider reducing the number of terms or degree k."
          )
        )
      }
      
      normTableWidget <- self$results$norms
      hasCI <- isTRUE(self$options$includeCI) && ("lowerCI" %in% names(tab))
      for (i in seq_len(nrow(tab))) {
        rowVals <- list(
          Raw        = tab$raw[[i]],
          Norm       = tab$norm[[i]],
          Percentile = tab$percentile[[i]]
        )
        if (hasCI) {
          rowVals$lowerCI    <- tab$lowerCI[[i]]
          rowVals$upperCI    <- tab$upperCI[[i]]
          rowVals$lowerCI_PR <- tab$lowerCI_PR[[i]]
          rowVals$upperCI_PR <- tab$upperCI_PR[[i]]
        }
        normTableWidget$addRow(rowKey = i, values = rowVals)
      }
      normTableWidget$setNote("info", foot, init = FALSE)
      
      # Populate spreadsheet outputs
      manifestNorms <- dataComplete$normValue
      manifestPerc  <- dataComplete$percentile * 100
      predictedNorms <- NULL
      predictedPerc  <- NULL
      
      if (regression) {
        predictedNorms <- predictNorm(
          dataComplete$raw,
          0,
          model,
          minNorm = minNorm,
          maxNorm = maxNorm
        )
        predictedPerc <- stats::pnorm((predictedNorms - m1) / sd1) * 100
      }
      
      if (self$options$saveManifest && self$results$saveManifest$isNotFilled()) {
        self$results$saveManifest$setRowNums(rowNames)
        self$results$saveManifest$setValues(manifestNorms)
      }
      if (self$options$saveManifestPerc && self$results$saveManifestPerc$isNotFilled()) {
        self$results$saveManifestPerc$setRowNums(rowNames)
        self$results$saveManifestPerc$setValues(manifestPerc)
      }
      if (regression && self$options$savePredicted && self$results$savePredicted$isNotFilled()) {
        self$results$savePredicted$setRowNums(rowNames)
        self$results$savePredicted$setValues(predictedNorms)
      }
      if (regression && self$options$savePredictedPerc && self$results$savePredictedPerc$isNotFilled()) {
        self$results$savePredictedPerc$setRowNums(rowNames)
        self$results$savePredictedPerc$setValues(predictedPerc)
      }
      
      # Lightweight state for plot (includes CI bounds)
      plotCols <- c("raw", "norm", "percentile", "lowerCI", "upperCI", "lowerCI_PR", "upperCI_PR", "type")
      plotState <- if (regression && !is.null(curveDF)) {
        rbind(tab1[, plotCols, drop = FALSE], curveDF[, plotCols, drop = FALSE])
      } else {
        tab1[, plotCols, drop = FALSE]
      }
      self$results$plot$setState(plotState)
    },
    
    .plot = function(image, ggtheme, theme, ...) {
      if (is.null(self$options$raw) || is.null(image$state))
        return(FALSE)
      
      data <- image$state
      plotNormScore <- identical(self$options$plotting, "Norm Score")
      obsData <- data[data$type == 0, , drop = FALSE]
      regData <- data[data$type == 1, , drop = FALSE]
      hasModel <- isTRUE(self$options$k) && nrow(regData) > 0
      showCI   <- isTRUE(self$options$includeCI)
      
      yCol     <- if (plotNormScore) "norm" else "percentile"
      yminCol  <- if (plotNormScore) "lowerCI" else "lowerCI_PR"
      ymaxCol  <- if (plotNormScore) "upperCI" else "upperCI_PR"
      yLabel   <- if (plotNormScore) "Norm Score" else "Percentile"
      pTitle   <- if (plotNormScore) "Norm Score Conversion" else "Percentile Conversion"
      ciLabel  <- paste0(round(as.numeric(self$options$ciLevel) * 100), "% CI")
      
      p <- ggplot2::ggplot()
      
      # 1. Optional Confidence Interval Ribbon
      if (showCI) {
        ciSource <- if (hasModel) regData else obsData
        ciSource <- ciSource[!is.na(ciSource[[yminCol]]) & !is.na(ciSource[[ymaxCol]]), , drop = FALSE]
        if (nrow(ciSource) > 1L) {
          p <- p +
            ggplot2::geom_ribbon(
              data = ciSource,
              ggplot2::aes(
                x = .data$raw,
                ymin = .data[[yminCol]],
                ymax = .data[[ymaxCol]],
                fill = ciLabel
              ),
              alpha = 0.22,
              color = NA
            ) +
            ggplot2::scale_fill_manual(
              name = NULL,
              values = stats::setNames("#d95f02", ciLabel)
            )
        }
      }
      
      # 2. Solid Black Observed Points (no alpha transparency)
      p <- p +
        ggplot2::geom_point(
          data = obsData,
          ggplot2::aes(x = .data$raw, y = .data[[yCol]], color = "Observed"),
          size = 1.5
        )
      
      # 3. Model Curve + Legend with explicit dot & line symbols
      if (hasModel) {
        p <- p +
          ggplot2::geom_line(
            data = regData,
            ggplot2::aes(x = .data$raw, y = .data[[yCol]], color = "Model"),
            linewidth = 1.0
          ) +
          ggplot2::scale_color_manual(
            name = NULL,
            values = c("Observed" = "black", "Model" = "#d95f02"),
            breaks = c("Observed", "Model")
          ) +
          ggplot2::guides(
            color = ggplot2::guide_legend(
              order = 1,
              override.aes = list(
                linetype = c("blank", "solid"),
                shape    = c(16, NA),
                size     = c(2.2, 1.0),
                linewidth = c(0, 1.0)
              )
            ),
            fill = ggplot2::guide_legend(order = 2)
          )
      } else {
        p <- p +
          ggplot2::scale_color_manual(
            name = NULL,
            values = c("Observed" = "black"),
            breaks = "Observed"
          ) +
          ggplot2::guides(
            color = ggplot2::guide_legend(
              order = 1,
              override.aes = list(
                linetype = "blank",
                shape = 16,
                size = 2.2
              )
            ),
            fill = ggplot2::guide_legend(order = 2)
          )
      }
      
      showLegend <- hasModel || showCI
      
      p <- p +
        ggplot2::labs(x = "Raw Score", y = yLabel, title = pTitle) +
        ggtheme +
        ggplot2::theme(
          legend.position = if (showLegend) "top" else "none",
          legend.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
        )
      
      print(p)
      return(TRUE)
    }
  )
)