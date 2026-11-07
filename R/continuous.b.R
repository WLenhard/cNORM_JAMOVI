continuousClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "continuousClass",
  inherit = continuousBase,
  private = list(
    .init = function() {
      if (is.null(self$options$raw) || is.null(self$options$group)) {
        self$results$instructions$setVisible(TRUE)
        self$results$instructions$setContent(
          "<html>
            <head></head>
            <body>
            <div class='instructions'>
              <p>This analysis estimates continuous norm scores across age or grade groups using the <b>cNORM</b> package (v3.7.0). It supports four modeling approaches:</p>
              <ul>
                <li><b>Distribution-Free (Taylor Polynomials)</b>: Models the raw score as a polynomial function of the location (norm score, degree <i>k</i>) and the explanatory variable (e.g., age group, degree <i>t</i>) without distributional assumptions.</li>
                <li><b>Beta-Binomial</b>: Parametric modeling for accuracy and power tests with a fixed number of items <i>n</i> (integer scores 0 to <i>n</i>; ideal for 1PL IRT / Rasch scales).</li>
                <li><b>Sinh-Arcsinh (SHASH)</b>: Parametric modeling for continuous, unbounded scores (e.g., biometric data, response times) with flexible location (<i>&mu;</i>), scale (<i>&sigma;</i>), skewness (<i>&epsilon;</i>), and tail weight (<i>&delta;</i>).</li>
                <li><b>Conway-Maxwell-Poisson (CMP)</b>: Parametric modeling for speeded tests and count data (non-negative integers) with location (<i>&mu;</i>), dispersion (<i>&nu;</i>), and optional ceiling truncation.</li>
              </ul>
              <ol>
                <li>Select your <b>Raw Score Variable</b> and <b>Explanatory / Grouping Variable</b> (e.g., age or grade).</li>
                <li>Choose the <b>Modeling method</b> and <b>Norm scale</b>, then inspect the percentile plot and model summary.</li>
                <li>Enter a <b>Value for norm score table</b> under <b>Output &amp; Norm Table</b> to generate a norm table for a specific age, or save fitted scores under <b>Save</b>.</li>
              </ol>
              <p>Further information is available at <a href=\"https://www.psychometrica.de/cNorm_jamovi_en.html\" target=\"_blank\">Psychometrica</a>.</p>
            </div>
            </body>
          </html>"
        )
        self$results$norms$setNote(
          "empty",
          "Specify a value of the explanatory/grouping variable under 'Output & Norm Table' to compute a norm table.",
          init = TRUE
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
    
    .addNotice = function(name, type, content, position = 1) {
      notice <- jmvcore::Notice$new(
        options = self$options,
        name = name,
        type = type
      )
      notice$setContent(content)
      self$results$insert(position, notice)
    },
    
    .weightedQuantile = function(x, probs, weights = NULL) {
      if (is.null(weights)) weights <- rep(1, length(x))
      idx <- !is.na(x) & !is.na(weights) & is.finite(x) & is.finite(weights)
      x <- x[idx]
      weights <- weights[idx]
      if (length(x) == 0) return(rep(NA_real_, length(probs)))
      ord <- order(x)
      x <- x[ord]
      weights <- weights[ord]
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
      result
    },
    
    .prepareData = function() {
      if (identical(self$options$raw, self$options$group)) {
        jmvcore::reject("Raw score variable and explanatory/grouping variable must be different.")
      }
      
      x <- jmvcore::toNumeric(self$data[[self$options$raw]])
      g <- jmvcore::toNumeric(self$data[[self$options$group]])
      w <- NULL
      
      if (!is.null(self$options$weights)) {
        w <- jmvcore::toNumeric(self$data[[self$options$weights]])
      } else {
        dsWeights <- attr(self$data, "jmv-weights")
        if (!is.null(dsWeights)) {
          w <- as.numeric(dsWeights)
        }
      }
      
      if (!is.null(w)) {
        df <- data.frame(raw = x, group = g, weights = w)
      } else {
        df <- data.frame(raw = x, group = g)
      }
      rownames(df) <- rownames(self$data)
      
      keep <- stats::complete.cases(df)
      if (any(keep)) {
        mat <- as.matrix(df[keep, , drop = FALSE])
        keep[keep] <- apply(mat, 1, function(r) all(is.finite(r)))
      }
      dfComplete <- df[keep, , drop = FALSE]
      
      if (nrow(dfComplete) < 10) {
        jmvcore::reject("Too few complete cases found (at least 10 valid observations are required).")
      }
      
      if (length(unique(dfComplete$raw)) < 2) {
        jmvcore::reject("The raw score variable has zero variance. At least two distinct values are required.")
      }
      
      if (length(unique(dfComplete$group)) < 2) {
        jmvcore::reject("The explanatory/grouping variable has only one distinct value. Use 'Single Group Norming' instead.")
      }
      
      if (!is.null(w)) {
        if (any(dfComplete$weights <= 0, na.rm = TRUE)) {
          jmvcore::reject("The weighting variable must contain positive values (> 0).")
        }
      }
      
      dfComplete
    },
    
    # =========================================================================
    # METHOD 1: Taylor Polynomials (Distribution-Free)
    # =========================================================================
    .runTaylor = function(dataComplete, scaleInfo) {
      hasWeights <- "weights" %in% names(dataComplete)
      k <- as.integer(self$options$k)
      t <- as.integer(self$options$t)
      descend <- isTRUE(self$options$descend)
      
      m1      <- scaleInfo$m1
      sd1     <- scaleInfo$sd1
      minNorm <- scaleInfo$minNorm
      maxNorm <- scaleInfo$maxNorm
      scale   <- scaleInfo$scaleParam
      
      dataRanked <- tryCatch({
        if (hasWeights) {
          rankByGroup(
            dataComplete,
            raw = dataComplete$raw,
            group = dataComplete$group,
            weights = dataComplete$weights,
            scale = scale,
            descend = descend
          )
        } else {
          rankByGroup(
            dataComplete,
            raw = dataComplete$raw,
            group = dataComplete$group,
            scale = scale,
            descend = descend
          )
        }
      }, error = function(e) {
        jmvcore::reject(paste0("Ranking by group failed: ", conditionMessage(e)))
      })
      
      dataPrepared <- computePowers(dataRanked, k = k, t = t, norm = "normValue")
      attr(dataPrepared, "k") <- k
      attr(dataPrepared, "t") <- t
      
      if (is.null(rownames(dataPrepared))) {
        rownames(dataPrepared) <- rownames(dataComplete)
      }
      rowNames <- rownames(dataPrepared)
      
      useManual <- identical(self$options$selectionType, "manualSelection")
      maxPossibleTerms <- (k + 1L) * (t + 1L) - 2L
      terms <- if (useManual) as.integer(self$options$terms) else NULL
      
      if (useManual && (terms < 1L || terms > maxPossibleTerms)) {
        jmvcore::reject(
          paste0("Number of terms (", terms, ") is out of valid range (1 to ", maxPossibleTerms, ").")
        )
      }
      
      wVec <- if (hasWeights) dataPrepared$weights else NULL
      model <- tryCatch({
        if (useManual) {
          bestModel(dataPrepared, raw = "raw", terms = terms, k = k, t = t, weights = wVec, plot = FALSE)
        } else {
          bestModel(dataPrepared, raw = "raw", k = k, t = t, weights = wVec, plot = FALSE)
        }
      }, error = function(e) {
        jmvcore::reject(paste0("Model fitting failed: ", conditionMessage(e)))
      })
      
      if (is.null(model)) {
        jmvcore::reject("Taylor polynomial model fitting failed. Try adjusting k, t, or the number of terms.")
      }
      
      # Populate Model Fit & Parameter Estimates Tables
      idealIdx <- model$ideal.model
      r2Val <- model$subset$adjr2[idealIdx]
      bicVal <- model$subset$bic[idealIdx]
      
      self$results$modelFit$setRow(rowNo = 1, values = list(
        methodName = "Taylor Polynomials",
        nobs       = nrow(dataPrepared),
        npar       = as.integer(idealIdx),
        bic        = bicVal,
        r2         = r2Val,
        rmse       = model$rmse
      ))
      
      coefTable <- self$results$modelCoef
      for (i in seq_along(model$coefficients)) {
        coefTable$addRow(rowKey = i, values = list(
          term = names(model$coefficients)[i],
          est  = as.numeric(model$coefficients[[i]])
        ))
      }
      
      # Norm Table (if normAge is provided)
      if (!is.null(self$options$normAge) && nzchar(trimws(self$options$normAge))) {
        normAge <- suppressWarnings(as.numeric(trimws(self$options$normAge)))
        if (is.na(normAge)) {
          jmvcore::reject("The value for the norm score table must be a valid number.")
        }
        
        minRaw <- self$options$minRaw
        maxRaw <- self$options$maxRaw
        if (is.null(minRaw) || is.null(maxRaw) || maxRaw <= minRaw) {
          minRaw <- NULL
          maxRaw <- NULL
        }
        stepVal <- as.numeric(self$options$stepping)
        ciVal   <- if (isTRUE(self$options$includeCI)) as.numeric(self$options$ciLevel) else NULL
        relVal  <- if (isTRUE(self$options$includeCI)) as.numeric(self$options$reliability) else NULL
        
        tab <- tryCatch({
          rawTable(
            normAge,
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
          jmvcore::reject(paste0("Error generating norm table: ", conditionMessage(e)))
        })
        
        private$.populateNormTable(
          tab = tab,
          rawCol = "raw",
          normCol = "norm",
          percCol = "percentile",
          descend = descend,
          noteText = paste0(
            "Norm score table for explanatory variable = ", normAge,
            if (!is.null(minRaw)) paste0(" (raw score range ", minRaw, " to ", maxRaw, ").")
            else " (raw score range retrieved from dataset)."
          )
        )
      }
      
      # Predictions & Spreadsheet Outputs
      manifestNorms <- dataPrepared$normValue
      manifestPerc  <- dataPrepared$percentile * 100
      predictedNorms <- predictNorm(
        dataPrepared$raw,
        dataPrepared$group,
        model,
        minNorm = minNorm,
        maxNorm = maxNorm
      )
      predictedPerc <- stats::pnorm((predictedNorms - m1) / sd1) * 100
      
      private$.saveColumns(
        rowNames = rowNames,
        manifestNorms = manifestNorms,
        manifestPerc = manifestPerc,
        predictedNorms = predictedNorms,
        predictedPerc = predictedPerc
      )
      
      # Build lightweight plot state in .run()
      percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
      T_vals <- stats::qnorm(percentiles, mean = m1, sd = sd1)
      NAMES  <- paste0("PR", percentiles * 100)
      
      grpRound <- round(dataPrepared$group, digits = 3)
      AGEP <- sort(unique(grpRound))
      
      obsList <- lapply(seq_along(AGEP), function(i) {
        gd <- dataPrepared[grpRound == AGEP[i], , drop = FALSE]
        w_i <- if (hasWeights) gd$weights else rep(1, nrow(gd))
        probs_i <- if (descend) rev(percentiles) else percentiles
        q_i <- private$.weightedQuantile(gd$raw, probs = probs_i, weights = w_i)
        data.frame(
          age = AGEP[i],
          value = q_i,
          percentile = factor(NAMES, levels = NAMES),
          stringsAsFactors = FALSE
        )
      })
      obsDF <- do.call(rbind, obsList)
      obsDF <- obsDF[!is.na(obsDF$value), , drop = FALSE]
      
      minAge <- min(dataPrepared$group, na.rm = TRUE)
      maxAge <- max(dataPrepared$group, na.rm = TRUE)
      AGEP_pred <- sort(unique(c(AGEP, seq(minAge, maxAge, length.out = 100))))
      rawMin <- min(dataPrepared$raw, na.rm = TRUE)
      rawMax <- max(dataPrepared$raw, na.rm = TRUE)
      
      predList <- lapply(seq_along(AGEP_pred), function(i) {
        yPred <- predictRaw(T_vals, AGEP_pred[i], model$coefficients, minRaw = rawMin, maxRaw = rawMax)
        data.frame(
          age = AGEP_pred[i],
          value = as.numeric(yPred),
          percentile = factor(NAMES, levels = NAMES),
          stringsAsFactors = FALSE
        )
      })
      predDF <- do.call(rbind, predList)
      
      self$results$plot$setState(list(
        method = "taylor",
        points = data.frame(age = dataPrepared$group, raw = dataPrepared$raw),
        obs = obsDF,
        pred = predDF,
        levels = NAMES,
        labels = paste0(percentiles * 100, "%"),
        title = "Percentile Plot (Taylor Polynomial Model)",
        subtitle = paste0("Terms: ", idealIdx, ", Adj. R\u00b2 = ", round(r2Val, 4)),
        discrete = FALSE,
        yLimits = NULL
      ))
    },
    
    # =========================================================================
    # METHOD 2, 3, 4: Parametric Continuous Norming (Beta-Binomial, SHASH, CMP)
    # =========================================================================
    .runParametric = function(dataComplete, scaleInfo, method) {
      hasWeights <- "weights" %in% names(dataComplete)
      wVec <- if (hasWeights) dataComplete$weights else NULL
      scaleParam <- scaleInfo$scaleParam
      m1  <- scaleInfo$m1
      sd1 <- scaleInfo$sd1
      sdRange <- as.numeric(self$options$range)
      rowNames <- rownames(dataComplete)
      
      age <- dataComplete$group
      score <- dataComplete$raw
      
      model <- NULL
      diag  <- NULL
      methodLabel <- ""
      nMax <- NULL
      
      if (method == "betabinomial") {
        methodLabel <- "Beta-Binomial"
        if (any(score < 0) || any(abs(score - round(score)) > 1e-8)) {
          jmvcore::reject(
            "Beta-Binomial modeling requires non-negative integer raw scores (0, 1, ..., n). For continuous scores, select SHASH or Taylor Polynomials."
          )
        }
        score <- as.integer(round(score))
        dataComplete$raw <- score
        
        nItem <- as.integer(self$options$itemnumber)
        nMax <- if (is.null(nItem) || is.na(nItem) || nItem <= 0L) max(score, na.rm = TRUE) else nItem
        if (max(score, na.rm = TRUE) > nMax) {
          jmvcore::reject(
            paste0("Observed raw scores exceed the specified number of items (", nMax, "). Increase 'Number of items in test' or set it to 0.")
          )
        }
        
        alphaDeg <- as.integer(self$options$alpha)
        betaDeg  <- as.integer(self$options$beta)
        
        warns <- character(0)
        model <- tryCatch({
          withCallingHandlers(
            suppressMessages(
              cnorm.betabinomial(
                age = age,
                score = score,
                n = nMax,
                weights = wVec,
                mode = 2,
                alpha = alphaDeg,
                beta = betaDeg,
                scale = scaleParam,
                plot = FALSE
              )
            ),
            warning = function(w) {
              warns <<- c(warns, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )
        }, error = function(e) {
          jmvcore::reject(paste0("Beta-Binomial fitting failed: ", conditionMessage(e)))
        })
        
        diag <- diagnostics.betabinomial(model, age = age, score = score, weights = wVec)
        if (!isTRUE(diag$converged)) {
          private$.addNotice(
            "bbConvNotice",
            jmvcore::NoticeType$WARNING,
            "The Beta-Binomial optimizer did not strictly converge. Inspect the percentile curves and consider reducing polynomial degrees."
          )
        }
        
        # Populate coefficients
        coefTable <- self$results$modelCoef
        for (i in seq_along(diag$param_estimates)) {
          coefTable$addRow(rowKey = i, values = list(
            term = names(diag$param_estimates)[i],
            est  = unname(diag$param_estimates[i]),
            se   = unname(diag$param_se[i]),
            z    = unname(diag$z_values[i]),
            p    = unname(diag$p_values[i])
          ))
        }
        
      } else if (method == "shash") {
        methodLabel <- "Sinh-Arcsinh (SHASH)"
        muDeg    <- as.integer(self$options$muDegree)
        sigmaDeg <- as.integer(self$options$sigmaDegree)
        epsDeg   <- as.integer(self$options$epsilonDegree)
        fixDelta <- isTRUE(self$options$fixDelta)
        deltaDeg <- if (fixDelta) NULL else as.integer(self$options$deltaDegree)
        deltaVal <- if (fixDelta) as.numeric(self$options$deltaValue) else 1.0
        
        warns <- character(0)
        model <- tryCatch({
          withCallingHandlers(
            suppressMessages(
              cnorm.shash(
                age = age,
                score = score,
                weights = wVec,
                mu_degree = muDeg,
                sigma_degree = sigmaDeg,
                epsilon_degree = epsDeg,
                delta_degree = deltaDeg,
                delta = deltaVal,
                scale = scaleParam,
                plot = FALSE
              )
            ),
            warning = function(w) {
              warns <<- c(warns, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )
        }, error = function(e) {
          jmvcore::reject(paste0("SHASH model fitting failed: ", conditionMessage(e)))
        })
        
        diag <- diagnostics.shash(model, age = age, score = score, weights = wVec)
        if (!isTRUE(diag$converged)) {
          private$.addNotice(
            "shashConvNotice",
            jmvcore::NoticeType$WARNING,
            paste0("The SHASH optimizer did not strictly converge (", diag$message, "). Consider reducing polynomial degrees or fixing delta.")
          )
        }
        
        # Assemble parameter estimates table
        estVec <- c(diag$mu_estimates, diag$sigma_estimates, diag$epsilon_estimates, diag$delta_estimates)
        seVec  <- c(diag$mu_se, diag$sigma_se, diag$epsilon_se, diag$delta_se)
        zVec   <- c(diag$mu_z_values, diag$sigma_z_values, diag$epsilon_z_values, diag$delta_z_values)
        pVec   <- c(diag$mu_p_values, diag$sigma_p_values, diag$epsilon_p_values, diag$delta_p_values)
        termNames <- c(
          paste0("mu_", seq_along(diag$mu_estimates) - 1L),
          paste0("log(sigma)_", seq_along(diag$sigma_estimates) - 1L),
          paste0("epsilon_", seq_along(diag$epsilon_estimates) - 1L),
          if (!is.null(diag$delta_estimates)) paste0("log(delta)_", seq_along(diag$delta_estimates) - 1L) else NULL
        )
        
        coefTable <- self$results$modelCoef
        for (i in seq_along(estVec)) {
          coefTable$addRow(rowKey = i, values = list(
            term = termNames[i],
            est  = unname(estVec[i]),
            se   = unname(seVec[i]),
            z    = unname(zVec[i]),
            p    = unname(pVec[i])
          ))
        }
        
      } else if (method == "cmp") {
        methodLabel <- "Conway-Maxwell-Poisson (CMP)"
        if (any(score < 0) || any(abs(score - round(score)) > 1e-8)) {
          jmvcore::reject(
            "Conway-Maxwell-Poisson (CMP) modeling requires non-negative integer counts (0, 1, 2, ...). For continuous scores, select SHASH or Taylor Polynomials."
          )
        }
        score <- as.integer(round(score))
        dataComplete$raw <- score
        
        muDeg  <- as.integer(self$options$cmpMuDegree)
        fixNu  <- isTRUE(self$options$cmpFixNu)
        nuDeg  <- if (fixNu) NULL else as.integer(self$options$cmpNuDegree)
        nuVal  <- if (fixNu) as.numeric(self$options$cmpNuValue) else 1.0
        msOpt  <- as.integer(self$options$cmpMaxScore)
        maxSc  <- if (is.null(msOpt) || is.na(msOpt) || msOpt <= 0L) NULL else msOpt
        
        if (!is.null(maxSc) && max(score, na.rm = TRUE) > maxSc) {
          jmvcore::reject(
            paste0("Observed raw scores (", max(score, na.rm = TRUE), ") exceed the specified maximum score ceiling (", maxSc, ").")
          )
        }
        
        warns <- character(0)
        model <- tryCatch({
          withCallingHandlers(
            suppressMessages(
              cnorm.cmp(
                age = age,
                score = score,
                weights = wVec,
                mu_degree = muDeg,
                nu_degree = nuDeg,
                nu = nuVal,
                scale = scaleParam,
                max_score = maxSc,
                plot = FALSE
              )
            ),
            warning = function(w) {
              warns <<- c(warns, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )
        }, error = function(e) {
          jmvcore::reject(paste0("CMP model fitting failed: ", conditionMessage(e)))
        })
        
        diag <- diagnostics.cmp(model, age = age, score = score, weights = wVec)
        if (!isTRUE(diag$converged) || isTRUE(diag$nu_at_bound)) {
          msg <- if (isTRUE(diag$nu_at_bound)) {
            "The dispersion predictor log(nu) reached its boundary at observed ages; standard errors and Wald tests may not be valid. Consider reducing the degree of nu."
          } else {
            paste0("The CMP optimizer did not strictly converge (", diag$message, "). Consider reducing polynomial degrees.")
          }
          private$.addNotice("cmpConvNotice", jmvcore::NoticeType$WARNING, msg)
        }
        
        estVec <- c(diag$mu_estimates, diag$nu_estimates)
        seVec  <- c(diag$mu_se, diag$nu_se)
        zVec   <- c(diag$mu_z_values, diag$nu_z_values)
        pVec   <- c(diag$mu_p_values, diag$nu_p_values)
        termNames <- c(
          paste0("log(mu)_", seq_along(diag$mu_estimates) - 1L),
          if (!is.null(diag$nu_estimates)) paste0("log(nu)_", seq_along(diag$nu_estimates) - 1L) else NULL
        )
        
        coefTable <- self$results$modelCoef
        for (i in seq_along(estVec)) {
          coefTable$addRow(rowKey = i, values = list(
            term = termNames[i],
            est  = unname(estVec[i]),
            se   = unname(seVec[i]),
            z    = unname(zVec[i]),
            p    = unname(pVec[i])
          ))
        }
      }
      
      # Populate Model Fit Table (shared structure across all 3 parametric models)
      self$results$modelFit$setRow(rowNo = 1, values = list(
        methodName = methodLabel,
        nobs       = as.integer(diag$n_obs),
        npar       = as.integer(diag$n_params),
        logLik     = diag$log_likelihood,
        aic        = diag$AIC,
        bic        = diag$BIC,
        r2         = diag$R2,
        rmse       = diag$rmse,
        bias       = diag$bias
      ))
      
      # Norm Score Table (if normAge is specified)
      if (!is.null(self$options$normAge) && nzchar(trimws(self$options$normAge))) {
        normAge <- suppressWarnings(as.numeric(trimws(self$options$normAge)))
        if (is.na(normAge)) {
          jmvcore::reject("The value for the norm score table must be a valid number.")
        }
        
        minRaw <- self$options$minRaw
        maxRaw <- self$options$maxRaw
        useCustomRange <- (!is.null(minRaw) && !is.null(maxRaw) && maxRaw > minRaw)
        stepVal <- as.numeric(self$options$stepping)
        ciVal   <- if (isTRUE(self$options$includeCI)) as.numeric(self$options$ciLevel) else NULL
        relVal  <- if (isTRUE(self$options$includeCI)) as.numeric(self$options$reliability) else NULL
        
        tab <- tryCatch({
          if (method == "betabinomial") {
            dfList <- normTable.betabinomial(
              model,
              ages = normAge,
              n = nMax,
              range = sdRange,
              CI = ciVal,
              reliability = relVal
            )
            df <- dfList[[1]]
            if (useCustomRange) {
              df <- df[df$x >= minRaw & df$x <= maxRaw, , drop = FALSE]
            }
            df
          } else if (method == "shash") {
            startSc <- if (useCustomRange) minRaw else attr(model$result, "min")
            endSc   <- if (useCustomRange) maxRaw else attr(model$result, "max")
            dfList <- normTable.shash(
              model,
              ages = normAge,
              start = startSc,
              end = endSc,
              step = stepVal,
              CI = ciVal,
              reliability = relVal
            )
            df <- dfList[[1]]
            df$z <- pmin(pmax(df$z, -sdRange), sdRange)
            df$norm <- m1 + sd1 * df$z
            df
          } else if (method == "cmp") {
            startSc <- if (useCustomRange) max(0L, as.integer(floor(minRaw))) else NULL
            endSc   <- if (useCustomRange) as.integer(ceiling(maxRaw)) else NULL
            stepInt <- max(1L, as.integer(round(stepVal)))
            dfList <- normTable.cmp(
              model,
              ages = normAge,
              start = startSc,
              end = endSc,
              step = stepInt,
              range = sdRange,
              CI = ciVal,
              reliability = relVal,
              mid_p = TRUE
            )
            dfList[[1]]
          }
        }, error = function(e) {
          jmvcore::reject(paste0("Error generating norm table: ", conditionMessage(e)))
        })
        
        private$.populateNormTable(
          tab = tab,
          rawCol = "x",
          normCol = "norm",
          percCol = "Percentile",
          descend = FALSE,
          noteText = paste0("Norm score table (", methodLabel, ") for explanatory variable = ", normAge, ".")
        )
      }
      
      # Predict on unique (age, raw) pairs for spreadsheet outputs
      uniquePairs <- unique(dataComplete[, c("group", "raw"), drop = FALSE])
      predNormUnique <- tryCatch({
        if (method == "betabinomial") {
          predict.cnormBetaBinomial(model, age = uniquePairs$group, score = uniquePairs$raw, range = sdRange)
        } else if (method == "shash") {
          predict.cnormShash(model, age = uniquePairs$group, score = uniquePairs$raw, range = sdRange)
        } else if (method == "cmp") {
          predict.cnormCMP(model, age = uniquePairs$group, score = uniquePairs$raw, range = sdRange, type = "norm")
        }
      }, error = function(e) {
        jmvcore::reject(paste0("Error predicting norm scores: ", conditionMessage(e)))
      })
      
      lookupKey <- paste(uniquePairs$group, uniquePairs$raw, sep = "|")
      lookupVec <- stats::setNames(predNormUnique, lookupKey)
      dataKey   <- paste(dataComplete$group, dataComplete$raw, sep = "|")
      predictedNorms <- as.numeric(lookupVec[dataKey])
      predictedPerc  <- stats::pnorm((predictedNorms - m1) / sd1) * 100
      
      private$.saveColumns(
        rowNames = rowNames,
        manifestNorms = NULL,
        manifestPerc = NULL,
        predictedNorms = predictedNorms,
        predictedPerc = predictedPerc
      )
      
      # Build lightweight plot state in .run() (without serializing the full model object)
      percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
      NAMES <- paste0("PR", percentiles * 100)
      n_points <- 100
      age_range <- range(age, na.rm = TRUE)
      pred_ages <- seq(age_range[1], age_range[2], length.out = n_points)
      
      # Manifest percentiles per group
      if (length(age) / length(unique(age)) > 50 && min(table(age)) > 30) {
        grp <- age
      } else {
        grp <- getGroups(age)
      }
      if (length(unique(grp)) > 30) {
        grp <- getGroups(age, n = 30)
      }
      
      wPlot <- if (hasWeights) wVec else rep(1, length(age))
      obsList <- lapply(split(seq_along(age), grp), function(idx) {
        a_mean <- mean(age[idx])
        q_vals <- private$.weightedQuantile(score[idx], probs = percentiles, weights = wPlot[idx])
        data.frame(
          age = a_mean,
          value = q_vals,
          percentile = factor(NAMES, levels = NAMES),
          stringsAsFactors = FALSE
        )
      })
      obsDF <- do.call(rbind, obsList)
      obsDF <- obsDF[!is.na(obsDF$value), , drop = FALSE]
      
      # Predicted percentile lines
      if (method == "betabinomial") {
        preds <- predictCoefficients2(model, pred_ages, nMax)
        pMat <- vapply(seq_along(pred_ages), function(j) {
          dist <- bb_distribution(preds$a[j], preds$b[j], nMax)
          if (anyNA(dist$cum)) return(rep(NA_real_, length(percentiles)))
          vapply(percentiles, function(p) as.numeric(dist$x[which.max(dist$cum >= p)]), numeric(1))
        }, numeric(length(percentiles)))
        pMat <- t(pMat)
        isDiscrete <- TRUE
        yLim <- c(0, nMax)
      } else if (method == "shash") {
        preds <- predictCoefficients_shash(model, pred_ages)
        pLines <- lapply(percentiles, function(p) {
          qshash(p, mu = preds$mu, sigma = preds$sigma, epsilon = preds$epsilon, delta = preds$delta)
        })
        pMat <- do.call(cbind, pLines)
        isDiscrete <- FALSE
        yLim <- NULL
      } else if (method == "cmp") {
        preds <- predictCoefficients_cmp(model, pred_ages)
        o <- cmp_opts(model)
        pLines <- lapply(percentiles, function(p) {
          cmp_quantile_mid(rep(p, n_points), mu = preds$mu, nu = preds$nu,
                           tol = o$tol, max_terms = o$max_terms, max_score = o$max_score)
        })
        pMat <- do.call(cbind, pLines)
        isDiscrete <- FALSE
        yLim <- NULL
      }
      
      predList <- lapply(seq_along(percentiles), function(i) {
        data.frame(
          age = pred_ages,
          value = as.numeric(pMat[, i]),
          percentile = factor(NAMES[i], levels = NAMES),
          stringsAsFactors = FALSE
        )
      })
      predDF <- do.call(rbind, predList)
      
      self$results$plot$setState(list(
        method = method,
        points = data.frame(age = age, raw = score),
        obs = obsDF,
        pred = predDF,
        levels = NAMES,
        labels = paste0(percentiles * 100, "%"),
        title = paste0("Percentile Plot (", methodLabel, " Model)"),
        subtitle = paste0("BIC = ", round(diag$BIC, 1), ", R\u00b2 = ", round(diag$R2, 4)),
        discrete = isDiscrete,
        yLimits = yLim
      ))
    },
    
    # =========================================================================
    # Shared Helpers for Norm Table & Output Columns
    # =========================================================================
    .populateNormTable = function(tab, rawCol, normCol, percCol, descend = FALSE, noteText = "") {
      if (is.null(tab) || nrow(tab) == 0L) return()
      
      nAdjusted <- 0L
      if (nrow(tab) > 1L) {
        for (i in seq_len(nrow(tab))[-1]) {
          if (!descend && !is.na(tab[[normCol]][i]) && !is.na(tab[[normCol]][i - 1]) &&
              tab[[normCol]][i] < tab[[normCol]][i - 1]) {
            tab[[normCol]][i] <- tab[[normCol]][i - 1]
            tab[[percCol]][i] <- tab[[percCol]][i - 1]
            nAdjusted <- nAdjusted + 1L
          } else if (descend && !is.na(tab[[normCol]][i]) && !is.na(tab[[normCol]][i - 1]) &&
                     tab[[normCol]][i] > tab[[normCol]][i - 1]) {
            tab[[normCol]][i] <- tab[[normCol]][i - 1]
            tab[[percCol]][i] <- tab[[percCol]][i - 1]
            nAdjusted <- nAdjusted + 1L
          }
        }
      }
      
      if (nAdjusted > 0L) {
        private$.addNotice(
          name = "nonMonotonicTableNotice",
          type = jmvcore::NoticeType$WARNING,
          content = paste0(
            "The norm table was non-monotonic at ", nAdjusted,
            " raw score step(s); values were carried forward. Check the percentile curves for model inconsistency."
          )
        )
      }
      
      tableWidget <- self$results$norms
      hasCI <- isTRUE(self$options$includeCI) && ("lowerCI" %in% names(tab))
      
      for (i in seq_len(nrow(tab))) {
        rowVals <- list(
          Raw        = tab[[rawCol]][i],
          Norm       = tab[[normCol]][i],
          Percentile = tab[[percCol]][i]
        )
        if (hasCI) {
          rowVals$lowerCI    <- tab$lowerCI[i]
          rowVals$upperCI    <- tab$upperCI[i]
          rowVals$lowerCI_PR <- tab$lowerCI_PR[i]
          rowVals$upperCI_PR <- tab$upperCI_PR[i]
        }
        tableWidget$addRow(rowKey = i, values = rowVals)
      }
      tableWidget$setNote("empty", noteText, init = FALSE)
    },
    
    .saveColumns = function(rowNames, manifestNorms, manifestPerc, predictedNorms, predictedPerc) {
      if (!is.null(manifestNorms) && self$options$saveManifest && self$results$saveManifest$isNotFilled()) {
        self$results$saveManifest$setRowNums(rowNames)
        self$results$saveManifest$setValues(manifestNorms)
      }
      if (!is.null(manifestPerc) && self$options$saveManifestPerc && self$results$saveManifestPerc$isNotFilled()) {
        self$results$saveManifestPerc$setRowNums(rowNames)
        self$results$saveManifestPerc$setValues(manifestPerc)
      }
      if (!is.null(predictedNorms) && self$options$savePredicted && self$results$savePredicted$isNotFilled()) {
        self$results$savePredicted$setRowNums(rowNames)
        self$results$savePredicted$setValues(predictedNorms)
      }
      if (!is.null(predictedPerc) && self$options$savePredictedPerc && self$results$savePredictedPerc$isNotFilled()) {
        self$results$savePredictedPerc$setRowNums(rowNames)
        self$results$savePredictedPerc$setValues(predictedPerc)
      }
    },
    
    # =========================================================================
    # Main .run() and .plot() Entry Points
    # =========================================================================
    .run = function() {
      if (is.null(self$options$raw) || is.null(self$options$group)) {
        return()
      }
      self$results$instructions$setVisible(FALSE)
      
      dataComplete <- private$.prepareData()
      scaleInfo <- private$.resolveScale(self$options$scale, self$options$range)
      method <- self$options$method
      
      if (method == "taylor") {
        private$.runTaylor(dataComplete, scaleInfo)
      } else {
        private$.runParametric(dataComplete, scaleInfo, method)
      }
    },
    
    .plot = function(image, ggtheme, theme, ...) {
      if (is.null(self$options$raw) || is.null(self$options$group) || is.null(image$state))
        return(FALSE)
      
      st <- image$state
      COL1 <- grDevices::rainbow(length(st$levels), end = 0.8)
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = st$points,
          ggplot2::aes(x = .data$age, y = .data$raw),
          color = "black",
          alpha = 0.2,
          size = 0.7
        )
      
      if (isTRUE(st$discrete)) {
        p <- p + ggplot2::geom_step(
          data = st$pred,
          ggplot2::aes(x = .data$age, y = .data$value, color = .data$percentile),
          direction = "mid",
          linewidth = 0.7
        )
      } else {
        p <- p + ggplot2::geom_line(
          data = st$pred,
          ggplot2::aes(x = .data$age, y = .data$value, color = .data$percentile),
          linewidth = 0.7
        )
      }
      
      p <- p +
        ggplot2::geom_point(
          data = st$obs,
          ggplot2::aes(x = .data$age, y = .data$value, color = .data$percentile),
          size = 2.2,
          shape = 18
        ) +
        ggplot2::scale_color_manual(
          values = stats::setNames(COL1, st$levels),
          breaks = st$levels,
          labels = st$labels,
          name   = "Percentile"
        ) +
        ggplot2::labs(
          title    = st$title,
          subtitle = st$subtitle,
          x        = self$options$group,
          y        = self$options$raw
        ) +
        ggtheme +
        ggplot2::theme(
          plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle   = ggplot2::element_text(hjust = 0.5),
          legend.position = "right"
        )
      
      if (!is.null(st$yLimits)) {
        p <- p + ggplot2::scale_y_continuous(limits = st$yLimits)
      }
      
      print(p)
      return(TRUE)
    }
  )
)