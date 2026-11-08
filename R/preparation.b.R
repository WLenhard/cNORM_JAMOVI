preparationClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "preparationClass",
  inherit = preparationBase,
  private = list(
    .init = function() {
      hasAge <- !is.null(self$options$age)
      hasStrat <- !is.null(self$options$stratVars) && length(self$options$stratVars) > 0L
      
      if (!hasAge && !hasStrat) {
        self$results$instructions$setVisible(TRUE)
        self$results$instructions$setContent(
          "<html>
            <head></head>
            <body>
            <div class='instructions'>
              <p>This utility prepares data for continuous norming using the <b>cNORM</b> package (v3.7.0):</p>
              <ul>
                <li><b>Norm Group Formation</b>: Splits a continuous explanatory variable (such as chronological age) into discrete norm groups (either equal-sized quantile groups or equidistant intervals) and computes each group's mean (midpoint). Recommended group size is <i>n</i> &ge; 100 (and at least <i>n</i> &ge; 50).</li>
                <li><b>Raking / Post-Stratification Weights</b>: Computes iterative proportional fitting (raking) weights to correct for deviations of the norm sample from population marginals across one or more stratification variables (e.g., sex, migration, education).</li>
              </ul>
              <ol>
                <li>To form <b>norm groups</b>, place a continuous variable into <b>Continuous Explanatory Variable</b> and tick <b>Norm group midpoints</b> under <b>Save to Dataset</b>.</li>
                <li>To compute <b>raking weights</b>, place one or more categorical variables into <b>Stratification Variables</b>. The table will display all observed levels and a template string that you can copy into <b>Population proportions</b> (e.g., <code>sex: 1=0.52, 2=0.48; migration: 0=0.70, 1=0.30</code>).</li>
              </ol>
            </div>
            </body>
          </html>"
        )
      } else {
        self$results$instructions$setVisible(FALSE)
      }
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
    
    # Parse user-entered marginals string:
    # Format: "sex: 1=0.52, 2=0.48; migration: 0=0.70, 1=0.30"
    .parseMarginsString = function(specStr) {
      if (is.null(specStr) || !nzchar(trimws(specStr))) {
        return(NULL)
      }
      blocks <- unlist(strsplit(specStr, ";"))
      rows <- list()
      
      for (b in blocks) {
        b <- trimws(b)
        if (!nzchar(b)) next
        
        parts <- unlist(strsplit(b, ":", fixed = TRUE))
        if (length(parts) != 2L) {
          jmvcore::reject(
            paste0("Invalid population proportion syntax in '", b, "'. Expected 'variable: level1=prop1, level2=prop2; ...'.")
          )
        }
        varName <- trimws(parts[1])
        pairs <- unlist(strsplit(parts[2], ","))
        
        for (p in pairs) {
          p <- trimws(p)
          if (!nzchar(p)) next
          kv <- unlist(strsplit(p, "=", fixed = TRUE))
          if (length(kv) != 2L) {
            jmvcore::reject(
              paste0("Invalid level=proportion pair '", p, "' for variable '", varName, "'. Expected 'level=proportion'.")
            )
          }
          lvl <- trimws(kv[1])
          propVal <- suppressWarnings(as.numeric(trimws(kv[2])))
          if (is.na(propVal) || propVal <= 0 || propVal >= 1) {
            jmvcore::reject(
              paste0("Invalid proportion '", kv[2], "' for ", varName, "=", lvl, ". Proportions must be numbers between 0 and 1.")
            )
          }
          rows[[length(rows) + 1L]] <- data.frame(
            variables = varName,
            levels = lvl,
            share = propVal,
            stringsAsFactors = FALSE
          )
        }
      }
      
      if (length(rows) == 0L) return(NULL)
      do.call(rbind, rows)
    },
    
    .runGrouping = function() {
      ageVar <- self$options$age
      if (is.null(ageVar)) return()
      
      x <- jmvcore::toNumeric(self$data[[ageVar]])
      df <- data.frame(age = x)
      rownames(df) <- rownames(self$data)
      
      keep <- !is.na(df$age) & is.finite(df$age)
      dfComplete <- df[keep, , drop = FALSE]
      
      if (nrow(dfComplete) < 6L) {
        jmvcore::reject("At least 6 valid observations are required to form norm groups.")
      }
      if (length(unique(dfComplete$age)) < 2L) {
        jmvcore::reject("The explanatory variable has only one distinct value.")
      }
      
      nGroupsOpt <- as.integer(self$options$nGroups)
      equidist <- isTRUE(self$options$equidistant)
      
      if (!is.na(nGroupsOpt) && nGroupsOpt == 1L) {
        jmvcore::reject("Number of norm groups must be at least 2 (or 0 for automatic selection).")
      }
      
      nArg <- if (is.na(nGroupsOpt) || nGroupsOpt <= 0L) NULL else nGroupsOpt
      
      warns <- character(0)
      grpMidpoints <- tryCatch({
        withCallingHandlers(
          getGroups(dfComplete$age, n = nArg, equidistant = equidist),
          warning = function(w) {
            warns <<- c(warns, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
      }, error = function(e) {
        jmvcore::reject(paste0("Could not form norm groups: ", conditionMessage(e)))
      })
      
      if (length(warns) > 0L) {
        private$.addNotice(
          name = "groupTieNotice",
          type = jmvcore::NoticeType$WARNING,
          content = paste(warns, collapse = " ")
        )
      }
      
      # Summarize groups ordered by midpoint
      uMids <- sort(unique(grpMidpoints))
      nTotal <- nrow(dfComplete)
      groupTab <- self$results$groupTab
      plotDF <- data.frame(
        grpNo = seq_along(uMids),
        midpoint = uMids,
        label = character(length(uMids)),
        n = integer(length(uMids)),
        stringsAsFactors = FALSE
      )
      
      minGroupN <- nTotal
      for (i in seq_along(uMids)) {
        idx <- which(grpMidpoints == uMids[i])
        vals <- dfComplete$age[idx]
        n_i <- length(idx)
        if (n_i < minGroupN) minGroupN <- n_i
        min_i <- min(vals)
        max_i <- max(vals)
        
        groupTab$addRow(rowKey = i, values = list(
          grpNo    = i,
          midpoint = uMids[i],
          minVal   = min_i,
          maxVal   = max_i,
          n        = n_i,
          prop     = (n_i / nTotal) * 100
        ))
        
        plotDF$label[i] <- sprintf("%.2f\n[%.1f-%.1f]", uMids[i], min_i, max_i)
        plotDF$n[i] <- n_i
      }
      
      if (minGroupN < 50L) {
        private$.addNotice(
          name = "smallNormGroupNotice",
          type = jmvcore::NoticeType$WARNING,
          content = paste0(
            "The smallest norm group contains n = ", minGroupN,
            " observations (recommended: n >= 100, minimum: n >= 50). Consider reducing the number of norm groups."
          )
        )
      }
      
      methodNote <- if (equidist) "Equidistant age intervals." else "Equal-sized (quantile) groups."
      groupTab$setNote("info", paste0(length(uMids), " norm groups formed (", methodNote, ")."), init = FALSE)
      
      if (self$options$saveGroups && self$results$saveGroups$isNotFilled()) {
        self$results$saveGroups$setRowNums(rownames(dfComplete))
        self$results$saveGroups$setValues(as.numeric(grpMidpoints))
      }
      
      self$results$groupPlot$setState(plotDF)
    },
    
    .runRaking = function() {
      stratVars <- self$options$stratVars
      if (is.null(stratVars) || length(stratVars) == 0L) return()
      
      df <- as.data.frame(self$data[, stratVars, drop = FALSE])
      rownames(df) <- rownames(self$data)
      
      keep <- stats::complete.cases(df)
      dfComplete <- df[keep, , drop = FALSE]
      
      if (nrow(dfComplete) < 5L) {
        jmvcore::reject("Too few complete cases across the selected stratification variables.")
      }
      
      # Convert stratification columns to character for clean level matching
      for (v in stratVars) {
        dfComplete[[v]] <- as.character(dfComplete[[v]])
      }
      
      nTotal <- nrow(dfComplete)
      marginsDF <- private$.parseMarginsString(self$options$margins)
      hasTargetMargins <- !is.null(marginsDF)
      
      # Build template string from observed sample proportions to help the user
      templateParts <- character(length(stratVars))
      for (i in seq_along(stratVars)) {
        v <- stratVars[i]
        tb <- table(dfComplete[[v]])
        props <- round(as.numeric(tb) / nTotal, 3)
        lvls <- names(tb)
        templateParts[i] <- paste0(v, ": ", paste0(lvls, "=", props, collapse = ", "))
      }
      templateStr <- paste(templateParts, collapse = "; ")
      
      rakedWeights <- NULL
      if (hasTargetMargins) {
        warns <- character(0)
        rakedWeights <- tryCatch({
          withCallingHandlers(
            suppressMessages(
              computeWeights(
                data = dfComplete,
                population.margins = marginsDF,
                standardized = isTRUE(self$options$standardized)
              )
            ),
            warning = function(w) {
              warns <<- c(warns, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )
        }, error = function(e) {
          jmvcore::reject(paste0("Raking failed: ", conditionMessage(e)))
        })
        
        if (length(warns) > 0L) {
          private$.addNotice(
            name = "rakingWarnNotice",
            type = jmvcore::NoticeType$WARNING,
            content = paste(warns, collapse = " ")
          )
        }
      }
      
      # Populate Marginals Table
      marginsTab <- self$results$marginsTab
      rowIdx <- 0L
      for (v in stratVars) {
        tb <- table(dfComplete[[v]])
        lvls <- names(tb)
        for (lvl in lvls) {
          rowIdx <- rowIdx + 1L
          n_lvl <- as.integer(tb[[lvl]])
          s_prop <- n_lvl / nTotal
          
          t_prop <- NA_real_
          c_weight <- NA_real_
          
          if (hasTargetMargins) {
            mIdx <- which(marginsDF$variables == v & as.character(marginsDF$levels) == as.character(lvl))
            if (length(mIdx) > 0L) {
              t_prop <- marginsDF$share[mIdx[1]]
            }
            if (!is.null(rakedWeights)) {
              c_weight <- mean(rakedWeights[dfComplete[[v]] == lvl], na.rm = TRUE)
            }
          }
          
          marginsTab$addRow(rowKey = rowIdx, values = list(
            variable   = v,
            level      = as.character(lvl),
            nObs       = n_lvl,
            sampleProp = s_prop,
            targetProp = t_prop,
            cellWeight = c_weight
          ))
        }
      }
      
      if (!hasTargetMargins) {
        marginsTab$setNote(
          "template",
          paste0("Enter target proportions under 'Population proportions' using the format: ", templateStr),
          init = FALSE
        )
      } else {
        marginsTab$setNote(
          "template",
          "Raking weights computed via iterative proportional fitting (computeWeights).",
          init = FALSE
        )
      }
      
      # Populate Weight Diagnostics Table
      nCells <- nrow(unique(dfComplete[, stratVars, drop = FALSE]))
      if (!is.null(rakedWeights)) {
        sumW  <- sum(rakedWeights)
        sumW2 <- sum(rakedWeights^2)
        nEff  <- (sumW^2) / sumW2
        deff  <- nTotal / nEff
        
        self$results$weightSummaryTab$setRow(rowNo = 1, values = list(
          nValid = nTotal,
          nCells = nCells,
          minW   = min(rakedWeights),
          maxW   = max(rakedWeights),
          meanW  = mean(rakedWeights),
          deff   = deff,
          nEff   = nEff
        ))
        
        if (self$options$saveWeights && self$results$saveWeights$isNotFilled()) {
          self$results$saveWeights$setRowNums(rownames(dfComplete))
          self$results$saveWeights$setValues(as.numeric(rakedWeights))
        }
      } else {
        self$results$weightSummaryTab$setRow(rowNo = 1, values = list(
          nValid = nTotal,
          nCells = nCells,
          minW   = NA_real_,
          maxW   = NA_real_,
          meanW  = NA_real_,
          deff   = NA_real_,
          nEff   = NA_real_
        ))
      }
    },
    
    .run = function() {
      hasAge <- !is.null(self$options$age)
      hasStrat <- !is.null(self$options$stratVars) && length(self$options$stratVars) > 0L
      
      if (!hasAge && !hasStrat) {
        return()
      }
      self$results$instructions$setVisible(FALSE)
      
      if (hasAge) {
        private$.runGrouping()
      }
      if (hasStrat) {
        private$.runRaking()
      }
    },
    
    .plotGroups = function(image, ggtheme, theme, ...) {
      if (is.null(self$options$age) || is.null(image$state))
        return(FALSE)
      
      df <- image$state
      df$labelFactor <- factor(df$label, levels = df$label)
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$labelFactor, y = .data$n)) +
        ggplot2::geom_col(fill = "#3E6DA9", alpha = 0.85, width = 0.65) +
        ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "#2ca25f", linewidth = 0.7) +
        ggplot2::geom_hline(yintercept = 50, linetype = "dotted", color = "#d95f02", linewidth = 0.7) +
        ggplot2::labs(
          title = "Observations per Norm Group",
          subtitle = "Dashed line: n = 100 (recommended); Dotted line: n = 50 (minimum)",
          x = "Group Midpoint [Range]",
          y = "Sample Size (N)"
        ) +
        ggtheme +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          axis.text.x = ggplot2::element_text(angle = if (nrow(df) > 8) 35 else 0, hjust = if (nrow(df) > 8) 1 else 0.5)
        )
      
      print(p)
      return(TRUE)
    }
  )
)