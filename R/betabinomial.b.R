betabinomialClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "betabinomialClass",
  inherit = betabinomialBase,
  private = list(
    .predictedNorms = NA,
    .predictedPerc  = NA,
    .rowNames       = NA,
    .model          = NA,
    
    .error = function(msg, e = NULL) {
      self$results$instructions$setVisible(visible = TRUE)
      self$results$instructions$setContent(paste0(
        "<html><head></head><body><div class='instructions'>",
        "<b>Error:</b> ", msg,
        if (!is.null(e)) paste0("<br><br><pre>", e$message, "</pre>") else "",
        "</div></body></html>"
      ))
    },
    
    .init = function() {
      private$.predictedNorms <- NULL
      private$.predictedPerc  <- NULL
      private$.rowNames       <- NULL
      private$.model          <- NULL
      
      self$results$instructions$setContent(
        "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>This module estimates continuous norm scores using the beta-binomial distribution, 
            which is particularly suitable for discrete item count data where raw scores are integers 
            ranging from 0 to the maximum number of items. In psychometric tests, this is especially
            true for IRT logistic models (e. g. Rasch).</p>
            <p>The beta-binomial model relates raw scores, norm scores, and an explanatory variable 
            (e.g., age) by modeling the parameters of a beta-binomial distribution as polynomial 
            functions of the explanatory variable.</p>
            <p>Please proceed as follows:</p>
            <ol>
              <li>Select your <b>raw score</b> variable (integer values from 0 to max)</li>
              <li>Place the <b>explanatory variable</b> (e.g., age) in the appropriate slot</li>
              <li>Specify the <b>number of items</b> in your test (or leave empty to use the maximum raw score)</li>
              <li>Adjust model parameters for optimal results:
                  <ul>
                    <li><b>Alpha parameter degree</b>: Controls the complexity of the alpha parameter curve (usually 3 is sufficient)</li>
                    <li><b>Beta parameter degree</b>: Controls the complexity of the beta parameter curve (usually 3 is sufficient)</li>
                  </ul>
              </li>
              <li><b>Norm Table Value</b>: Specify the value of the explanatory variable to generate a norm table</li>
            </ol>
            <p>Further information on the procedure is available via <a href=\"https://www.psychometrica.de/cNorm_betabinomial_en.html\" target=\"_blank\">Psychometrica</a>.</p>
            </div>
            </body>
            </html>")
      self$results$norms$setNote("empty", "Please specify value of explanatory variable for norm data computation", init = TRUE)
      self$results$modelTab$setVisible(self$options$model)
    },
    
    .run = function() {
      # Check if required variables are selected
      if (is.null(self$options$raw) || is.null(self$options$explanatory)) {
        self$results$instructions$setVisible(visible = TRUE)
        return()
      } else {
        self$results$instructions$setContent("<html/>")
        self$results$instructions$setVisible(visible = FALSE)
      }
      
      # Verify that required cNORM functions are accessible
      if (!exists("cnorm.betabinomial", mode = "function")) {
        private$.error("Function 'cnorm.betabinomial' not found. Make sure the cNORM package is properly loaded.")
        return()
      }
      
      tryCatch({
        # Retrieve raw data
        x   <- jmvcore::toNumeric(self$data[[self$options$raw]])
        age <- jmvcore::toNumeric(self$data[[self$options$explanatory]])
        
        if(!is.null(self$options$weights)) {
          w    <- jmvcore::toNumeric(self$data[[self$options$weights]])
          data <- data.frame(raw = x, age = age, weights = w)
        } else {
          data <- data.frame(raw = x, age = age)
        }
        
        # CRITICAL: Preserve original Jamovi row indices so that
        # setRowNums() maps values to the correct spreadsheet rows.
        # Without this, filtered datasets get sequential 1..M indices
        # instead of the true row positions, causing misalignment.
        rownames(data) <- rownames(self$data)
        
        # Filter complete cases — rownames carry through, so
        # filtered-out rows (NA due to Jamovi filter) are excluded
        # and the surviving rownames identify the correct positions
        dataComplete <- data[complete.cases(data), ]
        
        if(nrow(dataComplete) == 0) {
          private$.error("No complete cases found in the data. Check for missing values.")
          return()
        }
        
        # Check for non-finite values that survive complete.cases (e.g. Inf, NaN)
        numCols <- sapply(dataComplete, is.numeric)
        if(any(!is.finite(as.matrix(dataComplete[, numCols])))) {
          private$.error("Data contains non-finite values (NaN, Inf). Please check your data.")
          return()
        }
        
        # Validate weights
        if(!is.null(self$options$weights)) {
          if(min(dataComplete$weights, na.rm = TRUE) < 1) {
            private$.error("The weighting variable must only contain values >= 1.")
            return()
          }
        }
        
        # Validate and coerce raw scores to integers
        if(!all(abs(dataComplete$raw - round(dataComplete$raw)) < 1e-10)) {
          private$.error("The raw score variable must contain only integer values for beta-binomial modeling.")
          return()
        }
        dataComplete$raw <- as.integer(round(dataComplete$raw))
        
        # Resolve number of items
        n <- self$options$itemnumber
        if(is.null(n) || is.na(n) || n <= 0) {
          n <- max(dataComplete$raw, na.rm = TRUE)
        } else {
          n <- as.integer(n)
        }
        
        if(n <= 0) {
          private$.error("Number of items must be a positive integer.")
          return()
        }
        
        if(min(dataComplete$raw, na.rm = TRUE) < 0 || max(dataComplete$raw, na.rm = TRUE) > n) {
          private$.error(paste0("Raw scores must be between 0 and ", n, ". Check your data or adjust the number of items."))
          return()
        }
        
        # Resolve model parameters
        alpha <- as.integer(self$options$alpha)
        beta  <- as.integer(self$options$beta)
        scale <- self$options$scale
        
        # Fit the beta-binomial model
        model <- tryCatch({
          cnorm.betabinomial(
            age     = dataComplete$age,
            score   = dataComplete$raw,
            n       = n,
            weights = if(!is.null(self$options$weights)) dataComplete$weights else NULL,
            mode    = 2,
            alpha   = alpha,
            beta    = beta,
            scale   = scale,
            plot    = FALSE,
            control = list(factr = 1e-8, maxit = 1000)
          )
        }, error = function(e) {
          private$.error("Error in model fitting", e)
          return(NULL)
        })
        
        if(is.null(model)) return()
        
        private$.model <- model
        
        # Model summary table
        self$results$modelTab$setVisible(self$options$model)
        if(self$options$model) {
          summary_text <- tryCatch({
            paste(capture.output(
              summary(model,
                      age    = dataComplete$age,
                      score  = dataComplete$raw,
                      weights = if(!is.null(self$options$weights)) dataComplete$weights else NULL)
            ), collapse = "\n")
          }, error = function(e) {
            paste0("Error generating model summary: ", e$message)
          })
          self$results$modelTab$setContent(summary_text)
        } else {
          self$results$modelTab$setContent("")
        }
        
        # Norm table
        if(!is.null(self$options$normAge) && nchar(self$options$normAge) > 0) {
          normAge <- suppressWarnings(as.numeric(self$options$normAge))
          
          if(!is.na(normAge)) {
            norm_table_result <- tryCatch({
              normTable <- normTable.betabinomial(model, ages = normAge, n = n)
              list(success = TRUE, table = normTable)
            }, error = function(e) {
              list(success = FALSE, message = paste0("Error generating norm table: ", e$message))
            })
            
            if(norm_table_result$success) {
              df    <- norm_table_result$table[[1]]
              table <- self$results$norms
              table$setRow(rowNo = 1, values = list(
                Raw        = df$x[1],
                Norm       = df$norm[1],
                Percentile = df$Percentile[1]
              ))
              for(i in 2:nrow(df)) {
                table$addRow(rowKey = i, values = list(
                  Raw        = df$x[i],
                  Norm       = df$norm[i],
                  Percentile = df$Percentile[i]
                ))
              }
              self$results$norms$setNote(
                "empty",
                paste0("Norm score table for explanatory variable value '", normAge, "'."),
                init = FALSE
              )
            } else {
              private$.error(norm_table_result$message)
              return()
            }
          }
        }
        

        # Save row names before any operation
        savedRowNames <- rownames(dataComplete)
        
        # Predict on unique (age, raw) pairs, then map back via
        # lookup to guarantee: identical inputs → identical output
        uniquePairs <- unique(dataComplete[, c("age", "raw")])
        
        predictOK <- tryCatch({
          uniquePairs$predictedNorm <- predict.cnormBetaBinomial(
            model,
            age   = uniquePairs$age,
            score = uniquePairs$raw
          )
          TRUE
        }, error = function(e) {
          private$.error("Error predicting norm scores", e)
          FALSE
        })
        
        if(!predictOK) return()
        
        # Map predictions back to dataComplete via composite key;
        # this preserves row order and guarantees consistent values
        lookupKey <- paste(uniquePairs$age, uniquePairs$raw, sep = "|")
        lookupVec <- setNames(uniquePairs$predictedNorm, lookupKey)
        dataKey   <- paste(dataComplete$age, dataComplete$raw, sep = "|")
        dataComplete$predictedNorm <- as.numeric(lookupVec[dataKey])
        
        # Derive percentile scores
        scaleM  <- attr(model$result, "scaleMean")
        sdScale <- attr(model$result, "scaleSD")
        
        if(!is.null(scaleM) && !is.null(sdScale) &&
           !is.na(scaleM)  && !is.na(sdScale)  && sdScale > 0) {
          dataComplete$predictedPercentile <- pnorm(
            (dataComplete$predictedNorm - scaleM) / sdScale
          ) * 100
        } else {
          dataComplete$predictedPercentile <- pnorm(dataComplete$predictedNorm) * 100
        }
        
        # Row names are the original Jamovi positions (preserved from
        # rownames(self$data) above), so setRowNums() will write each
        # value to the correct spreadsheet row
        private$.rowNames       <- rownames(dataComplete)
        private$.predictedNorms <- dataComplete$predictedNorm
        private$.predictedPerc  <- dataComplete$predictedPercentile
        
        private$.populateOutputs()
        
        # Store state for plot
        self$results$plot$setState(list(
          data  = dataComplete,
          model = model,
          n     = n
        ))
        
        self$results$instructions$setVisible(FALSE)
        
      }, error = function(e) {
        private$.error("Unexpected error in main execution", e)
      })
    },
    
    .populateOutputs = function() {
      if(self$options$savePredicted && self$results$savePredicted$isNotFilled()) {
        self$results$savePredicted$setRowNums(private$.rowNames)
        self$results$savePredicted$setValues(private$.predictedNorms)
      }
      
      if(self$options$savePredictedPerc && self$results$savePredictedPerc$isNotFilled()) {
        self$results$savePredictedPerc$setRowNums(private$.rowNames)
        self$results$savePredictedPerc$setValues(private$.predictedPerc)
      }
    },
    
    .plot = function(image, ...) {
      if(is.null(self$options$raw) || is.null(self$options$explanatory) || is.null(image$state))
        return(FALSE)
      
      state <- image$state
      model <- state$model
      data  <- state$data
      n     <- state$n
      
      tryCatch({
        p <- plot.cnormBetaBinomial(
          model,
          age     = data$age,
          score   = data$raw,
          weights = if(!is.null(self$options$weights)) data$weights else NULL,
          points  = TRUE
        )
        print(p)
      }, error = function(e) {
        p <- ggplot2::ggplot(data = data, ggplot2::aes(x = age, y = raw)) +
          ggplot2::geom_point(alpha = 0.5) +
          ggplot2::labs(
            title    = "Beta-Binomial Model (Error in plotting)",
            subtitle = paste("Error:", e$message),
            x        = "Explanatory Variable",
            y        = "Raw Score"
          ) +
          ggplot2::theme_bw()
        print(p)
      })
      
      return(TRUE)
    }
  )
)