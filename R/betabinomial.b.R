betabinomialClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "betabinomialClass",
  inherit = betabinomialBase,
  private = list(
    .predictedNorms = NA,
    .predictedPerc = NA,
    .rowNames = NA,
    .rowMapping = NA,
    .model = NA,
    
    # Error logging function - only console and UI output
    .error = function(msg, e=NULL) {
      # Show error in the UI
      self$results$instructions$setVisible(visible = TRUE)
      self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                  "<b>Error:</b> ", msg,
                                                  if (!is.null(e)) paste("<br><br><pre>", e$message, "</pre>") else "",
                                                  "</div></body></html>"))
    },
    
    .init = function() {
      private$.predictedNorms <- NULL
      private$.predictedPerc <- NULL
      private$.rowNames <- NULL
      private$.rowMapping <- NULL
      private$.model <- NULL
      
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
      self$results$norms$setNote("empty", "Please specify value of explanatory variable for norm data computation", init=TRUE)
      
      # Initialize modelTab visibility to match the option
      self$results$modelTab$setVisible(self$options$model)
    },
    
    .run = function() {
      # Show computing message in UI immediately
      self$results$instructions$setContent("<html><head></head><body><div class='instructions'><p><b>Computing results, please wait...</b></p></div></body></html>")
      self$results$instructions$setVisible(TRUE)
      
      # Verify that we can access cNORM functions
      if (!exists("cnorm.betabinomial", mode="function")) {
        private$.error("Function 'cnorm.betabinomial' not found. Make sure cNORM package is properly loaded.")
        return()
      }
      
      # Check if required variables are selected
      if (is.null(self$options$raw) || is.null(self$options$explanatory)) {
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
        return()
      } else {
        self$results$instructions$setContent("<html/>")
        self$results$instructions$setVisible(visible = FALSE)
      }
      
      tryCatch({
        # Get data
        x <- jmvcore::toNumeric(self$data[[self$options$raw]])
        age <- jmvcore::toNumeric(self$data[[self$options$explanatory]])
        w <- rep(1, length(x))
        
        if(!is.null(self$options$weights)) {
          w <- jmvcore::toNumeric(self$data[[self$options$weights]])
        }
        
        # Create data frame and ensure all values are finite
        data <- data.frame(raw = x, age = age, weights = w)
        
        # Check for infinite or NaN values
        has_non_finite <- any(!is.finite(data$raw)) || any(!is.finite(data$age)) || any(!is.finite(data$weights))
        if (has_non_finite) {
          private$.error("Data contains non-finite values (NaN, Inf). Please check your data.")
          return()
        }
        
        # Ensure all values are strictly numeric (not factors or other types)
        data$raw <- as.numeric(data$raw)
        data$age <- as.numeric(data$age)
        data$weights <- as.numeric(data$weights)
        
        # Store original row indices before filtering
        originalIndices <- rownames(self$data)
        rownames(data) <- originalIndices
        
        # Filter and save complete data rows
        dataComplete <- data[complete.cases(data),]
        
        if(nrow(dataComplete) == 0) {
          private$.error("No complete cases found in the data. Check for missing values.")
          return()
        }
        
        private$.rowNames <- rownames(dataComplete)
        private$.rowMapping <- match(private$.rowNames, originalIndices)
        
        # Check for valid weights
        if(!is.null(self$options$weights)) {
          if(min(dataComplete$weights, na.rm = TRUE) < 1) {
            private$.error("The weighting variable must only contain values >= 1.")
            return()
          }
        }
        
        # Ensure raw scores are integers
        if(!all(abs(dataComplete$raw - round(dataComplete$raw)) < 1e-10)) {
          private$.error("The raw score variable must contain only integer values for beta-binomial modeling.")
          return()
        }
        
        # Force raw scores to be integers
        dataComplete$raw <- as.integer(round(dataComplete$raw))
        
        # Set parameters
        alpha <- as.integer(self$options$alpha)
        beta <- as.integer(self$options$beta)
        
        # Use user-specified itemNumber or max raw score if not provided
        n <- self$options$itemnumber
        if(is.null(n) || is.na(n) || n <= 0) {
          n <- max(dataComplete$raw, na.rm = TRUE)
        } else {
          n <- as.integer(n)
        }
        
        scale <- self$options$scale
        
        # Try to call the function with extra safety checks
        model <- tryCatch({
          # Additional validation before model fitting
          if (n <= 0) {
            stop("Number of items must be positive")
          }
          
          if (min(dataComplete$raw) < 0 || max(dataComplete$raw) > n) {
            stop(paste("Raw scores must be between 0 and", n))
          }
          
          # Call model with explicit parameter names for clarity
          cnorm.betabinomial(
            age = dataComplete$age,
            score = dataComplete$raw,
            n = n,
            weights = if(!is.null(self$options$weights)) dataComplete$weights else NULL,
            mode = 2,  # Explicitly set mode to 2
            alpha = alpha,
            beta = beta,
            scale = scale,
            plot = FALSE,
            control = list(factr = 1e-8, maxit = 1000)  # Add explicit control parameters
          )
        }, error = function(e) {
          private$.error(paste("Error in model fitting:", e$message))
          return(NULL)
        })
        
        if (is.null(model)) {
          return()  # Error already displayed
        }
        
        private$.model <- model
        
        # Update model summary visibility
        self$results$modelTab$setVisible(self$options$model)
        
        # Show model summary if requested
        if(self$options$model) {
          summary_text <- tryCatch({
            paste(capture.output(
              summary(model, age = dataComplete$age, score = dataComplete$raw, 
                      weights = if(!is.null(self$options$weights)) dataComplete$weights else NULL)
            ), collapse = "\n")
          }, error = function(e) {
            private$.error("Error generating model summary", e)
            return("Error generating model summary")
          })
          
          self$results$modelTab$setContent(summary_text)
        } else {
          # Clear content when not visible
          self$results$modelTab$setContent("")
        }
        
        # Generate norm table if normAge is specified
        if(!is.null(self$options$normAge) && nchar(self$options$normAge) > 0) {
          norm_table_result <- tryCatch({
            normAge <- as.numeric(self$options$normAge)
            
            # Generate norm table
            normTable <- normTable.betabinomial(model, ages = normAge, n = n)
            
            list(success = TRUE, table = normTable)
          }, error = function(e) {
            private$.error("Error generating norm table", e)
            return(list(success = FALSE))
          })
          
          if(norm_table_result$success) {
            # Get the table for this specific age
            df <- norm_table_result$table[[1]]
            
            # Populate the norm table
            table <- self$results$norms
            table$setRow(rowNo=1, values=list(Raw=df$x[1], Norm=df$norm[1], Percentile=df$Percentile[1]))
            
            for(i in 2:nrow(df)) {
              table$addRow(rowKey=i, values=list(Raw=df$x[i], Norm=df$norm[i], Percentile=df$Percentile[i]))
            }
            
            foot <- paste0("Norm score table for explanatory variable value '", normAge, "'.")
            self$results$norms$setNote("empty", foot, init=FALSE)
          }
        }
        
        # Calculate predicted norm scores for the original data
        predict_result <- tryCatch({
          dataComplete$predictedNorm <- predict.cnormBetaBinomial(model, age = dataComplete$age, score = dataComplete$raw)
          list(success = TRUE)
        }, error = function(e) {
          private$.error("Error predicting norm scores", e)
          return(list(success = FALSE))
        })
        
        if(!predict_result$success) {
          return()
        }
        
        # Get scale parameters to calculate percentiles
        scaleM <- attr(model$result, "scaleMean")
        sdScale <- attr(model$result, "scaleSD")
        
        # Calculate percentile scores
        if(!is.na(scaleM) && !is.na(sdScale)) {
          dataComplete$predictedPercentile <- pnorm((dataComplete$predictedNorm - scaleM) / sdScale) * 100
        } else {
          dataComplete$predictedPercentile <- dataComplete$predictedNorm
        }
        
        # Store predicted values for output
        private$.predictedNorms <- dataComplete$predictedNorm
        private$.predictedPerc <- dataComplete$predictedPercentile
        
        # Populate outputs
        private$.populateOutputs()
        
        # Store data for plotting
        image <- self$results$plot
        image$setState(list(
          data = dataComplete,
          model = model,
          n = n
        ))
        
        # Reset instructions to be invisible
        self$results$instructions$setVisible(FALSE)
        
      }, error = function(e) {
        private$.error("Unexpected error in main execution", e)
      })
    },
    
    .populateOutputs=function() {
      if (self$options$savePredicted && self$results$savePredicted$isNotFilled()) {
        self$results$savePredicted$setRowNums(private$.rowNames)
        self$results$savePredicted$setValues(private$.predictedNorms)
      }
      
      if (self$options$savePredictedPerc && self$results$savePredictedPerc$isNotFilled()) {
        self$results$savePredictedPerc$setRowNums(private$.rowNames)
        self$results$savePredictedPerc$setValues(private$.predictedPerc)
      }
    },    
    
    .plot=function(image, ...) {
      if (is.null(self$options$raw) || is.null(self$options$explanatory) || is.null(image$state)) {
        return(FALSE)
      }
      
      state <- image$state
      model <- state$model
      data <- state$data
      n <- state$n
      
      # Try to generate the plot
      tryCatch({
        p <- plot.cnormBetaBinomial(
          model, 
          age = data$age, 
          score = data$raw,
          weights = if(!is.null(self$options$weights)) data$weights else NULL,
          points = TRUE
        )
        
        print(p)
      }, error = function(e) {
        # Fallback plot
        p <- ggplot2::ggplot(data = data, ggplot2::aes(x = age, y = raw)) +
          ggplot2::geom_point(alpha = 0.5) +
          ggplot2::labs(
            title = "Beta-Binomial Model (Error in plotting)",
            subtitle = paste("Error:", e$message),
            x = "Explanatory Variable", 
            y = "Raw Score"
          ) +
          ggplot2::theme_bw()
        print(p)
      })
      
      return(TRUE)
    })
)