betabinomialClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "betabinomialClass",
  inherit = betabinomialBase,
  private = list(
    .model = NA,
    
    .init = function() {
      private$.model <- NULL
      
      self$results$instructions$setContent(
        "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>The Beta-Binomial Norming module implements parametric continuous norming using the beta-binomial 
            distribution. This approach is particularly well-suited for:</p>
            <ul>
                <li>Test scales with a fixed number of items</li>
                <li>Unspeeded tests (without strict time constraints)</li>
                <li>Tests where each item is scored as correct/incorrect</li>
                <li>Discrete raw scores (positive integers including zero)</li>
            </ul>
            <p>The beta-binomial distribution models the probability of achieving a certain raw score as a function of 
            both item difficulty and person ability. The shape parameters (alpha and beta) are modeled as polynomial 
            functions across the explanatory variable.</p>
            <p>Please proceed as follows:</p>
            <ol>
              <li>Select your <b>raw score</b> variable</li>
              <li>Place the <b>explanatory variable</b> (e.g., age or grade) in the corresponding slot</li>
              <li>Specify the <b>maximum possible score</b> on the test (typically the number of items)</li>
              <li>Adjust the polynomial degrees for alpha and beta parameters (typically 2-3 is sufficient)</li>
              <li>Specify a value of the explanatory variable to generate a norm table</li>
            </ol>
            <p>Further information on beta-binomial norming is available at 
            <a href=\"https://www.psychometrica.de/cNorm_parametric_en.html\" target=\"_blank\">Psychometrica</a>.</p>
            </div>
            </body>
            </html>")
      self$results$norms$setNote("empty", "Please specify value of explanatory variable for norm data computation", init=TRUE)
    },
    
    .run = function() {
      # Initial state - show computing message
      self$results$instructions$setContent("<html><head></head><body><div class='instructions'><p>Computing results, please wait...</p></div></body></html>")
      self$results$instructions$setVisible(TRUE)
      
      if (is.null(self$options$raw) || is.null(self$options$explanatory)) {
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>The Beta-Binomial Norming module implements parametric continuous norming using the beta-binomial 
            distribution. This approach is particularly well-suited for:</p>
            <ul>
                <li>Test scales with a fixed number of items</li>
                <li>Unspeeded tests (without strict time constraints)</li>
                <li>Tests where each item is scored as correct/incorrect</li>
                <li>Discrete raw scores (positive integers including zero)</li>
            </ul>
            <p>The beta-binomial distribution models the probability of achieving a certain raw score as a function of 
            both item difficulty and person ability. The shape parameters (alpha and beta) are modeled as polynomial 
            functions across the explanatory variable.</p>
            <p>Please proceed as follows:</p>
            <ol>
              <li>Select your <b>raw score</b> variable</li>
              <li>Place the <b>explanatory variable</b> (e.g., age or grade) in the corresponding slot</li>
              <li>Specify the <b>maximum possible score</b> on the test (typically the number of items)</li>
              <li>Adjust the polynomial degrees for alpha and beta parameters (typically 2-3 is sufficient)</li>
              <li>Specify a value of the explanatory variable to generate a norm table</li>
            </ol>
            <p>Further information on beta-binomial norming is available at 
            <a href=\"https://www.psychometrica.de/cNorm_parametric_en.html\" target=\"_blank\">Psychometrica</a>.</p>
            </div>
            </body>
            </html>")
        return()
      } else {
        self$results$instructions$setContent("<html/>")
        self$results$instructions$setVisible(visible = FALSE)
      }
      
      # Get data
      rawData <- jmvcore::toNumeric(self$data[[self$options$raw]])
      explanatoryData <- jmvcore::toNumeric(self$data[[self$options$explanatory]])
      weightsData <- NULL
      
      if(!is.null(self$options$weights)){
        weightsData <- jmvcore::toNumeric(self$data[[self$options$weights]])
      }
      
      # Basic validation
      if(length(rawData) == 0 || length(explanatoryData) == 0) {
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent("<html><head></head><body>
                                             <div class='instructions'>
                                             <p>Error: No valid data.</p>
                                             </div></body></html>")
        return()
      }
      
      # Create data frame for complete cases
      data <- data.frame(raw = rawData, explanatory = explanatoryData)
      if(!is.null(weightsData)) {
        data$weights <- weightsData
      }
      
      # Remove incomplete cases
      dataComplete <- data[complete.cases(data),]
      
      if(nrow(dataComplete) == 0) {
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent("<html><head></head><body>
                                             <div class='instructions'>
                                             <p>Error: No valid data after removing missing values.</p>
                                             </div></body></html>")
        return()
      }
      
      # Check weight values
      if(!is.null(weightsData) && min(dataComplete$weights, na.rm = TRUE) < 1) {
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent("<html><head></head><body>
                                             <div class='instructions'>
                                             <p>Error: Weights must be greater than or equal to 1.</p>
                                             </div></body></html>")
        return()
      }
      
      # Get maximum score
      maxScore <- self$options$maxScore
      
      # Validate maxScore
      if(maxScore <= 0) {
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent("<html><head></head><body>
                                             <div class='instructions'>
                                             <p>Error: Maximum score must be greater than 0.</p>
                                             </div></body></html>")
        return()
      }
      
      # Check if any raw scores exceed maxScore
      if(max(dataComplete$raw, na.rm = TRUE) > maxScore) {
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent(paste0("<html><head></head><body>
                                                   <div class='instructions'>
                                                   <p>Warning: Some raw scores (max = ", max(dataComplete$raw, na.rm = TRUE), 
                                                    ") exceed the specified maximum score (", maxScore, 
                                                    "). This may affect model accuracy.</p>
                                                   </div></body></html>"))
      }
      
      # Get model parameters
      alphaDegree <- as.integer(self$options$alpha)
      betaDegree <- as.integer(self$options$beta)
      
      # Set scale parameters based on selected norm scale
      scale <- self$options$scale
      sd <- self$options$range
      
      if(scale == "Wechsler"){
        m1 <- 10
        sd1 <- 3
        normScale <- c(10, 3)
        minNorm <- 10 - (sd*3)
        maxNorm <- 10 + (sd*3)
      }
      else if(scale == "PISA"){
        m1 <- 500
        sd1 <- 100              
        normScale <- c(500, 100)
        minNorm <- 500 - (sd*100)
        maxNorm <- 500 + (sd*100)
      } 
      else if(scale == "T"){
        m1 <- 50
        sd1 <- 10
        normScale <- c(50, 10)
        minNorm <- 50 - (sd*10)
        maxNorm <- 50 + (sd*10)
      }
      else if(scale == "IQ"){
        m1 <- 100
        sd1 <- 15
        normScale <- c(100, 15)
        minNorm <- 100 - (sd*15)
        maxNorm <- 100 + (sd*15)
      } 
      else if(scale == "z"){
        m1 <- 0
        sd1 <- 1
        normScale <- "z"
        minNorm <- -sd
        maxNorm <- sd
      } 
      
      # Get invert ranking order parameter
      descend <- self$options$descend
      
      # Fit beta-binomial model
      tryCatch({
        # Rename columns to match what the functions expect
        dataFit <- dataComplete
        names(dataFit)[names(dataFit) == "explanatory"] <- "age"
        names(dataFit)[names(dataFit) == "raw"] <- "score"
        
        # Fit the model
        bbModel <- NULL
        if(!is.null(dataFit$weights)) {
          bbModel <- cnorm.betabinomial(
            age = dataFit$age,
            score = dataFit$score,
            n = maxScore,
            weights = dataFit$weights,
            mode = 2,
            alpha = alphaDegree,
            beta = betaDegree,
            scale = normScale,
            plot = FALSE
          )
        } else {
          bbModel <- cnorm.betabinomial(
            age = dataFit$age,
            score = dataFit$score,
            n = maxScore,
            mode = 2,
            alpha = alphaDegree,
            beta = betaDegree,
            scale = normScale,
            plot = FALSE
          )
        }
        
        # Store model
        private$.model <- bbModel
        
        # Display model summary
        if(self$options$model){
          diag <- diagnostics.betabinomial(bbModel)
          
          # Extract coefficients from diagnostics
          coefficients <- data.frame(
            Parameter = names(diag$param_estimates),
            Value = diag$param_estimates,
            SE = diag$param_se,
            P = diag$p_values
          )
          
          # Add model info to the table
          for(i in 1:nrow(coefficients)){
            self$results$modelTab$addRow(rowKey = i, values = list(
              Parameter = coefficients$Parameter[i],
              Value = coefficients$Value[i],
              SE = coefficients$SE[i],
              P = coefficients$P[i]
            ))
          }
          
          # Add a separator row
          self$results$modelTab$addRow(rowKey = nrow(coefficients) + 1, values = list(
            Parameter = "---",
            Value = NA,
            SE = NA,
            P = NA
          ))
          
          # Add fit statistics
          rowIndex <- nrow(coefficients) + 2
          
          self$results$modelTab$addRow(rowKey = rowIndex, values = list(
            Parameter = "Log-likelihood",
            Value = diag$log_likelihood,
            SE = NA,
            P = NA
          ))
          rowIndex <- rowIndex + 1
          
          self$results$modelTab$addRow(rowKey = rowIndex, values = list(
            Parameter = "AIC",
            Value = diag$AIC,
            SE = NA,
            P = NA
          ))
          rowIndex <- rowIndex + 1
          
          self$results$modelTab$addRow(rowKey = rowIndex, values = list(
            Parameter = "BIC",
            Value = diag$BIC,
            SE = NA,
            P = NA
          ))
        }
        
        # Generate norm table if normAge is specified
        normAge <- suppressWarnings(as.numeric(self$options$normAge))
        
        if(!is.null(self$options$normAge) && !is.na(normAge) && nchar(self$options$normAge) > 0){
          stepping <- as.numeric(self$options$stepping)
          if(stepping <= 0) stepping <- 1
          
          # Create a norm table for the specified explanatory value
          normTable <- normTable.betabinomial(bbModel, normAge, n = maxScore)
          
          # Get the appropriate table from the list
          tableData <- normTable[[1]]
          
          # Clear any existing rows in the table
          self$results$norms$setRows(list())
          
          # Add rows to the table
          for(i in 1:nrow(tableData)){
            # Get norm score based on scale
            if(scale == "T") {
              normScore <- tableData$norm[i]
            } else if(scale == "IQ") {
              normScore <- m1 + (tableData$z[i] * sd1)
            } else if(scale == "z") {
              normScore <- tableData$z[i]
            } else if(scale == "Wechsler") {
              normScore <- m1 + (tableData$z[i] * sd1)
            } else if(scale == "PISA") {
              normScore <- m1 + (tableData$z[i] * sd1)
            } else {
              normScore <- tableData$norm[i]
            }
            
            # Add to table
            self$results$norms$addRow(rowKey = i, values = list(
              Raw = tableData$x[i],
              Norm = normScore,
              Percentile = tableData$Percentile[i]
            ))
          }
          
          # Add a note to the table
          foot <- paste0("Beta-binomial norm score table for value '", normAge, "' of the explanatory variable.")
          self$results$norms$setNote("empty", foot, init = FALSE)
        }
        
        # Save data for plotting
        image <- self$results$plot
        image$setState(list(
          data = dataFit,
          model = bbModel,
          maxScore = maxScore,
          scale = normScale
        ))
        
        paramsImage <- self$results$modelPlot
        paramsImage$setState(list(
          model = bbModel,
          minAge = min(dataFit$age),
          maxAge = max(dataFit$age)
        ))
        
      }, error = function(e) {
        # Show error message
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                    "<b>Error during model fitting:</b> ", e$message,
                                                    "<p>Possible solutions:</p>",
                                                    "<ul>",
                                                    "<li>Check that raw scores are positive integers</li>",
                                                    "<li>Verify that the maximum score is correctly specified</li>",
                                                    "<li>Try reducing the polynomial degrees for alpha and beta</li>",
                                                    "<li>Ensure you have sufficient data points across the explanatory variable range</li>",
                                                    "</ul>",
                                                    "</div></body></html>"))
        return()
      })
      
      # Reset instructions to be invisible
      self$results$instructions$setVisible(FALSE)
    },
    
    .plot = function(image, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      # Get state data
      state <- image$state
      data <- state$data
      bbModel <- state$model
      
      # Try to create the percentile plot
      try({
        # Use plot method from cNORM
        p <- plot.cnormBetaBinomial(bbModel, age = data$age, score = data$score, 
                                    weights = if("weights" %in% names(data)) data$weights else NULL)
        
        # Customize titles and labels
        p <- p + 
          ggplot2::ggtitle("Beta-Binomial Percentile Curves") +
          ggplot2::xlab("Explanatory Variable") +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title = ggplot2::element_text(size = 12, face = "bold")
          )
        
        print(p)
        return(TRUE)
      }, silent = FALSE)
      
      # If the above fails, create a simple plot
      p <- ggplot2::ggplot(data, ggplot2::aes(x = age, y = score)) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::labs(title = "Raw Scores by Explanatory Variable",
                      subtitle = "Beta-binomial modeling failed",
                      x = "Explanatory Variable", 
                      y = "Raw Score") +
        ggplot2::theme_minimal()
      
      print(p)
      return(TRUE)
    },
    
    .plotParams = function(image, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      # Get state data
      state <- image$state
      bbModel <- state$model
      minAge <- state$minAge
      maxAge <- state$maxAge
      
      # Generate age sequence
      ages <- seq(minAge, maxAge, length.out = 100)
      
      # Try to extract parameter information
      tryCatch({
        # Get predicted coefficients
        if (inherits(bbModel, "cnormBetaBinomial2")) {
          preds <- predictCoefficients2(bbModel, ages)
        } else {
          preds <- predictCoefficients(bbModel, ages)
        }
        
        # Create data frame for plotting
        paramsData <- data.frame(
          age = rep(ages, 2),
          value = c(preds$a, preds$b),
          parameter = rep(c("Alpha", "Beta"), each = length(ages))
        )
        
        # Create the plot
        p <- ggplot2::ggplot(paramsData, ggplot2::aes(x = age, y = value, color = parameter)) +
          ggplot2::geom_line(size = 1.5) +
          ggplot2::labs(
            title = "Beta Distribution Parameters by Explanatory Variable",
            x = "Explanatory Variable",
            y = "Parameter Value"
          ) +
          ggplot2::scale_color_manual(
            values = c("Alpha" = "blue", "Beta" = "red"),
            name = "Parameter"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title = ggplot2::element_text(size = 12, face = "bold"),
            legend.position = "top"
          )
        
        print(p)
        return(TRUE)
      }, error = function(e) {
        # Create an error message plot
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, 
                            label = "Unable to generate parameter plot", 
                            size = 5, hjust = 0.5) +
          ggplot2::theme_void() +
          ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
        
        print(p)
        return(TRUE)
      })
    }
  )
)