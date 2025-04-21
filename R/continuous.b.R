continuousClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "continuousClass",
  inherit = continuousBase,
  private = list(
    .manifestNorms = NA,
    .manifestPerc = NA,
    .predictedNorms = NA,
    .predictedPerc = NA,
    .rowNames = NA,
    .rowMapping = NA,
    .raw = NA,
    
    .init = function() {
      private$.manifestNorms <- NULL
      private$.manifestPerc <- NULL
      private$.predictedNorms <- NULL
      private$.predictedPerc <- NULL
      private$.raw <- NULL
      private$.rowNames <- NULL
      private$.rowMapping <- NULL
      
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
    
    .run = function() {
      # Initial state - show computing message
      self$results$instructions$setContent("<html><head></head><body><div class='instructions'><p>Computing results, please wait...</p></div></body></html>")
      self$results$instructions$setVisible(TRUE)
      
      if (is.null(self$options$raw)||is.null(self$options$group)){
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
        return()
      }else{
        self$results$instructions$setContent("<html/>")
        self$results$instructions$setVisible(visible = FALSE)
      }                
      
      x <- jmvcore::toNumeric(self$data[[self$options$raw]])
      g <- jmvcore::toNumeric(self$data[[self$options$group]])
      w <- rep(1, length(x))
      
      if(!is.null(self$options$weights)){
        w <- jmvcore::toNumeric(self$data[[self$options$weights]])
        data <- data.frame(raw = x, group=g, weights = w)
      }else{
        data <- data.frame(raw = x, group=g)  
      }
      
      # Store original row indices before filtering
      originalIndices <- rownames(self$data)
      rownames(data) <- originalIndices
      
      # Filter and save complete data rows
      dataComplete <- data[complete.cases(data),]
      private$.rowNames <- rownames(dataComplete)
      # Create mapping from filtered rows to original positions
      private$.rowMapping <- match(private$.rowNames, originalIndices)
      
      if(!is.null(self$options$weights) && min(dataComplete$weights, na.rm = TRUE) < 1){
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent("<html><head></head><body>
                                                     <div class='instructions'>
                                                     <p>The weighting variable must only contain values >= 1.</p>
                                                     </div></body></html>")
        return()
      }
      
      k <- 5
      if(self$options$k == '1')
        k <- 1
      else if(self$options$k == '2')
        k <- 2
      else if(self$options$k == '3')
        k <- 3
      else if(self$options$k == '4')
        k <- 4
      else if(self$options$k == '6')
        k <- 6
      
      t <- 3
      if(self$options$t == '1')
        t <- 1
      else if(self$options$t == '2')
        t <- 2
      else if(self$options$t == '5')
        t <- 5
      else if(self$options$t == '4')
        t <- 4
      else if(self$options$t == '6')
        t <- 6
      
      scale <- self$options$scale
      
      sd <- self$options$range
      minNorm <- 20
      maxNorm <- 80
      
      scale <- self$options$scale
      sd1 <- 1
      m1 <- 0
      
      scale <- self$options$scale
      if(scale=="Wechsler"){
        m1 <- 10
        sd1 <- 3
        scale <- c(10, 3)
        minNorm <- 10 - (sd*3)
        maxNorm <- 10 + (sd*3)
      }
      else if(scale=="PISA"){
        m1 <- 500
        sd1 <- 100              
        scale <- c(500, 100)
        minNorm <- 500 - (sd*100)
        maxNorm <- 500 + (sd*100)
      } 
      else if(scale=="T"){
        m1 <- 50
        sd1 <- 10
        minNorm <- 50 - (sd*10)
        maxNorm <- 50 + (sd*10)
      }
      else if(scale=="IQ"){
        m1 <- 100
        sd1 <- 15
        minNorm <- 100 - (sd*15)
        maxNorm <- 100 + (sd*15)
      } 
      else if(scale=="z"){
        minNorm <- -sd
        maxNorm <- sd
      } 
      
      descend <- self$options$descend
      
      if(!is.null(self$options$weights)){
        dataComplete <- rankByGroup(dataComplete, raw=dataComplete$raw, group=dataComplete$group, weights = dataComplete$weights, scale = scale, descend = descend)
      }else{
        dataComplete <- rankByGroup(dataComplete, raw=dataComplete$raw, group=dataComplete$group, scale = scale, descend = descend)
      }
      
      dataComplete <- computePowers(dataComplete, k=k, t=t)
      attr(dataComplete, "k") <- k
      attr(dataComplete, "t") <- t
      
      terms <- 4
      
      # Use safe model fitting with improved error handling
      if(self$options$selectionType=="automaticSelection"){
        modelResult <- private$.safeModelFit(
          data = dataComplete, 
          k = k, 
          t = t,
          weights = if(!is.null(self$options$weights)) dataComplete$weights else NULL
        )
      }else{
        terms <- as.integer(self$options$terms)
        modelResult <- private$.safeModelFit(
          data = dataComplete, 
          terms = terms, 
          k = k, 
          t = t,
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
        self$results$modelTab$setRow(rowNo=1, values=list(Variable="Model summary", Weight="", Terms=model$ideal.model, RMSE=model$rmse, R2adj=model$subset$adjr2[model$ideal.model], BIC=model$subset$bic[model$ideal.model]))
        for(i in 1:length(model$coefficients)){
          self$results$modelTab$addRow(rowKey=(i+1), values=list(Variable=names(model$coefficients)[i], Weight=model$coefficients[[i]], Terms="", RMSE="", R2adj="", BIC=""))
          
        }
      }
      
      normAge <- as.numeric(self$options$normAge)
      
      if(!is.null(self$options$normAge)){
        if(nchar(self$options$normAge)>0){
          minRaw <- self$options$minRaw
          maxRaw <- self$options$maxRaw
          
          if(maxRaw <= minRaw){
            minRaw <- NULL
            maxRaw <- NULL
          }
          
          # Use tryCatch to safely generate the raw table
          rawTableResult <- tryCatch({
            tab <- rawTable(normAge, model, minNorm = minNorm, maxNorm = maxNorm, minRaw = minRaw, maxRaw = maxRaw, step = as.numeric(self$options$stepping))
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
          
          tab <- rawTableResult$table
          table <- self$results$norms
          table$setRow(rowNo=1, values=list(Raw=tab$raw[[1]], Norm=tab$norm[[1]], Percentile=tab$percentile[[1]]))
          
          for(i in 2:nrow(tab)){
            if(tab$norm[[i]]<tab$norm[[i-1]]){
              tab$norm[[i]] <- tab$norm[[i-1]]
              tab$percentile[[i]] <- tab$percentile[[i-1]]
            }
            
            table$addRow(rowKey=i, values=list(Raw=tab$raw[[i]], Norm=tab$norm[[i]], Percentile=tab$percentile[[i]]))
          }
          
          foot <- paste0("Norm score table for value '", normAge, "' of the grouping variable.")
          if(!is.null(minRaw))
            foot <- paste0(foot, " The range of raw scores was manually set from ", minRaw, " to ", maxRaw, ".")
          else
            foot <- paste0(foot, " The range of raw scores was automatically retrieved from the dataset.")
          
          self$results$norms$setNote("empty", foot, init=FALSE)
        }
      }
      
      private$.raw <- dataComplete$raw
      private$.manifestNorms <- dataComplete$normValue
      private$.manifestPerc <- dataComplete$percentile * 100
      
      dataComplete$predictedNorm <- predictNorm(dataComplete$raw, dataComplete$group, model, minNorm=minNorm, maxNorm=maxNorm)
      dataComplete$predictedPercentile <- pnorm((dataComplete$predictedNorm-m1)/sd1) * 100
      
      private$.predictedNorms <- dataComplete$predictedNorm
      private$.predictedPerc <- dataComplete$predictedPercentile
      private$.populateOutputs()
      
      # Store the data for plotting
      image <- self$results$plot
      image$setState(dataComplete)
      
      # Reset instructions to be invisible
      self$results$instructions$setVisible(FALSE)
    },
    
    .populateOutputs=function() {
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
    
    .plot=function(image, ...) {
      if (is.null(self$options$raw) || is.null(self$options$group) || is.null(image$state))
        return(FALSE)
      
      # Get data state
      data <- image$state
      
      # Create model for plotting
      k <- 5
      if(self$options$k == '1')
        k <- 1
      else if(self$options$k == '2')
        k <- 2
      else if(self$options$k == '3')
        k <- 3
      else if(self$options$k == '4')
        k <- 4
      else if(self$options$k == '6')
        k <- 6
      
      t <- 3
      if(self$options$t == '1')
        t <- 1
      else if(self$options$t == '2')
        t <- 2
      else if(self$options$t == '5')
        t <- 5
      else if(self$options$t == '4')
        t <- 4
      else if(self$options$t == '6')
        t <- 6
      
      # Fit model
      if(self$options$selectionType=="automaticSelection") {
        if(!is.null(self$options$weights)) {
          model <- tryCatch({
            bestModel(data, k = k, t = t, weights = data$weights, plot = FALSE)
          }, error = function(e) {
            NULL
          })
        } else {
          model <- tryCatch({
            bestModel(data, k = k, t = t, plot = FALSE)
          }, error = function(e) {
            NULL
          })
        }
      } else {
        terms <- as.integer(self$options$terms)
        if(terms <= 0 || terms >= ((k + 1)*(t + 1) - 1)) {
          # Invalid terms - show error message
          p <- ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0.5, y = 0.5, 
                              label = paste("Invalid number of terms:", terms),
                              size = 6) +
            ggplot2::theme_void() +
            ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
          print(p)
          return(TRUE)
        }
        
        if(!is.null(self$options$weights)) {
          model <- tryCatch({
            bestModel(data, terms = terms, k = k, t = t, weights = data$weights, plot = FALSE)
          }, error = function(e) {
            NULL
          })
        } else {
          model <- tryCatch({
            bestModel(data, terms = terms, k = k, t = t, plot = FALSE)
          }, error = function(e) {
            NULL
          })
        }
      }
      
      if (is.null(model)) {
        # Model fitting failed
        p <- ggplot2::ggplot(data = data, ggplot2::aes(x = group, y = raw)) +
          ggplot2::geom_point(alpha = 0.3) +
          ggplot2::labs(title = "Data Distribution (Model fitting failed)",
                        x = "Group", 
                        y = "Raw Score") +
          ggplot2::theme_bw()
        print(p)
        return(TRUE)
      }
      
      # Now implement our own version of plotPercentiles
      # ------------------------------------------------------------
      # Define percentiles to plot
      percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
      
      # Set scale parameters based on options
      scale <- self$options$scale
      if(scale=="Wechsler"){
        T <- qnorm(percentiles, 10, 3)
      } else if(scale=="PISA"){
        T <- qnorm(percentiles, 500, 100)              
      } else if(scale=="T"){
        T <- qnorm(percentiles, 50, 10)
      } else if(scale=="IQ"){
        T <- qnorm(percentiles, 100, 15)
      } else if(scale=="z"){
        T <- qnorm(percentiles)
      } else {
        # Default to T
        T <- qnorm(percentiles, 50, 10)
      }
      
      # Generate variable names for percentiles
      NAMES <- paste("PR", percentiles * 100, sep = "")
      NAMESP <- paste("PredPR", percentiles * 100, sep = "")
      
      # Round group values to 3 decimal places
      data$group <- round(data$group, digits=3)
      
      # Get unique group values
      AGEP <- unique(data$group)
      
      # Function to calculate weighted quantiles
      weighted.quantile <- function(x, probs, weights) {
        if (missing(weights)) {
          weights <- rep(1, length(x))
        }
        
        # Remove missing values
        indices <- !is.na(x) & !is.na(weights)
        x <- x[indices]
        weights <- weights[indices]
        
        # Order by x
        order_indices <- order(x)
        x <- x[order_indices]
        weights <- weights[order_indices]
        
        # Calculate cumulative weights
        cum_weights <- cumsum(weights) / sum(weights)
        
        # Calculate quantiles
        result <- numeric(length(probs))
        for (i in seq_along(probs)) {
          if (probs[i] <= 0) {
            result[i] <- min(x)
          } else if (probs[i] >= 1) {
            result[i] <- max(x)
          } else {
            idx <- which(cum_weights >= probs[i])[1]
            if (idx == 1) {
              result[i] <- x[1]
            } else {
              # Linear interpolation
              w1 <- cum_weights[idx-1]
              w2 <- cum_weights[idx]
              x1 <- x[idx-1]
              x2 <- x[idx]
              result[i] <- x1 + (x2 - x1) * (probs[i] - w1) / (w2 - w1)
            }
          }
        }
        return(result)
      }
      
      # Determine if descending order is used
      descend <- self$options$descend
      
      # Calculate actual percentiles per group
      percentile.actual <- data.frame(matrix(NA, nrow = length(AGEP), ncol = length(percentiles)))
      rownames(percentile.actual) <- AGEP
      
      for (i in seq_along(AGEP)) {
        group_data <- data[data$group == AGEP[i], ]
        weights <- if (!is.null(group_data$weights)) group_data$weights else rep(1, nrow(group_data))
        
        if (descend) {
          percentile.actual[i, ] <- weighted.quantile(group_data$raw, probs = 1 - percentiles, weights = weights)
        } else {
          percentile.actual[i, ] <- weighted.quantile(group_data$raw, probs = percentiles, weights = weights)
        }
      }
      
      percentile.actual$group <- as.numeric(rownames(percentile.actual))
      colnames(percentile.actual) <- c(NAMES, "group")
      
      # Generate finer-grained group values for prediction
      minAge <- min(data$group, na.rm = TRUE)
      maxAge <- max(data$group, na.rm = TRUE)
      share <- seq(from = minAge, to = maxAge, length.out = 100)
      
      # Combine observed and prediction group values
      AGEP <- sort(unique(c(AGEP, share)))
      
      # Initialize fitted percentiles dataframe
      percentile.fitted <- data.frame(matrix(NA, nrow = length(AGEP), ncol = length(T)))
      
      # Calculate fitted percentiles
      for (i in seq_along(AGEP)) {
        percentile.fitted[i, ] <- predictRaw(T, AGEP[i], model$coefficients, 
                                                    minRaw = min(data$raw, na.rm = TRUE), 
                                                    maxRaw = max(data$raw, na.rm = TRUE))
      }
      
      percentile.fitted$group <- AGEP
      percentile.fitted <- percentile.fitted[!duplicated(percentile.fitted$group), ]
      colnames(percentile.fitted) <- c(NAMESP, "group")
      rownames(percentile.fitted) <- percentile.fitted$group
      
      # Merge actual and predicted scores
      percentile <- merge(percentile.actual, percentile.fitted, by = "group", all = TRUE)
      
      # Set colors for the plot
      END <- 0.8
      COL1 <- rainbow(length(percentiles), end = END)
      
      # Create title and subtitle
      title <- "Observed and Predicted Percentile Curves"
      subtitle <- paste0("Model: ", model$ideal.model, ", R² = ", round(model$subset$adjr2[model$ideal.model], digits = 4))
      
      # Prepare data for ggplot
      plot_data <- data.frame(
        group = rep(percentile$group, 2 * length(percentiles)),
        value = c(as.matrix(percentile[, NAMES]), as.matrix(percentile[, NAMESP])),
        type = rep(c("Observed", "Predicted"), each = nrow(percentile) * length(percentiles)),
        percentile = factor(rep(rep(NAMES, each = nrow(percentile)), 2), levels = NAMES)
      )
      
      plot_data_predicted <- plot_data[plot_data$type == "Predicted", ]
      plot_data_observed <- plot_data[plot_data$type == "Observed" & !is.na(plot_data$value), ]
      
      # Create the ggplot
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = value, color = percentile)) +
        ggplot2::geom_line(data = plot_data_predicted, size = 0.75) +
        ggplot2::geom_point(data = plot_data_observed, size = 2.5) +
        ggplot2::labs(title = title,
                      subtitle = subtitle,
                      x = "Explanatory Variable (group)",
                      y = "Raw Score") +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_manual(values = setNames(COL1, NAMES),
                                    name = "Percentiles") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
          axis.text = ggplot2::element_text(size = 10),
          legend.position = "right",
          legend.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10),
          panel.grid.major = ggplot2::element_line(color = "gray90"),
          panel.grid.minor = ggplot2::element_line(color = "gray95")
        )
      
      # Add raw data points (small dots)
      p <- p + ggplot2::geom_point(data = data, ggplot2::aes(x = group, y = raw), 
                                   color = "black", alpha = 0.2, size = 0.6,
                                   inherit.aes = FALSE)
      
      # Print the plot
      print(p)
      return(TRUE)
    }
  )
)