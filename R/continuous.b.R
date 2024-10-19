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
            (W. Lenhard, Lenhard & Gary, 2018). The modeling procedure minimizes the error variance contained in the norm scores.   
            It requires smaller samples sizes compared to conventional norming, closes gaps within and between the norm tables and 
            smoothes sampling errors.</p>
            <p>Select a model with a low number of terms while preserving a high R<sup>2</sup> of the model. Avoid intersecting 
            percentile curves. Please proceed as follows:</p>
            <ol>
              <li>Select your <b>raw score</b> variable</li>
              <li>Place the <b>grouping variable</b> (e. g. grade) in the 'Grouping Variable' slot. Each group in the dataset should contain at least 50 cases, preferably 100.</li>
              <li>Adjust model parameters for retrieving a favorable model via visual inspection of the percentile plot. Pay attention to the following parameters:
                  <ul>
                    <li><b>Invert ranking order</b>: Please tick, e. g. if higher values depict lower performance as it is the case in error rates, reaction times ...</li>
                    <li><b>Degree of location</b>: To model the distribution of raw scores per group, it is advisable to set this value to 5 (indicating modelling up to power 5 in the polynomials)</li>
                    <li><b>Degree of age</b>: Indicates the complexity of the age trajectory. Setting this value to three models the age trajectories up to cubic relationships in the polynomila.</li>
                    <li><b>Number of terms</b>: cNORM tries to find a model that is both well fitting and parsimonious. You might want to change this manually to find other suiting solutions. Beware of high values (e.g. > 10) since these entail the risk of overfitting.</li>
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
    .run = function() {
      
      if (is.null(self$options$raw)||is.null(self$options$group)){
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>The module estimates continuous norm scores by modeling the functional relationship between raw scores (raw), 
            norm scores (L) and the grouping variable (A; e. g. age, schooling duration ...) using the cNORM package 
            (W. Lenhard, Lenhard & Gary, 2018). The modeling procedure minimizes the error variance contained in the norm scores.   
            It requires smaller samples sizes compared to conventional norming, closes gaps within and between the norm tables and 
            smoothes sampling errors.</p>
            <p>Select a model with a low number of terms while preserving a high R<sup>2</sup> of the model. Avoid intersecting 
            percentile curves. Please proceed as follows:</p>
            <ol>
              <li>Select your <b>raw score</b> variable</li>
              <li>Place the <b>grouping variable</b> (e. g. grade) in the 'Grouping Variable' slot. Each group in the dataset should contain at least 50 cases, preferably 100.</li>
              <li>Adjust model parameters for retrieving a favorable model via visual inspection of the percentile plot. Pay attention to the following parameters:
                  <ul>
                    <li><b>Invert ranking order</b>: Please tick, e. g. if higher values depict lower performance as it is the case in error rates, reaction times ...</li>
                    <li><b>Degree of location</b>: To model the distribution of raw scores per group, it is advisable to set this value to 5 (indicating modelling up to power 5 in the polynomials)</li>
                    <li><b>Degree of age</b>: Indicates the complexity of the age trajectory. Setting this value to three models the age trajectories up to cubic relationships in the polynomila.</li>
                    <li><b>Number of terms</b>: cNORM tries to find a model that is both well fitting and parsimonious. You might want to change this manually to find other suiting solutions. Beware of high values (e.g. > 10) since these entail the risk of overfitting.</li>
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
      
      rownames(data) <- rownames(self$finalData)
      data <- data[complete.cases(data),]
      private$.rowNames <- rownames(data)
      
      if(min(w)<1){
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent("<html><head></head><body>
                                                     <div class='instructions'>
                                                     <p>The weighting variable must only contain >= 1.</p>
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
        data <- cNORM::rankByGroup(data, raw=data$raw, group=data$group, weights = data$weights, scale = scale, descend = descend)
      }else{
        data <- cNORM::rankByGroup(data, raw=data$raw, group=data$group, scale = scale, descend = descend)
      }
      data <- cNORM::computePowers(data, k=k, t=t)
      attr(data, "k") <- k
      attr(data, "t") <- t
      
      terms <- 4
      if(self$options$selectionType=="automaticSelection"){
        if(!is.null(self$options$weights)){
          model <- cNORM::bestModel(data, plot=FALSE, k = k, t=t, weights=data$weights)
        }else{
          model <- cNORM::bestModel(data, plot=FALSE, k = k, t=t)
        }
      }else{
        terms <- as.integer(self$options$terms)
        if(terms <= 0 || terms >= ((k + 1)*(t + 1) - 1)){
          self$results$instructions$setVisible(visible = TRUE)
          self$results$instructions$setContent("<html><head></head><body><div class='instructions'><p>The number of terms is out of range.</p></div></body></html>")
          return()
        }
        if(!is.null(self$options$weights)){
          model <- cNORM::bestModel(data, plot=FALSE, terms = terms, k = k, t=t, weights=data$weights)
        }else{
          model <- cNORM::bestModel(data, plot=FALSE, terms = terms, k = k, t=t)
        }
      }
      
      if(is.null(model)){
        self$results$instructions$setVisible(visible = TRUE)
        self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                    "<b>Ups! Something went wrong!</b>",
                                                    "</div></body></html>"))
        return()
      }
      
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
          
          tab <- cNORM::rawTable(normAge, model, minNorm = minNorm, maxNorm = maxNorm, minRaw = minRaw, maxRaw = maxRaw, step = as.numeric(self$options$stepping))                  
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
      
      private$.raw <- data$raw
      private$.manifestNorms <- data$normValue
      private$.manifestPerc <- data$percentile * 100
      
      data$predictedNorm <- cNORM::predictNorm(data$raw, data$group, model, minNorm=minNorm, maxNorm=maxNorm)
      data$predictedPercentile <- pnorm((data$predictedNorm-m1)/sd1) * 100
      
      private$.predictedNorms <- data$predictedNorm
      private$.predictedPerc <- data$predictedPercentile
      private$.populateOutputs()
      
      image <- self$results$plot
      image$setState(data)
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
    
    .plot=function(image, ...) {  # <-- the plot function
      
      if (is.null(self$options$raw)||is.null(self$options$group)||is.null(image$state))
        return()
      
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
      
      if(self$options$selectionType=="automaticSelection"){
        if(!is.null(self$options$weights)){
          model <- cNORM::bestModel(image$state, k = k, t=t, weights=image$state$weights)
        }else{
          model <- cNORM::bestModel(image$state, k = k, t=t)
        }
      }
      else{
        terms <- as.integer(self$options$terms)
        if(terms <= 0 || terms >= ((k + 1)*(t + 1) - 1)){
          self$results$instructions$setVisible(visible = TRUE)
          self$results$instructions$setContent("<html><head></head><body><div class='instructions'><p>The number of terms is out of range.</p></div></body></html>")
          return()
        }
        model <- cNORM::bestModel(image$state, terms = terms, k = k, t=t, weights=image$state$weights)
      }
      
      TRUE
    })
)
