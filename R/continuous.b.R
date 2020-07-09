continuousClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "continuousClass",
    inherit = continuousBase,
    private = list(
        .init = function() {
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
              <li>Adjust model parameters for retrieving a favorable model via visual inspection of the percentile plot.</li>
              <li>Specify the level of the grouping variable to generate norm table.</li>
            </ol>
            <p>Further information on the procedure is available via <a href=\"https://www.psychometrica.de/cNorm_en.html\">Psychometrica</a>.</p>
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
              <li>Adjust model parameters for retrieving a favorable model via visual inspection of the percentile plot.</li>
              <li>Specify the level of the grouping variable to generate norm table.</li>
            </ol>
            <p>Further information on the procedure is available via <a href=\"https://www.psychometrica.de/cNorm_en.html\">Psychometrica</a>.</p>
            </div>
            </body>
            </html>")
              self$results$model$setContent("<html/>")
              return()
            }else{
              self$results$instructions$setContent("<html/>")
              self$results$instructions$setVisible(visible = FALSE)
        }                
                
                
      
            x <- jmvcore::toNumeric(self$data[[self$options$raw]])
            g <- jmvcore::toNumeric(self$data[[self$options$group]])
            data <- data.frame(raw = x, group=g)
            data <- data[complete.cases(data),]
            
            casesPerGroup <- nrow(data)/unique(data$group)
            if(casesPerGroup<20){
                self$results$instructions$setVisible(visible = TRUE)
                self$results$instructions$setContent("<html><head></head><body>
                                                     <div class='instructions'>
                                                     <p>Warning! The data are inconsistent. There are not enough cases 
                                                     per group. Please check your grouping variable.</p>
                                                     </div></body></html>")
                self$results$model$setContent("<html/>")
                return()
            }
            
            k <- 4
            if(self$options$k == 'Cubic')
                k <- 3
            else if(self$options$k == 'Quadratic')
                k <- 2
            
            scale <- self$options$scale
            
            sd <- self$options$range
            minNorm <- 20
            maxNorm <- 80
            
            scale <- self$options$scale
            if(scale=="Wechsler"){
              scale <- c(10, 3)
              minNorm <- 10 - (sd*3)
              maxNorm <- 10 + (sd*3)
            }
            else if(scale=="PISA"){
              scale <- c(500, 100)
              minNorm <- 500 - (sd*100)
              maxNorm <- 500 + (sd*100)
            } 
            else if(scale=="T"){
              minNorm <- 50 - (sd*10)
              maxNorm <- 50 + (sd*10)
            }
            else if(scale=="IQ"){
              minNorm <- 100 - (sd*15)
              maxNorm <- 100 + (sd*15)
            } 
            else if(scale=="z"){
              minNorm <- -sd
              maxNorm <- sd
            } 
            
            descend <- self$options$descend
            
            data <- cNORM::rankByGroup(data, raw=data$raw, group=data$group, scale = scale, descend = descend)
            data <- cNORM::computePowers(data, k=k)
            attr(data, "k") <- k
            
            terms <- 4
            if(self$options$selectionType=="automaticSelection")
              model <- cNORM::bestModel(data, plot=FALSE, k = k)
            else{
                terms <- as.integer(self$options$terms)
                if(terms <= 0 || terms >= (k + 1)^2){
                  self$results$instructions$setVisible(visible = TRUE)
                  self$results$instructions$setContent("<html><head></head><body><div class='instructions'><p>The number of terms is out of range. It has to be 8 or lower in quadratic, 15 or lower in cubic and 24 or lower in quartic polynomials.</p></div></body></html>")
                  self$results$model$setContent("<html/>")
                  return()
                }
                model <- cNORM::bestModel(data, plot=FALSE, terms = terms, k = k)
            }

            if(is.null(model)){
              self$results$instructions$setVisible(visible = TRUE)
              self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                          "<b>Ups! Something went wrong!</b>",
                                                          "</div></body></html>"))
              self$results$model$setContent("<html/>")
              return()
            }
            
            if(self$options$model){
            self$results$model$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                        "<b>Model summary</b><br>", 
                                                        model$report[1], "<br>",
                                                        model$report[2], "<br>",
                                                        model$report[3], "<br>",
                                                        model$report[4], "<br>",
                                                        model$report[5], "<br>",
                                                        "</div></body></html>"))
            }else{
              self$results$model$setContent("<html/>")
            }
            normAge <- as.numeric(self$options$normAge)

            if(!is.null(self$options$normAge)){
              if(nchar(self$options$normAge)>0){
                self$results$norms$setVisible(visible = TRUE)
                tab <- cNORM::rawTable(normAge, model, minNorm = minNorm, maxNorm = maxNorm, step = as.numeric(self$options$stepping))                  
                table <- self$results$norms
                table$setRow(rowNo=1, values=list(Raw=tab$raw[[1]], Norm=tab$norm[[1]], Percentile=tab$percentile[[1]]))
                
                for(i in 2:nrow(tab))
                  table$addRow(rowKey=i, values=list(Raw=tab$raw[[i]], Norm=tab$norm[[i]], Percentile=tab$percentile[[i]]))
            
                 self$results$norms$setNote("empty", paste0("Norm score table for value '", normAge, "' of the grouping variable."), init=FALSE)
              }
            }
            
            image <- self$results$plot
            image$setState(data)
            
        },
    .plot=function(image, ...) {  # <-- the plot function

      if (is.null(self$options$raw)||is.null(self$options$group)||is.null(image$state))
            return()
      
      k <- 4
      if(self$options$k == 'Cubic')
        k <- 3
      else if(self$options$k == 'Quadratic')
        k <- 2              
        
        self$results$plot$setVisible(visible = TRUE)
        terms <- 4
        if(self$options$selectionType=="automaticSelection")
          model <- cNORM::bestModel(image$state)
        else{
          terms <- as.integer(self$options$terms)
          if(terms <= 0 || terms >= (k + 1)^2){
            return()
          }
          model <- cNORM::bestModel(image$state, terms = terms)
        }

        TRUE
    })
)
