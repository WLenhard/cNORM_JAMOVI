continuousClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "continuousClass",
    inherit = continuousBase,
    private = list(
        .run = function() {
            
            if (is.null(self$options$raw)||is.null(self$options$group)){
                self$results$instructions$setContent(
                    "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>cNORM estimates the functional relationship between raw score (raw), norm score (L) and the grouping variable (A, e. g. age, schooling duration ...) and 
            thus provides continuous norm scores. This improves the reliability of norm scores and reduces the necessary sizes of the norming samples, 
            because the method makes use of the complete dataset. The resulting model closes gaps within and between the norm tables and smoothes sampling
            errors.</p>
            <p>Select a model with a low number of terms while preserving a high R<sup>2</sup> of the model. Avoid intersecting 
            percentile curves in the location range of your interest. Please proceed as following:</p>
            <ol>
              <li>Select your <b>raw score</b> variable</li>
              <li>Place the <b>grouping variable</b> (e. g. grade) in the 'Grouping Variable' slot. Each group in the dataset should contain at minimum 50 cases, preferably 100.</li>
              <li>Adjust model parameters for retrieving a favorable model via visual inspection of the percentile plot.</li>
            </ol>
            <p>Further information on the procedure is available via <a href=\"https://www.psychometrica.de/cNorm_en.html\">Psychometrica</a>.</p>
            </div>
            </body>
            </html>")
            
              self$results$plot$setVisible(visible = FALSE)
              self$results$model$setVisible(visible = FALSE)
              self$results$norms$setVisible(visible = FALSE)
              return()
            }else{
              self$results$instructions$setVisible(visible = FALSE)
              self$results$model$setVisible(visible = TRUE)
              self$results$norms$setVisible(visible = TRUE)
        }                
                
                
      
            x <- jmvcore::toNumeric(self$data[[self$options$raw]])
            g <- jmvcore::toNumeric(self$data[[self$options$group]])
            data <- data.frame(raw = x, group=g)
            data <- data[complete.cases(data),]
            
            k <- 4
            if(self$options$k == 'Cubic')
                k <- 3
            else if(self$options$k == 'Quadratic')
                k <- 2
            
            scale <- self$options$scale
            
            if(scale=="Wechsler")
              scale <- c(10, 3)
            else if(scale=="PISA")
              scale <- c(500, 100)
            
            descend <- self$options$descend
            
            data <- cNORM::rankByGroup(data, raw=data$raw, group=data$group, scale = scale, descend = descend)
            data <- cNORM::computePowers(data, k=k)
            attr(data, "k") <- k
            
            terms <- 4
            if(!self$options$manually)
              model <- cNORM::bestModel(data, plot=FALSE, k = k)
            else{
                terms <- as.integer(self$options$terms)
                if(terms <= 0 || terms >= (k + 1)^2){
                  print("The number of terms is out of range. It has to be 8 or lower in quadratic, 15 or lower in cubic and 24 or lower in quartic polynomials.")
                  return()
                }
                model <- cNORM::bestModel(data, plot=FALSE, terms = terms, k = k)
            }

            if(is.null(model)){
              self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                          "<b>Ups! Something went wrong!</b>",
                                                          "</div></body></html>"))
              return()
            }
            
            if(self$options$model){
            self$results$model$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                        "<b>Model summary</b><br>", model$report[1], "<br>",
                                                        model$report[2], "<br>",
                                                        model$report[3], "<br>",
                                                        model$report[4], "<br>",
                                                        model$report[5], "<br>",
                                                        "</div></body></html>"))
            }
            normAge <- self$options$normAge
            if(!is.null(normAge)){
                self$results$norms$setVisible(visible = TRUE)
            tab <- cNORM::rawTable(as.numeric(normAge), model)
            
            table <- self$results$norms
            table$setRow(rowNo=1, values=list(Raw=tab$raw[[1]], Norm=tab$norm[[1]], Percentile=tab$percentile[[1]]))
            
            for(i in 2:nrow(tab))
                table$addRow(rowKey=i, values=list(Raw=tab$raw[[i]], Norm=tab$norm[[i]], Percentile=tab$percentile[[i]]))
            }
            
            image <- self$results$plot
            image$setState(data)
            
        },
    .plot=function(image, ...) {  # <-- the plot function
        if (is.null(self$options$raw)||is.null(self$options$group))
            return()

      k <- 4
      if(self$options$k == 'Cubic')
        k <- 3
      else if(self$options$k == 'Quadratic')
        k <- 2              
        
        self$results$plot$setVisible(visible = TRUE)
        terms <- 4
        if(!self$options$manually)
          model <- cNORM::bestModel(image$state)
        else{
          terms <- as.integer(self$options$terms)
          if(terms <= 0 || terms >= (k + 1)^2){
            print("The number of terms is out of range. It has to be 8 or lower in quadratic, 15 or lower in cubic and 24 or lower in quartic polynomials.")
            return()
          }
          model <- cNORM::bestModel(image$state, terms = terms)
        }

        TRUE
    })
)
