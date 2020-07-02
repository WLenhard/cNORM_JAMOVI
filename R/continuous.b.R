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
            percentile curves. Please proceed as following:</p>
            <ol>
              <li>Select your <b>raw score</b> variable</li>
              <li>Place the <b>grouping variable</b> (e. g. grade) in the 'Grouping Variable' slot. Each group in the dataset should contain at minimum 50 cases, preferably 100.</li>
              <li>Adjust model parameters for retrieving a favorable model via visual inspection of the percentile plot.</li>
            </ol>
            <p>Further information on the procedure is available via <a href=\"https://www.psychometrica.de/cNorm_en.html\">Psychometrica</a>. References:
            <ul>
                <li>Lenhard, W., Lenhard, A., & Gary, S. (2018). <i>cNORM: Continuous Norming (Version 1.2.2)</i>. Vienna: The Comprehensive R Network. available via <a href=\"https://cran.r-project.org/packages=cNORM\">https://cran.r-project.org/packages=cNORM</a></p></li>
                <li>Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2018). A continuous solution to the norming problem. <i>Assessment, 25</i>,  112-125. doi: 10.1177/1073191116656437</p></li>
                <li>Lenhard, W., & Lenhard, A. (2020). Improvement of Norm Score Quality via Regression-Based Continuous Norming. <i>Educational and Psychological Measurement (Online First)</i>, 1-33. https://doi.org/10.1177/0013164420928457</p></li>
            </ul>
            </div>
            </body>
            </html>")
            
              self$results$plot$setVisible(visible = FALSE)
              return()
            }else{
              self$results$instructions$setContent("<html/>")
              self$results$instructions$setVisible(visible = FALSE)
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
            
            terms <- 4
            if(self$options$terms == 'Automatic')
                terms <- NA
            else if(self$options$terms == '1')
                terms <- 1
            else if(self$options$terms == '2')
                terms <- 2
            else if(self$options$terms == '3')
                terms <- 3
            else if(self$options$terms == '4')
                terms <- 4
            else if(self$options$terms == '5')
                terms <- 5
            else if(self$options$terms == '6')
                terms <- 6
            else if(self$options$terms == '7')
                terms <- 7
            else if(self$options$terms == '8')
                terms <- 8
            else if(self$options$terms == '9')
                terms <- 9
            else if(self$options$terms == '10')
                terms <- 10
            else if(self$options$terms == '11')
                terms <- 11
            else if(self$options$terms == '12')
                terms <- 12
            else if(self$options$terms == '13')
                terms <- 13
            else if(self$options$terms == '14')
                terms <- 14
            else if(self$options$terms == '15')
                terms <- 15
            else if(self$options$terms == '16')
                terms <- 16
            
            if(k==2&&terms>8){
                print("When choosing quadratic functions, the number of terms may not exceed 8.")
                return()
            }
                
            
            scale <- self$options$scale
            if(scale=="Wechsler")
                scale <- c(10, 3)
            else if(scale=="PISA")
                scale <- c(500, 100)
            
            descend <- self$options$descend
            
            data <- cNORM::rankByGroup(data, raw=data$raw, group=data$group, scale = scale, descend = descend)
            data <- cNORM::computePowers(data, k=k)
            attr(data, "k") <- k
            
            if(is.na(terms))
                model <- cNORM::bestModel(data, plot=FALSE, k = k)
            else
                model <- cNORM::bestModel(data, plot=FALSE, terms = terms, k = k)
            
            if(is.null(model)){
              self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                          "<b>Ups! Something went wrong!</b>",
                                                          "</div></body></html>"))
              return()
            }
            
            if(self$options$model){
            self$results$model$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                        "<b>Model summary:</b><br>", model$report[1], "<br>",
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
        
        
        self$results$plot$setVisible(visible = TRUE)
        
        terms <- 4
        if(self$options$terms == 'Automatic')
            terms <- NA
        else if(self$options$terms == '1')
            terms <- 1
        else if(self$options$terms == '2')
            terms <- 2
        else if(self$options$terms == '3')
            terms <- 3
        else if(self$options$terms == '4')
            terms <- 4
        else if(self$options$terms == '5')
            terms <- 5
        else if(self$options$terms == '6')
            terms <- 6
        else if(self$options$terms == '7')
            terms <- 7
        else if(self$options$terms == '8')
            terms <- 8
        else if(self$options$terms == '9')
            terms <- 9
        else if(self$options$terms == '10')
            terms <- 10
        else if(self$options$terms == '11')
            terms <- 11
        else if(self$options$terms == '12')
            terms <- 12
        else if(self$options$terms == '13')
            terms <- 13
        else if(self$options$terms == '14')
            terms <- 14
        else if(self$options$terms == '15')
            terms <- 15
        else if(self$options$terms == '16')
            terms <- 16
        
        if(self$options$k == 'Quadratic'&&terms>8){
            print("When choosing quadratic functions, the number of terms may not exceed 8.")
            return()
        }
        
        if(is.na(terms))
            model <- cNORM::bestModel(image$state)
        else
            model <- cNORM::bestModel(image$state, terms = terms)

        TRUE
    })
)
