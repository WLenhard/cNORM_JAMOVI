
# This file is a generated template, your changes will not be overwritten

conventionalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "conventionalClass",
    inherit = conventionalBase,
    private = list(
        .init = function() {
            self$results$instructions$setContent(
                "<html>
                <head>
                </head>
                <body>
                <div class='instructions'>
                <p>The module estimates norm scores (= location, L) for raw scores within single groups and 
                compiles norm tables. It is based on the Rankit procedure and inverse normal transformation. In order to smooth 
                the norm score data and to close the missings, the functional relationship between raw score 
                and norm score is modeled via polynomial regression up to power 5, using the cNORM package 
                (W. Lenhard, Lenhard & Gary, 2018).</p>
                <p>Please specify the raw score variable, the ranking order, the method and the type of norm score scale.</p>
                <p>Further information on the procedure is available via <a href=\"https://www.psychometrica.de/cNorm_jamovi_en.html\" target=\"_blank\">Psychometrica</a>.</p>
                </div>
                </body>
                </html>")  
        },
        .run = function() {
            
            if (is.null(self$options$raw)){
                self$results$instructions$setContent(
                "<html>
                <head>
                </head>
                <body>
                <div class='instructions'>
                <p>The module estimates norm scores (= location, L) for raw scores within single groups and 
                compiles norm tables. It is based on the Rankit procedure and inverse normal transformation. In order to smooth 
                the norm score data and to close the missings, the functional relationship between raw score 
                and norm score is modeled via polynomial regression up to power 5, using the cNORM package 
                (W. Lenhard, Lenhard & Gary, 2018).</p>
                <p>Please specify the raw score variable, the ranking order, the method and the type of norm score scale.</p>
                <p>Further information on the procedure is available via <a href=\"https://www.psychometrica.de/cNorm_jamovi_en.html\" target=\"_blank\">Psychometrica</a>.</p>
                </div>
                </body>
                </html>")
                return()
            } else {
                self$results$instructions$setContent("<html/>")
                self$results$instructions$setVisible(visible = FALSE)
            }

            regression <- self$options$k

            terms <- 4
            if(self$options$terms == '1')
                terms <- 1
            else if(self$options$terms == '2')
                terms <- 2
            else if(self$options$terms == '3')
                terms <- 3
            else if(self$options$terms == '4')
                terms <- 4
            else if(self$options$terms == '5')
                terms <- 5
            
            x <- jmvcore::toNumeric(self$data[[self$options$raw]])
            x <- x[!is.na(x)]
            data <- data.frame(raw = x)
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
            
            data <- cNORM::rankByGroup(data, raw=data$raw, group=FALSE, scale = scale, descend = descend)
            
            tab1 <- data.frame(raw = data$raw, norm=data$normValue, percentile=data$percentile*100)
            tab1 <- unique(tab1)
            tab1 <- tab1[order(tab1$raw),]
            tab1$type <- rep(0)
            
            if(regression){
                minRaw <- self$options$minRaw
                maxRaw <- self$options$maxRaw
                
                if(maxRaw <= minRaw){
                    minRaw <- NULL
                    maxRaw <- NULL
                }
                
                if(!is.null(minRaw))
                    foot <- paste0(" The range of raw scores was manually set from ", minRaw, " to ", maxRaw, ".")
                else
                    foot <- paste0(" The range of raw scores was automatically retrieved from the dataset.")
                
                
                data <- cNORM::computePowers(data, k=5)
                model <- cNORM::bestModel(data, terms = terms)
                tab <- cNORM::rawTable(0, model, minNorm = minNorm, maxNorm = maxNorm, minRaw = minRaw, maxRaw = maxRaw, step = as.numeric(self$options$stepping))
                tab$type <- rep(1)
                
                if(is.null(model)){
                    self$results$instructions$setVisible(visible = TRUE)
                    self$results$instructions$setContent(paste0("<html><head></head><body><div class='instructions'>", 
                                                                "<b>Ups! Something went wrong!</b>",
                                                                "</div></body></html>"))
                    return()
                }else{
                    if(self$options$model){
                        if(self$options$model){
                            self$results$modelTab$setRow(rowNo=1, values=list(Variable="Model summary", Weight="", Terms=model$ideal.model, RMSE=model$rmse, R2adj=model$subset$adjr2[model$ideal.model], BIC=model$subset$bic[model$ideal.model]))
                            for(i in 1:length(model$coefficients)){
                                self$results$modelTab$addRow(rowKey=(i+1), values=list(Variable=names(model$coefficients)[i], Weight=model$coefficients[[i]], Terms="", RMSE="", R2adj="", BIC=""))
                                
                            }
                        }
                    }
                }
            }else{
                tab <- tab1
                foot <- "The norm score table is based on the manifest frequency distriution."
            }
            table <- self$results$norms
            table$setRow(rowNo=1, values=list(Raw=tab$raw[[1]], Norm=tab$norm[[1]], Percentile=tab$percentile[[1]]))
            
            for(i in 2:nrow(tab)){
                if(tab$norm[[i]]<tab$norm[[i-1]]){
                    tab$norm[[i]] <- tab$norm[[i-1]]
                    tab$percentile[[i]] <- tab$percentile[[i-1]]
                }
                table$addRow(rowKey=i, values=list(Raw=tab$raw[[i]], Norm=tab$norm[[i]], Percentile=tab$percentile[[i]]))
            }
            
            self$results$norms$setNote("empty", foot, init=FALSE)
            
            
            
            image <- self$results$plot
            if(regression){
                image$setState(rbind(tab1, tab))
            }else{
                image$setState(tab1)
            }
        },
        .plot=function(image, ...) {  # <-- the plot function
            if (is.null(self$options$raw))
                return()
            data <- image$state

            #self$results$instructions$setContent(paste0("data)
            #self$results$instructions$setVisible(visible = TRUE)
                
            regression <- self$options$k
            plotting <- TRUE
            if(self$options$plotting == 'Percentile')
                plotting <- FALSE
            
            if(plotting){
                plot(norm ~ raw, data = data[data$type==0, ], ylab = "Norm Score", xlab = "Raw Score")
            }else{
                plot(percentile ~ raw, data = data[data$type==0, ], ylab = "Percentile", xlab = "Raw Score")
            }
            
            if(regression){
                
                if(plotting){
                    lines(norm ~ raw, data = data[data$type==1, ], col = "blue")
                }else{
                    lines(percentile ~ raw, data = data[data$type==1, ], col = "blue")
                }
                
                leg <- "topleft"
                if(self$options$descend)
                    leg <- "topright"
                
                legend(leg, legend = c("Manifest", "Regression"), col = c("black", "blue"), pch = 19)
            }

            TRUE
    })
)
