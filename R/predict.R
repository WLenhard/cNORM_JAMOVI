#' Computes the curve for a specific T value
#'
#' As with this continuous norming regression approach, raw scores are modeled as a function of age and norm score
#' (location), getNormCurve is a straightforward approach to show the raw score development over
#' age, while keeping the norm value constant. This way, e. g. academic performance or intelligence development
#' of a specific ability is shown.
#' @param norm The specific norm score, e. g. T value
#' @param model The model from the regression modeling obtained with the cnorm function
#' @param minAge Age to start from
#' @param maxAge Age to stop at
#' @param step Stepping parameter for the precision when retrieving of the values, lower
#' values indicate higher precision (default 0.1).
#' @param minRaw lower bound of the range of raw scores (default = 0)
#' @param maxRaw upper bound of raw scores
#' @return data.frame of the variables raw, age and norm
#' @examples
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#' getNormCurve(35, cnorm.elfe)
#' @family predict
#' @export
getNormCurve <-
  function(norm,
             model,
             minAge = NULL,
             maxAge = NULL,
             step = 0.1,
             minRaw = NULL,
             maxRaw = NULL) {

    if(!inherits(model, "cnorm")&&!inherits(model, "cnormModel")){
      stop("Please provide a cnorm object.")
    }

    if(inherits(model, "cnorm")){
      model <- model$model
    }

    if (is.null(minAge)) {
      minAge <- model$minA1
    }

    if (is.null(maxAge)) {
      maxAge <- model$maxA1
    }

    if (is.null(minRaw)) {
      minRaw <- model$minRaw
    }

    if (is.null(maxRaw)) {
      maxRaw <- model$maxRaw
    }

    ages <- seq(minAge, maxAge, by = step)
    results <- lapply(ages, function(age) {
      r <- predictRaw(norm, age, model$coefficients, minRaw, maxRaw)
      data.frame(norm = paste(norm, "T"), age = age, raw = r)
    })
    curve <- do.call(rbind, results)

    return(curve)
  }

#' Predict raw values
#'
#' Most elementary function to predict raw score based on Location (L, T score),
#' Age (grouping variable) and the coefficients from a regression model.
#'
#' @param norm The norm score, e. g. a specific T score or a vector of scores
#' @param age The age value or a vector of scores
#' @param coefficients The a cnorm object or the coefficients from the regression model
#' @param minRaw Minimum score for the results; can be used for clipping unrealistic outcomes,
#' usually set to the lower bound of the range of values of the test (default: 0)
#' @param maxRaw Maximum score for the results; can be used for clipping unrealistic outcomes
#' usually set to the upper bound of the range of values of the test
#' @return the predicted raw score or a data.frame of scores in case, lists of norm scores or age is used
#' @examples
#' # Prediction of single scores
#' model <- cnorm(raw = elfe$raw, group = elfe$group)
#' predictRaw(35, 3.5, model)
#'
#'
#' @family predict
#' @export
predictRaw <-
  function(norm,
             age,
             coefficients,
             minRaw = -Inf,
             maxRaw = Inf) {

    if(inherits(coefficients, "cnorm")){
      coef <- coefficients$model$coefficients
    }else if(inherits(coefficients, "cnormModel")){
      coef <- coefficients$coefficients
    }else{
      coef <- coefficients
    }

    if(length(age) == 1 && length(norm) > 1){
      age <- rep(age, length(norm))
    }else if(length(norm) == 1 && length(age) > 1){
      norm <- rep(norm, length(age))
    }


    k <- max(as.numeric(gsub("L([0-9]+).*", "\\1", names(coef)[grep("^L[0-9]+", names(coef))])))
    t <- as.numeric(gsub(".*A([0-9]+)$", "\\1", names(coef)[grep("A[0-9]+$", names(coef))]))
    if(length(t) > 0)
      t = max(t)
    else
      t = 0

    # If k or t is -Inf (i.e., no L or A terms), set them to 0
    k <- if(is.finite(k)) k else 0
    t <- if(is.finite(t)) t else 0

    # Prepare the matrix for new data
    X_new <- prepare_matrix(norm, age, k, t)

    # Get all variable names from the model
    all_vars <- c("Intercept", colnames(X_new))

    # Identify which variables are in the model
    model_vars <- names(coef)
    model_vars <- gsub("^.*Intercept.*$", "Intercept", model_vars)

    # Create a matrix with all possible variables, filled with zeros
    X_new_full <- matrix(0, nrow = nrow(X_new), ncol = length(all_vars))
    colnames(X_new_full) <- all_vars

    # Fill in the values for the variables that are present in X_new
    X_new_full[, colnames(X_new)] <- X_new

    # Add the intercept column
    X_new_full[, "Intercept"] <- 1

    # Subset X_new_full to include only the variables that are in the model
    X_new_selected <- X_new_full[, model_vars]

    # Make predictions
    predictions <- as.vector(X_new_selected %*% coef)
    predictions[predictions < minRaw] <- minRaw
    predictions[predictions > maxRaw] <- maxRaw

    return(predictions)
  }

#' Create a norm table based on model for specific age
#'
#' This function generates a norm table for a specific age based on the regression
#' model by assigning raw scores to norm scores. Please specify the
#' range of norm scores, you want to cover. A T value of 25 corresponds to a percentile
#' of .6. As a consequence, specifying a range of T = 25 to T = 75 would cover 98.4 % of
#' the population. Please be careful when extrapolating vertically (at the lower and
#' upper end of the age specific distribution). Depending on the size of your standardization
#' sample, extreme values with T < 20 or T > 80 might lead to inconsistent results.
#' In case a confidence coefficient (CI, default .9) and the reliability is specified,
#' confidence intervals are computed for the true score estimates, including a correction for
#' regression to the mean (Eid & Schmidt, 2012, p. 272).
#'
#' @param A the age as single value or a vector of age values
#' @param model The regression model from the cnorm function
#' @param minNorm The lower bound of the norm score range
#' @param maxNorm The upper bound of the norm score range
#' @param minRaw clipping parameter for the lower bound of raw scores
#' @param maxRaw clipping parameter for the upper bound of raw scores
#' @param step Stepping parameter with lower values indicating higher precision
#' @param monotonuous corrects for decreasing norm scores in case of model inconsistencies (default)
#' @param CI confidence coefficient, ranging from 0 to 1, default .9
#' @param reliability coefficient, ranging between  0 to 1
#' @param pretty Format table by collapsing intervals and rounding to meaningful precision
#' @return either data.frame with norm scores, predicted raw scores and percentiles in case of simple A
#' value or a list #' of norm tables if vector of A values was provided
#' @seealso rawTable
#' @references Eid, M. & Schmidt, K. (2012). Testtheorie und Testkonstruktion. Hogrefe.
#' @examples
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # create single norm table
#' norms <- normTable(3.5, cnorm.elfe, minNorm = 25, maxNorm = 75, step = 0.5)
#'
#' # create list of norm tables
#' norms <- normTable(c(2.5, 3.5, 4.5), cnorm.elfe,
#'   minNorm = 25, maxNorm = 75,
#'   step = 1, minRaw = 0, maxRaw = 26
#' )
#'
#' # conventional norming, set age to arbitrary value
#' model <- cnorm(raw=elfe$raw)
#' normTable(0, model)
#'
#' @family predict
#' @export
normTable <- function(A,
                      model,
                      minNorm = NULL,
                      maxNorm = NULL,
                      minRaw = NULL,
                      maxRaw = NULL,
                      step = NULL,
                      monotonuous = TRUE,
                      CI = .9,
                      reliability = NULL,
                      pretty = T) {

  if(inherits(model, "cnormBetaBinomial")||inherits(model, "cnormBetaBinomial2")){
    return(normTable.betabinomial(model, A, CI = CI, reliability = reliability))
  }else if(inherits(model, "cnorm")){
    model <- model$model
  }else if(!inherits(model, "cnormModel")){
    stop("Please provide a cnorm object.")
  }

  if (model$useAge&&!is.numeric(A)){
    stop("Please specify age.")
  }

  if (is.null(model)){
    stop("No model specified")
  }

  if(is.null(CI)||is.na(CI)){
    reliability <- NULL
  }else if (CI > .99999 || CI < .00001){
    stop("Confidence coefficient (CI) out of range. Please specify value between 0 and 1.")
  }

  rel <- FALSE
  if(!is.null(reliability)){
    if(reliability > .9999 || reliability < .0001){
    stop("Reliability coefficient out of range. Please specify value between 0 and 1.")
  }else{
      se <- qnorm(1 - ((1 - CI)/2)) * sqrt(reliability * (1 - reliability));
      rel <- TRUE
  }
  }

  if (is.null(minNorm)||is.na(minNorm)){
    minNorm <- model$scaleM - 2.5 * model$scaleSD
  }

  if (is.null(maxNorm)||is.na(maxNorm)){
    maxNorm <- model$scaleM + 2.5 * model$scaleSD
  }

  # in case it still fails
  if (is.null(minNorm)||is.null(maxNorm)){
    stop("Please specify minNorm and maxNorm.")
  }

  if(is.null(step)||is.na(step)){
    step <- model$scaleSD / 10
  }

  descend <- model$descend


  if (is.null(minRaw)||is.na(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)||is.na(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  tables <- vector("list", length(A))

  for (x in 1:length(A)) {
    maxn <- maxNorm
    minn <- minNorm
    norm <- vector("list", (maxn - minn) / step + 1)
    raw <- vector("list", (maxn - minn) / step + 1)
    i <- 1
    l <- length(norm)

      while (i <= l) {
        r <- predictRaw(minn, A[[x]], model$coefficients, minRaw = minRaw, maxRaw = maxRaw)

        norm[[i]] <- minn
        raw[[i]] <- r

        minn <- minn + step
        i <- i + 1
      }


    normTable <-
      do.call(rbind, Map(data.frame, norm = norm, raw = raw))

    if (!is.na(model$scaleM) && !is.na(model$scaleSD)) {
      normTable$percentile <- pnorm((normTable$norm - model$scaleM) / model$scaleSD) * 100
    }

    if(monotonuous){
      if(!descend){
        minRawX <- normTable$raw[[1]]
      for(y in 1:length(normTable$raw)){
        if(normTable$raw[[y]] >= minRawX){
          minRawX <- normTable$raw[[y]]
        }else{
          normTable$raw[[y]] <- NA
        }
      }
      }else{
        y <- length(normTable$raw)
        maxRawX <- normTable$raw[[y]]
        while(y > 0){
          if(normTable$raw[[y]] >= maxRawX){
            maxRawX <- normTable$raw[[y]]
          }else{
            normTable$raw[[y]] <- NA
          }

          y <- y -1
        }
      }
    }

    if(rel){
      zPredicted <- reliability * (normTable$norm - model$scaleM)/model$scaleSD
      normTable$lowerCI <- (zPredicted - se) * model$scaleSD + model$scaleM
      normTable$upperCI <- (zPredicted + se) * model$scaleSD + model$scaleM
      normTable$lowerCI_PR <- pnorm(zPredicted - se) * 100
      normTable$upperCI_PR <- pnorm(zPredicted + se) * 100

    }
    if(pretty)
      tables[[x]] <- prettyPrint(normTable)
    else
      tables[[x]] <- normTable
  }

  names(tables) <- A

  if (length(A) == 1) {
    return(tables[[1]])
  } else {
    return(tables)
  }
}

#' Create a table with norm scores assigned to raw scores for a specific age based on the regression model
#'
#' This function is comparable to 'normTable', despite it reverses the assignment:
#' A table with raw scores and the according norm scores for a specific age based on the regression
#' model is generated. This way, the inverse function of the regression model is solved numerically with
#' brute force. Please specify the range of raw values, you want to cover. With higher precision
#' and smaller stepping, this function becomes computational intensive.
#' In case a confidence coefficient (CI, default .9) and the reliability is specified,
#' confidence intervals are computed for the true score estimates, including a correction for
#' regression to the mean (Eid & Schmidt, 2012, p. 272).
#' @param A the age, either single value or vector with age values
#' @param model The regression model or a cnorm object
#' @param minRaw The lower bound of the raw score range
#' @param maxRaw The upper bound of the raw score range
#' @param minNorm Clipping parameter for the lower bound of norm scores (default 25)
#' @param maxNorm Clipping parameter for the upper bound of norm scores (default 25)
#' @param step Stepping parameter for the raw scores (default 1)
#' @param monotonuous corrects for decreasing norm scores in case of model inconsistencies (default)
#' @param CI confidence coefficient, ranging from 0 to 1, default .9
#' @param reliability coefficient, ranging between  0 to 1
#' @param pretty Format table by collapsing intervals and rounding to meaningful precision
#' @return either data.frame with raw scores and the predicted norm scores in case of simple A value or a list
#' of norm tables if vector of A values was provided
#' @seealso normTable
#' @references Eid, M. & Schmidt, K. (2012). Testtheorie und Testkonstruktion. Hogrefe.
#' @examples
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#' # generate a norm table for the raw value range from 0 to 28 for the time point month 7 of grade 3
#' table <- rawTable(3 + 7 / 12, cnorm.elfe, minRaw = 0, maxRaw = 28)
#'
#' # generate several raw tables
#' table <- rawTable(c(2.5, 3.5, 4.5), cnorm.elfe, minRaw = 0, maxRaw = 28)
#'
#' # additionally compute confidence intervals
#' table <- rawTable(c(2.5, 3.5, 4.5), cnorm.elfe, minRaw = 0, maxRaw = 28, CI = .9, reliability = .94)
#'
#' # conventional norming, set age to arbitrary value
#' model <- cnorm(raw=elfe$raw)
#' rawTable(0, model)
#'
#' @family predict
#' @export
rawTable <- function(A,
                     model,
                     minRaw = NULL,
                     maxRaw = NULL,
                     minNorm = NULL,
                     maxNorm = NULL,
                     step = 1,
                     monotonuous = TRUE,
                     CI = .9,
                     reliability = NULL,
                     pretty = TRUE) {

  if(inherits(model, "cnorm")||inherits(model, "cnormTemp")){
    model <- model$model
  }else if(!inherits(model, "cnormModel")){
    stop("Please provide a cnorm object.")
  }

  if (model$useAge&&!is.numeric(A)){
    stop("Please specify age.")
  }

  if (is.null(step)||is.na(step)){
    step <- 1
  }

  if (is.null(model)){
    stop("No model specified")
  }

  if (is.null(minNorm)||is.na(minNorm)) {
    minNorm <- model$minL1
  }

  if (is.null(maxNorm)||is.na(maxNorm)) {
    maxNorm <- model$maxL1
  }

  if (is.null(minRaw)||is.na(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)||is.na(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  rel <- FALSE
  if(!is.null(reliability)){
    if(reliability > .9999 || reliability < .0001){
      stop("Reliability coefficient out of range. Please specify value between 0 and 1.")
    }else{
      se <- qnorm(1 - ((1 - CI)/2)) * sqrt(reliability * (1 - reliability));
      rel <- TRUE
    }
  }

  tables <- vector("list", length(A))

  for (x in 1:length(A)) {
    maxn <- maxNorm
    minn <- minNorm
    maxr <- maxRaw
    minr <- minRaw
    norm <- vector("list", (maxr - minr) / step)
    raw <- vector("list", (maxr - minr) / step)
    i <- 1
      while (minr <= maxr) {
        i <- i + 1
        n <-
          predictNorm(minr, A[[x]], model, minNorm, maxNorm)
        norm[[i]] <- n
        raw[[i]] <- minr

        minr <- minr + step
      }


    table <-
      do.call(rbind, Map(data.frame, raw = raw, norm = norm))

    if (!is.na(model$scaleM) && !is.na(model$scaleSD)) {
      table$percentile <- pnorm((table$norm - model$scaleM) / model$scaleSD) * 100
    }

    if(model$descend){
      table <- table[order(table$raw, decreasing = TRUE),]
    }
    # checking consistency
    k <- 1
    SUCCESS <- TRUE
    errorText <- ""

    if(monotonuous){
      minScore <- table$norm[[1]]
      minPR <- table$percentile[[1]]

      for(y in 1:length(table$norm)){
        if(table$norm[[y]] >= minScore){
          minScore <- table$norm[[y]]
          minPR <- table$percentile[[y]]
        }else{
          table$norm[[y]] <- minScore
          table$percentile[[y]] <- minPR

          SUCCESS <- FALSE
          errorText <- paste0(errorText, table$raw[y], ",")
        }
      }
    }

    if (!SUCCESS) {
      message(paste0("The raw table generation yielded indications of inconsistent raw score results: ", errorText, ". Please check the model consistency."))
    }

    if(rel){
      zPredicted <- reliability * (table$norm - model$scaleM)/model$scaleSD
      table$lowerCI <- (zPredicted - se) * model$scaleSD + model$scaleM
      table$upperCI <- (zPredicted + se) * model$scaleSD + model$scaleM
      table$lowerCI_PR <- pnorm(zPredicted - se) * 100
      table$upperCI_PR <- pnorm(zPredicted + se) * 100

    }

    if(pretty)
      tables[[x]] <- prettyPrint(table)
    else
      tables[[x]] <- table


  }

  names(tables) <- A

  if (length(A) == 1) {
    return(tables[[1]])
  } else {
    return(tables)
  }
}



#' Create a table based on first order derivative of the regression model for specific age
#'
#' In order to check model assumptions, a table of the first order derivative of the model
#' coefficients is created.
#' @param A the age
#' @param model The regression model or a cnorm object
#' @param minNorm The lower bound of the norm value range
#' @param maxNorm The upper bound of the norm value range
#' @param step Stepping parameter with lower values indicating higher precision
#' @return data.frame with norm scores and the predicted scores based on the
#' derived regression function
#' @seealso plotDerivative, derive
#' @examples
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # retrieve function for time point 6
#' d <- derivationTable(6, cnorm.elfe, step = 0.5)
#'
#' @family predict
#' @export
derivationTable <-
  function(A,
             model,
             minNorm = NULL,
             maxNorm = NULL,
             step = 0.1) {


    if(inherits(model, "cnorm")){
      model <- model$model
    }else if(!inherits(model, "cnormModel")){
      stop("Please provide a cnorm object.")
    }

    if (is.null(minNorm)) {
      minNorm <- model$minL1
    }

    if (is.null(maxNorm)) {
      maxNorm <- model$maxL1
    }

    norm <- vector("list", 1 + (maxNorm - minNorm) / step)
    raw <- vector("list", 1 + (maxNorm - minNorm) / step)
    i <- 1
    coeff <- derive(model)
    while (minNorm <= maxNorm) {
      i <- i + 1
      r <- predictRaw(minNorm, A, coeff)

      norm[[i]] <- minNorm
      raw[[i]] <- r

      minNorm <- minNorm + step
    }
    normTable <-
      do.call(rbind, Map(data.frame, norm = norm, raw = raw))
    return(normTable)
  }

#' Retrieve norm value for raw score at a specific age
#'
#' This functions numerically determines the norm score for raw scores depending on the
#' level of the explanatory variable A, e. g. norm scores for raw scores at given ages.
#' @param raw The raw value, either single numeric or numeric vector
#' @param A the explanatory variable (e. g. age), either single numeric or numeric vector
#' @param model The regression model or a cnorm object
#' @param minNorm The lower bound of the norm score range
#' @param maxNorm The upper bound of the norm score range
#' @param force Try to resolve missing norm scores in case of inconsistent models
#' @param silent set to TRUE to suppress messages
#' @return The predicted norm score for a raw score, either single value or vector
#' @examples
#' # Generate cnorm object from example data
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # return norm value for raw value 21 for grade 2, month 9
#' specificNormValue <- predictNorm(raw = 21, A = 2.75, cnorm.elfe)
#'
#' # predicted norm scores for the elfe dataset
#' # predictNorm(elfe$raw, elfe$group, cnorm.elfe)
#'
#' @family predict
#' @export
predictNorm <-
  function(raw,
             A,
             model,
             minNorm = NULL,
             maxNorm = NULL, force = FALSE,
             silent = FALSE) {

    if(!inherits(model, "cnormModel")&&!inherits(model, "cnorm")&&!inherits(model, "cnormLasso")){
      stop("Please provide a cnorm object.")
    }

    if(length(raw)==1&&is.na(raw)){
      return(rep(NA, times=length(A)))
    }else if(!is.numeric(raw)){
      stop("Please provide a single numeric value or a numeric vector for the raw score.")
    }

    if(length(A)==1&&is.na(A)){
      return(rep(NA, times=length(raw)))
    }else if(!is.numeric(A)){
      stop("Please provide a single numeric value or a numeric vector for A.")
    }

    if(length(A)>1&&length(raw)>1&&length(raw)!=length(A)){
      stop("A and raw need to have the same length.")
    }

    if(length(A)==0||length(raw)==0){
      return(NULL)
    }

    if(inherits(model, "cnorm")){
      if(is.null(minNorm)){
        minNorm <- attributes(model$data)$scaleMean - (attributes(model$data)$scaleSD * 2.5)
      }

      if(is.null(maxNorm)){
        maxNorm <- attributes(model$data)$scaleMean + (attributes(model$data)$scaleSD * 2.5)
      }

      model <- model$model
    } else if(inherits(model, "cnormModel")){
      if(is.null(minNorm)){
        minNorm <- attributes(model)$scaleMean - (attributes(model)$scaleSD * 2.5)
      }

      if(is.null(maxNorm)){
        maxNorm <- attributes(model)$scaleMean + (attributes(model)$scaleSD * 2.5)
      }

    } else{
      if (is.null(minNorm) || is.null(maxNorm)) {
        stop("ERROR: Please specify minimum and maximum norm score")
      }
    }



    # determine single norm value by optimization
    if (length(raw) == 1 && length(A) == 1) {
      coef <- model$coefficients
      startNormScore <- minNorm
      currentRawValue <- predictRaw(norm = minNorm, age = A, coefficients = coef)

      functionToMinimize <- function(norm) {
        currentRawValue <- predictRaw(norm = norm, age = A, coefficients = coef)
        functionValue <- (currentRawValue - raw)^2
      }

      optimum <- optimize(functionToMinimize, lower = minNorm, upper = maxNorm, tol = .Machine$double.eps)
      return(optimum$minimum)
    } else if (length(raw) > 1 || length(A)>1) {
      if(length(raw)==1){
        raw <- rep(raw, length(A))
      }else if(length(A)==1){
        A <- rep(A, length(raw))
      }


      # initialize vectors and starting values
      # of retrieved norm scores to specific cases; needed for later matching
      # create simple identifier based on combining string of raw and age
      hash <- paste0(raw, "_", A)

      # build norm table and use this as a lookup table
      # delete duplicates and NA
      normTable <- na.omit(data.frame(A = A, raw = raw, hash = hash))
      normTable <- normTable[!duplicated(normTable[,c('hash')]),]

      if(nrow(normTable)>500){
        if(!silent)
          cat("Retrieving norm scores, please stand by ...\n")
      }
      raw2 <- normTable$raw
      A2 <- normTable$A
      n <- length(raw2)
      values <- rep(NA, n)

      # iterate through cases
      for (i in 1:n) {
        v <- predictNormByRoots(raw2[[i]], A2[[i]], model, minNorm, maxNorm, force = force)
        if (length(v) == 0) {
          v <- NA
        }
        values[[i]] <- v
      }

      # project values on original data
      values <- values[match(hash, normTable$hash)]

      return(values)
    } else {
      stop("Please check raw and A value. Both have to be either single values or vectors of the same length.")
    }
  }

#' Format raw and norm tables
#' The function takes a raw or norm table, condenses intervals at the bottom and top
#' and round the numbers to meaningful interval.
#'
#' @param table The table to format
#'
#' @return formatted table
prettyPrint <- function(table){
  if(colnames(table)[1] == "raw")
    tab <- table[match(unique(table$norm), table$norm), ]
  else
    tab <- table[match(unique(table$raw), table$raw), ]

  row.table <- as.numeric(row.names(table))
  row.tab <- as.numeric(row.names(tab))
  if(nrow(tab)==1)
    return(tab)
  #rows <- row.names(tab)

  for(i in 1:nrow(tab)){
    x <- which(tab[i, 2] == table[, 2])
    if(length(x)>1){
      label <- paste0(table[x[1], 1], " - ", table[x[length(x)], 1])
      tab[i, 1] <- label
  }}


  #if(tab[2, 1] != table[2, 1])
  #  tab[1, 1] <- paste0(tab[1, 1], " - ", (table[row.tab[2] - 1, 1]))

  #if(tab[nrow(tab), 1] != table[nrow(table), 1])
  #  tab[nrow(tab), 1] <- paste0(tab[nrow(tab), 1], " - ", table[nrow(table), 1])


  # round to meaningful precision
  if(colnames(table)[1] == "raw")
    tab$norm <- round(tab$norm, digits = 2)
  else
    tab$raw <- round(tab$raw, digits = 2)

  tab$percentile <- round(tab$percentile, digits = 1)


  if(!is.null(tab$lowerCI_PR))
    tab$lowerCI_PR <- round(tab$lowerCI_PR, 1)

  if(!is.null(tab$upperCI_PR))
    tab$upperCI_PR <- round(tab$upperCI_PR, 1)

  if(!is.null(tab$lowerCI))
    tab$lowerCI <- round(tab$lowerCI, 2)

  if(!is.null(tab$upperCI))
    tab$upperCI <- round(tab$upperCI, 2)

  return(tab)
}


