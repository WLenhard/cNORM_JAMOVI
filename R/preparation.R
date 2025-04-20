.onLoad <- function(libname, pkgname) {
  op <- options()
  op.cNORM <-
    list(
      cNORM.install.args = "",
      cNORM.name = "Wolfgang Lenhard, Alexandra Lenhard & Sebastian Gary",
      cNORM.desc.author = "Wolfgang Lenhard <wolfgang.lenhard@uni-wuerzburg.de> [aut, cre];
      Alexandra Lenhard <lenhard@psychometrica.de> [aut];  Sebastian Gary [aut]",
      cNORM.desc.license = "AGPL-3",
      cNORM.desc = list()
    )
  toset <- !(names(op.cNORM) %in% names(op))
  if (any(toset)) {
    options(op.cNORM[toset])
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Good morning star-shine!\ncNORM is free software. Please report bugs: https://github.com/WLenhard/cNORM/issues")
}


#' Prepare data for modeling in one step (convenience method)
#'
#' This is a convenience method to either load the inbuilt sample dataset, or
#' to provide a data frame with the variables "raw" (for the raw scores) and "group"
#' The function ranks the data within groups, computes norm values, powers of the norm
#' scores and interactions. Afterwards, you can use these preprocessed data to
#' determine the best fitting model.
#'
#' The functions \code{rankBySlidingWindow}, \code{rankByGroup}, \code{bestModel},
#' \code{computePowers} and \code{prepareData} are usually not called directly, but accessed
#' through other functions like \code{cnorm}.
#'
#' @param data data.frame with a grouping variable named 'group' and a raw score variable
#' named 'raw'.
#' @param group grouping variable in the data, e. g. age groups, grades ...
#' Setting group = FALSE deactivates modeling in dependence of age. Use this in case you do want
#' conventional norm tables.
#' @param raw the raw scores
#' @param age the continuous explanatory variable; by default set to "group"
#' @param width if a width is provided, the function switches to rankBySlidingWindow to determine the
#' observed raw scores, otherwise, ranking is done by group (default)
#' @param weights Vector or variable name in the dataset with weights for each individual case. It can be used
#' to compensate for moderate imbalances due to insufficient norm data stratification. Weights should be numerical
#' and positive. Please use the 'computeWeights' function for this purpose.
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as well,
#' be provided f. e. c(10, 3) for Wechsler scale index point
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param k The power parameter, default = 4
#' @param t the age power parameter (default NULL). If not set, cNORM automatically uses k. The age power parameter
#' can be used to specify the k to produce rectangular matrices and specify the course of scores per  independently from k
#' @param silent set to TRUE to suppress messages
#' @return data frame including the norm scores, powers and interactions of the norm score and
#' grouping variable
#' @examples
#' # conducts ranking and computation of powers and interactions with the 'elfe' dataset
#' data.elfe <- prepareData(elfe)
#'
#' # use vectors instead of data frame
#' data.elfe <- prepareData(raw=elfe$raw, group=elfe$group)
#'
#' # variable names can be specified as well, here with the BMI data included in the package
#' \dontrun{
#' data.bmi <- prepareData(CDC, group = "group", raw = "bmi", age = "age")
#' }
#'
#' # modeling with only one group with the 'elfe' dataset as an example
#' # this results in conventional norming
#' data.elfe2 <- prepareData(data = elfe, group = FALSE)
#' m <- bestModel(data.elfe2)
#' @export
#' @family prepare
prepareData <- function(data = NULL, group = "group", raw = "raw", age = "group", k = 4, t = NULL, width = NA, weights = NULL, scale = "T", descend = FALSE, silent = FALSE) {
  if (is.null(data)) {
    normData <- data.frame(raw=raw)
    raw <- "raw"
  } else {
    normData <- as.data.frame(data)
    if (is.numeric(raw) && (length(raw) == nrow(normData))) {
      normData$raw <- raw
      raw <- "raw"
    }
  }

  # checks
  if (is.numeric(group) && (length(group) == nrow(normData))) {
    normData$group <- group
    group <- "group"
  }



  if (is.numeric(age) && (length(age) == nrow(normData))) {
    normData$age <- age
    age <- "age"
  }else if(is.character(age)){
    if(!(age %in% colnames(normData))){
      age <- group
      width <- NA
    }
  }

  if ((typeof(group) != "logical") && !(group %in% colnames(normData))) {
    stop(paste(c("ERROR: Grouping variable '", group, "' does not exist in data object."), collapse = ""))
  } else if (!(raw %in% colnames(normData))) {
    stop(paste(c("ERROR: Raw score variable '", raw, "' does not exist in data object."), collapse = ""))
  } else if (!(age %in% colnames(normData))) {
    stop(paste(c("ERROR: Age variable '", age, "' does not exist in data object."), collapse = ""))
  }

  if ((typeof(group) != "logical") && !is.numeric(normData[, group])) {
    warning(paste(c("Grouping variable '", group, "' has to be numeric."), collapse = ""))
  }

  if (!is.numeric(normData[, raw])) {
    warning(paste(c("Raw variable '", raw, "' has to be numeric."), collapse = ""))
  }

  if (!is.numeric(normData[, age])) {
    warning(paste(c("Age variable '", age, "' has to be numeric."), collapse = ""))
  }

  # exclude missings
  if (typeof(group) != "logical") {
    normData <- normData[!is.na(normData[, group]), ]
    normData <- normData[!is.na(normData[, age]), ]

    if(max(normData[, age])<min(normData[, group])||min(normData[, age])>max(normData[, group])){
      warning("The range of the age and group variable do not match. Please specify a grouping variable whose values relate to the range of the age variable. You can automatically generate a grouping variable by using the 'rankBySlidingWindow' function and setting a desired number of groups with the 'nGroup' parameter.")
      graphics::plot(normData[, age], normData[, group])
    }
  }
  normData <- normData[!is.na(normData[, raw]), ]

  # ranking and powers
  if (is.na(width)) {
    normData <- rankByGroup(data = normData, group = group, raw = raw, scale = scale, descend = descend, weights = weights)
  } else {
    normData <- rankBySlidingWindow(data = normData, group = group, raw = raw, width = width, weights = weights, scale = scale, descend = descend)
  }

  if (typeof(group) != "logical" || group) {
    normData <- computePowers(normData, k = k, t = t, norm = "normValue", age = age, silent = silent)
  } else {
    normData <- computePowers(normData, k = k, t = t, norm = "normValue", silent = silent)
  }

  return(normData)
}

#' Determine the norm scores of the participants in each subsample
#'
#' This is the initial step, usually done in all kinds of test norming projects,
#' after the scale is constructed and the norm sample is established. First,
#' the data is grouped according to a grouping variable and afterwards, the percentile
#' for each raw value is retrieved. The percentile can be used for the modeling
#' procedure, but in case, the samples to not deviate too much from normality,
#' T, IQ or z scores can be computed via a normal rank procedure based on the
#' inverse cumulative normal distribution. In case of bindings, we use the medium rank
#' and there are different methods for estimating the percentiles (default RankIt).
#'
#' @section Remarks on using covariates:
#' So far the inclusion of a binary covariate is experimental and far from optimized.
#' The according variable name has to be specified in the ranking procedure
#' and the modeling includes this in the further process. At the moment, during ranking
#' the data are split into the according cells group x covariate, which leads to small
#' sample sizes. Please take care to have enough cases in each combination. Additionally,
#' covariates can lead to unstable modeling solutions. The question, if it is really
#' reasonable to include covariates when norming a test is a decision beyond the pure data
#' modeling. Please use with care or alternatively split the dataset into the two groups
#' beforehand and model them separately.
#'
#' The functions \code{rankBySlidingWindow}, \code{rankByGroup}, \code{bestModel},
#' \code{computePowers} and \code{prepareData} are usually not called directly, but accessed
#' through other functions like \code{cnorm}.
#'
#' @param data data.frame with norm sample data. If no data.frame is provided, the raw score
#' and group vectors are directly used
#' @param group name of the grouping variable (default 'group') or numeric vector, e. g. grade, setting
#' group to FALSE cancels grouping (data is treated as one group)
#' @param raw name of the raw value variable (default 'raw') or numeric vector
#' @param weights Vector or variable name in the dataset with weights for each individual case. It can be used
#' to compensate for moderate imbalances due to insufficient norm data stratification. Weights should be numerical
#' and positive.  Please use the 'computeWeights' function for this purpose.
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as well,
#' be provided f. e. c(10, 3) for Wechsler scale index points
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param descriptives If set to TRUE (default), information in n, mean, median and
#' standard deviation per group is added to each observation
#' @param na.rm remove values, where the percentiles could not be estimated,
#' most likely happens in the context of weighting
#' @param silent set to TRUE to suppress messages
#' @return the dataset with the percentiles and norm scales per group
#'
#' @examples
#' # Transformation with default parameters: RankIt and converting to T scores
#' data.elfe <- rankByGroup(elfe, group = "group") # using a data frame with vector names
#' data.elfe2 <- rankByGroup(raw=elfe$raw, group=elfe$group) # use vectors for raw score and group
#'
#' # Transformation into Wechsler scores with Yu & Huang (2001) ranking procedure
#' data.elfe <- rankByGroup(raw = elfe$raw, group = elfe$group, method = 7, scale = c(10, 3))
#'
#' # cNORM can as well be used for conventional norming, in case no group is given
#' d <- rankByGroup(raw = elfe$raw)
#' d <- computePowers(d)
#' m <- bestModel(d)
#' rawTable(0, m) # please use an arbitrary value for age when generating the tables
#' @seealso rankBySlidingWindow, computePowers, computeWeights, weighted.rank
#' @export
#' @family prepare
rankByGroup <-
  function(data = NULL,
           group = "group",
           raw = "raw",
           weights = NULL,
           method = 4,
           scale = "T",
           descend = FALSE,
           descriptives = TRUE,
           na.rm = TRUE,
           silent = FALSE) {


    if(is.null(data)){
      d <- data.frame(raw=raw)
      if(is.numeric(group)){
        d$group <- group
        group <- "group"
      }else{
        group <- FALSE
      }

      raw <- "raw"
    }else{
      d <- as.data.frame(data)


      # check data types
      if (is.numeric(group) && (length(group) == nrow(d))) {
        d$group <- group
        group <- "group"
      }else if(is.character(group)){
        d$group <- d[, group]
        group <- "group"
      }

      if (is.numeric(raw) && (length(raw) == nrow(d))) {
        d$raw <- raw
        raw <- "raw"
      }else if(is.character(raw)){
        d$raw <- d[, raw]
        raw <- "raw"
      }
    }


    weighting <- NULL
    if(!is.null(weights)){
      if(is.character(weights)){

        if(!(weights %in% colnames(d))){
          if(!silent)
            warning(paste0("Weighting variable " , weights, " does not exist in dataset. Please provide the name of an existing column or a numeric vector. Proceeding without weighting."))

          weights <- NULL
        }else{
          weighting <- d[, weights]
        }
      }else{
        if(length(weights)!=nrow(d) &  !silent){
          warning("Length of vector with weights has to match the number of cases in the dataset. Proceeding without weighting.")

        }else{
          d$weights <- as.numeric(weights)
          weighting <- as.numeric(weights)
          weights <- "weights"
        }
      }

    }

    if (anyNA(d[, group]) || anyNA(d[, raw])) {
      if(!silent)
        cat("Missing values found in grouping or raw score variable... excluding from dataset\n")

      d <- d[!is.na(d[, group]), ]
      d <- d[!is.na(d[, raw]), ]
    }

    # check if columns exist
    if ((typeof(group) != "logical") & !(group %in% colnames(d)) & !silent) {
      stop(paste(c("ERROR: Grouping variable '", group, "' does not exist in data object."), collapse = ""))
    }

    if (!(raw %in% colnames(d)) & !silent) {
      stop(paste(c("ERROR: Raw value variable '", raw, "' does not exist in data object."), collapse = ""))
    }

    if ((typeof(group) != "logical") & !is.numeric(d[, group]) & !silent) {
      warning(paste(c("Grouping variable '", group, "' has to be numeric."), collapse = ""))
    }

    if (!is.numeric(d[, raw]) & !silent) {
      warning(paste(c("Raw variable '", raw, "' has to be numeric."), collapse = ""))
    }

    # define Q-Q-plot alorithm, use rankit as standard
    # 1 = Blom (1958), 2 = Tukey (1949), 3 = Van der Warden (1952), 4 = Rankit, 5 = Levenbach (1953),
    # 6 = Filliben (1975), 7 = Yu & Huang (2001)
    numerator <- c(-3.75, -1 / 3, 0, -0.5, -1 / 3, -0.3175, -0.326)
    denominator <- c(0.25, 1 / 3, 1, 0, 0.4, 0.365, 0.348)

    sign = 1
    if (descend)
      sign = -1

    if (method < 1 || method > length(numerator)) {
      message("Method parameter out of range, setting to RankIt")
    }

      if (typeof(group) == "logical" && !group) {
        cat("No grouping variable specified. Ranking without grouping ...")
        d$percentile <- (weighted.rank(sign * (d[, raw]), weights = weighting) + numerator[method]) / (length(d[, raw]) + denominator[method])
        if (descriptives) {
          d$n <- length(d[, raw])
          d$m <- mean(d[, raw])
          d$md <- median(d[, raw])
          d$sd <- sd(d[, raw])
        }
      } else {
        d <- d[order(d$group), ]
        d$percentile <- unlist(by(d, d$group, function(x) {
          (weighted.rank(sign * x$raw, weights = x$weights) + numerator[method]) / (nrow(x) + denominator[method])
        }))

        if (descriptives) {
          d$n <- ave(d[, raw], d[, group], FUN = function(x) {
            length(x)
          })
          d$m <- ave(d[, raw], d[, group], FUN = function(x) {
            mean(x)
          })
          d$md <- ave(d[, raw], d[, group], FUN = function(x) {
            median(x)
          })
          d$sd <- ave(d[, raw], d[, group], FUN = function(x) {
            sd(x)
          })
        }
      }


    scaleM <- NA
    scaleSD <- NA

    #check boundaries
    d$percentile[d$percentile<=0] <- 1e-12
    d$percentile[d$percentile>=1] <- 1 - 1e-12

    # descriptives
    if ((typeof(scale) == "double" && length(scale) == 2)) {
      d$normValue <- qnorm(d$percentile, scale[1], scale[2])
      scaleM <- scale[1]
      scaleSD <- scale[2]
    } else if (scale == "IQ") {
      d$normValue <- qnorm(d$percentile, 100, 15)
      scaleM <- 100
      scaleSD <- 15
    } else if (scale == "z") {
      d$normValue <- qnorm(d$percentile, 0, 1)
      scaleM <- 0
      scaleSD <- 1
    } else if (scale == "T") {
      scaleM <- 50
      scaleSD <- 10
      d$normValue <- qnorm(d$percentile, 50, 10)
    } else if (scale == "percentile") {
      d$normValue <- d$percentile
    }



    # add attributes to d
    attr(d, "group") <- group
    attr(d, "age") <- group
    attr(d, "raw") <- raw
    attr(d, "scaleMean") <- scaleM
    attr(d, "scaleSD") <- scaleSD
    attr(d, "descend") <- descend
    attr(d, "normValue") <- "normValue"
    attr(d, "width") <- NA
    attr(d, "weights") <- weights

    if(na.rm){
      naPerc <- sum(is.na(d$percentile))
      if(naPerc>0){
        if(!silent)
          message(paste0("Could not determine manifest percentile for ", naPerc, " cases in weighted ranking. These will be dropped."))

        d <- d[!is.na(d$percentile), ]
      }
    }

    if (descriptives & min(d$n) < 30 & !silent) {
      warning(paste0("The dataset includes cases, whose percentile depends on less than 30 cases (minimum is ", min(d$n), "). Please check the distribution of the cases over the grouping variable. The confidence of the norm scores is low in that part of the scale. Consider redividing the cases over the grouping variable. In cases of disorganized percentile curves after modeling, it might help to reduce the 'k' parameter."))
    }

    return(d)
  }



#' Determine the norm scores of the participants by sliding window
#'
#' The function retrieves all individuals in the predefined age range (x +/- width/2)
#' around each case and ranks that individual based on this individually drawn sample.
#' This function can be directly used with a continuous age variable in order to avoid
#' grouping. When collecting data on the basis of a continuous age variable, cases
#' located far from the mean age of the group receive distorted percentiles when building
#' discrete groups and generating percentiles with the traditional approach. The distortion
#' increases with distance from the group mean and this effect can be avoided by the
#' sliding window. Nonetheless, please ensure, that the optional grouping variable in fact
#' represents the correct mean age of the respective age groups, as this variable is
#' later on used for displaying the manifest data in the percentile plots.
#'
#' In case of bindings, the function uses the medium rank and applies the algorithms
#' already described in the \code{\link{rankByGroup}} function. At the upper and lower end of the
#' data sample, the sliding stops and the sample is drawn from the interval min + width and
#' max - width, respectively.
#'
#' @section Remarks on using covariates:
#' So far the inclusion of a binary covariate is experimental and far from optimized.
#' The according variable name has to be specified in the ranking procedure
#' and the modeling includes this in the further process. At the moment, during ranking
#' the data are split into the according degrees of the covariate and the ranking is done
#' separately. This may lead to small sample sizes. Please take care to have enough cases in each combination. Additionally,
#' covariates can lead to unstable modeling solutions. The question, if it is really
#' reasonable to include covariates when norming a test is a decision beyond the pure data
#' modeling. Please use with care or alternatively split the dataset into the two groups
#' beforehand and model them separately.
#'
#' The functions \code{rankBySlidingWindow}, \code{rankByGroup}, \code{bestModel},
#' \code{computePowers} and \code{prepareData} are usually not called directly, but accessed
#' through other functions like \code{cnorm}.
#'
#' @param data data.frame with norm sample data
#' @param age the continuous age variable. Setting 'age' to FALSE inhibits computation of
#' powers of age and the interactions
#' @param raw name of the raw value variable (default 'raw')
#' @param width the width of the sliding window
#' @param weights Vector or variable name in the dataset with weights for each individual case. It can be used
#' to compensate for moderate imbalances due to insufficient norm data stratification. Weights should be numerical
#' and positive. It can be resource intense when applied to the sliding window. Please use the 'computeWeights' function for this purpose.
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as well,
#' be provided f. e. c(10, 3) for Wechsler scale index points
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param descriptives If set to TRUE (default), information in n, mean, median and
#' standard deviation per group is added to each observation
#' @param nGroup If set to a positive value, a grouping variable is created with the desired number of
#' equi distant groups, named by the group mean age of each group. It creates the
#' column 'group' in the data.frame and in case, there is already one with that name,
#' overwrites it.
#' @param group Optional parameter for providing the name of the grouping variable (if present; overwritten
#' if ngroups is used)
#' @param na.rm remove values, where the percentiles could not be estimated,
#' most likely happens in the context of weighting
#' @param silent set to TRUE to suppress messages
#' @return the dataset with the individual percentiles and norm scores
#'
#' @examples
#' \dontrun{
#' # Transformation using a sliding window
#' data.elfe2 <- rankBySlidingWindow(relfe, raw = "raw", age = "group", width = 0.5)
#'
#' # Comparing this to the traditional approach should give us exactly the same
#' # values, since the sample dataset only has a grouping variable for age
#' data.elfe <- rankByGroup(elfe, group = "group")
#' mean(data.elfe$normValue - data.elfe2$normValue)
#' }
#' @seealso rankByGroup, computePowers, computeWeights, weighted.rank, weighted.quantile
#' @export
#' @family prepare
rankBySlidingWindow <- function(data = NULL,
                                age = "age",
                                raw = "raw",
                                weights = NULL,
                                width,
                                method = 4,
                                scale = "T",
                                descend = FALSE,
                                descriptives = TRUE,
                                nGroup = 0,
                                group = NA,
                                na.rm = TRUE,
                                silent = FALSE) {

  if(is.null(data)){
    d <- data.frame(raw=raw, age = age)
    raw <- "raw"
    age <- "age"
  }else{
    d <- as.data.frame(data)

    # check data types
    if (is.numeric(raw) && (length(raw) == nrow(d))) {
      d$raw <- raw
      raw <- "raw"
    }

    if (is.numeric(age) && (length(age) == nrow(d))) {
      d$age <- age
      age <- "age"
    }
  }


  weighting <- NULL
  if(!is.null(weights)){

    if(is.character(weights)){

      if(!(weights %in% colnames(d))){
        if(!silent)
          warning(paste0("Weighting variable " , weights, " does not exist in dataset. Please provide the name of an existing column or a numeric vector. Proceeding without weighting."))

        weights <- NULL
      }else{
        weighting <- d[, weights]
      }
    }else{
      if(length(weights)!=nrow(data)  & !silent){
        warning("Length of vector with weights has to match the number of cases in the dataset. Proceeding without weighting.")

      }else{
        d$weights <- as.numeric(weights)
        weighting <- as.numeric(weights)
        weights <- "weights"
      }
    }
  }

  if (anyNA(d[, raw]) || anyNA(d[, age])) {
    if(!silent)
      cat("Missing values found in raw score or age variable... excluding from dataset\n")

    d <- d[!is.na(d[, raw]), ]
    d <- d[!is.na(d[, age]), ]
  }

  # check if columns exist
  if (!(age %in% colnames(d))) {
    stop(paste(c("ERROR: Age variable '", age, "' does not exist in data object."), collapse = ""))
  }

  if (!(raw %in% colnames(d))) {
    stop(paste(c("ERROR: Raw value variable '", raw, "' does not exist in data object."), collapse = ""))
  }

  if (!is.numeric(d[, age]) & !silent) {
    warning(paste(c("Age variable '", age, "' has to be numeric."), collapse = ""))
  }

  if (!is.numeric(d[, raw]) & !silent) {
    warning(paste(c("Raw variable '", raw, "' has to be numeric."), collapse = ""))
  }

  # define Q-Q-plot algorithm, use rankit as standard
  # 1 = Blom (1958), 2 = Tukey (1949), 3 = Van der Warden (1952), 4 = Rankit, 5 = Levenbach (1953),
  # 6 = Filliben (1975), 7 = Yu & Huang (2001)
  numerator <- c(-3.75, -1 / 3, 0, -0.5, -1 / 3, -0.3175, -0.326)
  denominator <- c(0.25, 1 / 3, 1, 0, 0.4, 0.365, 0.348)

  # add columns to data.frame
  d$percentile <- NA
  if (descriptives) {
    d$n <- NA
    d$m <- NA
    d$md <- NA
    d$sd <- NA
  }

  # upper and lower bounds
  i <- 1
  n <- nrow(d)
  MIN.AGE <- min(d[, age])
  MAX.AGE <- max(d[, age])

  while (i <= n) {
    a <- d[i, age]
    r <- d[i, raw]


    minAge <- a - (width / 2)
    maxAge <- a + (width / 2)

    # limitation at the upper and lower end of the distribution
    if (minAge < MIN.AGE) {
      minAge <- MIN.AGE
      maxAge <- MIN.AGE + width
    } else if (maxAge > MAX.AGE) {
      minAge <- MAX.AGE - width
      maxAge <- MAX.AGE
    }


    observations <- d[which(d[, age] >= minAge & d[, age] <= maxAge), ]
    nObs <- nrow(observations)

    sign <- 1
    if (descend) {
      sign <- -1
    }

    if(is.null(weights))
      observations$percentile <- (rank(sign * observations[, raw]) + numerator[method]) / (nObs + denominator[method])
    else
      observations$percentile <- (weighted.rank(sign * observations[, raw], weights = observations[, weights]) + numerator[method]) / (nObs + denominator[method])

    # get percentile for raw value in sliding window subsample
    d$percentile[[i]] <- tail(observations$percentile[which(observations[, raw] == r)], n = 1)
    if (descriptives) {
      d$n[[i]] <- nObs
      d$m[[i]] <- mean(observations[, raw])
      d$md[[i]] <- median(observations[, raw])
      d$sd[[i]] <- sd(observations[, raw])
    }
    i <- i + 1
  }





  # norm scale definition
  scaleM <- NA
  scaleSD <- NA

  if ((typeof(scale) == "double" && length(scale) == 2)) {
    d$normValue <- qnorm(d$percentile, scale[1], scale[2])
    scaleM <- scale[1]
    scaleSD <- scale[2]
  } else if (scale == "IQ") {
    d$normValue <- qnorm(d$percentile, 100, 15)
    scaleM <- 100
    scaleSD <- 15
  } else if (scale == "z") {
    d$normValue <- qnorm(d$percentile, 0, 1)
    scaleM <- 0
    scaleSD <- 1
  } else if (scale == "T") {
    scaleM <- 50
    scaleSD <- 10
    d$normValue <- qnorm(d$percentile, 50, 10)
  } else if (scale == "percentile") {
    d$normValue <- d$percentile
  }

  # build grouping variable - unnecessary for norming,
  # but necessary for plotting the percentiles
  if (nGroup > 0) {
    group <- as.factor(as.numeric(cut(d[, age], nGroup)))
    d$group <- ave(d[, age], group, FUN = function(x) {
      mean(x)
    })
  }

  # add attributes to d
  attr(d, "age") <- age
  attr(d, "raw") <- raw
  attr(d, "scaleMean") <- scaleM
  attr(d, "scaleSD") <- scaleSD
  attr(d, "descend") <- descend
  attr(d, "width") <- width
  attr(d, "normValue") <- "normValue"
  attr(d, "group") <- "group"
  attr(d, "weights") <- weights



  if(na.rm){
    naPerc <- sum(is.na(d$percentile))
    if(naPerc>0){
      if(!silent)
        message(paste0("Could not determine manifest percentile for ", naPerc, " cases in weighted ranking. These will be dropped."))

      d <- d[!is.na(d$percentile), ]
    }
  }

  if (descriptives & min(d$n) < 30 & !silent) {
    warning(paste0("The dataset includes cases, whose percentile depends on less than 30 cases (minimum is ", min(d$n), "). Please check the distribution of the cases over the explanatory variable and have a look at the extreme upper and lower boundary. Increasing the width parameter might help."))
  }
  return(d)
}


#' Compute powers of the explanatory variable a as well as of the person
#' location l (data preparation)
#'
#' The function computes powers of the norm variable e. g. T scores (location, L),
#' an explanatory variable, e. g. age or grade of a data frame (age, A) and the
#' interactions of both (L X A). The k variable indicates the degree up to which
#' powers and interactions are build. These predictors can be used later on in the
#' \code{\link{bestModel}} function to model the norm sample. Higher values of k
#' allow for modeling the norm sample closer, but might lead to over-fit. In general
#' k = 3 or k = 4 (default) is sufficient to model human performance data. For example,
#' k = 2 results in the variables L1, L2, A1, A2, and their interactions L1A1, L2A1, L1A2
#' and L2A2 (but k = 2 is usually not sufficient for the modeling). Please note, that
#' you do not need to use a normal rank transformed scale like T r IQ, but you can
#' as well use the percentiles for the 'normValue' as well.
#'
#' The functions \code{rankBySlidingWindow}, \code{rankByGroup}, \code{bestModel},
#' \code{computePowers} and \code{prepareData} are usually not called directly, but accessed
#' through other functions like \code{cnorm}.
#'
#' @param data data.frame with the norm data
#' @param k degree
#' @param norm the variable containing the norm data in the data.frame; might be
#' T scores, IQ scores, percentiles ...
#' @param age Explanatory variable like age or grade, which was as well used for the grouping.
#' Can be either the grouping variable itself or a finer grained variable like the exact age. Other
#' explanatory variables can be used here instead an age variable as well, as long as the variable is
#' at least ordered metric, e. g. language or development levels ... The label 'age' is used, as this is the
#' most common field of application.
#' @param t the age power parameter (default NULL). If not set, cNORM automatically uses k. The age power parameter
#' can be used to specify the k to produce rectangular matrices and specify the course of scores per independently from k
#' @param silent set to TRUE to suppress messages
#' @return data.frame with the powers and interactions of location and explanatory variable / age
#' @seealso bestModel
#' @examples
#' # Dataset with grade levels as grouping
#' data.elfe <- rankByGroup(elfe)
#' data.elfe <- computePowers(data.elfe)
#'
#' # Dataset with continuous age variable and k = 5
#' data.ppvt <- rankByGroup(ppvt)
#' data.ppvt <- computePowers(data.ppvt, age = "age", k = 5)
#' @export
#' @family prepare
computePowers <-
  function(data,
           k = 5,
           norm = NULL,
           age = NULL,
           t = 3,
           silent = FALSE) {
    d <- as.data.frame(data)

    # check variables, if NULL take attributes from d
    if (is.null(norm)) {
      norm <- attr(d, "normValue")
    }

    useAge <- TRUE

    if (is.null(age)) {
      age <- attr(d, "age")
    }

    if ((typeof(age) == "logical") && !age) {
      useAge <- FALSE
    }

    # check if columns exist
    if (!(norm %in% colnames(d))) {
      stop(paste0("ERROR: Norm variable '", norm, "' does not exist in data object."))
    }

    if (!is.numeric(d[, norm])) {
      warning(paste0("Norm score variable '", norm, "' has to be numeric."))
    }

    if (is.numeric(age) && (length(age) == nrow(d))) {
      d$age <- age
      age <- "age"
    }

    if (useAge && !is.numeric(d[, age])) {
      warning(paste(c("Age variable '", age, "' has to be numeric."), collapse = ""))
    }

    if (useAge && !(age %in% colnames(d))) {
      stop(paste(c("ERROR: Explanatory variable '", age, "' does not exist in data object."), collapse = ""))
    }

    if ((k < 1) | (k > 6)) {
      message("Parameter k out of range, setting to 4")
      k <- 6
    }

    if(is.null(t)){
      t <- k
    }

    if ((t < 1) | (t > 6)) {
      message("Parameter t out of range, setting to k")
      t <- k
    }

    # generate powers and interactions of location and age up to parameters k and A
    L1 <- as.numeric(d[[norm]])
    if (useAge) {
      A1 <- as.numeric(d[[age]])

      for(j in 1:t){
        d[paste0("A", j)] <- A1^j
      }

      for(i in 1:k){
        d[paste0("L", i)] <- L1^i
      }

      for(i in 1:k){
        for(j in 1:t){
          d[paste0("L", i, "A", j)] <- L1^i*A1^j
        }
      }
    } else {
      for(i in 1:k){
        d[paste0("L", i)] <- L1^i
      }
    }

    # attributes
    attr(d, "age") <- age
    attr(d, "normValue") <- norm
    attr(d, "k") <- k
    attr(d, "t") <- t
    attr(d, "useAge") <- useAge

    # check, if it is worthwhile to continue with continuous norming
    if (useAge&&!silent) {
      cat(paste0("Powers of location: k = ", k))
      cat(paste0("\nPowers of age:      t = ", t))
      r2 <- summary.lm(lm(as.numeric(d[[attr(d, "raw")]]) ~ poly(A1, t, raw=TRUE)))$r.squared

      if (r2 < .05 && t>2) {
        warning(paste0("\nMultiple R2 between the explanatory variable and the raw score is low with R2 = ", r2, ". Thus, there is not much variance that can be captured by the continuous norming procedure. The models are probably unstable. You can try to reduce the powers of A indepentently from k and/or to reduce the number of age groups. To model a simple linear age effect, this means to reduce the number of groups to 2 and to set t to 1.\n\n"))
      }else{
        cat(paste0("\nMultiple R2 between raw score and explanatory variable: R2 = ", round(r2, 4), "\n\n"))
      }
    }

    return(d)
  }
