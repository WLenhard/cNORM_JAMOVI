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
  packageStartupMessage(
    "Good morning star-shine!\ncNORM is free software. Please report bugs: https://github.com/WLenhard/cNORM/issues"
  )
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
#' \dontrun{
#' # conducts ranking and computation of powers and interactions with the 'elfe' dataset
#' data.elfe <- prepareData(elfe)
#'
#' # use vectors instead of data frame
#' data.elfe <- prepareData(raw=elfe$raw, group=elfe$group)
#'
#' # variable names can be specified as well, here with the BMI data included in the package
#' data.bmi <- prepareData(CDC, group = "group", raw = "bmi", age = "age")
#' }
#'
#' # modeling with only one group with the 'elfe' dataset as an example
#' # this results in conventional norming
#' data.elfe2 <- prepareData(data = elfe, group = FALSE)
#' m <- bestModel(data.elfe2)
#' @export
#' @family prepare
prepareData <- function(data = NULL,
                        group = "group",
                        raw = "raw",
                        age = "group",
                        k = 4,
                        t = NULL,
                        width = NA,
                        weights = NULL,
                        scale = "T",
                        descend = FALSE,
                        silent = FALSE) {

  # ---- Assemble working data.frame ------------------------------------------
  if (is.null(data)) {
    normData <- data.frame(raw = raw)
    raw <- "raw"
  } else {
    normData <- as.data.frame(data)
    if (is.numeric(raw) && length(raw) == nrow(normData)) {
      normData$raw <- raw
      raw <- "raw"
    }
  }
  n_rows <- nrow(normData)

  # ---- Grouping variable ----------------------------------------------------
  has_group <- TRUE
  if (is.logical(group)) {
    has_group <- FALSE                      # group = FALSE -> conventional norming
  } else if (is.numeric(group)) {
    if (length(group) != n_rows)
      stop("Length of the group vector (", length(group),
           ") does not match the number of cases (", n_rows, ").")
    normData$group <- group
    group <- "group"
  } else if (is.character(group)) {
    if (!(group %in% colnames(normData)))
      stop("Grouping variable '", group, "' does not exist in data object.")
  } else {
    stop("Invalid 'group' specification.")
  }

  # ---- Raw score variable ---------------------------------------------------
  if (is.numeric(raw)) {                    # only reachable if it did NOT match n_rows
    stop("Length of the raw score vector (", length(raw),
         ") does not match the number of cases (", n_rows, ").")
  } else if (is.character(raw)) {
    if (!(raw %in% colnames(normData)))
      stop("Raw score variable '", raw, "' does not exist in data object.")
  } else {
    stop("Invalid 'raw' specification.")
  }

  # ---- Age (continuous modelling) variable ----------------------------------
  has_age <- FALSE
  if (!has_group) {
    age   <- FALSE                          # conventional norming: no age modelling
    width <- NA
  } else if (is.numeric(age)) {
    if (length(age) != n_rows)
      stop("Length of the age vector (", length(age),
           ") does not match the number of cases (", n_rows, ").")
    normData$age <- age
    age <- "age"
    has_age <- TRUE
  } else if (is.character(age)) {
    if (age %in% colnames(normData)) {
      has_age <- TRUE
    } else {
      # age column absent -> use the grouping variable as the age proxy,
      # and disable the sliding window (no continuous age available)
      age   <- group
      width <- NA
      has_age <- TRUE
    }
  } else {
    stop("Invalid 'age' specification.")
  }

  # ---- Type checks (numeric requirements) -----------------------------------
  if (has_group && !is.numeric(normData[[group]]))
    warning("Grouping variable '", group, "' has to be numeric.")
  if (!is.numeric(normData[[raw]]))
    warning("Raw variable '", raw, "' has to be numeric.")
  if (has_age && !is.numeric(normData[[age]]))
    warning("Age variable '", age, "' has to be numeric.")

  # ---- Age / group range plausibility ---------------------------------------
  if (has_group && has_age && group != age &&
      is.numeric(normData[[group]]) && is.numeric(normData[[age]])) {
    rng_age <- range(normData[[age]],   na.rm = TRUE)
    rng_grp <- range(normData[[group]], na.rm = TRUE)
    if (rng_age[2] < rng_grp[1] || rng_age[1] > rng_grp[2]) {
      warning("The range of the age and group variable do not match. ",
              "Please specify a grouping variable whose values relate to the range of the age variable. ",
              "You can automatically generate a grouping variable by using the 'rankBySlidingWindow' ",
              "function and setting a desired number of groups with the 'nGroup' parameter.")
    }
  }



  # ---- Dispatch decision ----------------------------------------------------
  use_group_ranking <- is.null(width) || (length(width) == 1L && is.na(width))

  # ---- Remove cases with missing age in the GROUP-ranking path ---------------
  # rankByGroup() does not see 'age', yet computePowers() will model on it.
  # Keep a supplied weights *vector* aligned with the rows we drop.
  if (use_group_ranking && has_age &&
      (age %in% colnames(normData)) && anyNA(normData[[age]])) {
    age_ok <- !is.na(normData[[age]])
    if (!silent)
      message("Excluding ", sum(!age_ok), " case(s) with missing age before modelling.")
    if (!is.null(weights) && !is.character(weights) &&
        length(weights) == nrow(normData))
      weights <- weights[age_ok]
    normData <- normData[age_ok, , drop = FALSE]
  }

  # ---- Ranking --------------------------------------------------------------
  if (use_group_ranking) {
    normData <- rankByGroup(
      data    = normData,
      group   = group,
      raw     = raw,
      scale   = scale,
      descend = descend,
      weights = weights,
      silent  = silent
    )
  } else {
    normData <- rankBySlidingWindow(
      data    = normData,
      age     = age,
      raw     = raw,
      group   = group,        # passed for the 'group' attribute / plotting
      width   = width,
      weights = weights,
      scale   = scale,
      descend = descend,
      silent  = silent
    )
  }

  # ---- Powers & interactions ------------------------------------------------
  if (has_group) {
    normData <- computePowers(normData, k = k, t = t,
                              norm = "normValue", age = age, silent = silent)
  } else {
    normData <- computePowers(normData, k = k, t = t,
                              norm = "normValue", silent = silent)
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
#' \dontrun{
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
#' }
#'
#' @seealso rankBySlidingWindow, computePowers, computeWeights, weighted.rank
#' @export
#' @family prepare
rankByGroup <- function(data = NULL,
                        group = "group",
                        raw = "raw",
                        weights = NULL,
                        method = 4,
                        scale = "T",
                        descend = FALSE,
                        descriptives = TRUE,
                        na.rm = TRUE,
                        silent = FALSE) {

  # ---- assemble working data.frame ------------------------------------------
  if (is.null(data)) {
    d <- data.frame(raw = raw)
    if (is.numeric(group)) {
      if (length(group) != nrow(d))
        stop("Length of the group vector (", length(group),
             ") does not match the number of cases (", nrow(d), ").")
      d$group <- group
      group <- "group"
    } else {
      group <- FALSE
    }
    raw <- "raw"
  } else {
    d <- as.data.frame(data)

    # Grouping variable: existence checked BEFORE assignment
    if (is.character(group)) {
      if (!(group %in% colnames(d)))
        stop("Grouping variable '", group, "' does not exist in the data object.")
      d$group <- d[[group]]
      group <- "group"
    } else if (is.numeric(group)) {
      if (length(group) != nrow(d))
        stop("Length of the group vector (", length(group),
             ") does not match the number of cases (", nrow(d), ").")
      d$group <- group
      group <- "group"
    } # else: group is logical FALSE -> no grouping

    # Raw score variable
    if (is.character(raw)) {
      if (!(raw %in% colnames(d)))
        stop("Raw value variable '", raw, "' does not exist in the data object.")
      d$raw <- d[[raw]]
      raw <- "raw"
    } else if (is.numeric(raw)) {
      if (length(raw) != nrow(d))
        stop("Length of the raw score vector (", length(raw),
             ") does not match the number of cases (", nrow(d), ").")
      d$raw <- raw
      raw <- "raw"
    }
  }

  # ---- weights --------------------------------------------------------------
  weighting <- NULL
  has_weights <- FALSE
  if (!is.null(weights)) {
    if (is.character(weights)) {
      if (!(weights %in% colnames(d))) {
        if (!silent) warning("Weighting variable '", weights,
                             "' does not exist in dataset. Proceeding without weighting.")
        weights <- NULL
      } else {
        weighting <- as.numeric(d[[weights]])
        has_weights <- TRUE
      }
    } else {
      if (length(weights) != nrow(d)) {
        if (!silent) warning("Length of weights vector has to match the number of cases. Proceeding without weighting.")
        weights <- NULL
      } else {
        d$weights <- as.numeric(weights)
        weighting <- as.numeric(weights)
        weights <- "weights"
        has_weights <- TRUE
      }
    }
  }

  # ---- drop missing values (raw / group / weight) ---------------------------
  group_has_na  <- if (is.logical(group)) FALSE else anyNA(d[["group"]])
  raw_has_na    <- anyNA(d[["raw"]])
  weight_has_na <- has_weights && anyNA(weighting)

  if (group_has_na || raw_has_na || weight_has_na) {
    if (!silent) message("Missing values found in grouping, raw score or weight variable. Excluding affected cases.")
    valid_idx <- !is.na(d[["raw"]])
    if (!is.logical(group)) valid_idx <- valid_idx & !is.na(d[["group"]])
    if (has_weights)        valid_idx <- valid_idx & !is.na(weighting)
    d <- d[valid_idx, , drop = FALSE]
    if (has_weights) weighting <- weighting[valid_idx]
  }

  # ---- validate weight positivity -------------------------------------------
  if (has_weights && any(weighting <= 0)) {
    if (!silent) warning("Weights must be positive, non-zero values. Proceeding without weighting.")
    has_weights <- FALSE; weighting <- NULL; weights <- NULL
  }

  # ---- plotting-position constants ------------------------------------------
  numerator   <- c(-3/8, -1/3, 0, -1/2, -0.3, -0.3175, -0.326)
  denominator <- c( 1/4,  1/3, 1,    0,  0.4,  0.365,   0.348)

  if (method < 1 || method > length(numerator)) {
    if (!silent) message("Method parameter out of range, setting to RankIt (4).")
    method <- 4
  }
  num <- numerator[method]
  den <- denominator[method]
  sign_mult <- if (descend) -1 else 1

  # ---- ranking --------------------------------------------------------------
  if (is.logical(group) && !group) {
    n_cases <- nrow(d)

    x <- sign_mult * d[["raw"]]
    ranks <- if (has_weights) weighted.rank(x, weighting) else rank(x)
    d$percentile <- (ranks + num) / (n_cases + den)

    if (descriptives) {
      d$n  <- n_cases
      d$m  <- mean(d[["raw"]])
      d$md <- median(d[["raw"]])
      d$sd <- if (n_cases > 1) sd(d[["raw"]]) else NA_real_
    }

  } else {
    idx_split <- split(seq_len(nrow(d)), d[["group"]])
    ranks   <- numeric(nrow(d))
    n_group <- numeric(nrow(d))

    for (idx in idx_split) {
      x <- sign_mult * d[["raw"]][idx]
      ranks[idx] <- if (has_weights) weighted.rank(x, weighting[idx]) else rank(x)
      n_group[idx] <- length(idx)
    }
    d$percentile <- (ranks + num) / (n_group + den)

    if (descriptives) {
      raw_split <- split(d[["raw"]], d[["group"]])
      grp_m  <- vapply(raw_split, mean,   numeric(1))
      grp_md <- vapply(raw_split, median, numeric(1))
      grp_sd <- vapply(raw_split, function(z) if (length(z) > 1) sd(z) else NA_real_, numeric(1))
      match_idx <- match(as.character(d[["group"]]), names(raw_split))

      d$n  <- n_group
      d$m  <- grp_m[match_idx]
      d$md <- grp_md[match_idx]
      d$sd <- grp_sd[match_idx]
    }
  }

  # ---- norm scale -----------------------------------------------------------
  d$percentile <- clipPercentile(d$percentile)
  sc <- applyNormScale(d$percentile, scale)
  d$normValue <- sc$normValue

  attr(d, "group")     <- group
  attr(d, "age")       <- group
  attr(d, "raw")       <- raw
  attr(d, "scaleMean") <- sc$M
  attr(d, "scaleSD")   <- sc$SD
  attr(d, "descend")   <- descend
  attr(d, "normValue") <- "normValue"
  attr(d, "width")     <- NA
  attr(d, "weights")   <- weights

  if (na.rm) {
    naPerc <- sum(is.na(d$percentile))
    if (naPerc > 0) {
      if (!silent) message("Could not determine manifest percentile for ", naPerc, " cases. These will be dropped.")
      d <- d[!is.na(d$percentile), , drop = FALSE]
    }
  }

  if (descriptives && nrow(d) > 0 && min(d$n) < 30 && !silent) {
    warning("The dataset includes cases whose percentile depends on fewer than 30 cases. Consider redividing the cases over the grouping variable or reducing 'k'.")
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
#' data.elfe2 <- rankBySlidingWindow(elfe, raw = "raw", age = "group", width = 0.5)
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

  if (missing(width) || !is.numeric(width) || length(width) != 1 || is.na(width) || width <= 0)
    stop("Please provide a single positive numeric value for 'width'.")

  # ---- assemble working data.frame ------------------------------------------
  if (is.null(data)) {
    d <- data.frame(raw = raw, age = age)
    raw <- "raw"; age <- "age"
  } else {
    d <- as.data.frame(data)
    if (is.numeric(raw) && length(raw) == nrow(d)) { d$raw <- raw; raw <- "raw" }
    if (is.numeric(age) && length(age) == nrow(d)) { d$age <- age; age <- "age" }
  }

  if (!(age %in% colnames(d))) stop("Age variable '", age, "' does not exist in data object.")
  if (!(raw %in% colnames(d))) stop("Raw value variable '", raw, "' does not exist in data object.")

  # ---- weights --------------------------------------------------------------
  weighting <- NULL
  has_weights <- FALSE
  if (!is.null(weights)) {
    if (is.character(weights)) {
      if (!(weights %in% colnames(d))) {
        if (!silent) warning("Weighting variable '", weights, "' does not exist. Proceeding without weighting.")
        weights <- NULL
      } else {
        weighting <- as.numeric(d[[weights]]); has_weights <- TRUE
      }
    } else {
      if (length(weights) != nrow(d)) {
        if (!silent) warning("Length of weights vector does not match the number of cases. Proceeding without weighting.")
        weights <- NULL
      } else {
        d$weights <- as.numeric(weights)
        weighting <- as.numeric(weights)
        weights <- "weights"; has_weights <- TRUE
      }
    }
  }

  # ---- drop missing values --------------------------------------------------
  weight_has_na <- has_weights && anyNA(weighting)
  if (anyNA(d[[raw]]) || anyNA(d[[age]]) || weight_has_na) {
    if (!silent) message("Missing values found in raw score, age or weight variable. Excluding affected cases.")
    valid_idx <- !is.na(d[[raw]]) & !is.na(d[[age]])
    if (has_weights) valid_idx <- valid_idx & !is.na(weighting)
    d <- d[valid_idx, , drop = FALSE]
    if (has_weights) weighting <- weighting[valid_idx]
  }

  if (has_weights && any(weighting <= 0)) {
    if (!silent) warning("Weights must be positive, non-zero values. Proceeding without weighting.")
    has_weights <- FALSE; weighting <- NULL; weights <- NULL
  }

  # ---- plotting-position constants ------------------------------------------
  numerator   <- c(-3/8, -1/3, 0, -1/2, -0.3, -0.3175, -0.326)
  denominator <- c( 1/4,  1/3, 1,    0,  0.4,  0.365,   0.348)

  if (method < 1 || method > length(numerator)) {
    if (!silent) message("Method parameter out of range, setting to RankIt (4).")
    method <- 4
  }
  num <- numerator[method]
  den <- denominator[method]

  n <- nrow(d)
  if (n == 0) stop("No valid cases left after removing missing values.")

  # ---- sort by age, keep original order -------------------------------------
  d$orig_id <- seq_len(n)
  ord <- order(d[[age]])
  d <- d[ord, , drop = FALSE]

  a_vec <- d[[age]]
  r_vec <- d[[raw]]
  w_vec <- if (has_weights) d[["weights"]] else NULL

  # descend handled once, outside the loop
  r_vec_eval <- if (descend) -r_vec else r_vec

  MIN.AGE <- a_vec[1L]
  MAX.AGE <- a_vec[n]

  minAge <- a_vec - (width / 2)
  maxAge <- a_vec + (width / 2)

  idx_low  <- minAge < MIN.AGE
  idx_high <- (maxAge > MAX.AGE) & !idx_low

  if (any(idx_low)) {
    minAge[idx_low] <- MIN.AGE
    maxAge[idx_low] <- MIN.AGE + width
  }
  if (any(idx_high)) {
    minAge[idx_high] <- MAX.AGE - width
    maxAge[idx_high] <- MAX.AGE
  }

  left_idx  <- findInterval(minAge - 1e-12, a_vec) + 1L
  right_idx <- findInterval(maxAge + 1e-12, a_vec)

  # ---- group identical windows into contiguous runs -------------------------
  # left_idx/right_idx are monotone non-decreasing in i, so identical windows
  # are contiguous: a run-length id groups them without any overflow-prone key.
  if (n == 1L) {
    run_id <- 1L
  } else {
    changed <- (diff(left_idx) != 0L) | (diff(right_idx) != 0L)
    run_id  <- cumsum(c(TRUE, changed))
  }
  win_groups <- split(seq_len(n), run_id)

  percentile_vec <- numeric(n)
  if (descriptives) {
    n_vec  <- integer(n)
    m_vec  <- numeric(n)
    md_vec <- numeric(n)
    sd_vec <- numeric(n)
  }

  for (members in win_groups) {
    l <- left_idx[members[1L]]
    r <- right_idx[members[1L]]
    nObs <- r - l + 1L
    vals <- r_vec_eval[l:r]

    if (length(members) == 1L) {
      # single case -> cheap mid-rank, no sorting
      t_raw <- r_vec_eval[members]
      if (has_weights) {
        ww <- w_vec[l:r]
        W_less  <- sum(ww[vals <  t_raw])
        W_equal <- sum(ww[vals == t_raw])
        rank_val <- ((W_less + W_equal / 2) / sum(ww)) * nObs + 0.5
      } else {
        n_less  <- sum(vals <  t_raw)
        n_equal <- sum(vals == t_raw)
        rank_val <- n_less + (n_equal + 1) / 2
      }
      percentile_vec[members] <- (rank_val + num) / (nObs + den)
    } else {
      # several cases share this window -> rank the window once, then index
      if (has_weights) {
        ranks_window <- weighted.rank(vals, w_vec[l:r])
      } else {
        ranks_window <- rank(vals)                 # ties.method = "average"
      }
      pos <- members - l + 1L
      percentile_vec[members] <- (ranks_window[pos] + num) / (nObs + den)
    }

    if (descriptives) {
      orig_raw <- r_vec[l:r]                        # original (non-descended)
      n_vec[members]  <- nObs
      m_vec[members]  <- mean(orig_raw)
      md_vec[members] <- median(orig_raw)
      sd_vec[members] <- if (nObs > 1L) sd(orig_raw) else NA_real_
    }
  }

  d$percentile <- clipPercentile(percentile_vec)
  if (descriptives) {
    d$n  <- n_vec
    d$m  <- m_vec
    d$md <- md_vec
    d$sd <- sd_vec
  }

  # ---- restore original order -----------------------------------------------
  d <- d[order(d$orig_id), , drop = FALSE]
  d$orig_id <- NULL

  # ---- norm scale -----------------------------------------------------------
  sc <- applyNormScale(d$percentile, scale)
  d$normValue <- sc$normValue

  # ---- grouping variable for display ----------------------------------------
  group_name <- "group"
  if (nGroup > 0) {
    group_cut <- as.factor(as.numeric(cut(d[[age]], nGroup)))
    d$group   <- ave(d[[age]], group_cut, FUN = mean)
  } else if (is.character(group) && length(group) == 1 && group %in% colnames(d)) {
    group_name <- group
  }

  attr(d, "age")       <- age
  attr(d, "raw")       <- raw
  attr(d, "scaleMean") <- sc$M
  attr(d, "scaleSD")   <- sc$SD
  attr(d, "descend")   <- descend
  attr(d, "width")     <- width
  attr(d, "normValue") <- "normValue"
  attr(d, "group")     <- group_name
  attr(d, "weights")   <- weights

  if (na.rm) {
    naPerc <- sum(is.na(d$percentile))
    if (naPerc > 0) {
      if (!silent) message("Could not determine manifest percentile for ", naPerc, " cases. These will be dropped.")
      d <- d[!is.na(d$percentile), , drop = FALSE]
    }
  }

  if (descriptives && nrow(d) > 0 && min(d$n) < 30 && !silent) {
    warning("The dataset includes cases whose percentile depends on fewer than 30 cases. Increasing the width parameter might help.")
  }

  return(d)
}

#' Clip percentiles into the open interval (0, 1), NA-safe
#' @keywords internal
#' @noRd
clipPercentile <- function(p) {
  lo <- !is.na(p) & p <= 0
  hi <- !is.na(p) & p >= 1
  p[lo] <- 1e-12
  p[hi] <- 1 - 1e-12
  p
}

#' Map percentiles onto the requested norm scale
#' @return list(normValue, M, SD)
#' @keywords internal
#' @noRd
applyNormScale <- function(percentile, scale) {
  if (is.numeric(scale) && length(scale) == 2) {
    return(list(normValue = qnorm(percentile, scale[1], scale[2]),
                M = scale[1], SD = scale[2]))
  }
  if (is.character(scale) && length(scale) == 1) {
    if (scale == "T")          return(list(normValue = qnorm(percentile, 50, 10),  M = 50,  SD = 10))
    if (scale == "IQ")         return(list(normValue = qnorm(percentile, 100, 15), M = 100, SD = 15))
    if (scale == "z")          return(list(normValue = qnorm(percentile, 0, 1),    M = 0,   SD = 1))
    if (scale == "percentile") return(list(normValue = percentile,                 M = NA,  SD = NA))
  }
  warning("Unknown 'scale' specification; defaulting to T scores (M = 50, SD = 10).")
  list(normValue = qnorm(percentile, 50, 10), M = 50, SD = 10)
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
#' you do not need to use a normal rank transformed scale like T or IQ; you can
#' use the percentiles for the 'normValue' as well.
#'
#' The functions \code{rankBySlidingWindow}, \code{rankByGroup}, \code{bestModel},
#' \code{computePowers} and \code{prepareData} are usually not called directly, but accessed
#' through other functions like \code{cnorm}.
#'
#' @param data data.frame with the norm data
#' @param k degree of the location polynomial (1..6)
#' @param norm name of the norm variable in the data.frame (T scores, IQ, percentiles, ...).
#'   If `NULL`, the `"normValue"` attribute of `data` is used.
#' @param age explanatory variable (e.g. age or grade). May be a column name, a numeric
#'   vector of `nrow(data)`, `FALSE` to disable age handling, or `NULL` to fall back
#'   to the `"age"` attribute of `data`.
#' @param t age power parameter (1..6). If `NULL`, falls back to `k`.
#' @param silent set to TRUE to suppress messages
#' @return data.frame with the powers and interactions of location and explanatory
#'   variable / age
#' @seealso bestModel
#' @examples
#' \dontrun{
#' # Dataset with grade levels as grouping
#' data.elfe <- rankByGroup(elfe)
#' data.elfe <- computePowers(data.elfe)
#'
#' # Dataset with continuous age variable and k = 5
#' data.ppvt <- rankByGroup(ppvt)
#' data.ppvt <- computePowers(data.ppvt, age = "age", k = 5)
#' }
#' @export
#' @family prepare
computePowers <- function(data,
                          k = 5,
                          norm = NULL,
                          age = NULL,
                          t = 3,
                          silent = FALSE) {
  d <- as.data.frame(data)

  if (is.null(norm)) {
    norm <- attr(d, "normValue")
  }
  if (is.null(norm) || !(norm %in% colnames(d))) {
    stop("ERROR: Norm variable '",
         norm,
         "' does not exist in data object.")
  }
  if (!is.numeric(d[[norm]])) {
    warning("Norm score variable '", norm, "' has to be numeric.")
  }

  if (is.null(age)) {
    age <- attr(d, "age")
  }

  if (is.numeric(age) && length(age) == nrow(d)) {
    d$age <- age
    age   <- "age"
  }

  useAge <- TRUE
  if (is.null(age) ||
      (is.logical(age) && !isTRUE(age))) {
    useAge <- FALSE
    age    <- NULL
  }

  if (useAge) {
    if (!(age %in% colnames(d))) {
      stop("ERROR: Explanatory variable '",
           age,
           "' does not exist in data object.")
    }
    if (!is.numeric(d[[age]])) {
      warning("Age variable '", age, "' has to be numeric.")
    }
  }

  if (k < 1 || k > 6) {
    if (!silent)
      message("Parameter k out of range, setting to 5.")
    k <- 5
  }

  if (is.null(t)) {
    t <- k
  }
  if (t < 1 || t > 6) {
    if (!silent)
      message("Parameter t out of range, setting to 3.")
    t <- 3
  }

  L1 <- as.numeric(d[[norm]])

  # Powers of L
  for (i in seq_len(k)) {
    d[[paste0("L", i)]] <- L1^i
  }

  if (useAge) {
    A1 <- as.numeric(d[[age]])

    # Powers of A
    for (j in seq_len(t)) {
      d[[paste0("A", j)]] <- A1^j
    }

    # Interactions L_i * A_j
    for (i in seq_len(k)) {
      for (j in seq_len(t)) {
        d[[paste0("L", i, "A", j)]] <- L1^i * A1^j
      }
    }
  }

  attr(d, "age")        <- age
  attr(d, "normValue")  <- norm
  attr(d, "k")          <- k
  attr(d, "t")          <- t
  attr(d, "useAge")     <- useAge

  if (useAge && !silent) {
    cat("Powers of location: k = ", k, "\n", sep = "")
    cat("Powers of age:      t = ", t, "\n", sep = "")

    raw_name <- attr(d, "raw")
    if (!is.null(raw_name) && raw_name %in% colnames(d)) {
      A1 <- as.numeric(d[[age]])
      r2 <- summary.lm(lm(as.numeric(d[[raw_name]]) ~ poly(A1, t, raw = TRUE)))$r.squared

      if (r2 < 0.05 && t > 2) {
        warning(
          "Multiple R^2 between the explanatory variable and the raw score is low ",
          "(R^2 = ",
          round(r2, 4),
          "). The continuous norming model may be unstable. ",
          "Consider reducing t (e.g. to 1 for a linear age effect) and/or the number ",
          "of age groups."
        )
      } else {
        cat(
          "Multiple R^2 between raw score and explanatory variable: R^2 = ",
          round(r2, 4),
          "\n\n",
          sep = ""
        )
      }
    }
  }

  return(d)
}
