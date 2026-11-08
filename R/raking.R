#' Weighting of cases through iterative proportional fitting (Raking)
#'
#' Computes and standardizes weights via raking to compensate for non-stratified
#' samples. It is based on the implementation in the survey R package. It reduces
#' data collection biases in the norm data by the means of post stratification,
#' thus reducing the effect of unbalanced data in percentile estimation and norm
#' data modeling.
#'
#' This function computes standardized raking weights to overcome biases in norm
#' samples. It generates weights by drawing on the information of population
#' shares (e. g. for sex, ethnic group, region ...) and subsequently reduces the
#' influence of over-represented groups or increases underrepresented cases. The
#' returned weights are either raw or standardized and scaled to be larger than 0.
#'
#' Raking in general has a number of advantages over post stratification and it
#' additionally allows cNORM to draw on larger datasets, since less cases have
#' to be removed during stratification. To use this function, additionally to the
#' data, a data frame with stratification variables has to be specified. The data
#' frame should include a row with (a) the variable name, (b) the level of the
#' variable and (c) the according population proportion.
#'
#' @param data data.frame with norm sample data.
#' @param population.margins A data.frame including three columns, specifying the
#' variable name in the original dataset used for data stratification, the factor
#' level of the variable and the according population share. Please ensure the
#' original data does not include factor levels not present in the
#' population.margins. Additionally, summing up the shares of the different
#' levels of a variable should result in a value near 1.0. The first column must
#' specify the name of the stratification variable, the second the level and
#' the third the proportion.
#' @param standardized If TRUE (default), the raking weights are scaled to
#' weights/min(weights)
#' @return a vector with the standardized weights
#' @examples
#' \dontrun{
#' # cNORM features a dataset on vocabulary development (ppvt)
#' # that includes variables like sex or migration. In order
#' # to weight the data, we have to specify the population shares.
#' # According to census, the population includes 52% boys
#' # (factor level 1 in the ppvt dataset) and 70% / 30% of persons
#' # without / with a history of migration (= 0 / 1 in the dataset).
#' # First we set up the population margins with all shares of the
#' # different levels:
#'
#' margins <- data.frame(variables = c("sex", "sex",
#'                                     "migration", "migration"),
#'                       levels = c(1, 2, 0, 1),
#'                       share = c(.52, .48, .7, .3))
#' head(margins)
#'
#' # Now we use the population margins to generate weights
#' # through raking
#'
#' weights <- computeWeights(ppvt, margins)
#'
#'
#' # There are as many different weights as combinations of
#' # factor levels, thus only four in this specific case
#'
#' unique(weights)
#'
#'
#' # To include the weights in the cNORM modelling, we have
#' # to pass them as weights. They are then used to set up
#' # weighted quantiles and as weights in the regression.
#'
#' model <- cnorm(raw = ppvt$raw,
#'                group = ppvt$group,
#'                weights = weights)
#' }
#' @export
computeWeights <- function(data, population.margins, standardized = TRUE) {
  data <- as.data.frame(data)
  names(population.margins) <- c("sv", "level", "proportion")
  sv <- unique(population.margins[, 1])

  # Check if all stratification variables are present in the dataset
  missing.variables <- sv[!sv %in% names(data)]
  if (length(missing.variables) > 0) {
    stop(
      paste0(
        "Weighting aborted due to missing variable(s): ",
        paste(missing.variables, collapse = ", "),
        " not present in the dataset."
      )
    )
  }

  # Joint distribution of the sample; drop empty cross-classification cells,
  # as they are inert in the IPF iteration, are never mapped onto a data row,
  # and would otherwise distort the standardization (min of weights).
  props <- as.data.frame(prop.table(xtabs(
    formula(paste0("~", paste(sv, collapse = " + "))), data = data
  )))
  props <- props[props$Freq > 0, , drop = FALSE]
  rownames(props) <- NULL
  props.adjusted <- props

  # Check that the levels of every stratification variable match between the
  # data and the population margins (in both directions).
  for (lev in seq_along(sv)) {
    data_levels   <- unique(as.character(data[[sv[lev]]]))
    margin_levels <- unique(as.character(
      population.margins$level[population.margins$sv == sv[lev]]))

    if (!setequal(data_levels, margin_levels)) {
      stop(
        "Levels of the data and the population margins in variable '",
        sv[lev],
        "' do not match.\n  In data but not in margins: ",
        paste(setdiff(data_levels, margin_levels), collapse = ", "),
        "\n  In margins but not in data: ",
        paste(setdiff(margin_levels, data_levels), collapse = ", "),
        "\n  Please make sure every level of every variable is contained in",
        " both the data set and the population margins."
      )
    }
  }

  # Check if the sum of proportions per variable lies between 0.95 and 1.05
  for (marg in seq_along(sv)) {
    marg_sum <- abs(sum(population.margins$proportion[population.margins$sv == sv[marg]]) - 1)
    if (marg_sum > 0.05) {
      warning(
        paste(
          "Sum of proportions of variable",
          sv[marg],
          "is not within [0.95;1.05].\nPlease make sure the proportions for every single stratification variable sum up to almost 1.00"
        )
      )
    }
  }

  # Iterative proportional fitting (raking) -------------------------------------
  weights <- rep(1, length.out = nrow(props))

  converged <- FALSE
  for (repetitions in 1:100) {
    weights_start <- weights   # snapshot before the full cycle over all variables

    for (i in seq_along(sv)) {
      sv.sum <- aggregate(formula(paste0("Freq ~ ", sv[i])),
                          data = props.adjusted, FUN = sum)
      tmp <- population.margins[population.margins$sv == sv[i], ]

      for (j in seq_len(nrow(sv.sum))) {
        current_level <- as.character(sv.sum[j, 1])

        # Find matching row in population margins
        match_idx <- which(as.character(tmp$level) == current_level)
        if (length(match_idx) == 0) {
          warning(
            paste(
              "No matching level found for",
              current_level,
              "in variable",
              sv[i],
              "- check your population.margins data"
            )
          )
          next
        }

        # Rows of the joint distribution belonging to this level
        indizes <- which(as.character(props[, sv[i]]) == current_level)
        if (length(indizes) == 0) {
          warning(
            paste(
              "No matching observations found for level",
              current_level,
              "in variable",
              sv[i]
            )
          )
          next
        }

        weights[indizes] <- weights[indizes] *
          tmp$proportion[match_idx] / sv.sum[j, 2]

        props.adjusted$Freq <- props$Freq * weights
      }
    }

    # Convergence is evaluated after a complete pass through ALL variables,
    # not after a single level adjustment.
    if (sum(abs(weights - weights_start)) < 1e-6) {
      converged <- TRUE
      message("Raking converged after ", repetitions, " iterations.")
      break
    }
  }

  if (!converged)
    warning("Raking did not converge within 100 iterations. ",
            "Using the raking weights is not recommended.")

  checkWeights(weights)

  if (standardized)
    weights <- standardizeRakingWeights(weights)

  # Map the cell weights back onto the individual observations -------------------
  stratification.weights <- rep(1, length.out = nrow(data))

  if (length(sv) > 1) {
    # Multiple stratification variables: match by the full level combination
    data_combined <- do.call("paste", c(lapply(sv, function(v)
      as.character(data[[v]])), sep = "|"))
    props_combined <- do.call("paste", c(lapply(sv, function(v)
      as.character(props[[v]])), sep = "|"))

    for (i in seq_along(weights)) {
      stratification.weights[data_combined == props_combined[i]] <- weights[i]
    }
  } else {
    # Single stratification variable
    sv_name <- sv[1]
    for (i in seq_len(nrow(props))) {
      level_val <- as.character(props[[sv_name]][i])
      stratification.weights[as.character(data[[sv_name]]) == level_val] <- weights[i]
    }
  }

  return(stratification.weights)
}


#' Check, if NA or values <= 0 occur and issue warning
#' @param weights Raking weights
#' @keywords internal
checkWeights <- function(weights) {
  if (anyNA(weights)) {
    warning("Undefined value occurred during raking. Using the raking weights is not recommended.")
  } else if (any(weights <= 0)) {
    warning("Negative values or zeros occurred during raking. Using the raking weights is not recommended.")
  }
}


#' Function for standardizing raking weights
#'
#' Raking weights get divided by the smallest weight. Thereby, all weights
#' become larger or equal to 1 without changing the ratio of the weights
#' to each other.
#' @param weights Raking weights computed by computeWeights()
#' @return the standardized weights
#' @keywords internal
standardizeRakingWeights <- function(weights) {
  weights_min <- min(weights)
  if (weights_min <= 0) {
    warning("Smallest raking weight is zero or below. Weights cannot be standardized.\nNon-standardized weights are returned.")
    return(weights)
  }
  weights / weights_min
}
