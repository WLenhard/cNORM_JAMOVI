#' Weighting of cases through iterative proportional fitting (Raking)
#'
#' Computes and standardizes weights via raking to compensate for non-stratified
#' samples. It is based on the implementation in the survey R package. It reduces
#' data collection #' biases in the norm data by the means of post stratification,
#' thus reducing the effect of unbalanced data in percentile estimation and norm
#' data modeling.
#'
#' This function computes standardized raking weights to overcome biases in norm
#' samples. It generates weights, by drawing on the information of population
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
#' level of the variable and the according population share. Please ensure, the
#' original data does not include factor levels, not present in the
#' population.margins. Additionally, summing up the shares of the different
#' levels of a variable should result in a value near 1.0. The first column must
#' specify the name of the stratification variable, the second the level and
#' the third the proportion
#' @param standardized If TRUE (default), the raking weights are scaled to
#' weights/min(weights)
#' @return a vector with the standardized weights
#' @examples
#' # cNORM features a dataset on vocabulary development (ppvt)
#' # that includes variables like sex or migration. In order
#' # to weight the data, we have to specify the population shares.
#' # According to census, the population includes 52% boys
#' # (factor level 1 in the ppvt dataset) and 70% / 30% of persons
#' # without / with a a history of migration (= 0 / 1 in the dataset).
#' # First we set up the popolation margins with all shares of the
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
#' # weighted quantiles and as weights in the regession.
#'
#' model <- cnorm(raw = ppvt$raw,
#'                group=ppvt$group,
#'                weights = weights)
#' @export
computeWeights <- function(data, population.margins, standardized = TRUE ){
  data <- as.data.frame(data)
  names(population.margins) <- c("sv", "level", "proportion")
  sv <- unique(population.margins[,1])

  # check if all variables are present in the dataset
  missing.variables <- sv[!sv %in% names(data)]
  if(length(missing.variables)>0){
    stop(paste0("Weighting aborted due to missing variable(s): ", paste(missing.variables, collapse = ", "), " not present in the dataset."))
  }

  props <- as.data.frame(prop.table(xtabs(formula(paste0("~", paste(sv , collapse = " + "))), data=data)))
  props.adjusted <- props

  # Check if every every level of every sv in the marginals is contained
  # in the data set and vice versa.
  for(lev in 1:length(names)){
    l1 <- unique(props[sv[lev]])
    l2 <- unique(data[sv[lev]])

    if(!all(l1[,1] %in% l2[,1])||!all(l2[,1] %in% l1[,1])){
      stop("Levels of the data and the population marginals in variable ", sv[lev],
           " do not match.\n  Please make sure, every level of every variable is contained in the data set\n at least once and vice versa.")
    }
  }

  # Check, if sum of proportions lies between 0.95 and 1.05 for
  # every single stratification variable
  for(marg in 1:length(sv)){
    marg_sum <- abs(sum(population.margins[population.margins$sv==sv[marg],]$proportion) - 1)
    if(marg_sum>0.05)
    {
      warning(paste("Sum of proportions of variable", sv[marg],
                    "is not within [0.95;1.05].\nPlease make sure, the proportions for every single sratification variable sum up to almost 1.00"))
    }
  }

  weights <- rep(1, length.out=nrow(props))
  w <- rep(1, length.out=nrow(props))

  stop.loop <- FALSE
  for(repetitions in 1:100){
    for(i in 1:length(sv)){
      sv.sum <- aggregate(formula(paste0("Freq ~ ", sv[i])), data=props.adjusted, FUN=sum)
      tmp <- population.margins[which(population.margins$sv %in% sv[i]), ]

      for(j in 1:nrow(sv.sum)){
        indizes <- which(props[, sv[i]] %in% sv.sum[j, 1])
        weights[indizes] <- weights[indizes] * tmp$proportion[tmp$level==sv.sum[j, 1]] / sv.sum[j, 2]
        props.adjusted$Freq <- props$Freq * weights

        if(sum(abs(w-weights))<0.000001){
          cat(paste0("Raking converged normally after " , repetitions , " iterations.\n"))
          stop.loop <- TRUE
          break
        }

        w <- weights
      }
      if(stop.loop)
        break
    }
    if(stop.loop)
      break
  }

  checkWeights(weights)

  if(standardized)
    weights <- standardizeRakingWeights(weights)

  stratification.weights <- rep(1, length.out=nrow(data))
  if(length(sv)>1){
    x <- match(do.call("paste", data[, sv]), do.call("paste", props[, sv]))
    for(i in 1:length(weights)){
      stratification.weights[x == i] <- weights[i]
    }
  }
  else{
    for(i in 1:length(weights)){
      stratification.weights[data[, sv[1]] == i] <- weights[i]
    }
  }


  return(stratification.weights)
}

#' Check, if NA or values <= 0 occur and issue warning
#' @param weights Raking weights
checkWeights <- function(weights){
  # Get weights from the survey object
  if(min(weights<=0)){
    warning("Negative values or zeros occured during raking. Using the raking weights is not recommended.")
  }else if(sum(is.na(weights)>0)){
    warning("Undefined value occured during raking. Using the raking weights is not recommended.")
  }
}

#' Function for standardizing raking weights
#' Raking weights get divided by the smallest weight. Thereby, all weights
#' become larger or equal to 1 without changing the ratio of the weights
#' to each other.
#' @param weights Raking weights computed by computeRakingWeightsStandardized()
#' @param weights Raking weights computed by computeWeights()
#' @return the standardized weights
standardizeRakingWeights <- function(weights){
  weights_min <- min(weights)
  if(min(weights) <=0){
    warning("Smallest raking weight is zero or below. Weights can not be standardized.\nTherefore, non-standardized weights are used.")
  }
  weights <- weights/weights_min
  return(weights)
}
