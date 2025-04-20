#' cNORM: Continuous Norming
#'
#' The package provides methods for generating regression based continuous standard
#' scores, as f. e. for psychometric test development, biometrics (e. g. physiological
#' growth curves), and screenings in the medical domain. It includes a distribution free approach
#' on the basis of Taylor polynomials and parametric modelling with beta binomial distributions.
#' Both approaches can generate robust norm data models and alleviate the computation of norm scores
#' and norm tables.
#'
#' Conventional methods for producing test norm score tables are often plagued with
#' "jumps" or "gaps" (i.e., discontinuities) in norm tables and low confidence for
#' assessing extreme scores.  The continuous norming method introduced by A. Lenhard et
#' al. (2016, <doi:10.1177/1073191116656437>; 2019, <doi:10.1371/journal.pone.0222279>)
#' addresses these problems and also has the added advantage of not requiring assumptions
#' about the distribution of the raw data: The norm scores are established from raw data
#' by modeling the latter ones as a function  of both percentile scores and an explanatory
#' variable (e.g., age). The method minimizes bias arising from sampling and measurement
#' error, while handling marked deviations from normality - such as are commonplace in
#' clinical samples. For pre-requisites and use cases of the beta binomial modelling, please
#' consult the vignette 'Beta Binomial'.
#'
#' Conducting the analysis consists of four steps and cNORM offers all according functions
#' for preparing data, conducting the  regression, selecting the best model and generating
#' norm tables (according functions in brackets):
#' \enumerate{
#'   \item Data preparation (\code{\link{rankByGroup}}, \code{\link{rankBySlidingWindow}},
#'   \code{\link{computePowers}})
#'   \item Establishing the regression model and selecting the parameters (\code{\link{bestModel}},
#'   \code{\link{printSubset}}, \code{\link{plotSubset}}, \code{\link{regressionFunction}},
#'   \code{\link{derive}})
#'   \item Validating the model (\code{\link{checkConsistency}}, \code{\link{plotPercentiles}},
#'   \code{\link{plotPercentileSeries}}, \code{\link{plotRaw}}, \code{\link{plotNorm}}, \code{\link{derivationTable}},
#'   \code{\link{plotDerivative}})
#'   \item Generating norm tables and predicting scores (\code{\link{predictNorm}},
#'   \code{\link{predictRaw}}, \code{\link{normTable}}, \code{\link{getNormCurve}},
#'   \code{\link{plotNormCurves}})
#' }
#'
#' The function \link{cnorm}
#' For an easy start, you can use the graphical user interface by typing \code{cNORM.GUI()} on the console.
#' Example datasets with large cohorts are available for demonstration purposes ('elfe',
#' 'ppvt', and 'CDC' sample data from the references). Use
#' \code{model <- cnorm(raw = elfe$raw, group = elfe$group)} to get a first impression.
#' Use  \code{vignette(cNORM-Demo)} for a walk through on
#' conducting  the modeling and \url{https://www.psychometrica.de/cNorm_en.html} for a
#' comprehensive tutorial.
#'
#' @references
#' \enumerate{
#'   \item CDC (2012). National Health and Nutrition Examination Survey: Questionnaires, Datasets
#'   and Related Documentation. available: https://wwwn.cdc.gov/nchs/nhanes/.
#'   date of retrieval: 25/08/2018
#'   \item Lenhard, W., & Lenhard, A. (2021). Improvement of Norm Score Quality via Regression-Based
#'   Continuous Norming. Educational and Psychological Measurement, 81(2), 229–261.
#'   doi: 10.1177/0013164420928457
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests:
#'   A simulation study of parametric and semi-parametric approaches.
#'   PLoS ONE, 14(9),  e0222279. doi: 10.1371/journal.pone.0222279
#'   \item Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to
#'   the norming problem. Assessment, Online first, 1-14. doi: 10.1177/1073191116656437
#'   \item Lenhard, A., Lenhard, W., Segerer, R. & Suggate, S. (2015). Peabody Picture Vocabulary
#'   Test - Revision IV (German Adaption). Frankfurt a. M.: Pearson Assessment.
#'   \item Lenhard, W. & Schneider, W. (2006). ELFE 1-6 - Ein Leseverstaendnistest fuer Erst- bis
#'   Sechstklässler. Goettingen: Hogrefe.
#' }
#' @author Wolfgang Lenhard, Alexandra Lenhard and Sebastian Gary
#' @keywords Psychometrics Biometrics Test Development Regression Based Norming
#' @docType _PACKAGE
#' @name cNORM
#' @seealso cNORM.GUI
#' @examples
#' \dontrun{
#' # Model internal 'elfe' dataset with the default k = 4 regression on T scores
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # Show model fit of models with progressing number of predictors
#' print(results)
#' plot(results, "subset")
#'
#' # Plot manifest and predicted values, plot series of percentile charts
#' plot(results, "raw")
#' plot(results, "series", start = 3, end = 9)
#'
#'
#' # Additional tests: Check model assumptions
#' checkConsistency(results)
#' plot(results, "derivative")
#'
#' # Generate norm tables; predict values, here: grade 3.75 from T score 25
#' # to 75 and within the raw value range of this specific test (0 to 28)
#' normTable <- normTable(3.75, results, minNorm=25, maxNorm=75, step=0.5)
#' rawTable <- rawTable(3.75, results, minRaw = 0, maxRaw = 28, minNorm=25,
#'                      maxNorm=75)
#'
#' # Predict a specific norm score
#' score <- predictNorm(raw = 21, A = 3.75,
#'                           model = results, minNorm=25, maxNorm=75)
#' }
NULL


#' Launcher for the graphical user interface of cNORM
#'
#' @param launch.browser Default TRUE; automatically open browser for GUI
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch graphical user interface
#' cNORM.GUI()
#' }
cNORM.GUI <- function(launch.browser=TRUE){
  packageList <- c("shiny", "shinycssloaders", "foreign", "readxl", "markdown")

  if (!requireNamespace(packageList, quietly = TRUE)) {
    cat("Additional packages are needed to start the user interface. Would you like to try to install them now?")
    installChoice <- menu(c("yes", "no"))
    if(installChoice == 1){
      utils::install.packages(packageList)
    } else {
      stop("Packages are missing. Unable to start the GUI")
    }
  }

  shiny::runApp(system.file('shiny', package='cNORM'),
                launch.browser=TRUE)
}

#' Continuous Norming
#'
#' Conducts continuous norming in one step and returns an object including ranked
#' raw data and the continuous norming model. Please consult the function
#' description ' of 'rankByGroup', 'rankBySlidingWindow' and 'bestModel' for specifics
#' of the steps in the data preparation and modeling process. In addition to the
#' raw scores, either provide
#' \itemize{
#'  \item{a numeric vector for the grouping information (group)}
#'  \item{a numeric age vector and the width of the sliding window (age, width)}
#' }
#' for the ranking of the raw scores. You can
#' adjust the grade of smoothing of the regression model by setting the k and terms
#' parameter. In general, increasing k to more than 4 and the number of terms lead
#' to a higher fit, while lower values lead to more smoothing. The power parameter
#' for the age trajectory can be specified independently by 't'. If both parameters
#' are missing, cnorm uses k = 5 and t = 3 by default.
#'
#' @param raw Numeric vector of raw scores
#' @param group Numeric vector of grouping variable, e. g. grade. If no group
#' or age variable is provided, conventional norming is applied
#' @param age Numeric vector with chronological age, please additionally specify
#' width of window
#' @param width Size of the sliding window in case an age vector is used
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as
#' well, be provided f. e. c(10, 3) for Wechsler scale index points
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param weights Vector or variable name in the dataset with weights for each
#' individual case. It can be used to compensate for moderate imbalances due to
#' insufficient norm data stratification. Weights should be numerical and positive.
#' @param terms Selection criterion for model building. The best fitting model with
#' this number of terms is used
#' @param R2 Adjusted R square as a stopping criterion for the model building
#' (default R2 = 0.99)
#' @param k The power constant. Higher values result in more detailed approximations
#' but have the danger of over-fit (max = 6). If not set, it uses t and if both
#' parameters are NULL, k is set to 5.
#' @param t The age power parameter (max = 6). If not set, it uses k and if both
#' parameters are NULL, k is set to 3, since age trajectories are most often well
#' captured by cubic polynomials.
#' @param plot Default TRUE; plots the regression model and prints report
#' @param extensive If TRUE, screen models for consistency and - if possible, exclude inconsistent ones
#' @param subsampling If TRUE (default), model coefficients are calculated using 10-folds and averaged across the folds.
#'                    This produces more robust estimates with a slight increase in bias.
#'
#' @return cnorm object including the ranked raw data and the regression model
#' @seealso rankByGroup, rankBySlidingWindow, computePowers, bestModel
#' @examples
#' \dontrun{
#' # Using this function with the example dataset 'elfe'
#'
#' # Conventional norming (no modelling over age)
#' cnorm(raw=elfe$raw)
#'
#' # Continuous norming
#' # You can use the 'getGroups()' function to set up grouping variable in case,
#' # you have a continuous age variable.
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # return norm tables including 90% confidence intervals for a
#' # test with a reliability of r = .85; table are set to mean of quartal
#' # in grade 3 (children completed 2 years of schooling)
#' normTable(c(2.125, 2.375, 2.625), cnorm.elfe, CI = .90, reliability = .95)
#'
#' # ... or instead of raw scores for norm scores, the other way round
#' rawTable(c(2.125, 2.375, 2.625), cnorm.elfe, CI = .90, reliability = .95)
#'
#'
#' # Using a continuous age variable instead of distinct groups, using a sliding
#' # window for percentile estimation. Please specify continuos variable for age
#' # and the sliding window size.
#' cnorm.ppvt.continuous <- cnorm(raw = ppvt$raw, age = ppvt$age, width=1)
#'
#'
#' # In case of unbalanced datasets, deviating from the census, the norm data
#' # can be weighted by the means of raking / post stratification. Please generate
#' # the weights with the computeWeights() function and pass them as the weights
#' # parameter. For computing the weights, please specify a data.frame with the
#' # population margins (further information is available in the computeWeights
#' # function). A demonstration based on sex and migration status in vocabulary
#' # development (ppvt dataset):
#' margins <- data.frame(variables = c("sex", "sex",
#'                                     "migration", "migration"),
#'                       levels = c(1, 2, 0, 1),
#'                       share = c(.52, .48, .7, .3))
#' weights <- computeWeights(ppvt, margins)
#' model <- cnorm(raw = ppvt$raw, group=ppvt$group, weights = weights)
#' }
#' @export
#' @references
#' \enumerate{
#'   \item Gary, S. & Lenhard, W. (2021). In norming we trust. Diagnostica.
#'   \item Gary, S., Lenhard, W. & Lenhard, A. (2021). Modelling Norm Scores with the cNORM Package in R. Psych, 3(3), 501-521. https://doi.org/10.3390/psych3030033
#'   \item Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to the norming problem. Assessment, Online first, 1-14. doi:10.1177/1073191116656437
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2018). Continuous Norming (cNORM). The Comprehensive R Network, Package cNORM, available: https://CRAN.R-project.org/package=cNORM
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. PLoS ONE, 14(9),  e0222279. doi:10.1371/journal.pone.0222279
#'   \item Lenhard, W., & Lenhard, A. (2020). Improvement of Norm Score Quality via Regression-Based Continuous Norming. Educational and Psychological Measurement(Online First), 1-33. https://doi.org/10.1177/0013164420928457
#'
#' }
cnorm <- function(raw = NULL,
                  group = NULL,
                  age = NULL,
                  width = NA,
                  weights = NULL,
                  scale = "T",
                  method = 4,
                  descend = FALSE,
                  k = NULL,
                  t = NULL,
                  terms = 0,
                  R2 = NULL,
                  plot = TRUE,
                  extensive = TRUE,
                  subsampling = TRUE){

  if(!is.null(group)&&!is.null(age)){
    warning("Specifying both 'group' as well as 'age' is discouraged.")
  }

  if(is.null(k)&&is.null(t)){
    k <- 5
    t <- 3
  }else if(!is.null(k)&&is.null(t)){
     t <- k
  }else if(is.null(k)&&!is.null(t)){
    k <- t
  }

  silent <- !plot

  if(is.numeric(raw)&&is.numeric(group)){
    if(length(raw)!=length(group)){
      stop("Please provide numeric vectors of equal length for raw score and group data.")
    }

    if(is.numeric(age)&&!is.na(width)){
      if(length(raw)!=length(age)){
        stop("Please provide numeric vectors of equal length for raw score and group data.")
      }

      if(is.null(weights))
        data <- data.frame(raw = raw, age = age)
      else
        data <- data.frame(raw = raw, age = age, weights = weights)

      # removing missing cases
      data <- data[complete.cases(data), ]

      if(plot)
        message("Ranking data with sliding window ...")

      data <- rankBySlidingWindow(raw=data$raw, age=data$age, scale=scale, weights=data$weights, descend = descend, width = width, method = method)

      # again remove missing cases; might occur due to weighting
      data <- data[complete.cases(data), ]
    }else{

      if(is.null(weights))
        data <- data.frame(raw = raw, group = group)
      else
        data <- data.frame(raw = raw, group = group, weights = weights)

      # removing missing cases
      data <- data[complete.cases(data), ]

      # model with rank by group
      data <- rankByGroup(raw=data$raw, group=data$group, scale=scale, weights=data$weights, descend = descend, method = method)

      # again remove missing cases; might occur due to weighting
      data <- data[complete.cases(data), ]

    }

    if(is.numeric(age)){
      if(length(raw)!=length(age)){
        warning("Length of the age vector does not match the raw score vector, ignoring age information.")
        data <- computePowers(data, k = k, t = t, silent = silent)
      }else{
        data$age <- age
        data <- computePowers(data, k = k, t = t, age = age, silent = silent)
      }
    }else{
      data <- computePowers(data, k = k, t = t, silent = silent)
    }
  }

  # conventional norming
  else if(is.numeric(raw)&&is.null(group)&&is.null(age)){
    if(is.null(weights))
      data <- data.frame(raw = raw)
    else
      data <- data.frame(raw = raw, weights = weights)

    data <- rankByGroup(data, raw=data$raw, group=FALSE, scale=scale, weights=data$weights, descend = descend, method = method)
    data <- computePowers(data, k = k, t = t, silent = silent)
    model <- bestModel(data, k = k, t = t, terms = terms, R2 = R2, plot = plot, extensive = extensive, subsampling = subsampling)

    result <- list(data = data, model = model)
    class(result) <- "cnorm"
    print(rawTable(0, result))
    return(result)
  }

  # if no grouping variable is given
  else if(is.numeric(raw)&&is.numeric(age)){
    if(length(raw)!=length(age)){
      stop("Please provide numeric vectors of equal length for raw score and group data.")
    }

    if(is.na(width)){
      if (length(age) / length(unique(age)) > 50 && min(table(data$age)) > 30) {
        message("Width for the sliding window is missing. Using age as grouping variable and resorting to rankByGroups.")
        if(is.null(weights))
          data <- data.frame(raw = raw, group = age)
        else
          data <- data.frame(raw = raw, group = age, weights = weights)

        # removing missing cases
        data <- data[complete.cases(data), ]
        data <- rankByGroup(raw=data$raw, group=data$group, scale=scale, weights=data$weights, descend = descend, method = method)
        data <- computePowers(data, k = k, t = t, silent = silent)

      }else{
        message("Width for the sliding window is missing. Building group variable and resorting to rankByGroups.")

        if(is.null(weights))
          data <- data.frame(raw = raw, group = getGroups(age))
        else
          data <- data.frame(raw = raw, group = getGroups(age), weights = weights)

        # removing missing cases
        data <- data[complete.cases(data), ]
        data <- rankByGroup(raw=data$raw, group=data$group, scale=scale, weights=data$weights, descend = descend, method = method)
        data <- computePowers(data, k = k, t = t, silent = silent)
      }
    }else{
      message("Ranking data with sliding window ...")
      if(is.null(weights))
        data <- data.frame(raw = raw, age = age)
      else
        data <- data.frame(raw = raw, age = age, weights = weights)

      # removing missing cases
      data <- data[complete.cases(data), ]
      data <- rankBySlidingWindow(raw=data$raw, age=data$age, scale=scale, weights=data$weights, descend = descend, width = width, method = method)
      data <- computePowers(data, k = k, t = t, silent = silent)
    }


  }else{
    stop("Please provide a numerical vector for the raw scores and either a vector for grouping and/or age of the same length. If you use an age vector only, please specify the width of the window.")
  }

  model <- bestModel(data, R2=R2, terms=terms, weights = data$weights, plot = plot, extensive = extensive, subsampling = subsampling)
  result <- list(data = data, model = model)
  class(result) <- "cnorm"
  return(result)
}

#' Swiftly compute Taylor regression models for distribution free continuous norming
#'
#' Conducts distribution free continuous norming and aims to find a fitting model. Raw data are modelled as a Taylor polynomial
#' of powers of age and location and their interactions. In addition to the
#' raw scores, either provide a numeric vector for the grouping information (group)
#' for the ranking of the raw scores. You can adjust the grade of smoothing of the regression model by setting the k, t and terms
#' parameter. In general, increasing k and t leads to a higher fit, while lower values lead to more smoothing. If both parameters
#' are missing, taylorSwift uses k = 5 and t = 3 by default.
#'
#' @param raw Numeric vector of raw scores
#' @param group Numeric vector of grouping variable, e. g. grade. If no group
#' or age variable is provided, conventional norming is applied
#' @param age Numeric vector with chronological age, please additionally specify
#' width of window
#' @param width Size of the sliding window in case an age vector is used
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as
#' well, be provided f. e. c(10, 3) for Wechsler scale index points
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param weights Vector or variable name in the dataset with weights for each
#' individual case. It can be used to compensate for moderate imbalances due to
#' insufficient norm data stratification. Weights should be numerical and positive.
#' @param terms Selection criterion for model building. The best fitting model with
#' this number of terms is used
#' @param R2 Adjusted R square as a stopping criterion for the model building
#' (default R2 = 0.99)
#' @param k The power constant. Higher values result in more detailed approximations
#' but have the danger of over-fit (max = 6). If not set, it uses t and if both
#' parameters are NULL, k is set to 5.
#' @param t The age power parameter (max = 6). If not set, it uses k and if both
#' parameters are NULL, k is set to 3, since age trajectories are most often well
#' captured by cubic polynomials.
#' @param plot Default TRUE; plots the regression model and prints report
#' @param extensive If TRUE, screen models for consistency and - if possible, exclude inconsistent ones
#' @param subsampling If TRUE (default), model coefficients are calculated using 10-folds and averaged across the folds.
#'                    This produces more robust estimates with a slight increase in bias.
#'
#' @return cnorm object including the ranked raw data and the regression model
#' @seealso rankByGroup, rankBySlidingWindow, computePowers, bestModel
#' @examples
#' \dontrun{
#' # Using this function with the example dataset 'ppvt'
#' # You can use the 'getGroups()' function to set up grouping variable in case,
#' # you have a continuous age variable.
#' model <- taylorSwift(raw = ppvt$raw, group = ppvt$group)
#'
#' # return norm tables including 90% confidence intervals for a
#' # test with a reliability of r = .85; table are set to mean of quartal
#' # in grade 3 (children completed 2 years of schooling)
#' normTable(c(5, 15), model, CI = .90, reliability = .95)
#'
#' # ... or instead of raw scores for norm scores, the other way round
#' rawTable(c(8, 12), model, CI = .90, reliability = .95)
#' }
#' @export
#' @references
#' \enumerate{
#'   \item Gary, S. & Lenhard, W. (2021). In norming we trust. Diagnostica.
#'   \item Gary, S., Lenhard, W. & Lenhard, A. (2021). Modelling Norm Scores with the cNORM Package in R. Psych, 3(3), 501-521. https://doi.org/10.3390/psych3030033
#'   \item Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to the norming problem. Assessment, Online first, 1-14. doi:10.1177/1073191116656437
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2018). Continuous Norming (cNORM). The Comprehensive R Network, Package cNORM, available: https://CRAN.R-project.org/package=cNORM
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. PLoS ONE, 14(9),  e0222279. doi:10.1371/journal.pone.0222279
#'   \item Lenhard, W., & Lenhard, A. (2020). Improvement of Norm Score Quality via Regression-Based Continuous Norming. Educational and Psychological Measurement(Online First), 1-33. https://doi.org/10.1177/0013164420928457
#'
#' }
taylorSwift <- cnorm
