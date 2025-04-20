#' Sentence completion test from ELFE 1-6
#'
#' A dataset containing the raw data of 1400 students from grade 2 to 5 in the sentence
#' comprehension test from ELFE 1-6 (Lenhard & Schneider, 2006). In this test, students
#' are presented lists of sentences with one gap. The student has to fill in the correct
#' solution by selecting from a list of 5 alternatives per sentence. The alternatives
#' include verbs, adjectives, nouns, pronouns and conjunctives. Each item stems from
#' the same word type. The text is speeded, with a time cutoff of 180 seconds. The
#' variables are as follows:
#'
#' @format A data frame with 1400 rows and 3 variables:
#' \describe{
#'   \item{personID}{ID of the student}
#'   \item{group}{grade level, with x.5 indicating the end of the school year and x.0 indicating the middle of the school year}
#'   \item{raw}{the raw score of the student, spanning values from 0 to 28}
#' }
#' @source \url{https://www.psychometrica.de/elfe2.html}
#' @references Lenhard, W. & Schneider, W.(2006). Ein Leseverstaendnistest fuer Erst- bis Sechstklaesser. Goettingen/Germany: Hogrefe.
#' @docType data
#' @keywords datasets
#' @concept reading comprehension
#' @name elfe
#' @examples
#' # prepare data, retrieve model and plot percentiles
#' model <- cnorm(elfe$group, elfe$raw)
#' @format A data frame with 1400 rows and 3 columns
"elfe"

#' Vocabulary development from 2.5 to 17
#'
#' A dataset based on an unstratified sample of PPVT4 data (German adaption). The PPVT4 consists of blocks of items with
#' 12 items each. Each item consists of 4 pictures. The test taker is given a word orally and he or she has to point out
#' the picture matching the oral word. Bottom and ceiling blocks of items are determined according to age and performance. For
#' instance, when a student knows less than 4 word from a block of 12 items, the testing stops. The sample is not identical
#' with the norm sample and includes doublets of cases in order to align the sample size per age group. It is
#' primarily intended for running the cNORM analyses with regard to modeling and stratification.
#'
#' @format A data frame with 4542 rows and 6 variables:
#' \describe{
#'   \item{age}{the chronological age of the child}
#'   \item{sex}{the sex of the test taker, 1=male, 2=female}
#'   \item{migration}{migration status of the family, 0=no, 1=yes}
#'   \item{region}{factor specifying the region, the data were collected; grouped into south, north, east and west}
#'   \item{raw}{the raw score of the student, spanning values from 0 to 228}
#'   \item{group}{age group of the child, determined by the getGroups()-function with 12 equidistant age groups}
#' }
#' @source \url{https://www.psychometrica.de/ppvt4.html}
#' @references Lenhard, A., Lenhard, W., Segerer, R. & Suggate, S. (2015). Peabody Picture Vocabulary Test - Revision IV (Deutsche Adaption). Frankfurt a. M./Germany: Pearson Assessment.
#' @docType data
#' @keywords datasets
#' @concept vocabulary acquisition development receptive
#' @name ppvt
#' @examples
#' \dontrun{
#' # Example with continuous age variable, ranked with sliding window
#' model.ppvt.sliding <- cnorm(age=ppvt$age, raw=ppvt$raw, width=1)
#'
#' # Example with age groups; you might first want to experiment with
#' # the granularity of the groups via the 'getGroups()' function
#' model.ppvt.group <- cnorm(group=ppvt$group, raw=ppvt$raw) # with predefined groups
#' model.ppvt.group <- cnorm(group=getGroups(ppvt$age, n=15, equidistant = T),
#'                           raw=ppvt$raw) # groups built 'on the fly'
#'
#'
#' # plot information function
#' plot(model.ppvt.group, "subset")
#'
#' # check model consistency
#' checkConsistency(model.ppvt.group)
#'
#' # plot percentiles
#' plot(model.ppvt.group, "percentiles")
#' }
#' @format A data frame with 5600 rows and 9 columns
"ppvt"

#' BMI growth curves from age 2 to 25
#'
#' By the courtesy of the Center of Disease Control (CDC), cNORM includes human growth data for children and adolescents
#' age 2 to 25 that can be used to model trajectories of the body mass index and to estimate percentiles for clinical
#' definitions of under- and overweight. The data stems from the NHANES surveys in the US and was published in 2012
#' as public domain. The data was cleaned by removing missing values and it includes the following variables from or
#' based on the original dataset.
#'
#' @format A data frame with 45053 rows and 7 variables:
#' \describe{
#'   \item{age}{continuous age in years, based on the month variable}
#'   \item{group}{age group; chronological age in years at the time of examination}
#'   \item{month}{chronological age in month at the time of examination}
#'   \item{sex}{sex of the participant, 1 = male, 2 = female}
#'   \item{height}{height of the participants in cm}
#'   \item{weight}{weight of the participants in kg}
#'   \item{bmi}{the body mass index, computed by (weight in kg)/(height in m)^2}
#' }
#' @docType data
#' @keywords datasets
#' @concept Body Mass Index growth curves weight height
#' @source \url{https://www.cdc.gov/nchs/nhanes/index.htm}
#' @references CDC (2012). National Health and Nutrition Examination Survey: Questionnaires, Datasets and Related
#' Documentation. available \url{https://www.cdc.gov/nchs/nhanes/index.htm} (date of retrieval: 25/08/2018)
#' @name CDC
#' @format A data frame with 45035 rows and 7 columns
"CDC"
