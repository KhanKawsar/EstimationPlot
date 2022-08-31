#' Insulin data
#'
#' Results of experiments testing whether insulin reduces blood sugar.
#'
#' @format A data frame with 104 observations and 5 variables. Insulin was administered in rabbits and blood sugar was measured within three hours. Data collated from Table 1 and Table 2 of Banting et al., (1922)
#' \describe{
#'   \item{sugar}{Measured blood sugar level}
#'   \item{treatment}{blood sugar measurements treatment group; \code{before} or \code{after} administering insulin}
#'   \item{id}{Identifier of indivdual being measured (not in original data set)}
#'   \item{experiment_time}{intial of researchers who performed the experiment and at what date}
#'   \item{time}{Time in minutes when blood sugar were measured administering insulin}
#' }
#'
#' @source Banting, F. G., Best, C. H., Collip, J. B., Macleod, J. J., & Noble, E. C. (1922). The effect of pancreatic extract (insulin) on normal rabbits. American Journal of Physiology-Legacy Content, 62(1), 162-176.
"insulin"

#' Petunia data
#'
#' Charles Darwins's experimental results on petunia plants to determine the difference of plant length between self fertilised and cross-fertilised, either with the same stock (inter-cross) or with a fresh stock (westerham-cross)
#'
#' @format A data frame with 64 observations and 3 variables. Plats (self fertilised, and inter-cross, and westerham-ccrross) were measured to the tops of their stems when coming into flower. Data collated from of Darwin (1877)
#' \describe{
#'   \item{height}{Measured height of plantsl}
#'   \item{group}{plant fertilisation groups; \code{weesterham_cross} or \code{inter_ccross} or \code{self_fertilised} administering insulin}
#'   \item{pot}{Identifier of pot number from where indivdual plants were grrown and height's were measured}
#' }
#'
#' @source Darwin, C. (1877). The effects of cross and self fertilisation in the vegetable kingdom. John Murray, Albemarle Street, London.
"petunia"

#' Fertilisation data TODO
"fertilisation"

#' Length data TODO
"length"

