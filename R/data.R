# Data were converted from CSV files. E.g. to convert the insulin CSV to a usable dataset, run:
# insulin <- read.csv("data/insulin.csv")
# usethis::use_data(insulin, overwrite = T)


#' Insulin data
#'
#' Selected results of experiments performed by Banting \emph{et al}., (1922),
#' testing whether insulin reduces blood sugar. Insulin was administered in
#' rabbits and blood sugar was measured within three hours. Data collated from
#' Table 1 and Table 2 of Banting \emph{et al}., (1922)
#'
#' @format A data frame with 104 observations and 5 variables.
#'
#' \describe{
#'   \item{sugar}{Measured blood sugar level}
#'   \item{treatment}{Blood sugar measurements treatment group; \code{before} or \code{after}
#'   administering insulin}
#'   \item{id}{Identifier of individual being measured
#'   (not in original data set)}
#'   \item{experimenter_time}{Initial of researchers
#'   who performed the experiment and at what date}
#'   \item{time}{Time of blood sugar measurement; minutes after administration of insulin}
#' }
#'
#' @source Banting, F. G., Best, C. H., Collip, J. B., Macleod, J. J., & Noble,
#'   E. C. (1922). The effect of pancreatic extract (insulin) on normal rabbits.
#'   American Journal of Physiology-Legacy Content, 62(1), 162-176.
"insulin"

#' Petunia data
#'
#' Charles Darwin's experimental results on petunia plants to determine the difference of plant length between self fertilised and cross-fertilised, either with the same stock (inter-cross) or with a fresh stock (westerham-cross)
#'
#' @format A data frame with 64 observations and 3 variables. Plants (self fertilised, inter-cross and westerham-cross) were measured to the tops of their stems when coming into flower. Data collated from Darwin (1877)
#' \describe{
#'   \item{height}{Measured height of plants}
#'   \item{group}{Plant fertilisation groups; \code{westerham_cross}, \code{inter_cross} or \code{self_fertilised}}
#'   \item{pot_no}{Identifier of pot number in which each plant was grown}
#' }
#'
#' @source Darwin, C. (1877). The effects of cross and self fertilisation in the vegetable kingdom. John Murray, Albemarle Street, London.
"petunia"

#' Damselfly data
#'
#' Data of the body size and weight difference between adult and juvenile males of \emph{Xanthagrion erythroneurum} damselflies.
#'
#' @format A data frame with 77 observations and 3 variables.
#' \describe{
#'   \item{length}{Measured body length of damselflies}
#'   \item{weight}{Measured weight of damselflies}
#'   \item{group}{Male age groups; \code{adult} or \code{juvenile} used for body size and weight measurements}
#' }
#'
#' @source Khan, M. K., & Herberstein, M. E. (2021). Male-male interactions select for conspicuous male coloration in damselflies. Animal Behaviour, 176, 157-166.
"damselfly"


