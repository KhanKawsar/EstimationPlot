# Data were converted from CSV files. E.g. to convert the insulin CSV to a usable dataset, run:
# insulin <- read.csv("data/insulin.csv")
# usethis::use_data(insulin, overwrite = T)
#
# insulin.wide was created by reshaping insulin as follows:
# insulin.wide <- reshape(insulin, direction = "wide", idvar = "id", timevar = "treatment", v.names = c("sugar", "time"))
# insulin.wide$time <- insulin.wide$time.after
# insulin.wide$experimenter <- sub(" .*", "", insulin.wide$experimenter_time)
# insulin.wide$date <- sub("[^ ]* ", "", insulin.wide$experimenter_time)
# insulin.wide$time.before <- insulin.wide$time.after <- insulin.wide$id <- insulin.wide$experimenter_time <- NULL
# usethis::use_data(insulin.wide, overwrite = T)


#' Insulin data
#'
#' Selected results of experiments performed by Banting \emph{et al}., (1922),
#' testing whether insulin reduces blood sugar. Insulin was administered in
#' rabbits and blood sugar was measured within three hours. Data collated from
#' Table 1 and Table 2 of Banting \emph{et al}., (1922). This data set is in
#' \emph{long format}. \link{insulin.wide} is an equivalent data set in \emph{wide
#' format}.
#'
#' @format A data frame with 104 observations and 5 variables.
#'
#' \describe{
#'   \item{\code{sugar}}{Measured blood sugar level}
#'   \item{\code{treatment}}{Blood sugar measurements treatment group; \code{before} or \code{after}
#'   administering insulin}
#'   \item{\code{id}}{Identifier of individual being measured
#'   (not in original data set)}
#'   \item{\code{experimenter_time}}{Initial of researchers
#'   who performed the experiment and at what date}
#'   \item{\code{time}}{Time of blood sugar measurement; minutes after administration of insulin}
#' }
#'
#' @seealso \link{insulin.wide}, \link{petunia}, \link{damselfly}
#'
#' @source Banting, F. G., Best, C. H., Collip, J. B., Macleod, J. J., & Noble,
#'   E. C. (1922). The effect of pancreatic extract (insulin) on normal rabbits.
#'   American Journal of Physiology-Legacy Content, 62(1), 162-176.
"insulin"

#' "Wide format" Insulin data
#'
#' This data set contains the same information as \link{insulin}, however it is
#' in \emph{wide format} rather than \emph{long format}. Refer to \link{insulin} for further details.
#'
#' @format A data frame with 52 observations and 5 variables.
#'
#' \describe{
#'   \item{\code{sugar.before}}{Blood sugar measurement before administering insulin}
#'   \item{\code{sugar.after}}{Blood sugar measurement after administering insulin}
#'   \item{\code{time}}{Time of blood sugar measurement; minutes after administration of insulin}
#'   \item{\code{experimenter}}{Initials of researcher who performed the experiment}
#'   \item{\code{date}}{Date of experiment (month day)}
#' }
#'
#' @seealso \link{insulin}, \link{petunia}, \link{damselfly}
#'
#' @source Banting, F. G., Best, C. H., Collip, J. B., Macleod, J. J., & Noble,
#'   E. C. (1922). The effect of pancreatic extract (insulin) on normal rabbits.
#'   American Journal of Physiology-Legacy Content, 62(1), 162-176.
"insulin.wide"

#' Petunia data
#'
#' Charles Darwin's experimental results on petunia plants to determine the difference of plant length between self fertilised and cross-fertilised, either with the same stock (inter-cross) or with a fresh stock (westerham-cross)
#'
#' @format A data frame with 64 observations and 3 variables. Plants (self fertilised, inter-cross and westerham-cross) were measured to the tops of their stems when coming into flower. Data collated from Darwin (1877)
#' \describe{
#'   \item{\code{height}}{Measured height of plants}
#'   \item{\code{group}}{Plant fertilisation groups; \code{westerham_cross}, \code{inter_cross} or \code{self_fertilised}}
#'   \item{\code{pot_no}}{Identifier of pot number in which each plant was grown}
#' }
#'
#' @seealso \link{insulin}, \link{insulin.wide}, \link{damselfly}
#'
#' @source Darwin, C. (1877). The effects of cross and self fertilisation in the vegetable kingdom. John Murray, Albemarle Street, London.
"petunia"

#' Damselfly data
#'
#' Measurements of the body size and mass for adult and juvenile males
#' of \emph{Xanthagrion erythroneurum} damselflies. In this species, juvenile
#' males are coloured yellow and change to red upon sexual maturity.
#'
#' @format A data frame with 77 observations and 3 variables.
#' \describe{
#'   \item{\code{length}}{Measured body length (mm) of damselflies}
#'   \item{\code{mass}}{Measured body mass (mg) of damselflies}
#'   \item{\code{maturity}}{Male age groups; \code{adult} or \code{juvenile} used for body size and weight measurements}
#' }
#'
#' @seealso \link{petunia}, \link{insulin.wide}, \link{insulin}
#'
#' @source Khan, M. K., & Herberstein, M. E. (2021). Male-male interactions
#'   select for conspicuous male coloration in damselflies. Animal Behaviour,
#'   176, 157-166.
"damselfly"


