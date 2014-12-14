#' The survival function of a Gompertz-gamma distribution
#'
#' @param x A vector of positive numbers
#' @param shape The \emph{shape} parameter
#' @param scale The \emph{scale} parameter
#' @param frail The \emph{frailty} parameter
#' @param log.p Logical, should the result be on a log scale?
#' @return A numerical vector of the same length as \code{x}.
#' @details
#' The survivor function \eqn{S(x)} is defined as
#' @examples
#' SGgompertz(c(1, 3, 5))
#' @export
#' @importFrom survival Surv


SGgompertz <- function(x, shape = 1, scale = 1, frail = 0, log.p = FALSE){
    if (frail > 0){
        ret <-  -log(1 + frail * shape * scale * (exp(x / scale) - 1)) / frail
    }else{
        ret <- -shape * scale * (exp(x / scale) - 1)
    }
    if (!log.p) ret <- exp(ret)
    ret
}

#' The hazard function of a Gompertz-gamma distribution
#'
#' @param x A vector of positive numbers
#' @param shape The \code{shape} parameter
#' @param scale The \code{scale} parameter
#' @param frail The \code{frailty} parameter
#' @param log.p Logical, should the result be on a log scale?
#' @return A numerical vector of the same length as \code{x}.
#' @examples
#' hGgompertz(c(1, 3, 5))
#' @description The hazard function \eqn{h(x)} is defined as
#' \deqn{
#' h(x) = shape * exp(x / scale) / (1 + frail * scale * shape * (exp(x / scale) - 1))
#' }
#' When \eqn{frail = 0}, this simplifies to
#' \deqn{h(x) = shape * exp(x / scale)}
#' which is the hazard function of the standard Gompertz distribution
#' @export

hGgompertz <- function(x, shape = 1, scale = 1, frail = 0, log.p = FALSE){
   frail <- max(0, frail)
    kern <- exp(x / scale)
    ret <- shape * kern / (1 + frail * scale * shape * (kern - 1))
    if (log.p) ret <- log(ret)
    ret
}
