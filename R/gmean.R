#' Calculates the mean of Gompertz-Gamma distribution.
#' @param shape The shape parameter
#' @param scale The scale parameter
#' @param frail The frailty parameter
#' @export

gmean <- function(shape, scale, frail = 0){
    res <- integrate(SGgompertz, 0, Inf,
              shape = shape, scale = scale, frail = frail)
    res$value
}
