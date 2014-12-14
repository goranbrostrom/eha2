#' Maximizes a likelihood for the Gompertz-gamma distribution
#' @param enter Late entrance
#' @param exit Time of failure or censoring
#' @param event Indicator of failure
#' @export

gompreg <- function(enter, exit, event){
    logh <- function(beta){
        shape <- exp(beta[1])
        scale <- exp(beta[2])
        frail <- exp(beta[3])
        hGgompertz(exit, shape, scale, frail, log.p = TRUE)
    }
    logS <- function(beta){
        shape <- exp(beta[1])
        scale <- exp(beta[2])
        frail <- exp(beta[3])
        SGgompertz(exit, shape, scale, frail, log.p = TRUE) -
           SGgompertz(enter, shape, scale, frail, log.p = TRUE)

    }

    loglik <- function(beta){
        sum(event * logh(beta) + logS(beta))
    }

    beta <- c(0, 0, 0)

    res <- optim(beta, loglik, control = list(fnscale = -1), hessian = TRUE)
    par <- res$par
    rus <- c(par[1], exp(-par[2]), frail = exp(par[3]))
    names(rus) <- c("lnalpha", "gamma", "frail")
    list(par = rus, sd = sqrt(diag(solve(-res$hessian))), loglik = res$value)
}
