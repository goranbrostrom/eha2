#' Calendar time cut of survival data
#'
#' For a given time interval, each spell is cut to fit into the
#' given calendar time interval.
#'
#' @param dat Input data frame. Must contain survival data.
#' @param window Vector of length two: The time interval.
#' @param surv Vector of length four giving tha names of the central
#'    variables (an "extended" Surv object) in the dataframe 'dat'.
#' @return A dataframe of the same form as the input, but 'cut' as
#'    desired. Intervals exceeding \code{window[2]} are cut there and
#'    given event = 0 (censoring).
#'
#' @author Göran Broström
#' @seealso \code{\link{age.window}}.
#' @examples
#' dat <- data.frame(enter = 0, exit = 5.731, event = 1,
#' birthdate = 1962.505, x = 2)
#' window <- c(1963, 1965)
#' dat.trim <- cal.window(dat, window)

#' @export
cal.window <- function(dat, window,
                       surv = c("enter", "exit", "event", "birthdate")){

    if (!is.data.frame(dat))stop("dat must be a data frame")
    if (length(surv) != 4) stop("surv must have length 4")
    fixed.names <- names(dat)
    surv.indices <- match(surv, fixed.names)
    if (length(which(is.na(surv.indices)))){
        x <- which(is.na(surv.indices))
        stop(paste(surv[x], " is not a name in the fixed data frame."))
    }

    ## Utilize list property:
    enter <- dat[[surv.indices[1]]]
    exit <- dat[[surv.indices[2]]]
    event <- dat[[surv.indices[3]]]
    bdate <- dat[[surv.indices[4]]]

    who <- ((exit > (window[1] - bdate)) &
            (enter < (window[2] - bdate)))
    if (sum(who) > 0.5){
        enter <- enter[who]
        exit <- exit[who]
        event <- event[who]
        bdate <- bdate[who]
        overShoot <- exit > window[2] - bdate
        event[overShoot] <- 0
        exit[overShoot] <- window[2] - bdate[overShoot]
        underShoot <- enter < window[1] - bdate
        enter[underShoot] <- window[1] - bdate[underShoot]

        dat <- dat[who, ]
        dat[surv.indices[1]] <- enter
        dat[surv.indices[2]] <- exit
        dat[surv.indices[3]] <- event
        dat[surv.indices[4]] <- bdate
    }else{
        warning(paste("The period", window[1], "-", window[2], "is empty."))
        dat <- NULL
    }
    dat
}
