#' Model predictions
#'
#' Obtain predictions based on a model fit with \code{hierCredibility}
#'
#' @param object a model object for which prediction is desired.
#' @param newdata optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted values are used.
#' @param ... ignored.
#'
#' @method predict hierCredibility
#' @return If \code{newdata} is omitted the predictions are based on the data used for the fit.
#' @seealso \code{\link{hierCredibility}}
predict.hierCredibility <- function(object, newdata = NULL, ...) {
  if(missing(newdata)) {
    return(fitted(object))
  } else {
    REs = unlist(object$Hierarchy)
    if(!all(REs %in% names(newdata)))
      stop(paste0(paste0(REs[!REs %in% names(newdata)], collapse = ", "), " not found in newdata"))
    olddata = copy(object$Premiums$group)
    if(!is.data.table(newdata))
      newdata = as.data.table(newdata)
    setkeyv(olddata, REs)
    setkeyv(newdata, REs)
    olddata$Vjk[olddata[, ..REs][newdata[, ..REs], nomatch = 0L, which = T]]
  }
}

#' Model predictions
#'
#' Obtain predictions based on the model fit with \code{hierCredGLM}
#'
#' @param object a model object for which prediction is desired.
#' @param newdata optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted values are used.
#' @param ... arguments passed to \code{\link[speedglm]{speedglm}}
#'
#' @details The random effects are taken into account by specifying these as an offset in the \code{\link[speedglm]{predict.speedglm}} function.
#'
#' @method predict hierCredGLM
#' @return If \code{newdata} is omitted the predictions are based on the data used for the fit.
#' @seealso \code{\link{hierCredGLM}}
predict.hierCredGLM <- function(object, newdata = NULL, ...) {
  if(missing(newdata)) {
    return(fitted(object))
  } else {
    newdata = .addREs(object, newdata)
    predict(object$fitGLM, newdata, ...)
  }
}

#' Model predictions
#'
#' Obtain predictions based on the model fit with \code{hierCredTweedie}
#'
#' @param object a model object for which prediction is desired.
#' @param newdata optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted values are used.
#' @param ... arguments passed to \code{\link[cplm]{cpglm}}
#'
#' @details The random effects are taken into account by specifying these as an offset in the \code{predict.cpglm} function.
#' @method predict hierCredTweedie
#' @return If \code{newdata} is omitted the predictions are based on the data used for the fit.
#' @seealso \code{\link{hierCredTweedie}}
predict.hierCredTweedie <- function(object, newdata = NULL, ...) {
  if(missing(newdata)) {
    return(fitted(object))
  } else {
    newdata = .addREs(object, newdata)
    predict(object$fitGLM, newdata, ...)
  }
}
