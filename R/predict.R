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
#' @param ... arguments passed to \code{glm}
#'
#' @details The random effects are taken into account by specifying these as an offset in the \code{predict.glm} function.
#'
#' @method predict hierCredGLM
#' @return If \code{newdata} is omitted the predictions are based on the data used for the fit.
#' @seealso \code{\link{hierCredGLM}}
predict.hierCredGLM <- function(object, newdata = NULL, ...) {
  if(missing(newdata)) {
    return(fitted(object))
  } else {
    newdata = .addREs(object, newdata)
    newdata[is.na(Uj)]  = 1
    newdata[is.na(Ujk)] = 1
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
    newdata[is.na(Uj)]  = 1
    newdata[is.na(Ujk)] = 1
    predict(object$fitGLM, newdata, ...)
  }
}



#' @rdname buhlmannStraub-class
#' @method predict buhlmannStraub
#' @param newdata optionally, a data frame in which to look for variables with which to predict.
predict.buhlmannStraub <- function(object, newdata = NULL, ...) {
  if(is.null(newdata)) {
    return(fitted(object))
  }
  
  MLF = object$Hierarchy$MLFj
  Argz = as.list(object$call)[-1]
  
  newDf = copy(newdata)
  newDf$MLFj = eval(Argz$MLFj, newDf)
  
  if(is.factor(newDf$MLFj))
    newDf$MLFj = as.character(newDf$MLFj)
  
  newDt = as.data.table(newDf)
  
  # Get premiums for each cluster
  Premiums = object$Premiums$MLFj
  setnames(Premiums, MLF, "MLFj")
  setkeyv(Premiums, "MLFj")
  setkeyv(newDt, "MLFj")
  
  # Match clusters and get predictions
  predictions = Premiums$Vj[match(newDt$MLFj, Premiums$MLFj)]
  
  # For clusters not in training data, use portfolio mean
  predictions[is.na(predictions)] = object$Premiums$Portfolio
  
  return(predictions)
}

#' @rdname buhlmannStraubGLM-class
#' @method predict buhlmannStraubGLM
#' @param newdata optionally, a data frame in which to look for variables with which to predict.
predict.buhlmannStraubGLM <- function(object, newdata = NULL, ...) {
  if(is.null(newdata)) {
    return(fitted(object))
  } else {
    newdata = .addREs(object, newdata)
    newdata[is.na(Uj)] = 1
    predict(object$fitGLM, newdata, ...)
  }
}




#' @rdname buhlmannStraubTweedie-class
#' @method predict buhlmannStraubTweedie
#' @param newdata optionally, a data frame in which to look for variables with which to predict.
predict.buhlmannStraubTweedie <- function(object, newdata = NULL, ...) {
  if(is.null(newdata)) {
    return(fitted(object))
  }
  
  # Get predictions from GLM part
  pred_glm = predict(object$fitGLM, newdata = newdata, type = "response")
  
  # Get random effects
  MLF = object$CredibilityResults$Hierarchy$MLFj
  Argz = as.list(object$call)[-1]
  
  newDf = copy(newdata)
  newDf$MLFj = eval(Argz[[which(names(Argz) == "weights") - 1]], newDf) # Get cluster variable
  
  if(is.factor(newDf$MLFj))
    newDf$MLFj = as.character(newDf$MLFj)
  
  # Get random effects for each cluster
  Relativity = object$CredibilityResults$Relativity$MLFj
  setnames(Relativity, MLF, "MLFj")
  
  Uj = Relativity$Uj[match(newDf$MLFj, Relativity$MLFj)]
  
  # For clusters not in training data, use 1
  Uj[is.na(Uj)] = 1
  
  return(pred_glm * Uj)
}