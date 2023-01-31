#' Adjust the intercept to regain the balance property
#'
#' This function updates the intercept term of the model fit such that the balance property is satisfied.
#'
#' @param obj an object of type \code{\link{glm}}, \code{\link{speedglm}}, \code{\link{cpglm}} or \code{\link{cpglmm}}
#' containing the model fit.
#' @param data a \code{\link{data.frame}} or \code{\link{data.table}} object that was used to fit the model.
#'
#' @return The object with the adjusted (fixed effects) coefficients.
#'
#' @references Campo, B.D.C. and Antonio, Katrien (2023). Insurance pricing with hierarchically structured data an illustration with a workers' compensation insurance portfolio. \emph{Scandinavian Actuarial Journal}, doi: 10.1080/03461238.2022.2161413
#' @references Wüthrich, M. V. (2020). Bias regularization in neural network models for general insurance pricing. \emph{European actuarial journal} \bold{10}(1), 179–202.
#'
#' @examples
#' library(statmod)
#' datas  = dataCar[1:1e3, ]
#' Fit    = glm(Y ~ area + gender, data = datas, weights = datas$w, family = tweedie(1.75, 0),
#' model = TRUE, control = glm.control(epsilon = 1e-4, maxit = 5e2))
#' w      = weights(Fit, "prior")
#' y      = Fit$y
#' sum(w * y) == sum(w * fitted(Fit))
#' adjFit = adjustIntercept(Fit, datas)
#' coef(adjFit)
#' sum(w * y) == sum(w * fitted(adjFit))
adjustIntercept <- function(obj, data) {
  if(missing(data))
    stop("Provide data set that was used to fit the model.")
  if(any(c("cpglm", "cpglmm") %in% class(obj))) {
    if(obj@link.power != 0)
      stop("Function can only be used with the log-link function.")
  } else {
    if(family(obj)$link != "mu^0" & family(obj)$link != "log")
      stop("Function can only be used with the log-link function.")
  }

  if(any(c("speedglm", "glm", "cpglm") %in% class(obj))) {
    yHat  = fitted(obj)
    w     = weights(obj, "prior")
    y     = obj$y
    Alpha = sum(w * y) / sum(w * yHat)
    Bhat  = coef(obj)
    Bhat[1] = Bhat[1] + log(Alpha)
    if("cpglm" %in% class(obj)) {
      obj@coefficients      = Bhat
      obj@fitted.values     = as.numeric(predict(obj, data, type = "response"))
      obj@linear.predictors = as.numeric(predict(obj, data, type = "link"))
    } else {
      obj$coefficients      = Bhat
      obj$fitted.values     = predict(obj, data, type = "response")
      obj$linear.predictors = predict(obj, data, type = "link")
    }
  } else if("cpglmm" %in% class(obj)) {
    yHat  = fitted(obj)
    w     = obj@frame$`(weights)`
    y     = model.response(obj@frame)
    Alpha = sum(w * y) / sum(w * yHat)
    Bhat  = cplm::fixef(obj)
    Bhat[1]   = Bhat[1] + log(Alpha)
    obj@fixef = Bhat
    obj@mu    = as.numeric(predict(obj, data, type = "response"))
    obj@eta   = as.numeric(predict(obj, data, type = "link"))
  }
  obj
}
