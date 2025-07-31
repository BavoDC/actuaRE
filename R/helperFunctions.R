#' Number of unique elements in a vector
#'
#' @param x object of type \code{vector}.
#' @param na.rm logical indicating if missing values have to be removed. Default to \code{TRUE}.
#'
#' @return vector with the number of unique elements
#'
#' @examples
#' set.seed(1)
#' x = sample(letters, 50, TRUE)
#' NrUnique(x)
NrUnique <- function(x, na.rm = TRUE) {
  if(!is.vector(x))
    stop("Provide object of type vector.")
  if (na.rm)
    length(unique(na.omit(x)))
  else
    length(unique(x))
}

#' Formula
#'
#' Checks if the object is of the type formula
#'
#' @param x the object.
#'
#' @return logical indicating if the object is a formula or not.
#'
#' @examples
#' f = formula(y ~ x)
#' is.formula(f)
is.formula <- function(x) {
  inherits(x,"formula")
}


#' Add random effects to the data frame
#'
#' Internal function
#'
#' @param obj object with model fit
#' @param newdata an object coercible to \code{data.table}.
#'
.addREs <- function(obj, newdata) {
  if(!any(c("hierCredibility", "hierCredGLM", "hierCredTweedie") %in% class(obj)))
    stop("Only objects of type hierCredibility, hierCredGLM and hierCredTweedie allowed.")
  if(!is.data.table(newdata))
    newdata = as.data.table(newdata)
  if("hierCredibility" %in% class(obj)) {
    MLFj  = obj$Hierarchy$sector
    MLFjk = obj$Hierarchy$group
  } else {
    formulaRE  = lme4::findbars(obj$call$formula)
    MLFj  = all.vars(formulaRE[!sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])
    MLFjk = all.vars(formulaRE[sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])[1]
  }
  newdata$.MLFj  = newdata[[MLFj]]
  newdata$.MLFjk = newdata[[MLFjk]]

  hierObj = if("hierCredibility" %in% class(obj)) obj else obj$HierarchicalResults
  DtUj    = hierObj$Relativity$sector
  DtUjk   = hierObj$Relativity$group

  newdata[["Uj"]]  = DtUj$Uj[match(newdata$.MLFj, DtUj[[MLFj]])]
  newdata[["Ujk"]] = DtUjk$Ujk[match(newdata$.MLFjk, DtUjk[[MLFjk]])]
  return(newdata)
}

.capitalize <- function (string)
{
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}


#' Extract the random effect estimates from a fitted random effects model
#'
#' A generic function to extract the estimates/predictions of the random effects from a fitted random effects model.
#'
#' @name ranef-actuaRE
#' @param object an object of type \code{\link{hierCredibility}}, \code{\link{hierCredGLM}} or \code{\link{hierCredTweedie}}
#' @param ... Currently ignored.
#'
#' @return A list of data frames, one for each grouping factor for the random effects. The number of rows in the data frame is the number of levels of the grouping factor.
#' The first (two) columns correspond(s) to the grouping factor. The last column corresponds to the estimated random effect.
#'
#' @method ranef hierCredibility
ranef.hierCredibility <- function(object, ...) {
  object$Relativity
}
#' @rdname ranef-actuaRE
#' @method ranef hierCredGLM
ranef.hierCredGLM     <- function(object, ...) {
  object$HierarchicalResults$Relativity
}
#' @rdname ranef-actuaRE
#' @method ranef hierCredTweedie
ranef.hierCredTweedie <- function(object, ...) {
  object$HierarchicalResults$Relativity
}
#' Extract the fixed-effects estimates from a fitted random effects model
#'
#' A generic function to extract the fixed effects (i.e. the company-specific effects) estimates from a fitted random effects model.
#'
#' @name fixef-actuaRE
#' @param object an object of type \code{\link{hierCredGLM}} or \code{\link{hierCredTweedie}}
#' @param ... ignored.
#' @method fixef hierCredGLM
#'
#' @return a named, numeric vector of fixed-effects estimates.
#'
#' @examples
#' \donttest{
#' fit = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar,
#' weights = w, p = 1.75, epsilon = 1e-6)
#' fixef(fit)
#' }
fixef.hierCredGLM     <- function(object, ...) {
  coef(object$fitGLM)
}
#' @rdname fixef-actuaRE
#' @method fixef hierCredTweedie
fixef.hierCredTweedie <- function(object, ...) {
  coef(object$fitGLM)
}

#' Balance property
#'
#' Function to assess whether the balance property holds
#'
#' @param obj an object containing the model fit
#'
#' @return a list with the slots \code{call} (the original call), \code{BalanceProperty} (logical indicating whether the balance
#' property is satisfied) and \code{Alpha} (Ratio total observed damage to total predicted damage).
#'
#' @references Campo, B.D.C. and Antonio, Katrien (2023). Insurance pricing with hierarchically structured data an illustration with a workers' compensation insurance portfolio. \emph{Scandinavian Actuarial Journal}, doi: 10.1080/03461238.2022.2161413
#' @references Wüthrich, M. V. (2020). Bias regularization in neural network models for general insurance pricing. \emph{European actuarial journal} \bold{10}(1), 179–202.
#' @examples
#' \donttest{
#' fit = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w,
#'  p = 1.75, epsilon = 1e-6)
#' BalanceProperty(fit)
#' }
BalanceProperty <- function(obj) {
  call  = match.call
  yHat  = fitted(obj)
  w     = weights(obj, "prior")
  y     = obj$y
  Alpha = sum(w * y) / sum(w * yHat)
  # Floating point error in R
  # https://stackoverflow.com/questions/6874867/floating-point-issue-in-r
  # https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
  # https://www.h-schmidt.net/FloatConverter/IEEE754.html
  BP    = is.logical(all.equal(Alpha, 1))
  if(BP) {
    cat("\nBalance property is satisfied.\n\n")
  } else {
    warning("\nBalance property is not satisfied.\n", immediate. = T)
    cat("\nRatio total observed damage to total predicted damage:", Alpha, "\n\n")
  }
  Results =
    structure(
      list(call,
           BalanceProperty = BP,
           Alpha = Alpha),
      class = "BalanceProperty"
    )
  return(invisible(Results))
}

#' Extract the model weights
#'
#' \code{weights} is a generic function which extracts fitting weights from objects returned by modeling functions.
#' Methods can make use of \code{\link[stats]{napredict}} methods to compensate for the omission of missing values. The default methods does so.
#'
#' @name weights-actuaRE
#' @param object an object for which the extraction of model weights is meaningful. Can be either \code{\link[cplm]{cpglm}},
#' \code{\link{glm}}, \code{\link{hierCredibility}}, \code{\link{hierCredGLM}} or \code{\link{hierCredTweedie}}
#' @param type indicates if prior or working weights need to be extracted.
#' @param ... ignored
#'
#' @return Weights extracted from the object \code{object}: the default method looks for component "weights" and if not \code{NULL} calls \code{\link[stats]{napredict}} on it.
#' @method weights cpglm
#' @seealso \code{\link[stats]{weights}}, \code{\link{cpglm}}, \code{glm}, \code{\link{hierCredibility}}, \code{\link{hierCredGLM}} or \code{\link{hierCredTweedie}}
weights.cpglm <- function(object, type = c("prior", "working"), ...) {
  type = match.arg(type)
  res  = if(type == "prior") object$prior.weights else object$weights
  if(is.null(object$na.action))
    res
  else
    naresid(object$na.action, res)
}

#' #' @rdname weights-actuaRE
#' #' @method weights speedglm
#' weights.speedglm <- function(object, type = c("prior", "working"), ...) {
#'   type = match.arg(type)
#'   res  = if(type == "prior") object$prior.weights else object$weights
#'   if(is.null(object$na.action))
#'     res
#'   else
#'     naresid(object$na.action, res)
#' }

#' @rdname weights-actuaRE
#' @method weights hierCredGLM
weights.hierCredGLM <- function(object, type = c("prior", "working"), ...) {
  type = match.arg(type)
  res  = if(type == "prior") object$prior.weights else object$weights
  if(is.null(object$na.action))
    res
  else
    naresid(object$na.action, res)
}

#' @rdname weights-actuaRE
#' @method weights hierCredTweedie
weights.hierCredTweedie <- function(object, type = c("prior", "working"), ...) {
  type = match.arg(type)
  res  = if(type == "prior") object$prior.weights else object$weights
  if(is.null(object$na.action))
    res
  else
    naresid(object$na.action, res)
}




