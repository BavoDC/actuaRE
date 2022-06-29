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
NrUnique <- function(x, na.rm = T) {
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

#' @inherit nlme::random.effects
ranef.hierCredibility <- function(object, ...) {
  object$Relativity
}
#' @inherit nlme::random.effects
ranef.hierCredGLM     <- function(object, ...) {
  object$HierarchicalResults$Relativity
}
#' @inherit nlme::random.effects
ranef.hierCredTweedie <- function(object, ...) {
  object$HierarchicalResults$Relativity
}
#' Extract fixed-effects estimates
#'
#' @param object any fitted model object from which fixed effects estimates can be extracted.
#' @param ... ignored.
#'
#' @return a named, numeric vector of fixed-effects estimates.
#'
#' @examples
#' \dontrun{
#' fit = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w, p = 1.75, epsilon = 1e-6)
#' fixef(fit)
#' }
fixef.hierCredGLM     <- function(object, ...) {
  coef(object$fitGLM)
}
#' @rdname fixef.hierCredGLM
fixef.hierCredTweedie <- function(object, ...) {
  coef(object$fitGLM)
}

#' Balance property
#'
#' Assesses whether the balance property holds
#'
#' @param obj an object containing the model fit
#'
#' @return a list with the slots \code{BalanceProperty} (logical indicating whether this is satisfied) and \code{Alpha} (Ratio total observed damage to total predicted damage).
#'
#' @examples
#' fit = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w, p = 1.75, epsilon = 1e-6)
#' BalanceProperty(fit)
BalanceProperty <- function(obj) {
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
      list(BalanceProperty = BP,
           Alpha = Alpha),
      class = "BalanceProperty"
    )
  return(invisible(Results))
}

#' Extract model weights
#'
#' \code{weights} is a generic function which extracts fitting weights from objects returned by modeling functions.
#' Methods can make use of \code{\link[stats]{napredict}} methods to compensate for the omission of missing values. The default methods does so.
#'
#' @param object an object for which the extraction of model weights is meaningful.
#' @param type indicates if prior or working weights need to be extracted.
#' @param ... ignored
#'
#' @return Weights extracted from the object \code{object}: the default method looks for component "weights" and if not \code{NULL} calls \code{\link[stats]{napredict}} on it.
#' @method weights cpglm
weights.cpglm <- function(object, type = c("prior", "working"), ...) {
  type = match.arg(type)
  res  = if(type == "prior") object$prior.weights else object$weights
  if(is.null(object$na.action))
    res
  else
    naresid(object$na.action, res)
}

#' @rdname weights.cpglm
weights.speedglm <- function(object, type = c("prior", "working"), ...) {
  type = match.arg(type)
  res  = if(type == "prior") object$prior.weights else object$weights
  if(is.null(object$na.action))
    res
  else
    naresid(object$na.action, res)
}

#' @rdname weights.cpglm
weights.hierCredGLM <- function(object, type = c("prior", "working"), ...) {
  type = match.arg(type)
  res  = if(type == "prior") object$prior.weights else object$weights
  if(is.null(object$na.action))
    res
  else
    naresid(object$na.action, res)
}

#' @rdname weights.cpglm
weights.hierCredTweedie <- function(object, type = c("prior", "working"), ...) {
  type = match.arg(type)
  res  = if(type == "prior") object$prior.weights else object$weights
  if(is.null(object$na.action))
    res
  else
    naresid(object$na.action, res)
}




