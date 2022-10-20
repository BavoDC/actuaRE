#' Fitting a Tweedie GLMM, using the initial estimates of hierCredTweedie
#'
#' This function first estimates the random effects model using Ohlsson's GLMC algorithm (Ohlsson, 2008) and then uses
#' these estimates as initial estimates when fitting a Tweedie GLMM.
#'
#' @param formula object of type \code{\link{formula}} that specifies which model should be fitted. Syntax is the same as for
#' \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}}. For example, \code{Yijkt ~ x1 + x2 + (1 | Industry / Branch)}.
#' @param data an object that is coercible by \code{\link[data.table]{as.data.table}}, containing the variables in the model.
#' @param weights variable name of the exposure weight.
#' @param muHatGLM indicates which estimate has to be used in the algorithm for the intercept term. Default is \code{TRUE},
#'  which used the intercept as estimated by the GLM. If \code{FALSE}, the estimate of the hierarchical credibility model is used.
#' @param epsilon positive convergence tolerance \eqn{\epsilon}; the iterations converge when
#' \eqn{||\theta[k] - \theta[k - 1]||^2[[2]]/||\theta[k - 1]||^2[[2]] < \epsilon}. Here, \eqn{\theta[k]} is the parameter vector at the \eqn{k^{th}} iteration.
#' @param maxiter maximum number of iterations.
#' @param verbose logical indicating if output should be produced during the algorithm.
#' @param balanceProperty logical indicating if the balance property should be satisfied.
#'
#' @return an object of class \code{cpglmm}, containing the model fit.
#'
#' @seealso \code{\link[cplm]{cpglmm}} and \code{\link{hierCredTweedie}}
#'
#' @examples
#' \dontrun{
#' data("dataCar")
#' fitTweedieGLMM = tweedieGLMM(Y ~ area + gender + (1 | VehicleType / VehicleBody), dataCar, weights = w, verbose = TRUE, epsilon = 1e-4)
#' }
tweedieGLMM <- function(formula, data, weights, muHatGLM = F, epsilon = 1e-4,
                        maxiter = 5e2, verbose = F, balanceProperty = T) {
  # Combining the hierarchical credibility model with a GLM (Ohlsson, 2008)
  call = match.call()
  if(!is.logical(muHatGLM))
    stop("Argument muHatGLM has to be of type logical.")
  if(!is.formula(formula))
    stop("Has to be of type formula.")
  if(!all(all.vars(formula) %in% names(data)))
    stop("Did not find the variables in the formula in the dataframe")

  ## Copied from lme4: glmer
  ## extract x, y, etc from the model formula and frame
  mc <- mcout <- match.call()
  mc[[1]] = quote(lme4::glFormula)
  for(i in names(mc) %>% .[!. %in% names(formals(lme4::glFormula))] %>% .[. != ""])
    mc[[i]] = NULL
  glmod   = eval(mc, parent.frame(1L))
  mcout$formula = glmod$formula
  glmod$formula = NULL
  glmod$REML    = NULL
  weights       = tryCatch(eval(weights, model.frame(1L)), error = function(e) eval(call$weights, data))

  formulaGLM = lme4::nobars(formula)
  formulaRE  = lme4::findbars(formula)
  if(length(formulaRE) != 2)
    stop("Number of nested random effects is limited to two.")
  if(!any(sapply(formulaRE, function(x) grepl(":", deparse(x)))))
    stop("No nested random effects specified.")
  MLFj  = all.vars(formulaRE[!sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])
  MLFjk = all.vars(formulaRE[sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])[1]

  data$Yijkt   = model.extract(glmod$fr, "response")
  data$wijkt   = model.extract(glmod$fr, "weights")
  data$MLFj    = data[[MLFj]]
  data$MLFjk   = data[[MLFjk]]
  if(!lme4::isNested(data$MLFjk, data$MLFj))
    stop(paste(MLFjk, "is not nested within", MLFj))
  if(length(all.vars(lme4::nobars(formulaGLM))[-1]) == 0)
    stop("No contract-specific covariates specified.")

  if(any(table(data$MLFj) == 1) | any(table(data$MLFjk) == 1))
    warning("There are categories with only 1 observation.", immediate. = T)

  ArgzFn = match.call()
  for(i in names(ArgzFn) %>% .[!. %in% names(formals(hierCredTweedie))] %>% .[. != ""])
    ArgzFn[[i]] = NULL
  ArgzFn$weights = weights

  formulaGLM = lme4::nobars(formula)
  formulaRE  = lme4::findbars(formula)

  initTw = eval(do.call("substitute", list(expr = ArgzFn, env = list(tweedieGLMM = as.name("hierCredTweedie")))))
  REs    = unique(unlist(sapply(formulaRE, all.vars)))
  for(i in REs)
    data[[i]] %<>% as.factor

  ArgzFn =
    alist(
      data = data,
      link = "log",
      optimizer = "bobyqa",
      control = list(max.fun = 1e6, trace = if(verbose) 4 else 1),
      inits = list(
        beta = coef(initTw$fitGLM),
        phi = initTw$fitGLM$phi,
        p = initTw$fitGLM$p,
        Sigma = rep(1, 2)
      )
    )
  ArgzFn$formula = formula
  ArgzFn$weights = weights
  TwFit = do.call("cpglmm", ArgzFn)
  if(balanceProperty)
    TwFit = adjustIntercept(TwFit, data)
  return(TwFit)
}
