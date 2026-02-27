#' Fitting a Tweedie GLMM, using initial estimates from credibility models
#'
#' This function first estimates the random effects model using Ohlsson's GLMC algorithm (Ohlsson, 2008) and then uses
#' these estimates as initial estimates when fitting a Tweedie GLMM. Supports both single random effects and
#' nested random effects.
#'
#' @param formula object of type \code{\link{formula}} that specifies which model should be fitted. Syntax is the same as for
#' \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}}. For single random effect: \code{Y ~ x1 + x2 + (1 | Cluster)}.
#' For nested random effects: \code{Y ~ x1 + x2 + (1 | cluster / subcluster)}.
#' @param data an object that is coercible by \code{\link[data.table]{as.data.table}}, containing the variables in the model.
#' @param weights variable name of the exposure weight.
#' @param muHatGLM indicates which estimate has to be used in the algorithm for the intercept term. Default is \code{FALSE},
#'  which uses the intercept as estimated by the credibility model. If \code{TRUE}, the estimate of the GLM is used.
#' @param epsilon positive convergence tolerance \eqn{\epsilon}; the iterations converge when
#' \eqn{||\theta[k] - \theta[k - 1]||^2_2/||\theta[k - 1]||^2_2 < \epsilon}. Here, \eqn{\theta[k]} is the parameter vector at the \eqn{k^{th}} iteration.
#' @param maxiter maximum number of iterations.
#' @param verbose logical indicating if output should be produced during the algorithm.
#' @param balanceProperty logical indicating if the balance property should be satisfied.
#'
#' @return an object of class \code{cpglmm}, containing the model fit.
#' @references Campo, B.D.C. and Antonio, Katrien (2023). Insurance pricing with hierarchically structured data an illustration with a workers' compensation insurance portfolio. \emph{Scandinavian Actuarial Journal}, doi: 10.1080/03461238.2022.2161413
#' @references Ohlsson, E. (2008). Combining generalized linear models and credibility models in practice. \emph{Scandinavian Actuarial Journal} \bold{2008}(4), 301–314.
#'
#' @seealso \code{\link[cplm]{cpglmm}}, \code{\link{hierCredTweedie}}
#'
#' @examples
#' \donttest{
#' # Nested random effects example
#' data("tweedietraindata")
#' fit = tweedieGLMM(y ~ x1 + (1 | cluster / subcluster), tweedietraindata, weights = wt)
#' fit
#' }
tweedieGLMM <- function(formula, data, weights, muHatGLM = FALSE, epsilon = 1e-4,
                           maxiter = 5e2, verbose = FALSE, balanceProperty = TRUE) {
  # Combining credibility models with a Tweedie GLMM (Ohlsson, 2008)
  call = match.call()
  if(!is.logical(muHatGLM))
    stop("Argument muHatGLM has to be of type logical.")
  if(!is.formula(formula))
    stop("Has to be of type formula.")
  if(!all(all.vars(formula) %in% names(data)))
    stop("Did not find the variables in the formula in the dataframe")

  ## Extract x, y, etc from the model formula and frame
  mc <- mcout <- match.call()
  mc[[1]] = quote(glFormula)
  for(i in names(mc) %>% .[!. %in% names(formals(glFormula))] %>% .[. != ""])
    mc[[i]] = NULL
  glmod   = eval(mc, parent.frame(1L))
  mcout$formula = glmod$formula
  glmod$formula = NULL
  glmod$REML    = NULL
  weights       = tryCatch(eval(weights, model.frame(1L)), error = function(e) eval(call$weights, data))

  formulaGLM = nobars(formula)
  formulaRE  = findbars(formula)

  # Check number of random effects
  if(length(formulaRE) == 0)
    stop("No random effects specified.")
  if(length(formulaRE) > 2)
    stop("Maximum of two nested random effects allowed.")

  # Determine if single or nested random effects
  isNested = length(formulaRE) == 2 && any(sapply(formulaRE, function(x) grepl(":", deparse(x))))

  if(length(all.vars(nobars(formulaGLM))[-1]) == 0)
    stop("No contract-specific covariates specified.")

  # Get initial estimates based on structure
  if(isNested) {
    # Two nested random effects - use hierCredTweedie
    if(!any(sapply(formulaRE, function(x) grepl(":", deparse(x)))))
      stop("For two random effects, they must be nested (e.g., (1 | A / B)).")

    MLFj  = all.vars(formulaRE[!sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])
    MLFjk = all.vars(formulaRE[sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])[1]

    data$Yijkt   = model.extract(glmod$fr, "response")
    data$wijkt   = model.extract(glmod$fr, "weights")
    data$MLFj    = data[[MLFj]]
    data$MLFjk   = data[[MLFjk]]

    if(!isNested(data$MLFjk, data$MLFj))
      stop(paste(MLFjk, "is not nested within", MLFj))

    if(any(table(data$MLFj) == 1) | any(table(data$MLFjk) == 1))
      warning("There are categories with only 1 observation.", immediate. = TRUE)

    # Prepare arguments for hierCredTweedie
    ArgzFn = match.call()
    for(i in names(ArgzFn) %>% .[!. %in% names(formals(hierCredTweedie))] %>% .[. != ""])
      ArgzFn[[i]] = NULL
    ArgzFn$weights = weights

    initTw = eval(do.call("substitute", list(expr = ArgzFn, env = list(tweedieGLMM = as.name("hierCredTweedie")))))

    REs = unique(unlist(sapply(formulaRE, all.vars)))
    for(i in REs)
      data[[i]] %<>% as.factor

    # Number of variance components for nested structure
    nVarComp = 2

  } else {
    # Single random effect
    if(length(formulaRE) == 2)
      stop("For two random effects, they must be nested (use / syntax, e.g., (1 | A / B)).")

    MLFj = all.vars(formulaRE[[1]])

    data$Yij   = model.extract(glmod$fr, "response")
    data$wij   = model.extract(glmod$fr, "weights")
    data$.MLFj  = data[[MLFj]]

    if(any(table(data$.MLFj) == 1))
      warning("There are clusters with only 1 observation.", immediate. = TRUE)

    # Prepare arguments for hierCredTweedie
    ArgzFn = match.call()
    for(i in names(ArgzFn) %>% .[!. %in% names(formals(buhlmannStraubGLM))] %>% .[. != ""])
      ArgzFn[[i]] = NULL
    ArgzFn$weights = weights

    initTw = eval(do.call("substitute", list(expr = ArgzFn, env = list(tweedieGLMM = as.name("buhlmannStraubTweedie")))))

    REs = unique(unlist(sapply(formulaRE, all.vars)))
    for(i in REs)
      data[[i]] %<>% as.factor

    # Number of variance components for nested structure
    nVarComp = 1
  }

  # Fit cpglmm with appropriate number of variance components
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
        Sigma = rep(1, nVarComp)
      )
    )

  ArgzFn$formula = formula
  ArgzFn$weights = weights
  TwFit = do.call("cpglmm", ArgzFn)

  if(balanceProperty)
    TwFit = adjustIntercept(TwFit, data)

  return(TwFit)
}
















