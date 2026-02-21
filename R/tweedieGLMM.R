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
#' @references   Campo, B.D.C. and Antonio, Katrien (2023). Insurance pricing with hierarchically structured data an illustration with a workers' compensation insurance portfolio. \emph{Scandinavian Actuarial Journal}, doi: 10.1080/03461238.2022.2161413
#'
#' @seealso \code{\link[cplm]{cpglmm}} and \code{\link{hierCredTweedie}}
#'
#' @examples
#' \donttest{
#' data("dataCar")
#' fitTweedieGLMM = tweedieGLMM(Y ~ area + gender + (1 | VehicleType / VehicleBody), dataCar,
#'  weights = w, verbose = TRUE, epsilon = 1e-4)
#' }
tweedieGLMM <- function(formula, data, weights, muHatGLM = FALSE, epsilon = 1e-4,
                        maxiter = 5e2, verbose = FALSE, balanceProperty = TRUE) {
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



#' Fitting a Tweedie GLMM, using initial estimates from credibility models
#'
#' This function first estimates the random effects model using Ohlsson's GLMC algorithm (Ohlsson, 2008) and then uses
#' these estimates as initial estimates when fitting a Tweedie GLMM. Supports both single random effects and
#' nested random effects.
#'
#' @param formula object of type \code{\link{formula}} that specifies which model should be fitted. Syntax is the same as for
#' \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}}. For single random effect: \code{Y ~ x1 + x2 + (1 | Cluster)}.
#' For nested random effects: \code{Y ~ x1 + x2 + (1 | Industry / Branch)}.
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
#' data("dataCar")
#' fitNested = tweedieGLMM(Y ~ area + gender + (1 | VehicleType / VehicleBody), dataCar,
#'                         weights = w, verbose = TRUE, epsilon = 1e-4)
#'
#' # Single random effect example
#' data("hachemeister", package = "actuar")
#' X = as.data.frame(hachemeister)
#' Df = reshape(X, idvar = "state",
#'              varying = list(paste0("ratio.", 1:12), paste0("weight.", 1:12)),
#'              direction = "long")
#' Df$time_factor = factor(Df$time)
#' fitSingle = tweedieGLMM(ratio.1 ~ time_factor + (1 | state), Df,
#'                         weights = weight.1, verbose = TRUE)
#' }
newtweedieGLMM <- function(formula, data, weights, muHatGLM = FALSE, epsilon = 1e-4,
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

  # Check number of random effects
  if(length(formulaRE) == 0)
    stop("No random effects specified.")
  if(length(formulaRE) > 2)
    stop("Maximum of two nested random effects allowed.")

  # Determine if single or nested random effects
  isNested = length(formulaRE) == 2 && any(sapply(formulaRE, function(x) grepl(":", deparse(x))))

  if(length(all.vars(lme4::nobars(formulaGLM))[-1]) == 0)
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

    if(!lme4::isNested(data$MLFjk, data$MLFj))
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

    if(is.factor(data$.MLFj))
      data$.MLFj = as.character(data$.MLFj)

    data$Uj = 1

    Conv = FALSE
    iter = 1
    RelFactorsHist = list()
    FormulaGLM     = formula(paste0(deparse(formulaGLM, width.cutoff = 5e2), "+ offset(log(Uj))"))
    Covars         = all.vars(FormulaGLM)[-1]

    if(verbose)
      cat("\nRunning algorithm for single random effect.\n")

    # Run Buhlmann-Straub + GLM algorithm
    while(!Conv) {
      Start = Sys.time()

      #### 1. Fit GLM ####
      fitGLM = tryCatch(cpglm(FormulaGLM, data = data, link = "log", weights = wij,
                              control = list(bound.p = c(1.01, 1.99)),
                              optimizer = "bobyqa"),
                        error = function(e) TRUE,
                        warning = function(w) TRUE)
      if(is.logical(fitGLM))
        break

      p      = fitGLM$p
      muHat  = coef(fitGLM)[1]
      data[, Gammai := exp(as.vector(as(cplm::model.matrix(fitGLM, data = data)[, -1], "sparseMatrix") %*% coef(fitGLM)[-1]))]
      GammaiRaw = c(coef(fitGLM)[-1], p)

      #### 2. Backtransform data for Buhlmann-Straub model ####
      data[, Ytilde := Yij / Gammai]
      data[, wtilde := wij * Gammai^(2 - p)]

      BuhlmannStraub =
        if (muHatGLM) {
          eval(
            substitute(
              buhlmannStraub(Ytilde, wtilde, MLFj, data, muHat = exp(muHat), type = "multiplicative"),
              list(MLFj = as.name(MLFj))
            ))
        } else {
          eval(
            substitute(
              buhlmannStraub(Ytilde, wtilde, MLFj, data, type = "multiplicative"),
              list(MLFj = as.name(MLFj))
            ))
        }

      data[, Uj := BuhlmannStraub$Relativity$MLFj$Uj[match(.MLFj, BuhlmannStraub$Relativity$MLFj[[MLFj]])]]

      End = Sys.time()
      RelFactorsHist[[iter]] = GammaiRaw

      if(length(all.vars(FormulaGLM)[-1]) == 2)
        break

      if(iter > 1) {
        RelDiff = sqrt(crossprod(GammaiRaw - Gammai0)) / sqrt(crossprod(Gammai0))
        if(verbose & iter %% 2 == 0)
          cat("\nIteration", iter, ": relative difference =", RelDiff,
              "\nTime since last iteration = ", round(difftime(End, Start, units = "mins"), 2), "minutes\n")
        if(RelDiff  < epsilon)
          break
      }

      if(iter > maxiter)
        break

      iter = iter + 1
      Gammai0 = GammaiRaw
    }

    if(iter > maxiter)
      warning("Maximum number of iterations reached.", immediate. = TRUE)

    if(is.logical(fitGLM)) {
      warning("Convergence problems with model!")
      TmpForm = formula(paste0(all.vars(FormulaGLM)[1], " ~ 1"))
      fitGLM  = cpglm(TmpForm, data = data, link = "log", weights = wij,
                      control = list(bound.p = c(1.01, 1.99)),
                      optimizer = "bobyqa")
      fitGLM@converged = FALSE
      fitGLM@aic = Inf
    } else {
      fitGLM = cpglm(FormulaGLM, data = data, link = "log", weights = wij,
                     control = list(bound.p = c(1.01, 1.99)),
                     optimizer = "bobyqa")
    }

    # Store as initTw for consistency with nested case
    initTw = list(
      fitGLM = fitGLM,
      iter = iter,
      Converged = iter <= maxiter & fitGLM$converged
    )

    # Convert MLFj to factor
    data[[MLFj]] %<>% as.factor

    # Number of variance components for single random effect
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
















