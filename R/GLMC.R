#' Combining the Buhlmann-Straub credibility model with a GLM (Ohlsson, 2008)
#'
#' Fit a single-level random effects model using Ohlsson's methodology combined with Buhlmann-Straub credibility.
#' This is the single-level analogue of \code{\link{hierCredGLM}}.
#'
#' @param formula object of type \code{\link{formula}} that specifies which model should be fitted. Syntax follows
#' \code{\link[lme4]{lmer}}: e.g., \code{Y ~ x1 + x2 + (1 | Cluster)}. Only one random effect is allowed.
#' @param data an object that is coercible by \code{\link[data.table]{as.data.table}}, containing the variables in the model.
#' @param weights variable name of the exposure weight.
#' @param p the value for the power parameter of the Tweedie distribution, which is passed to \code{\link[statmod]{tweedie}}. Default is \code{1.5}.
#' @param link.power index of power link function, which is passed to \code{\link[statmod]{tweedie}}. \code{link.power = 0} produces a log-link.
#' Defaults to the canonical link, which is \code{1 - p}.
#' @param muHatGLM indicates which estimate has to be used in the algorithm for the intercept term. Default is \code{TRUE},
#'  which uses the intercept as estimated by the GLM. If \code{FALSE}, the estimate of the Buhlmann-Straub credibility model is used.
#' @param epsilon positive convergence tolerance \eqn{\epsilon}; the iterations converge when
#' \eqn{||\theta[k] - \theta[k - 1]||^2_2/||\theta[k - 1]||^2_2 < \epsilon}. Here, \eqn{\theta[k]} is the parameter vector at the \eqn{k^{th}} iteration.
#' @param maxiter maximum number of iterations.
#' @param maxiterGLM maximum number of iterations when fitting the GLM part. Passed to \code{glm}.
#' @param verbose logical indicating if output should be produced during the algorithm.
#' @param returnData logical indicating if input data has to be returned.
#' @param balanceProperty logical indicating if the balance property should be satisfied.
#' @param y logical indicating whether the response vector should be returned as a component of the returned value.
#' @param ... arguments passed to \code{glm}
#'
#' @return An object of type \code{buhlmannStraubGLM} with the following slots:
#' @return \item{call}{the matched call}
#' @return \item{CredibilityResults}{results of the Buhlmann-Straub credibility model.}
#' @return \item{fitGLM}{the results from fitting the GLM part.}
#' @return \item{iter}{total number of iterations.}
#' @return \item{Converged}{logical indicating whether the algorithm converged.}
#' @return \item{LevelsCov}{object that summarizes the unique levels of each of the contract-specific covariates.}
#' @return \item{fitted.values}{the fitted mean values, resulting from the model fit.}
#' @return \item{prior.weights}{the weights (exposure) initially supplied.}
#' @return \item{y}{if requested, the response vector. Default is \code{TRUE}.}
#'
#' @seealso \code{\link{buhlmannStraubGLM-class}}, \code{\link{buhlmannStraubTweedie}}, \code{\link{buhlmannStraub}}, \code{\link{plotRE}},
#' \code{\link{adjustIntercept}}, \code{\link{BalanceProperty}}, \code{\link{tweedieGLMM}}
#' @references Campo, B.D.C. and Antonio, Katrien (2023). Insurance pricing with hierarchically structured data an illustration with a workers' compensation insurance portfolio. \emph{Scandinavian Actuarial Journal}, doi: 10.1080/03461238.2022.2161413
#' @references Ohlsson, E. (2008). Combining generalized linear models and credibility models in practice. \emph{Scandinavian Actuarial Journal} \bold{2008}(4), 301–314.
#'
#' @examples
#' \donttest{
#' data("hachemeister", package = "actuar")
#' # Prepare data
#' X = as.data.frame(hachemeister)
#' Df = reshape(X, idvar = "state",
#'              varying = list(paste0("ratio.", 1:12), paste0("weight.", 1:12)),
#'              direction = "long")
#' # Add a covariate
#' Df$time_factor = factor(Df$time)
#' # Fit model
#' fit = buhlmannStraubGLM(ratio.1 ~ time_factor + (1 | state), Df,
#'                         weights = weight.1, p = 1.5)
#' summary(fit)
#' ranef(fit)
#' }
buhlmannStraubGLM <-
  function(formula, data, weights, p = 1.5, link.power = 0, muHatGLM = FALSE, epsilon = 1e-4,
           maxiter = 5e2, maxiterGLM = 5e2, verbose = FALSE, returnData = TRUE, balanceProperty = TRUE, y = TRUE, ...) {

    call = match.call()
    if(!is.logical(muHatGLM))
      stop("Argument muHatGLM has to be of type logical.")
    if(!is.formula(formula))
      stop("Has to be of type formula.")
    if(!all(all.vars(formula) %in% names(data)))
      stop("Did not find the variables in the formula in the dataframe")
    if(!is.data.table(data))
      data = as.data.table(data)

    ## Extract x, y, etc from the model formula and frame
    mc <- mcout <- match.call()
    mc[[1]] = quote(glFormula)
    for(i in names(mc) %>% .[!. %in% names(formals(glFormula))] %>% .[. != ""])
      mc[[i]] = NULL
    glmod   = eval(mc, parent.frame(1L))
    mcout$formula = glmod$formula
    glmod$formula = NULL
    glmod$REML    = NULL

    formulaGLM = nobars(formula)
    formulaRE  = findbars(formula)

    if(length(formulaRE) != 1)
      stop("Exactly one random effect must be specified, e.g., (1 | Cluster)")

    MLFj       = all.vars(formulaRE[[1]])
    data$Yijt   = model.extract(glmod$fr, "response")
    data$wijt   = model.extract(glmod$fr, "weights")
    if(is.null(data$wijt))
      data[, wijt := rep(1, nrow(data))]
    data$.MLFj = data[[MLFj]]

    if(length(all.vars(nobars(formulaGLM))[-1]) == 0) {
      warning("No contract-specific covariates specified. Returning results of the multiplicative Buhlmann-Straub credibility model", immediate. = TRUE)
      return(eval(
        substitute(
          buhlmannStraub(Yijt, wijt, MLFj, data, type = "multiplicative"),
          list(MLFj = as.name(MLFj))
        )))
    }

    if(any(table(data$.MLFj) == 1))
      warning("There are clusters with only 1 observation.", immediate. = TRUE)

    if(is.factor(data$.MLFj))
      data$.MLFj = as.character(data$.MLFj)

    data$Uj = 1
    Conv    = FALSE
    iter    = 1
    RelFactorsHist = list()
    FormulaGLM     = formula(paste0(deparse(formulaGLM, width.cutoff = 5e2), "+ offset(log(Uj))"))
    Covars         = all.vars(FormulaGLM)[-1]

    if(verbose)
      cat("\nRunning algorithm.\n")

    while(!Conv) {
      Start = Sys.time()

      #### 1. Fit GLM ####
      fitGLM = glm(FormulaGLM, data = data, weights = data$wijt,
                   family = tweedie(var.power = p, link.power = link.power),
                   control = glm.control(maxit = maxiterGLM), ...)
      muHat  = coef(fitGLM)[1]
      data[, Gammai := exp(as.vector(as(model.matrix(fitGLM, data = data)[, -1], "sparseMatrix") %*% coef(fitGLM)[-1]))]
      GammaiRaw = coef(fitGLM)[-1]

      #### 2. Backtransform data for Buhlmann-Straub model ####
      data[, Ytilde := Yijt / Gammai]
      data[, wtilde := wijt * Gammai^(2 - p)]

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
        if(verbose & iter %% 10 == 0)
          cat("\fIteration", iter, ": relative difference =", RelDiff,
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

    fitGLM = glm(FormulaGLM, data = data, weights = data$wijt,
                 family = tweedie(var.power = p, link.power = 0),
                 y = TRUE, model = TRUE, ...)
    fitGLM$prior.weights = data$wijt

    if(balanceProperty)
      fitGLM = adjustIntercept(fitGLM, data = data)

    fitted = fitted(fitGLM)

    LevelsCov = setNames(lapply(
      Covars, function(x) {
        if(is.factor(data[[x]]))
          levels(data[[x]])
        else
          unique(data[[x]])
      }), Covars)

    Results = structure(
      list(call = call,
           CredibilityResults = BuhlmannStraub,
           fitGLM = fitGLM,
           iter = iter,
           Converged = iter <= maxiter,
           LevelsCov = LevelsCov,
           fitted.values = fitted,
           prior.weights = fitGLM$prior.weights,
           y = if(y) fitGLM$y else NULL
      ),
      class = "buhlmannStraubGLM")

    if(returnData)
      Results$data = data

    return(Results)
  }

#' Combining the Buhlmann-Straub credibility model with a Tweedie GLM (Ohlsson, 2008)
#'
#' Fit a single-level random effects model using Ohlsson's methodology combined with Buhlmann-Straub credibility.
#' This function estimates the power parameter p. For fixed p, see \code{\link{buhlmannStraubGLM}}.
#'
#' @param formula object of type \code{\link{formula}} that specifies which model should be fitted. Syntax follows
#' \code{\link[lme4]{lmer}}: e.g., \code{Y ~ x1 + x2 + (1 | Cluster)}. Only one random effect is allowed.
#' @param data an object that is coercible by \code{\link[data.table]{as.data.table}}, containing the variables in the model.
#' @param weights variable name of the exposure weight.
#' @param muHatGLM indicates which estimate has to be used in the algorithm for the intercept term. Default is \code{TRUE},
#'  which uses the intercept as estimated by the GLM. If \code{FALSE}, the estimate of the Buhlmann-Straub credibility model is used.
#' @param epsilon positive convergence tolerance \eqn{\epsilon}; the iterations converge when
#' \eqn{||\theta[k] - \theta[k - 1]||^2_2/||\theta[k - 1]||^2_2 < \epsilon}. Here, \eqn{\theta[k]} is the parameter vector at the \eqn{k^{th}} iteration.
#' @param maxiter maximum number of iterations.
#' @param verbose logical indicating if output should be produced during the algorithm.
#' @param returnData logical indicating if input data has to be returned.
#' @param cpglmControl a list of parameters to control the fitting process in the GLM part. By default,
#' \code{cpglmControl = list(bound.p = c(1.01, 1.99))} which restricts the range of the power parameter p to [1.01, 1.99] in the fitting
#' process. This list is passed to \code{\link[cplm]{cpglm}}.
#' @param balanceProperty logical indicating if the balance property should be satisfied.
#' @param optimizer a character string that determines which optimization routine is to be used in estimating the index and the dispersion parameters.
#'  Possible choices are \code{"nlminb"} (the default, see \code{\link[stats]{nlminb}}), \code{"bobyqa"} (\code{\link[minqa]{bobyqa}}) and \code{"L-BFGS-B"} (\code{\link[stats]{optim}}).
#' @param y logical indicating whether the response vector should be returned as a component of the returned value.
#' @param ... arguments passed to \code{\link[cplm]{cpglm}}.
#'
#' @return An object of type \code{buhlmannStraubTweedie} with the following slots:
#' @return \item{call}{the matched call}
#' @return \item{CredibilityResults}{results of the Buhlmann-Straub credibility model.}
#' @return \item{fitGLM}{the results from fitting the GLM part.}
#' @return \item{iter}{total number of iterations.}
#' @return \item{Converged}{logical indicating whether the algorithm converged.}
#' @return \item{LevelsCov}{object that summarizes the unique levels of each of the contract-specific covariates.}
#' @return \item{fitted.values}{the fitted mean values, resulting from the model fit.}
#' @return \item{prior.weights}{the weights (exposure) initially supplied.}
#' @return \item{y}{if requested, the response vector. Default is \code{TRUE}.}
#'
#' @details When estimating the GLM part, this function uses the \code{\link[cplm]{cpglm}} function from the \code{cplm} package.
#'
#' @seealso \code{\link{buhlmannStraubTweedie-class}}, \code{\link{buhlmannStraub}}, \code{\link{buhlmannStraubGLM}},
#' \code{\link{hierCredTweedie}}, \code{\link[cplm]{cpglm}}, \code{\link{plotRE}}, \code{\link{adjustIntercept}}, \code{\link{BalanceProperty}}
#' @references Campo, B.D.C. and Antonio, Katrien (2023). Insurance pricing with hierarchically structured data an illustration with a workers' compensation insurance portfolio. \emph{Scandinavian Actuarial Journal}, doi: 10.1080/03461238.2022.2161413
#' @references Ohlsson, E. (2008). Combining generalized linear models and credibility models in practice. \emph{Scandinavian Actuarial Journal} \bold{2008}(4), 301–314.
#'
#' @examples
#' \donttest{
#' data("hachemeister", package = "actuar")
#' X = as.data.frame(hachemeister)
#' Df = reshape(X, idvar = "state",
#'              varying = list(paste0("ratio.", 1:12), paste0("weight.", 1:12)),
#'              direction = "long")
#' Df$time_factor = factor(Df$time)
#' fit = buhlmannStraubTweedie(ratio.1 ~ time_factor + (1 | state), Df,
#'                             weights = weight.1, epsilon = 1e-6)
#' summary(fit)
#' ranef(fit)
#' fixef(fit)
#' }
buhlmannStraubTweedie <-
  function(formula, data, weights, muHatGLM = FALSE, epsilon = 1e-4,
           maxiter = 5e2, verbose = FALSE, returnData = TRUE, cpglmControl = list(bound.p = c(1.01, 1.99)),
           balanceProperty = TRUE, optimizer = "bobyqa", y = TRUE, ...) {

    call = match.call()
    if(!is.logical(muHatGLM))
      stop("Argument muHatGLM has to be of type logical.")
    if(!is.formula(formula))
      stop("Has to be of type formula.")
    if(!all(all.vars(formula) %in% names(data)))
      stop("Did not find the variables in the formula in the dataframe")
    if(!is.data.table(data))
      data = as.data.table(data)

    ## Extract x, y, etc from the model formula and frame
    mc <- mcout <- match.call()
    mc[[1]] = quote(glFormula)
    for(i in names(mc) %>% .[!. %in% names(formals(glFormula))] %>% .[. != ""])
      mc[[i]] = NULL
    glmod   = eval(mc, parent.frame(1L))
    mcout$formula = glmod$formula
    glmod$formula = NULL
    glmod$REML    = NULL

    formulaGLM = nobars(formula)
    formulaRE  = findbars(formula)

    if(length(formulaRE) != 1)
      stop("Exactly one random effect must be specified, e.g., (1 | Cluster)")

    MLFj  = all.vars(formulaRE[[1]])

    data$Yijt   = model.extract(glmod$fr, "response")
    data$wijt   = model.extract(glmod$fr, "weights")
    data$.MLFj  = data[[MLFj]]

    if(length(all.vars(nobars(formulaGLM))[-1]) == 0) {
      warning("No contract-specific covariates specified. Returning results of the multiplicative Buhlmann-Straub credibility model", immediate. = TRUE)
      return(eval(
        substitute(
          buhlmannStraub(Yijt, wijt, MLFj, data, type = "multiplicative"),
          list(MLFj = as.name(MLFj))
        )))
    }

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
      cat("\nRunning algorithm.\n")

    while(!Conv) {
      Start = Sys.time()

      #### 1. Fit GLM ####
      fitGLM = tryCatch(cpglm(FormulaGLM, data = data, link = "log", weights = wijt, control = cpglmControl,
                              optimizer = optimizer, ...),
                        error = function(e) TRUE,
                        warning = function(w) TRUE)
      if(is.logical(fitGLM))
        break

      p      = fitGLM$p
      muHat  = coef(fitGLM)[1]
      data[, Gammai := exp(as.vector(as(cplm::model.matrix(fitGLM, data = data)[, -1], "sparseMatrix") %*% coef(fitGLM)[-1]))]
      GammaiRaw = c(coef(fitGLM)[-1], p)

      #### 2. Backtransform data for Buhlmann-Straub model ####
      data[, Ytilde := Yijt / Gammai]
      data[, wtilde := wijt * Gammai^(2 - p)]

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
      fitGLM  = cpglm(TmpForm, data = data, link = "log", weights = wijt, control = cpglmControl,
                      optimizer = optimizer, ...)
      fitGLM@converged = FALSE
      fitGLM@aic = Inf
      BuhlmannStraub = list()
    } else {
      fitGLM = cpglm(FormulaGLM, data = data, link = "log",
                     weights = wijt, control = cpglmControl,
                     optimizer = optimizer, ...)
      if(balanceProperty)
        fitGLM = adjustIntercept(fitGLM, data)
    }

    Convergence = (iter <= maxiter & fitGLM$converged)
    fitted = fitted(fitGLM)

    LevelsCov   = setNames(lapply(
      Covars, function(x) {
        if(is.factor(data[[x]]))
          levels(data[[x]])
        else
          unique(data[[x]])
      }), Covars)

    Results = structure(
      list(call = call,
           CredibilityResults = BuhlmannStraub,
           fitGLM = fitGLM,
           iter = iter,
           Converged = Convergence,
           LevelsCov = LevelsCov,
           fitted.values = fitted,
           prior.weights = fitGLM@prior.weights,
           y = if(y) fitGLM@y else NULL
      ),
      class = "buhlmannStraubTweedie")

    if(returnData)
      Results$data = data

    return(Results)
  }


utils::globalVariables(c("Gammai", "Ytilde", "Yijt", "wtilde", "wijt", "Uj", ".MLFj"))
