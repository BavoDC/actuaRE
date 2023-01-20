#' Combining the hierarchical credibility model with a GLM (Ohlsson, 2008)
#'
#' Fit a random effects model using Ohlsson's methodology. In this function you explicitly specify the power parameter p.
#' See \code{\link{hierCredTweedie}} when you also want to estimate the p.
#'
#' @param formula object of type \code{\link{formula}} that specifies which model should be fitted. Syntax is the same as for
#' \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}}. For example, \code{Yijkt ~ x1 + x2 + (1 | Industry / Branch)}.
#' @param data an object that is coercible by \code{\link[data.table]{as.data.table}}, containing the variables in the model.
#' @param weights variable name of the exposure weight.
#' @param p the value for the power parameter of the Tweedie distribution, which is passed to \code{\link[statmod]{tweedie}}. Default is \code{1.5}.
#' @param link.power index of power link function, which is passed to \code{\link[statmod]{tweedie}}. \code{link.power = 0} produces a log-link.
#' Defaults to the canonical link, which is \code{1 - p}.
#' @param muHatGLM indicates which estimate has to be used in the algorithm for the intercept term. Default is \code{TRUE},
#'  which used the intercept as estimated by the GLM. If \code{FALSE}, the estimate of the hierarchical credibility model is used.
#' @param epsilon positive convergence tolerance \eqn{\epsilon}; the iterations converge when 7
#' \eqn{||\theta[k] - \theta[k - 1]||^2[[2]]/||\theta[k - 1]||^2[[2]] < \epsilon}. Here, \eqn{\theta[k]} is the parameter vector at the \eqn{k^{th}} iteration.
#' @param maxiter maximum number of iterations.
#' @param maxiterGLM maximum number of iterations when fitting the GLM part. Passed to \code{\link[speedglm]{speedglm}}.
#' @param verbose logical indicating if output should be produced during the algorithm.
#' @param returnData logical indicating if input data has to be returned.
#' @param balanceProperty logical indicating if the balance property should be satisfied.
#' @param y logical indicating whether the response vector should be returned as a component of the returned value.
#' @param ... arguments passed to \code{\link[speedglm]{speedglm}}
#'
#' @return An object of type \code{hierCredGLM} with the following slots:
#' @return \item{call}{the matched call}
#' @return \item{HierarchicalResults}{results of the hierarchical credibility model.}
#' @return \item{fitGLM}{the results from fitting the GLM part.}
#' @return \item{iter}{total number of iterations.}
#' @return \item{Converged}{logical indicating whether the algorithm converged.}
#' @return \item{LevelsCov}{object that summarizes the unique levels of each of the contract-specific covariates.}
#' @return \item{fitted.values}{the fitted mean values, resulting from the model fit.}
#' @return \item{prior.weights}{the weights (exposure) initially supplied.}
#' @return \item{y}{if requested, the response vector. Default is \code{TRUE}.}
#'
#' @seealso \code{\link{hierCredibility}}, \code{\link{hierCredTweedie}}
#' @references Ohlsson, E. (2008). Combining generalized linear models and credibility models in practice. \emph{Scandinavian Actuarial Journal} \bold{2008}(4), 301–314.
#'
#' @examples
#' data("dataCar")
#' fit = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w,
#' p = 1.7)
#' fit
#' summary(fit)
#' ranef(fit)
#' fixef(fit)
hierCredGLM <-
  function(formula, data, weights, p = 1.5, link.power = 0, muHatGLM = TRUE, epsilon = 1e-4,
           maxiter = 5e2, maxiterGLM = 5e2, verbose = FALSE, returnData = TRUE,  balanceProperty = TRUE, y = TRUE, ...) {
    # Combining the hierarchical credibility model with a GLM (Ohlsson, 2008)
    call = match.call()
    if(!is.logical(muHatGLM))
      stop("Argument muHatGLM has to be of type logical.")
    if(!is.formula(formula))
      stop("Has to be of type formula.")
    if(!all(all.vars(formula) %in% names(data)))
      stop("Did not find the variables in the formula in the dataframe")

    # Fix 'No visible global binding for global variable' note
    # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when



    ## Copied from lme4: glmer
    ## extract x, y, etc from the model formula and frame
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
    if(length(formulaRE) != 2)
      stop("Number of nested random effects is limited to two.")
    if(!any(sapply(formulaRE, function(x) grepl(":", deparse(x)))))
      stop("No nested random effects specified.")
    MLFj  = all.vars(formulaRE[!sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])
    MLFjk = all.vars(formulaRE[sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])[1]

    data$Yijkt   = model.extract(glmod$fr, "response")
    data$wijkt   = model.extract(glmod$fr, "weights")
    data$.MLFj    = data[[MLFj]]
    data$.MLFjk   = data[[MLFjk]]
    if(!isNested(data$.MLFjk, data$.MLFj))
      stop(paste(.MLFjk, "is not nested within", .MLFj))
    if(length(all.vars(nobars(formulaGLM))[-1]) == 0) {
      warning("No contract-specific covariates specified. Returning results of the multiplicative hierarchical credibility model", immediate. = T)
      return(eval(
        substitute(
          hierCredibility(Ytilde, wijkt, MLFj, MLFjk, data, type = "multiplicative"),
          list(MLFj = as.name(MLFj), MLFjk = as.name(MLFjk))
        )))
    }

    if(any(table(data$.MLFj) == 1) | any(table(data$.MLFjk) == 1))
      warning("There are categories with only 1 observation.", immediate. = T)

    if(is.factor(data$.MLFj))
      data$.MLFj %<>%
      as.character
    if(is.factor(data$.MLFjk))
      data$.MLFjk %<>%
      as.character
    data$Uj = data$Ujk = 1

    Conv = F
    iter = 1
    RelFactorsHist = list()
    FormulaGLM     = formula(paste0(deparse(formulaGLM, width.cutoff = 5e2), "+ offset(log(Uj)) + offset(log(Ujk))"))
    Covars         = all.vars(FormulaGLM)[-1]
    cat("\rRunning algorithm.")
    while(!Conv) {
      Start = Sys.time()
      #### 1. Fit GLM ####
      fitGLM = speedglm(FormulaGLM, data = data, weights = data$wijkt, family = tweedie(var.power = p, link.power = link.power),
                        maxit = maxiterGLM, ...)
      muHat  = coef(fitGLM)[1]
      data[, Gammai := exp(as.vector(as(model.matrix(fitGLM, data = data)[, -1], "sparseMatrix") %*% coef(fitGLM)[-1]))]
      GammaiRaw = coef(fitGLM)[-1]

      #### 2. Backtransform data for Hierarchical model ####
      data[, Ytilde := Yijkt / Gammai]
      data[, wtilde := wijkt * Gammai^(2 - p)]

      Jewell =
        if (muHatGLM) {
          eval(
            substitute(
            hierCredibility(Ytilde, wijkt, MLFj, MLFjk, data, muHat = exp(muHat), type = "multiplicative"),
            list(MLFj = as.name(MLFj), MLFjk = as.name(MLFjk))
          ))
        } else {
          eval(
            substitute(
            hierCredibility(Ytilde, wijkt, MLFj, MLFjk, data, type = "multiplicative"),
            list(MLFj = as.name(MLFj), MLFjk = as.name(MLFjk))
          ))
        }
      data[, `:=` (
        Uj  = Jewell$Relativity$sector$Uj[match(.MLFj, Jewell$Relativity$sector[[MLFj]])],
        Ujk = Jewell$Relativity$group$Ujk[match(.MLFjk, Jewell$Relativity$group[[MLFjk]])]
      )]

      End = Sys.time()
      RelFactorsHist[[iter]] = GammaiRaw
      if(length(all.vars(FormulaGLM)[-1]) == 2)
        break
      if(iter > 1) {
        RelDiff = sqrt(crossprod(GammaiRaw - Gammai0)) / sqrt(crossprod(Gammai0))
        if(verbose & iter %% 10 == 0)
          cat("\fIteration", iter, ": relative difference =", RelDiff,
              "\nTime since last iteration = ", round(difftime(End, Start, units = "mins"), 2), "minutes")
        if(RelDiff  < epsilon)
          break
      }

      if(iter > maxiter)
        break
      iter = iter + 1
      Gammai0 = GammaiRaw
    }
    if(iter > maxiter)
      warning("Maximum number of iterations reached.", immediate. = T)
    fitGLM = speedglm(FormulaGLM, data = data, weights = data$wijkt, family = tweedie(var.power = p, link.power = 0),
                      fitted = T, y = T, model = T, ...)
    fitGLM$prior.weights = data$wijkt
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
           HierarchicalResults = Jewell,
           fitGLM = fitGLM,
           iter = iter,
           Converged = iter <= maxiter,
           LevelsCov = LevelsCov,
           fitted.values = fitted,
           prior.weights = fitGLM$prior.weights,
           y = if(y) fitGLM$y else NULL
           ),
      class = "hierCredGLM")
    if(returnData)
      Results$data = data
    return(
      Results
    )
  }

#' Combining the hierarchical credibility model with a GLM (Ohlsson, 2008)
#'
#' Fit a random effects model using Ohlsson's methodology. In this function you estimate the power parameter p. See \code{hierCredGLM} when
#' you want fix p.
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
#' @return An object of type \code{hierCredTweedie} with the following slots:
#' @return \item{call}{the matched call}
#' @return \item{HierarchicalResults}{results of the hierarchical credibility model.}
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
#' @seealso \code{\link{hierCredibility}}, \code{\link{hierCredGLM}}, \code{\link[cplm]{cpglm}}
#' @references Ohlsson, E. (2008). Combining generalized linear models and credibility models in practice. \emph{Scandinavian Actuarial Journal} \bold{2008}(4), 301–314.
#'
#' @examples
#' \dontrun{
#' data("dataCar")
#' fit = hierCredTweedie(Y ~ area + (1 | VehicleType / VehicleBody), dataCar,
#' weights = w, epsilon = 1e-6)
#' fit
#' summary(fit)
#' ranef(fit)
#' fixef(fit)
#' }
hierCredTweedie <-
  function(formula, data, weights, muHatGLM = TRUE, epsilon = 1e-4,
           maxiter = 5e2, verbose = FALSE, returnData = TRUE, cpglmControl = list(bound.p = c(1.01, 1.99)),
           balanceProperty = TRUE, optimizer = "bobyqa", y = TRUE, ...) {
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
    mc[[1]] = quote(glFormula)
    for(i in names(mc) %>% .[!. %in% names(formals(glFormula))] %>% .[. != ""])
      mc[[i]] = NULL
    glmod   = eval(mc, parent.frame(1L))
    mcout$formula = glmod$formula
    glmod$formula = NULL
    glmod$REML    = NULL

    formulaGLM = nobars(formula)
    formulaRE  = findbars(formula)
    if(length(formulaRE) != 2)
      stop("Number of nested random effects is limited to two.")
    if(!any(sapply(formulaRE, function(x) grepl(":", deparse(x)))))
      stop("No nested random effects specified.")
    MLFj  = all.vars(formulaRE[!sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])
    MLFjk = all.vars(formulaRE[sapply(formulaRE, function(x) grepl(":", deparse(x)))][[1]])[1]

    data$Yijkt   = model.extract(glmod$fr, "response")
    data$wijkt   = model.extract(glmod$fr, "weights")
    data$.MLFj    = data[[MLFj]]
    data$.MLFjk   = data[[MLFjk]]
    if(!isNested(data$.MLFjk, data$.MLFj))
      stop(paste(.MLFjk, "is not nested within", .MLFj))
    if(length(all.vars(nobars(formulaGLM))[-1]) == 0) {
      warning("No contract-specific covariates specified. Returning results of the multiplicative hierarchical credibility model", immediate. = T)
      return(eval(
        substitute(
          hierCredibility(Ytilde, wijkt, MLFj, MLFjk, data, type = "multiplicative"),
          list(MLFj = as.name(MLFj), MLFjk = as.name(MLFjk))
        )))
    }

    if(any(table(data$.MLFj) == 1) | any(table(data$.MLFjk) == 1))
      warning("There are categories with only 1 observation.", immediate. = T)

    if(is.factor(data$.MLFj))
      data$.MLFj %<>%
      as.character
    if(is.factor(data$.MLFjk))
      data$.MLFjk %<>%
      as.character
    data$Uj = data$Ujk = 1

    Conv = F
    iter = 1
    RelFactorsHist = list()
    FormulaGLM     = formula(paste0(deparse(formulaGLM, width.cutoff = 5e2), "+ offset(log(Uj)) + offset(log(Ujk))"))
    Covars         = all.vars(FormulaGLM)[-1]

    cat("\nRunning algorithm.")
    while(!Conv) {
      Start = Sys.time()
      #### 1. Fit GLM ####
      fitGLM = tryCatch(cpglm(FormulaGLM, data = data, link = "log", weights = wijkt, control = cpglmControl,
                              optimizer = optimizer, ...),
                        error = function(e) T,
                        warning = function(w) T)
      if(is.logical(fitGLM))
        break
      p      = fitGLM$p
      muHat  = coef(fitGLM)[1]
      data[, Gammai := exp(as.vector(as(cplm::model.matrix(fitGLM, data = data)[, -1], "sparseMatrix") %*% coef(fitGLM)[-1]))]
      GammaiRaw = c(coef(fitGLM)[-1], p)

      #### 2. Backtransform data for Hierarchical model ####
      data[, Ytilde := Yijkt / Gammai]
      data[, wtilde := wijkt * Gammai^(2 - p)]

      Jewell =
        if (muHatGLM) {
          eval(
            substitute(
              hierCredibility(Ytilde, wijkt, MLFj, MLFjk, data, muHat = exp(muHat), type = "multiplicative"),
              list(MLFj = as.name(MLFj), MLFjk = as.name(MLFjk))
            ))
        } else {
          eval(
            substitute(
              hierCredibility(Ytilde, wijkt, MLFj, MLFjk, data, type = "multiplicative"),
              list(MLFj = as.name(MLFj), MLFjk = as.name(MLFjk))
            ))
        }
      data[, `:=` (
        Uj  = Jewell$Relativity$sector$Uj[match(.MLFj, Jewell$Relativity$sector[[MLFj]])],
        Ujk = Jewell$Relativity$group$Ujk[match(.MLFjk, Jewell$Relativity$group[[MLFjk]])]
      )]

      End = Sys.time()
      RelFactorsHist[[iter]] = GammaiRaw
      if(length(all.vars(FormulaGLM)[-1]) == 2)
        break
      if(iter > 1) {
        RelDiff = sqrt(crossprod(GammaiRaw - Gammai0)) / sqrt(crossprod(Gammai0))
        if(verbose & iter %% 2 == 0)
          cat("\nIteration", iter, ": relative difference =", RelDiff,
              "\nTime since last iteration = ", round(difftime(End, Start, units = "mins"), 2), "minutes")
        if(RelDiff  < epsilon)
          break
      }

      if(iter > maxiter)
        break
      iter = iter + 1
      Gammai0 = GammaiRaw
    }
    if(iter > maxiter)
      warning("Maximum number of iterations reached.", immediate. = T)
    if(is.logical(fitGLM)) {
      warning("Convergence problems with model!")
      TmpForm = formula(paste0(all.vars(FormulaGLM)[1], " ~ 1"))
      fitGLM  = cpglm(TmpForm, data = data, link = "log", weights = wijkt, control = cpglmControl,
                      optimizer = optimizer, ...)
      fitGLM@converged = F
      fitGLM@aic = Inf
      Jewell = list()
    } else {
      fitGLM = cpglm(FormulaGLM, data = data, link = "log",
                     weights = wijkt, control = cpglmControl,
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
           HierarchicalResults = Jewell,
           fitGLM = fitGLM,
           iter = iter,
           Converged = Convergence,
           LevelsCov = LevelsCov,
           fitted.values = fitted,
           prior.weights = fitGLM@prior.weights,
           y = if(y) fitGLM@y else NULL
      ),
      class = "hierCredTweedie")
    if(returnData)
      Results$data = data
    return(
      Results
    )
  }
utils::globalVariables(c(".", ".MLFj", ".MLFjk", "Gammai", "Ytilde", "Yijkt", "wtilde", "wijkt", "Yjk_BarTilde", "wjk",
                         "wjsq", "zjk", "qj", "Vj", "Vjk", "Uj", "Ujk", "..REs"))

