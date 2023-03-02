#' Class "hierCredibility" of fitted hierarchical credibility models
#'
#' @name hierCredibility-class
#' @method print hierCredibility
#' @param x an object of class \code{\link{hierCredibility}}
#' @param object an object of class \code{\link{hierCredibility}}
#' @param ... currently ignored.
#' @seealso \code{\link{hierCredibility}}
#'
#'
#' @section {S3 methods}:
#' \describe{
#'  \item{\code{print}:}{Prints the \code{call}, the estimated variance parameters and the unique number of categories
#'   of the hierarchical MLF. The \code{...} argument is currently ignored. Returns an invisible copy of the original
#'   object.}
#'  \item{\code{summary}:}{In addition to the output of the \code{print.hierCredibility} function, the \code{summary} function
#'   prints the random effect estimates as well. Returns an invisible copy of the original object.}
#'   \item{\code{fitted}:}{Returns the fitted values.}
#' }
#'
#' @return The function \code{\link{hierCredibility}} returns an object of class \code{hierCredibility}, which has the following slots:
#' @return \item{call}{the matched call}
#' @return \item{type}{Whether additive or multiplicative hierarchical credibility model is used.}
#' @return \item{Variances}{The estimated variance components. \code{s2} is the estimated variance of the individual contracts,
#'  \code{tausq} the estimate of \eqn{Var(V[j])} and \code{nusq} is the estimate of \eqn{Var(V[jk])}.}
#' @return \item{Means}{The estimated averages at the portfolio level (intercept term \eqn{\mu}), at the first
#' hierarchical level (\eqn{bar(Y)[\%.\% j \%.\% \%.\%]^z}) and at the second hierarchical level (\eqn{bar(Y)[\%.\% jk \%.\%]}).}
#' @return \item{Weights}{The weights at the first hierarchical level \eqn{z[j\%.\%]} and at the second hierarchical level \eqn{w[\%.\%jk\%.\%]}.}
#' @return \item{Credibility}{The credibility weights at the first hierarchical level \eqn{q[j\%.\%]} and at the second hierarchical level \eqn{z[jk]}.}
#' @return \item{Premiums}{The overall expectation \eqn{widehat(\mu)}, sector expectation \eqn{widehat(V)[j]} and group expectation \eqn{widehat(V)[jk]}.}
#' @return \item{Relativity}{The estimated random effects \eqn{widehat(U)[j]} and \eqn{widehat(U)[jk]} of the sector and group, respectively.}
#' @return \item{RawResults}{Objects of type \code{data.table} with all intermediate results.}
#' @return \item{fitted.values}{the fitted mean values, resulting from the model fit.}
print.hierCredibility <- function(x, ...) {
  cat("Call:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  Sect = x$Hierarchy$sector
  Grp  = x$Hierarchy$group
  cat(paste0("\n", .capitalize(x$type), " hierarchical credibility model\n\n"))
  cat("Estimated variance parameters:\n")
  cat("  Individual contracts:", x$Variances[1], "\n")
  cat("  Var(V[jk]):", x$Variances[2], "\n")
  cat("  Var(V[j]):", x$Variances[3], "\n")
  cat(paste0("Unique number of categories of ", x$Hierarchy$sector, ": ", NrUnique(x$RawResults$Dfj[[Sect]]), "\n"))
  cat(paste0("Unique number of categories of ", x$Hierarchy$group, ": ", NrUnique(x$RawResults$Dfjk[[Grp]])))
  return(invisible(x))
}
#' @rdname hierCredibility-class
#' @method summary hierCredibility
summary.hierCredibility <- function(object, ...) {
  cat("Call:\n",
      paste(deparse(object$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  Sect = object$Hierarchy$sector
  Grp  = object$Hierarchy$group
  cat(paste0("\n", .capitalize(object$type), " hierarchical credibility model\n\n"))
  cat("Estimated variance parameters:\n")
  cat("  Individual contracts:", object$Variances[1], "\n")
  cat("  Var(V[jk]):", object$Variances[2], "\n")
  cat("  Var(V[j]):", object$Variances[3], "\n")

  cat(paste0("Unique number of categories of ", object$Hierarchy$sector, ": ", NrUnique(object$RawResults$Dfj[[Sect]]), "\n"))
  cat(paste0("Unique number of categories of ", object$Hierarchy$group, ": ", NrUnique(object$RawResults$Dfjk[[Grp]])), "\n\n")

  cat("Estimates at the", object$Hierarchy$sector, "level:\n\n")
  Dfj = object$RawResults$Dfj
  print(Dfj[, !colnames(Dfj) %in% c("wj", "Yj_BarTilde"), with = F], ...)

  cat("\nEstimates at the", object$Hierarchy$group, "level:\n\n")
  Dfjk = object$RawResults$Dfjk
  print(Dfjk[, !colnames(Dfjk) %in% c("Vj", "Yj_BarTilde"), with = F], ...)
  return(invisible(object))
}
#' Class "hierCredGLM" of fitted random effects models estimated with Ohlsson's GLMC algorithm
#'
#' @name hierCredGLM-class
#' @method print hierCredGLM
#' @param x an object of class \code{\link{hierCredGLM}}
#' @param object an object of class \code{\link{hierCredGLM}}
#' @param ... currently ignored.
#' @seealso \code{\link{hierCredGLM}}
#'
#' @section {S3 methods}:
#' \describe{
#'  \item{\code{print}:}{Prints the \code{call}, the estimated variance parameters, the unique number of categories
#'   of the hierarchical MLF and the output of the GLM part. The \code{...} argument is currently ignored. Returns an
#'   invisible copy of the original object.}
#'  \item{\code{summary}:}{In addition to the output of the \code{print.hierCredGLM} function, the \code{summary} function
#'   also prints the random effect estimates and a summary of the GLM (see \code{\link{summary.speedglm}}). Returns an
#'   invisible copy of the original object.}
#'   \item{\code{fitted}:}{Returns the fitted values.}
#' }
#'
#'
#' @return The function \code{\link{hierCredGLM}} returns an object of class \code{hierCredGLM}, which has the following slots:
#' @return \item{call}{the matched call}
#' @return \item{HierarchicalResults}{results of the hierarchical credibility model.}
#' @return \item{fitGLM}{the results from fitting the GLM part.}
#' @return \item{iter}{total number of iterations.}
#' @return \item{Converged}{logical indicating whether the algorithm converged.}
#' @return \item{LevelsCov}{object that summarizes the unique levels of each of the contract-specific covariates.}
#' @return \item{fitted.values}{the fitted mean values, resulting from the model fit.}
#' @return \item{prior.weights}{the weights (exposure) initially supplied.}
#' @return \item{y}{if requested, the response vector. Default is \code{TRUE}.}
print.hierCredGLM <- function(x, ...) {
  cat("Call:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  Sect = x$HierarchicalResults$Hierarchy$sector
  Grp  = x$HierarchicalResults$Hierarchy$group
  cat("\nCombination of the hierarchical credibility model with a GLM\n\n")
  cat("Estimated variance parameters:\n")
  cat("  Var(V[jk]):", x$HierarchicalResults$Variances[2], "\n")
  cat("  Var(V[j]):", x$HierarchicalResults$Variances[3], "\n")
  cat(paste0("Unique number of categories of ", x$HierarchicalResults$Hierarchy$sector, ": ", NrUnique(x$HierarchicalResults$RawResults$Dfj[[Sect]]), "\n"))
  cat(paste0("Unique number of categories of ", x$HierarchicalResults$Hierarchy$group, ": ", NrUnique(x$HierarchicalResults$RawResults$Dfjk[[Grp]])))
  cat("\n\nResults contract-specific risk factors:\n\n")
  print(x$fitGLM)
  return(invisible(x))
}

#' @rdname hierCredGLM-class
#' @method summary hierCredGLM
summary.hierCredGLM <- function(object, ...) {
  cat("Call:\n",
      paste(deparse(object$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  Sect = object$HierarchicalResults$Hierarchy$sector
  Grp  = object$HierarchicalResults$Hierarchy$group
  cat("\nCombination of the hierarchical credibility model with a GLM\n\n")

  cat("Estimated variance parameters:\n")
  cat("  Individual contracts:", object$HierarchicalResults$Variances[1], "\n")
  cat("  Var(V[jk]):", object$HierarchicalResults$Variances[2], "\n")
  cat("  Var(V[j]):", object$HierarchicalResults$Variances[3], "\n")
  cat(paste0("Unique number of categories of ", object$HierarchicalResults$Hierarchy$sector, ": ", NrUnique(object$HierarchicalResults$RawResults$Dfj[[Sect]]), "\n"))
  cat(paste0("Unique number of categories of ", object$HierarchicalResults$Hierarchy$group, ": ", NrUnique(object$HierarchicalResults$RawResults$Dfjk[[Grp]])))
  cat("\n\nResults contract-specific risk factors:\n\n")
  print(summary(object$fitGLM))
  return(invisible(object))
}

#' Class "hierCredTweedie" of fitted random effects models estimated with Ohlsson's GLMC algorithm
#'
#' @name hierCredTweedie-class
#' @method print hierCredTweedie
#' @param x an object of class \code{\link{hierCredTweedie}}
#' @param object an object of class \code{\link{hierCredTweedie}}
#' @param ... currently ignored.
#' @seealso \code{\link{hierCredTweedie}}
#'
#' @section {S3 methods}:
#' \describe{
#'  \item{\code{print}:}{Prints the \code{call}, the estimated variance parameters, the unique number of categories
#'   of the hierarchical MLF and the output of the GLM part. The \code{...} argument is currently ignored. Returns an
#'   invisible copy of the original object.}
#'  \item{\code{summary}:}{In addition to the output of the \code{print.hierCredTweedie} function, the \code{summary} function
#'   also prints the random effect estimates and a summary of the GLM (see \code{\link{summary.speedglm}}). Returns an
#'    invisible copy of the original object.}
#'    \item{\code{fitted}:}{Returns the fitted values.}
#' }
#'
#' @return The function \code{\link{hierCredGLM}} returns an object of class \code{hierCredGLM}, which has the following slots:
#' @return \item{call}{the matched call}
#' @return \item{HierarchicalResults}{results of the hierarchical credibility model.}
#' @return \item{fitGLM}{the results from fitting the GLM part.}
#' @return \item{iter}{total number of iterations.}
#' @return \item{Converged}{logical indicating whether the algorithm converged.}
#' @return \item{LevelsCov}{object that summarizes the unique levels of each of the contract-specific covariates.}
#' @return \item{fitted.values}{the fitted mean values, resulting from the model fit.}
#' @return \item{prior.weights}{the weights (exposure) initially supplied.}
#' @return \item{y}{if requested, the response vector. Default is \code{TRUE}.}
print.hierCredTweedie <- print.hierCredGLM
#' @rdname hierCredTweedie-class
#' @method summary hierCredTweedie
summary.hierCredTweedie <- summary.hierCredGLM


.onAttach <- function(libname, pkgname) {
  msg = c(
    "              _                 ______  _____ ",
    "             | |                | ___ \\|  ___|",
    "  __ _   ___ | |_  _   _   __ _ | |_/ /| |__  ",
    " / _` | / __|| __|| | | | / _` ||    / |  __| ",
    "| (_| || (__ | |_ | |_| || (_| || |\\ \\ | |___ ",
    " \\__,_| \\___| \\__| \\__,_| \\__,_|\\_| \\_|\\____/ ",
    "\nType 'citation(\"actuaRE\")' for citing this R package in publications."
  )
  if(!interactive())
    msg <- paste("\nPackage 'actuaRE' version", packageVersion("actuaRE"))


  for(i in seq_along(msg)) {
    packageStartupMessage("\r", msg[[i]])
    Sys.sleep(0.075)
  }
  invisible()
  packageStartupMessage("\nThis is version ", packageVersion(pkgname), " of ", pkgname)
}


#' Print method for an object of class \code{BalanceProperty}
#'
#' @param x an object of type \code{BalanceProperty}
#' @param ... Currently ignored.
#' @seealso \code{\link{BalanceProperty}}
#'
#' @return Prints the call and whether the balance property is satisfied or not. Returns an invisible copy
#' of the original object.
print.BalanceProperty <- function(x, ...) {
  cat("Call:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  if(x$BalanceProperty) {
    cat("\nBalance property is satisfied.\n\n")
  } else {
    warning("\nBalance property is not satisfied.\n", immediate. = T)
    cat("\nRatio total observed damage to total predicted damage:", x$Alpha, "\n\n")
  }
  invisible(x)
}

#' @rdname hierCredibility-class
#' @method fitted hierCredibility
fitted.hierCredibility <- function(object, ...) object$fitted.values

#' @rdname hierCredGLM-class
#' @method fitted hierCredGLM
fitted.hierCredGLM <- function(object, ...) object$fitted.values

#' @rdname hierCredTweedie-class
#' @method fitted hierCredTweedie
fitted.hierCredTweedie <- function(object, ...) object$fitted.values

