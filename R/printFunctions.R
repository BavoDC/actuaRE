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
print.hierCredTweedie <- print.hierCredGLM
#' @rdname hierCredTweedie-class
#' @method summary hierCredTweedie
summary.hierCredTweedie <- summary.hierCredGLM

.startactuaRE <- function() {
  msg = c(
    "              _                 ______  _____ ",
    "             | |                | ___ \\|  ___|",
    "  __ _   ___ | |_  _   _   __ _ | |_/ /| |__  ",
    " / _` | / __|| __|| | | | / _` ||    / |  __| ",
    "| (_| || (__ | |_ | |_| || (_| || |\\ \\ | |___ ",
    " \\__,_| \\___| \\__| \\__,_| \\__,_|\\_| \\_|\\____/ "
  )


  for(i in seq_along(msg)) {
    cat("\r", msg[[i]])
    cat("\n")
    Sys.sleep(0.075)
  }
  cat("\n\nType 'citation(\"actuaRE\")' for citing this R package in publications.")
}

.onAttach <- function(libname, pkgname) {
  msg <- .startactuaRE()
  if(!interactive())
    msg[1] <- paste("Package 'actuaRE' version", packageVersion("actuaRE"))
  packageStartupMessage(msg)
  invisible()
  packageStartupMessage("This is version ", packageVersion(pkgname), " of ", pkgname)
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

