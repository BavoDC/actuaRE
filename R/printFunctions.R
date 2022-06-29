#' @inherit base::print.default
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
}

#' @inherit base::summary
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
}

#' @inherit base::print.default
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
}

#' @inherit base::summary
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
}

#' @inherit base::print.default
print.hierCredTweedie <- print.hierCredGLM
#' @inherit base::summary
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

#' @inherit base::print.default
print.BalanceProperty <- function(x, ...) {
  if(x$BalanceProperty) {
    cat("\nBalance property is satisfied.\n\n")
  } else {
    warning("\nBalance property is not satisfied.\n", immediate. = T)
    cat("\nRatio total observed damage to total predicted damage:", x$Alpha, "\n\n")
  }
}

