#' Visualizing the random effect estimates using ggplot2
#'
#' Using this function, you can create plots of the random effect estimates from fitted random effects models. To make
#' the plots, we rely on the \code{\link[ggplot2]{ggplot2}} package.
#'
#' @param obj an object of type \code{\link{hierCredibility}}, \code{\link{hierCredGLM}} or \code{\link{hierCredTweedie}}
#' @param levelRE indicates which hierarchical level has to be used. \code{"all"} plots both levels in the hierarchy,
#' \code{"first"} the first level in the hierarchy and \code{"second"} the second level.
#' @param colour colour for \code{\link[ggplot2]{geom_point}}
#' @param plot logical indicating if the \code{\link[ggplot2]{ggplot}} objects have to be plotted.
#'
#' @return a list with \code{\link[ggplot2]{ggplot}} objects.
#'
#' @examples
#' \donttest{
#' fitHGLM <- hierCredGLM(Y ~ area + gender + (1 | VehicleType / VehicleBody), dataCar, weights = w)
#' plotRE(fitHGLM)
#' }
plotRE <- function(obj, levelRE = c("all", "first", "second"), colour = "black",
                   plot = TRUE) {
  levelRE = match.arg(levelRE)
  REs     = ranef(obj)
  hierObj = if("hierCredibility" %in% class(obj)) obj else obj$HierarchicalResults
  type    = hierObj$type
  MLFj    = hierObj$Hierarchy$sector
  MLFjk   = hierObj$Hierarchy$group
  ggMLFj =
    do.call("ggplot", list(data = REs[[1]], mapping = substitute(aes(x = Uj, reorder(MLFj, Uj)), list(MLFj = as.name(MLFj))))) +
    geom_point(size = 3, colour = colour) + theme(legend.position="none") +
    geom_vline(xintercept = if(type == "additive") 0 else 1) +
    xlab(expression(U[j])) + ylab(MLFj) +
    theme_bw() +
    theme(legend.key = element_rect(fill = "gray"),
          plot.background=element_blank(),
          panel.border = element_rect(colour = "black",
                                      fill = NA),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
  ggMLFjk =
    do.call("ggplot", list(data = REs[[2]], mapping = substitute(aes(x = Ujk, reorder(MLFjk, Ujk)), list(MLFjk = as.name(MLFjk))))) +
    geom_point(size = 3, colour = colour) + theme(legend.position="none") +
    do.call("facet_wrap", list(facets = substitute(MLFj ~ .,  list(MLFj = as.name(MLFj))), ncol = 3, scales = "free_y")) +
    geom_vline(xintercept = if(type == "additive") 0 else 1) +
    xlab(expression(U[jk])) + ylab(MLFjk) +
    theme_bw() +
    theme(legend.key = element_rect(fill = "gray"),
          plot.background=element_blank(),
          panel.border = element_rect(colour = "black",
                                      fill = NA),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
  ggPlots = list(ggMLFj = if(levelRE == "second") NULL else ggMLFj,
                 ggMLFjk = if(levelRE == "first") NULL else ggMLFjk)
  if(plot)
    lapply(ggPlots, print)
  return(ggPlots)
}
