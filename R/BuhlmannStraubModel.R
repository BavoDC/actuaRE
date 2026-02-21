#' Buhlmann-Straub credibility model
#'
#' Fit a credibility model using the Buhlmann-Straub approach with heterogeneous exposures.
#'
#' @param Yij variable name of the response variable (the loss cost within actuarial applications).
#' @param wij variable name of the exposure weight.
#' @param MLFj variable name of the risk class or cluster.
#' @param data an object that is coercible by \code{\link[data.table]{as.data.table}}, containing the variables in the model.
#' @param muHat estimate for the collective premium (portfolio mean). Default is \code{NULL} and in this case, the credibility-weighted estimator is used.
#' @param type specifies whether the additive or multiplicative formulation of the credibility model is used. Default is additive.
#' @param returnData Logical, indicates whether the data object has to be returned. Default is \code{FALSE}.
#'
#' @return An object of type \code{buhlmannStraub} with the following slots:
#' @return \item{call}{the matched call}
#' @return \item{type}{Whether additive or multiplicative credibility model is used.}
#' @return \item{Variances}{The estimated variance components. \code{Sigma} is the estimated within-group variance (process variance),
#'  and \code{Tau} is the estimate of the between-group variance (variance of the hypothetical means).}
#' @return \item{Means}{The estimated averages at the portfolio level (collective premium \eqn{\mu}) and
#' at the cluster level (weighted average \eqn{\bar{Y}_j}).}
#' @return \item{Weights}{The total weights \eqn{w_j} for each cluster.}
#' @return \item{Credibility}{The credibility factors \eqn{z_j} for each cluster.}
#' @return \item{Premiums}{The collective premium \eqn{\hat{\mu}} and individual premiums \eqn{\hat{V}_j} for each cluster.}
#' @return \item{Relativity}{The estimated random effects \eqn{\hat{U}_j} of each cluster.}
#' @return \item{RawResults}{Object of type \code{data.table} with all intermediate results.}
#' @return \item{fitted.values}{the fitted mean values, resulting from the model fit.}
#'
#' @references Buhlmann, H. and Straub, E. (1970). Glaubwurdigkeit fur Schadensatze.
#' \emph{Mitteilungen der Vereinigung schweizerischer Versicherungsmathematiker}, 70, 111-133.
#' @references Buhlmann, H. and Gisler, A. (2005). \emph{A Course in Credibility Theory and its Applications}. Springer.
#' @seealso \code{\link{hierCredibility}}, \code{\link{hierCredTweedie}}, \code{\link{hierCredGLM}}
#' @examples
#' library(actuar)
#' library(actuaRE)
#' data("hachemeister", package = "actuar")
#' # Prepare data
#' X = as.data.frame(hachemeister)
#' Df = reshape(X, idvar = "state",
#'              varying = list(paste0("ratio.", 1:12), paste0("weight.", 1:12)),
#'              direction = "long")
#' # Fit Buhlmann-Straub model
#' fitBS = buhlmannStraub(ratio.1, weight.1, state, Df)
#' summary(fitBS)
#' # Compare with actuar package
#' fit <- cm(~state, hachemeister, ratios = ratio.1:ratio.12,
#'           weights = weight.1:weight.12)
#' summary(fit)
buhlmannStraub <- function(Yij, wij, MLFj, data, muHat = NULL,
                           type = c("additive", "multiplicative"),
                           returnData = FALSE) {
  #### 1. Settings ####
  Argz = as.list(match.call())[-1]
  call = match.call()
  type = match.arg(type)

  Df = copy(data)
  Df$Yij    = eval(Argz$Yij, Df)
  Df$wij    = eval(Argz$wij, Df)
  Df$MLFj   = eval(Argz$MLFj, Df)

  if(is.factor(Df$MLFj))
    Df$MLFj = as.character(Df$MLFj)

  Dt = as.data.table(Df)

  cluster = deparse(substitute(MLFj))

  setkey(Dt, MLFj)

  #### 2. Input validation ####
  if(!is.numeric(Dt$Yij))
    stop("Response variable must be numeric.")
  if(!is.numeric(Dt$wij))
    stop("Weight variable must be numeric.")
  if(any(Dt$wij <= 0))
    stop("Weights must be positive.")

  #### 3. Buhlmann-Straub credibility ####
  # Calculate statistics by cluster
  Dfj = Dt[, .(
    Yj_Bar = as.vector(crossprod(Yij, wij) / sum(wij)),
    wj = sum(wij),
    nj = .N
  ), by = MLFj]

  # Calculate within-cluster variance for each cluster
  Dt[, Yj_Bar := Dfj$Yj_Bar[which(Dfj$MLFj == .BY)], by = MLFj]
  SigmaJ = Dt[, .(SigmaJ = sum(wij * (Yij - Yj_Bar)^2) / (.N - 1)), by = MLFj]$SigmaJ
  Dfj[, SigmaJ := SigmaJ]

  # Number of clusters
  J = NrUnique(Dfj$MLFj)

  # Overall weighted average
  w    = sum(Dfj$wj)
  Ybar = as.vector(Dfj[, crossprod(wj, Yj_Bar) / w])

  # Estimate Sigma (within-group variance)
  SumNj = sum(Dfj$nj - 1)
  Sigma = as.vector(Dfj[, crossprod((nj - 1), SigmaJ)]) / SumNj

  if(Sigma < 0)
    stop("Negative variance estimate for Sigma (within-group variance).")

  # Estimate Tau (between-group variance)
  a = Dfj[, sum(wj * (Yj_Bar - Ybar)^2)] - (J - 1) * Sigma
  b = w - Dfj[, sum(wj^2)] / w
  Tau = a / b

  if(Tau < 0) {
    warning("Negative variance estimate for Tau (between-group variance). Setting Tau = 1e-4.")
    Tau = 1e-4
  }

  # Calculate credibility weights
  Dfj[, zj := wj / (wj + Sigma / Tau)]

  # Estimate collective premium if not provided
  if(is.null(muHat))
    muHat = as.vector(Dfj[, crossprod(zj, Yj_Bar) / sum(zj)])

  # Calculate individual premiums
  Dfj[, Vj := zj * Yj_Bar + (1 - zj) * muHat]

  # Calculate relativities
  Dfj[, Uj := if(type == "additive") Vj - muHat else Vj / muHat]

  # Rename MLFj column to original name
  setnames(Dfj, "MLFj", cluster)

  #### 4. Prepare fitted values ####
  setkeyv(Dt, "MLFj")
  Vj_fitted = Dfj[["Vj"]][match(Df[[cluster]], Dfj[[cluster]])]

  #### 5. Return results ####
  Results = list(
    call = call,
    type = type,
    Variances = c(Sigma = Sigma, Tau = Tau),
    Means = list(
      Portfolio = muHat,
      MLFj = Dfj[, c(cluster, "Yj_Bar"), with = FALSE]
    ),
    Weights = list(
      MLFj = Dfj[, c(cluster, "wj"), with = FALSE]
    ),
    Credibility = list(
      MLFj = Dfj[, c(cluster, "zj"), with = FALSE]
    ),
    Premiums = list(
      Portfolio = muHat,
      MLFj = Dfj[, c(cluster, "Vj"), with = FALSE]
    ),
    Relativity = list(
      MLFj = Dfj[, c(cluster, "Uj"), with = FALSE]
    ),
    RawResults = Dfj,
    Hierarchy = list(MLFj = cluster),
    fitted.values = Vj_fitted
  )

  if(returnData)
    Results$data = data

  class(Results) = "buhlmannStraub"
  Results
}


