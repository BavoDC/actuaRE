#' Hierarchical credibility model of Jewell
#'
#' Fit a random effects model, without contract-specific risk factors,  using the hierarchical credibility model of Jewell.
#'
#' @param Yijkt variable name of the response variable (the loss cost within actuarial applications).
#' @param wijkt variable name of the exposure weight.
#' @param sector variable name of the first hierarchical level.
#' @param group variable name of the second hierarchical level that is nested within the first hierarchical level.
#' @param data an object that is coercible by \code{\link[data.table]{as.data.table}}, containing the variables in the model.
#' @param muHat estimate for the intercept term. Default is \code{NULL} and in this case, the estimator as given in Ohlsson (2005) is used.
#' @param type specifies whether the additive (Dannenburg, 1996) or multiplicative (Ohlsson, 2005) formulation of the hierarchical credibility model is used. Default is additive.
#' @param returnData Logical, indicates whether the data object has to be returned. Default is \code{FALSE}.
#'
#' @return An object of type \code{hierCredibility} with the following slots:
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
#'
#' @references Campo, B.D.C. and Antonio, Katrien (2023). Insurance pricing with hierarchically structured data an illustration with a workers' compensation insurance portfolio. \emph{Scandinavian Actuarial Journal}, doi: 10.1080/03461238.2022.2161413
#' @references Dannenburg, D. R., Kaas, R. and Goovaerts, M. J. (1996). \emph{Practical actuarial credibility models}. Amsterdam: IAE (Institute of Actuarial Science and Econometrics of the University of Amsterdam).
#' @references Jewell, W. S. (1975). \emph{The use of collateral data in credibility theory: a hierarchical model}. Laxenburg: IIASA.
#' @references Ohlsson, E. (2005). Simplified estimation of structure parameters in hierarchical credibility. \emph{Presented at the Zurich ASTIN Colloquium}.
#' @seealso \code{\link{hierCredibility-class}}, \code{\link{fitted.hierCredibility}}, \code{\link{predict.hierCredibility}}, \code{\link{ranef-actuaRE}},
#'  \code{\link{weights-actuaRE}}, \code{\link{hierCredTweedie}}, \code{\link{hierCredGLM}}, \code{\link[cplm]{cpglm}}, \code{\link{plotRE}}
#' @examples
#' library(actuar)
#' library(actuaRE)
#' data("hachemeister", package = "actuar")
#' Df = as.data.frame(hachemeister)
#' X  = as.data.frame(cbind(cohort = c(1, 2, 1, 2, 2), hachemeister))
#' Df = reshape(X, idvar = "state", varying = list(paste0("ratio.", 1:12),
#'  paste0("weight.", 1:12)), direction = "long")
#' fitActuar  = cm(~ cohort + cohort:state, data = X, ratios = ratio.1:ratio.12,
#' weights = weight.1:weight.12, method = "Ohlsson")
#' fitActuaRE = hierCredibility(ratio.1, weight.1, cohort, state, Df)
#' summary(fitActuar)
#' summary(fitActuaRE)
hierCredibility <- function(Yijkt, wijkt, sector, group, data, muHat = NULL, type = c("additive", "multiplicative"),
                            returnData = FALSE) {
  #### 1. Settings ####
  Argz = as.list(match.call())[-1]
  call = match.call()
  type = match.arg(type)

  Df = copy(data)
  Df$Yijkt  = eval(Argz$Yijkt, Df)
  Df$wijkt  = eval(Argz$wijkt, Df)
  Df$sector = eval(Argz$sector, Df)
  Df$group  = eval(Argz$group, Df)
  if(is.factor(Df$sector))
    Df$sector = as.character(Df$sector)
  if(is.factor(Df$group))
    Df$group = as.character(Df$group)
  Dt = as.data.table(Df)

  Sect = deparse(substitute(sector))
  Grp  = deparse(substitute(group))

  setkey(Dt, sector, group)

  #### 2. Hierarchical credibility ####
  SumTjk = sum(Dt[, .(Tjk = .N - 1), keyby = key(Dt)][, get("Tjk")])
  SumKj  = sum(Dt[, .(Kj = NrUnique(group) - 1), by = sector][, get("Kj")])

  Dfijkt = Dt[,  .(
      Yjk_BarTilde = as.vector(crossprod(Yijkt, wijkt) / sum(wijkt)),
      wijkt = wijkt,
      Yijkt = Yijkt
    ), by = .(sector, group)]

  s2 = Dfijkt[, sum(wijkt * (Yijkt - Yjk_BarTilde)^2)] / SumTjk
  if(s2 < 0)
    stop("Negative variance estimate for sigma^2 (E[Var(Y_{ijkt} | V_j, V_{jk})]).")

  Dfjk = Dfijkt[, .(wjk = sum(wijkt),
                    Yjk_BarTilde = unique(Yjk_BarTilde)), by = .(sector, group)]
  Dfj  = Dfjk[, .(wj = sum(wjk),
                  wjsq = sum(wjk ^ 2),
                  Yj_BarTilde = as.vector(crossprod(wjk, Yjk_BarTilde) / sum(wjk))), by = sector]
  w    = sum(Dfj$wj)

  Dfjk[, Yj_BarTilde := Dfj$Yj_BarTilde[which(Dfj$sector == .BY)], by = sector]

  nusq  = (Dfjk[, sum(wjk * (Yjk_BarTilde - Yj_BarTilde)^2)] - s2 * SumKj) / (w - Dfj[, sum(wjsq / wj)])
  if(is.nan(nusq) || nusq < 0)
    stop("Negative variance estimate for nu^2 (E[Var(V_{jk} | V_j)]).")

  Dfjk[, zjk := wjk / (wjk + s2 / nusq)]
  Dfj = Dfjk[, {
    zj = sum(zjk)
    wj = sum(wjk)
    Yj_BarTilde  = as.vector(crossprod(wjk, Yjk_BarTilde) / wj)
    Yjz_BarTilde = as.vector(crossprod(zjk, Yjk_BarTilde)) / zj
    list(
      zj = zj,
      wj = wj,
      Yj_BarTilde  = Yj_BarTilde,
      Yjz_BarTilde = Yjz_BarTilde
    )
  }, by = sector]
  J     = NrUnique(Dfj$sector)
  Yz_BarTilde = as.vector(Dfj[, crossprod(zj, Yjz_BarTilde) / sum(zj)])
  z     = sum(Dfj$zj)

  tausq = (Dfj[, sum(zj * (Yjz_BarTilde - Yz_BarTilde)^2)] - nusq * (J - 1)) / (z - sum(Dfj$zj^2) / z)
  if(tausq <= 0)
    stop("Negative variance estimate for tau^2 (Var[V_j]).")

  Dfj[, qj := zj / (zj + nusq / tausq), by = sector]

  muHat = if(is.null(muHat)) as.vector(Dfj[, crossprod(qj, Yjz_BarTilde) / sum(qj)]) else muHat

  Dfj[, Vj := qj * Yjz_BarTilde + (1 - qj) * muHat]
  Dfj[, Uj := if(type == "additive") Vj - muHat else Vj / muHat]
  Dfjk[, Vj := rep(Dfj$Vj[which(Dfj$sector == .BY)], .N), by = sector]
  Dfjk[, Vjk := zjk * Yjk_BarTilde + (1 - zjk) * Vj]
  Dfjk[, Ujk := if(type == "additive") Vjk - Vj else Vjk / Vj]

  setnames(Dfj, "sector", Sect)
  setnames(Dfjk, c("sector", "group"), c(Sect, Grp))

  REs = c(Sect, Grp)
  setkeyv(Dt, REs)
  setkeyv(Dfjk, REs)
  Vjk = Dfjk$Vjk[Dfjk[, ..REs][Dt[, ..REs], nomatch = 0L, which = T]]


  Results = list(
    call = call,
    type = type,
    Variances = c(s2 = s2, nusq = nusq, tausq = tausq),
    Means = list(
      Portfolio = muHat,
      sector = Dfj[, c(Sect, "Yjz_BarTilde"), with = F],
      group = Dfjk[, c(Sect, Grp, "Yjk_BarTilde"), with = F]
    ),
    Weights = list(sector = Dfj[, c(Sect, "zj"), with = F],
                   group =   Dfjk[, c(Sect, Grp, "wjk"), with = F]),
    Credibility = list(sector = Dfj[, c(Sect, "qj"), with = F],
                       group =   Dfjk[, c(Sect, Grp, "zjk"), with = F]),
    Premiums = list(
      Portfolio = muHat,
      sector = Dfj[, c(Sect, "Vj"), with = F],
      group =   Dfjk[, c(Sect, Grp, "Vjk"), with = F]
    ),
    Relativity = list(sector = Dfj[, c(Sect, "Uj"), with = F],
                      group =   Dfjk[, c(Sect, Grp, "Ujk"), with = F]),
    RawResults = list(Dfj = Dfj,
                      Dfjk = Dfjk),
    Hierarchy = list(sector = Sect, group = Grp),
    fitted.values = Vjk)
  if(returnData)
    Results$data = data
  class(Results) = "hierCredibility"
  Results
}
