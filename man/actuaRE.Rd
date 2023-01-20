\name{actuaRE-package}
\alias{actuaRE-package}
\alias{actuaRE}
\docType{package}
\title{
  \packageTitle{actuaRE}
}
\description{
  \packageDescription{actuaRE}
}
%\details{

  %The DESCRIPTION file:
    %\packageDESCRIPTION{AGREL}
  %\packageIndices{AGREL}
  %~~ An overview of how to use the package, including the most important functions ~~
    %}
\author{
  \packageAuthor{actuaRE}

  Maintainer: \packageMaintainer{actuaRE}
}
\references{
  Dannenburg, D. R., Kaas, R. and Goovaerts, M. J. (1996). \emph{Practical actuarial credibility models}. Amsterdam: IAE (Institute of Actuarial Science and Econometrics of the University of Amsterdam).
  Jewell, W. S. (1975). \emph{The use of collateral data in credibility theory: a hierarchical model}. Laxenburg: IIASA.
  Ohlsson, E. (2005). Simplified estimation of structure parameters in hierarchical credibility. \emph{Presented at the Zurich ASTIN Colloquium}.\url{http://www.actuaries.org/ASTIN/Colloquia/Zurich/Ohlsson.pdf}
  Ohlsson, E. (2008). Combining generalized linear models and credibility models in practice. \emph{Scandinavian Actuarial Journal} \bold{2008}(4), 301–314.
}
\keyword{ package }
\seealso{
  \code{\link{hierCredibility}}
  \code{\link{hierCredGLM}}
  \code{\link{hierCredTweedie}}
  \code{\link{tweedieGLMM}}
  \code{\link{BalanceProperty}}
}
\examples{
\dontrun{
  # Vignette of the package
  vignette(package = "actuaRE")

  # Load data
  data(hachemeisterLong)
  data(dataCar)

  # Hierarchical credibility model of Jewell
  fit = hierCredibility(ratio, weight, cohort, state, hachemeisterLong)

  # Combination of the hierarchical credibility model with a GLM (Ohlsson, 2008)
  fit = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w,
  p = 1.7)
}
}
