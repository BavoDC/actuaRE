\docType{methods}
\name{ranef}
\alias{ranef}
\title{Extract the modes of the random effects}
\arguments{
  \item{object}{an object of a class of fitted models with
    random effects.}
}
\value{
  \itemize{
    \item{
      From \code{ranef}:
        An object composed of
      a list of data frames, one for each grouping factor for
      the random effects.  The number of rows in the data frame
      is the number of levels of the grouping factor.  The
      number of columns is the dimension of the random effect
      associated with each level of the factor.

    }
  } % itemize
} % value
\description{
  A generic function to extract the conditional modes of
  the random effects from a fitted model object.  For
  linear mixed models the conditional modes of the random
  effects are also the conditional means.
}
\details{
  If grouping factor i has k levels and j random effects
  per level the ith component of the list returned by
  \code{ranef} is a data frame with k rows and j columns.
}
\examples{
  \dontrun{
  library(lattice) ## for dotplot, qqmath
  library(lme4)
  fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
  fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy)
  fm3 <- lmer(diameter ~ (1|plate) + (1|sample), Penicillin)
  ranef(fm1)
  }
}
\keyword{methods}
\keyword{models}
