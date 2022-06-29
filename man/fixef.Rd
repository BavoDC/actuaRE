\docType{methods}
\name{fixef}
\alias{fixed.effects}
\alias{fixef}
\title{Extract fixed-effects estimates}
\arguments{
  \item{object}{any fitted model object from which fixed
    effects estimates can be extracted.}
}
\value{
  a named, numeric vector of fixed-effects estimates.
}
\description{
  Extract the fixed-effects estimates
}
\details{
  Extract the estimates of the fixed-effects parameters
  from a fitted model.
}
\examples{
  \dontrun{
  fixef(lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy))
  fm2 <- lmer(Reaction ~ Days + Days2 + (1|Subject),
              data=transform(sleepstudy,Days2=Days))
  fixef(fm2,add.dropped=TRUE)
  ## first two parameters are the same ...
  stopifnot(all.equal(fixef(fm2,add.dropped=TRUE)[1:2],
                      fixef(fm2)))
  }
}
\keyword{models}
