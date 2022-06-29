\name{hachemeisterLong}
\docType{data}
\alias{hachemeisterLong}
\title{Hachemeister Data Set}
\description{
  Long format of the Hachemeister (1975) data set giving average claim amounts in private passenger bodily injury insurance. We have data of five U.S. states over 12 quarters between July 1970 and June 1973
  and we have the corresponding number of claims. To obtain a hierarchical structure, we created an artificial variable \code{cohort}. With this, we created a hierarchical multi-level factor, with \code{cohort}
  as the first hierarchical level and \code{state} as the second hierarchical level, nested within \code{cohort}.
}
\usage{hachemeisterLong}
\format{
  A data.frame with 60 rows and the following 5 columns:
    \describe{
      \item{\code{cohort}}{artificially created variable;}
      \item{\code{state}}{the state number;}
      \item{\code{time}}{time variable (quarter of the observation);}
      \item{\code{ratio}}{the average claim amount;}
      \item{\code{weight}}{the corresponding number of claims.}
    }
}
\source{
  Hachemeister, C. A. (1975), \emph{Credibility for regression models with application to trend}, Proceedings of the Berkeley Actuarial Research Conference on Credibility, Academic Press.
}
\keyword{datasets}
