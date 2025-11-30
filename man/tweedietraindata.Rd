\name{simulatedclustereddata}
\alias{tweedietraindata}
\alias{tweedietestdata}
\docType{data}
\title{
  Simulated data sets to illustrate the package functionality
}
\description{
  Both the \code{tweedietraindata} and \code{tweedietestdata} dataframe are synthetically generated data sets to illustrate the functionality of the package.
  The \code{tweedietraindata} has 250 000 observations and the \code{tweedietestdata} has 250 000 observations. The same settings were used to generate both data sets.
}
\usage{
  data(tweedietraindata)
  data(tweedietestdata)
}
\format{
  \describe{
    \item{\code{y}}{the tweedie distributed outcome variable}
    \item{\code{cluster}}{the cluster}
    \item{\code{subcluster}}{the subcluster nested within cluster}
    \item{\code{x1}}{covariate 1}
    \item{\code{x2}}{covariate 2}
    \item{\code{x3}}{covariate 3}
    \item{\code{x4}}{covariate 4}
    \item{\code{x5}}{covariate 5}
  }
}

\details{See the examples for how the data sets were generated.}
\examples{
  # The data sets were generated as follows
  lapply(c("magrittr", "dplyr", "data.table", "tweedie"), library, character.only = TRUE)
  set.seed(1)

  # Simulate training data
  set.seed(1)
  nClusters    = 5
  nSubclusters = 5
  p            = 5
  Uj           = scale(rnorm(nClusters))
  Ujk          = do.call("c", lapply(seq_len(nClusters), function(x) scale(rnorm(nSubclusters))))
  nPop         = 1e6
  nSample      = 50
  nTest        = 1e3
  X            = replicate(p, rnorm(nPop))
  Beta         = rnorm(p)
  cluster      = sample(seq_len(nClusters), nPop, TRUE)
  subcluster   = NULL
  uniqueCl     = sort(unique(cluster))
  for(cl in uniqueCl)
    subcluster[cluster == cl] <- sample(
      1 - seq_len(nSubclusters) + which(cl == uniqueCl) * nSubclusters,
      sum(cluster == cl),
      TRUE)
  table(subcluster, cluster)
  eta       = X \%*\% Beta + Uj[match(cluster, seq_len(nClusters))] +
              Ujk[match(subcluster, seq_len(nClusters * nSubclusters))]
  y         = rtweedie(nPop, mu = exp(as.vector(eta)), phi = 1, power = 1.5)
  wt        = runif(nPop)
  Dt        = data.frame(y, X, wt, cluster, subcluster)
  colnames(Dt) \%<>\% tolower

  tweedietraindata = Dt \%>\%
    group_by(subcluster) \%>\%
    sample_n(size = nSample) \%>\%
    as.data.table

  tweedietestdata = Dt \%>\%
    group_by(subcluster) \%>\%
    sample_n(size = nSample) \%>\%
    as.data.table
}
\keyword{datasets}
