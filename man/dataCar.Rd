\name{dataCar}
\alias{dataCar}
\docType{data}
\title{
  data Car
}
\description{
  This data set is taken from the \code{\link[insuranceData]{dataCar}} data set of the \code{insuranceData} package and slightly adjusted (see the code in examples for reproducing this data set).
  The original data set is based on  one-year vehicle insurance policies taken    out in 2004 or 2005. There are 67566 policies, of which  4589 (6.8\%) had at least one claim.
}
\usage{data(dataCar)}
\format{
  A data frame with 67566 observations on the following 15 variables.
  \describe{
    \item{\code{veh_value}}{vehicle value, in $10,000s}
    \item{\code{exposure}}{0-1}
    \item{\code{clm}}{occurrence of claim (0 = no, 1 = yes)}
    \item{\code{numclaims}}{number of claims}
    \item{\code{claimcst0}}{claim amount (0 if no claim)}
    \item{\code{veh_body}}{vehicle body, coded as \code{BUS} \code{CONVT} \code{COUPE} \code{HBACK} \code{HDTOP} \code{MCARA} \code{MIBUS} \code{PANVN} \code{RDSTR} \code{SEDAN} \code{STNWG} \code{TRUCK} \code{UTE}}
    \item{\code{veh_age}}{1 (youngest), 2, 3, 4}
    \item{\code{gender}}{a factor with levels \code{F} \code{M}}
    \item{\code{area}}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{E} \code{F}}
    \item{\code{agecat}}{1 (youngest), 2, 3, 4, 5, 6}
    \item{\code{X_OBSTAT_}}{a factor with levels \code{01101    0    0    0}}
    \item{\code{Y}}{the loss ratio, defined as the number of claims divided by the exposure}
    \item{\code{w}}{the exposure, identical to \code{exposure}}
    \item{\code{VehicleType}}{type of vehicle, \code{common vehicle} or \code{uncommon vehicle}}
    \item{\code{VehicleBody}}{vehicle body, identical to \code{veh_body}}
  }
}

\details{Adjusted data set \code{dataCar}, where we removed claims with a loss ratio larger than 1 000 000. In addition, we summed the exposure per vehicle body and removed those where
the summed exposure was less than 100. Hereby, we ensure that there is sufficient exposure for each vehicle body category.}
\source{http://www.acst.mq.edu.au/GLMsforInsuranceData}

\references{
  De Jong P., Heller G.Z. (2008), Generalized linear models for insurance data,
  Cambridge University Press
}


\examples{
  \dontrun{
  library(plyr)
  data("dataCar", package = "insuranceData")
  dataCar$Y = with(dataCar, claimcst0 / exposure)
  dataCar$w = dataCar$exposure
  dataCar   = dataCar[which(dataCar$Y < 1e6), ]
  Yw = ddply(dataCar, .(veh_body), function(x) c(crossprod(x$Y, x$w) / sum(x$w), sum(x$w)))
  dataCar = dataCar[!dataCar$veh_body %in% Yw[Yw$V2 < 1e2, "veh_body"], ]
  dataCar$veh_body %<>% droplevels()
  dataCar$VehicleType = sapply(tolower(dataCar$veh_body), function(x) {
    if(x %in% c("sedan", "ute", "hback"))
      "Common vehicle"
    else
      "Uncommon vehicle"
  })
  dataCar$VehicleBody = dataCar$veh_body
  }
}
\keyword{datasets}
