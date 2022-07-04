## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----logo, echo=FALSE, out.width="25%"----------------------------------------
knitr::include_graphics("./actuaRE.png")

## ----hMLF, fig.align = 'center', fig.cap = "Figure 1: Hierarchical structure of a hypothetical example", fig.topcaption = TRUE, echo = FALSE----
knitr::include_graphics("./HierarchicalStructureAdj.png")

## -----------------------------------------------------------------------------
capture.output(library(actuaRE), file = tempfile()) # suppress startup message
data("hachemeisterLong")
fitHC = hierCredibility(ratio, weight, cohort, state, hachemeisterLong)
fitHC

## ---- eval = FALSE------------------------------------------------------------
#  fitHCMult = hierCredibility(ratio, weight, cohort, state, hachemeisterLong, type = "multiplicative")
#  fitHCMult

## -----------------------------------------------------------------------------
summary(fitHC)

## -----------------------------------------------------------------------------
fitted(fitHC)

## -----------------------------------------------------------------------------
ranef(fitHC)

## ---- fig.show = 'hold'-------------------------------------------------------
ggPlots = plotRE(fitHC, plot = FALSE)
ggPlots[[1]]
ggPlots[[2]]

## -----------------------------------------------------------------------------
newDt = hachemeisterLong[sample(1:nrow(hachemeisterLong), 5, F), ]
predict(fitHC, newDt)

## -----------------------------------------------------------------------------
data("dataCar")
fit = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w)
summary(fit)

## -----------------------------------------------------------------------------
fixef(fit)
ranef(fit)

## -----------------------------------------------------------------------------
head(fitted(fit))
predict(fit, newdata = dataCar[1:2, ], type = "response")
ggPlots = plotRE(fit, plot = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  fitGLMM = tweedieGLMM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w, verbose = TRUE)

## -----------------------------------------------------------------------------
fitnoBP  = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w, balanceProperty = F)
yHatnoBP = fitted(fitnoBP)
w        = weights(fitnoBP, "prior")
y        = fitnoBP$y

fitBP  = hierCredGLM(Y ~ area + (1 | VehicleType / VehicleBody), dataCar, weights = w, balanceProperty = T)
yHatBP = fitted(fitBP)

sum(w * y) / sum(w * yHatnoBP)
sum(w * y) / sum(w * yHatBP)

## -----------------------------------------------------------------------------
BalanceProperty(fitnoBP)
BalanceProperty(fitBP)

