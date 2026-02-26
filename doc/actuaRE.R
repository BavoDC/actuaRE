## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----logo, echo=FALSE, out.width="25%"----------------------------------------
knitr::include_graphics("./actuaRE.png")

## ----hMLF, fig.align = 'center', fig.cap = "Figure 1: Hierarchical structure of a hypothetical example", fig.topcaption = TRUE, echo = FALSE, out.width="100%"----
knitr::include_graphics("./HierarchicalStructureAdj.png")

## ----message = FALSE----------------------------------------------------------
capture.output(library(actuaRE), file = tempfile()) # suppress startup message
library(actuar)
data("hachemeister")
# Reshape to long format for single state analysis
X = as.data.frame(hachemeister)
Df = reshape(X, idvar = "state", 
             varying = list(paste0("ratio.", 1:12), paste0("weight.", 1:12)), 
             direction = "long")

fitBS = buhlmannStraub(ratio.1, weight.1, state, Df)
fitBS

## -----------------------------------------------------------------------------
summary(fitBS)

## -----------------------------------------------------------------------------
head(fitted(fitBS))

## -----------------------------------------------------------------------------
ranef(fitBS)

## ----fig.show = 'hold', fig.width = 6, fig.height = 4-------------------------
plotRE(fitBS, plot = FALSE)

## -----------------------------------------------------------------------------
newDt = Df[sample(1:nrow(Df), 5, FALSE), ]
predict(fitBS, newDt)

## ----multBS-------------------------------------------------------------------
fitBSMult = buhlmannStraub(ratio.1, weight.1, state, Df, type = "multiplicative")
fitBSMult

## -----------------------------------------------------------------------------
data("hachemeisterLong")
fitHC = hierCredibility(ratio, weight, cohort, state, hachemeisterLong)
fitHC

## ----eval = FALSE-------------------------------------------------------------
# fitHCMult = hierCredibility(ratio, weight, cohort, state, hachemeisterLong, type = "multiplicative")
# fitHCMult

## -----------------------------------------------------------------------------
summary(fitHC)

## -----------------------------------------------------------------------------
head(fitted(fitHC))

## -----------------------------------------------------------------------------
ranef(fitHC)

## ----fig.show = 'hold'--------------------------------------------------------
ggPlots = plotRE(fitHC, plot = FALSE)
ggPlots[[1]]
ggPlots[[2]]

## -----------------------------------------------------------------------------
newDt = hachemeisterLong[sample(1:nrow(hachemeisterLong), 5, FALSE), ]
predict(fitHC, newDt)

## -----------------------------------------------------------------------------
# Add a time factor to the reshaped Hachemeister data
Df$time_factor = factor(Df$time)
fitBSGLM = buhlmannStraubGLM(ratio.1 ~ time_factor + (1 | state), Df, 
                             weights = weight.1, p = 1.5)
summary(fitBSGLM)

## -----------------------------------------------------------------------------
fixef(fitBSGLM)
ranef(fitBSGLM)

## -----------------------------------------------------------------------------
data("tweedietraindata")
fit = hierCredGLM(y ~ x1 + (1 | cluster / subcluster), tweedietraindata, weights = wt)
summary(fit)

## -----------------------------------------------------------------------------
fixef(fit)
ranef(fit)

## -----------------------------------------------------------------------------
head(fitted(fit))
predict(fit, newdata = tweedietraindata[1:2, ], type = "response")
ggPlots = plotRE(fit, plot = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# # Single random effect - uses Buhlmann-Straub for initial estimates
# fitGLMM_single = tweedieGLMM(y ~ x1 + (1 | cluster),
#                               tweedietraindata, weights = wt, verbose = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# # Nested random effects - uses hierarchical credibility for initial estimates
# fitGLMM_nested = tweedieGLMM(y ~ x1 + (1 | cluster / subcluster),
#                               tweedietraindata, weights = wt, verbose = TRUE)

## -----------------------------------------------------------------------------
fitnoBP  = hierCredGLM(y ~ x1 + (1 | cluster / subcluster), tweedietraindata, weights = wt, balanceProperty = FALSE)
yHatnoBP = fitted(fitnoBP)
w        = weights(fitnoBP, "prior")
y        = fitnoBP$y

fitBP  = hierCredGLM(y ~ x1 + (1 | cluster / subcluster), tweedietraindata, weights = wt, balanceProperty = TRUE)
yHatBP = fitted(fitBP)

sum(w * y) / sum(w * yHatnoBP)
sum(w * y) / sum(w * yHatBP)

## -----------------------------------------------------------------------------
BalanceProperty(fitnoBP)
BalanceProperty(fitBP)

