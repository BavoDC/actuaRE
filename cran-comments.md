## Resubmission
This is a resubmission. In this version I have:
 - Description file: 
  + replaced Author, Maintainer field by Authors@R
  + acronyms are now written in full
  + reference to paper on arXiv has been added
 - man/dataCar.Rd: added `library(magrittr)` and corrected the text in the example (i.e. changed % to \%) so that code is now executable
 - Added the missing \value tags in the following .Rd files: hierCredibility-class.Rd, hierCredGLM-class.Rd, hierCredTweedie-class.Rd and ranef-actuaRE.Rd
 - \dontrun has been replaced by \donttest where applicable (all these examples run longer than 5 sec)
 - Functions hierCredGLM and hierCredTweedie no longer write messages to the console when `verbose = FALSE`


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
