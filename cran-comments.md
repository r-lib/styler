## Test environments

* local OS X install (10.14.6): R 3.6.0
* ubuntu 14.04 (on travis-ci): R devel, R 3.6, R 3.5, R 3.4, R 3.2
* r-hub: R devel (Windows and Linux) and R 3.6 (Windows and Linux).
* win-builder: R devel, R 3.6 

## R CMD check results

0 ERRORS | 0 WARNINGS | 0 NOTES

## Downstream Dependencies

I also ran R CMD check on all downstream dependencies of styler using the 
revdepcheck package. The 
downstream dependencies are: exampletestr, languageserver, crunch, 
drake, knitr, nph, reprex, shinydashboardPlus, tradestatistics, usethis.

All of them finished R CMD CHECK with the same number of ERRORS, WARNINGS and 
NOTES as with the current CRAN version of styler, which means the new 
submission of styler does not introduce any ERRORS, WARNINGS and NOTES in 
downstream dependencies.
