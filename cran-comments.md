## Test environments
* local OS X install: R 3.5
* ubuntu 14.04 (on travis-ci): R devel, R 3.5, R 3.4, R 3.1, R 3.2
* win-builder: R devel, R 3.5 

## R CMD check results

0 ERRORS | 0 WARNINGS | 0 NOTES

## Downstream Dependencies

I also ran R CMD check on all downstream dependencies of styler. The 
downstream dependencies are exampletestr, languageserver, sealr, usethis, 
pmatch, reprex and shinydashboardPlus. All of them **except** drake, 
pass R CMD check with 

0 ERRORS | 0 WARNINGS | 0 NOTES

The drake devel version passes the reverse dependency check with 

0 ERRORS | 0 WARNINGS | 0 NOTES

The problem causing an R CMD CHECK ERROR for drake was resolved in 
https://github.com/ropensci/drake/commit/87e473d9fb4b54db3d7adc371003ab973a4d1273.
