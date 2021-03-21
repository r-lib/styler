## Test environments

* local OS X install (10.15.7): R 4.0.3
* ubuntu 16.04 (on GitHub Actions): R devel, R 4.0.3, R 3.6, R 3.5, R 3.4, R 3.3
* Windows Server 10 (on GitHub Actions): R 3.6, R 4.0.3
* win-builder: R devel

## R CMD check results

0 ERRORS | 0 WARNINGS | 0 NOTES

## Downstream Dependencies

I also ran R CMD check on all downstream dependencies of styler using the 
revdepcheck package. The 
downstream dependencies are: 

* Reverse imports:	biocthis, exampletestr, languageserver, questionr,
  shinyobjects, ShinyQuickStarter, systemPipeShiny.
* Reverse suggests:	autothresholdr, crunch, datastructures, drake, epigraphdb,
  knitr, netReg, nph, precommit, reprex, shinydashboardPlus, shinyMonacoEditor,
  usethis

All of them finished R CMD CHECK with the same number of ERRORS, WARNINGS and 
NOTES as with the current CRAN version of styler, which means the new 
submission of styler does not introduce any ERRORS, WARNINGS and NOTES in 
downstream dependencies.
