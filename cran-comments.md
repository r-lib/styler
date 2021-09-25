This is a re-submission to fix a problem with persistent caching. Note that
all cached files are of size 0 and this is by design to keep the cache small.
Hence, the cache will still have files of size 0, but we remove all files older
than a week and remove all caches for versions of styler other than the one 
running as requested.

## Test environments

* local OS X install (10.15.7): R 4.1.0
* ubuntu 18.04 (on GitHub Actions): R devel, R 4.1.1, R 4.0.5, R 3.6, R 3.5, R 3.4
* Windows Server 10 (on GitHub Actions): R 3.6, R 4.0.5
* win-builder: R devel

## R CMD check results

0 ERRORS | 0 WARNINGS | 1 NOTES

The note was generated on winbuilder when incoming checks were enabled only and 
contained many blocks like this: 

```
Found the following (possibly) invalid URLs:
  URL: https://github.com/ropensci/drake
    From: inst/doc/third-party-integrations.html
          NEWS.md
    Status: 429
    Message: Too Many Requests
```    

It seems my package contains many URLs to GitHub and their rate limit prevents
the checking of all of them. I confirm that all URLs in my
package are compliant with the requirements of CRAN.

## Downstream Dependencies

I also ran R CMD check on all downstream dependencies of styler using the 
revdepcheck package. The 
downstream dependencies are: 

* Reverse imports: biocthis, boomer, exampletestr, flow, iNZightTools, 
  languageserver, questionr, shinymeta, shinyobjects, ShinyQuickStarter, 
  systemPipeShiny, tidypaleo
  	
* Reverse suggests: autothresholdr, autothresholdr, crunch, datastructures, 
  drake, epigraphdb, knitr, multiverse, nph, precommit, reprex, shiny.react,
  shinydashboardPlus, shinyMonacoEditor, usethis.



All of them finished R CMD CHECK with the same number of ERRORS, WARNINGS and 
NOTES as with the current CRAN version of styler, which means the new 
submission of styler does not introduce any ERRORS, WARNINGS and NOTES in 
downstream dependencies.
