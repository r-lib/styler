---
editor_options: 
  markdown: 
    wrap: 79
---

This release does not check for a specific error message from `parse()` anymore
when the input involves unparsable use of `_`. The release was requested by
Luke Tierney.

## Test environments

-   ubuntu 20.04 (on GitHub Actions): R devel, R 4.1.2, R 4.0.5, R 3.6, R 3.5,
    R 3.4
-   Windows Server 10 (on GitHub Actions): R 3.6, R 4.0.5
-   win-builder: R devel

## R CMD check results

0 ERRORS \| 0 WARNINGS \| 1 NOTES

The note was generated on winbuilder when incoming checks were enabled only and
contained many blocks like this:

    Found the following (possibly) invalid URLs:
      URL: https://github.com/ropensci/drake
        From: inst/doc/third-party-integrations.html
              NEWS.md
        Status: 429
        Message: Too Many Requests

It seems my package contains many URLs to GitHub and their rate limit prevents
the checking of all of them. I confirm that all URLs in my package are
compliant with the requirements of CRAN.

## Downstream Dependencies

I also ran R CMD check on all downstream dependencies of styler using the
revdepcheck package. The downstream dependencies are:

-   Reverse imports: biocthis, boomer, exampletestr, flow, iNZightTools,
    languageserver, questionr, shinymeta, shinyobjects, ShinyQuickStarter,
    systemPipeShiny, tidypaleo.

-   Reverse suggests: admiral, autothresholdr, crunch, datastructures, drake,
    epigraphdb, ghclass, knitr, multiverse, nph, precommit, reprex,
    shiny.react, shinydashboardPlus, shinyMonacoEditor, upsetjs, usethis.

All of them finished R CMD CHECK with zero (0) ERRORS, WARNINGS and NOTES.
