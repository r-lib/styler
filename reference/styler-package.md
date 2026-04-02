# styler: Non-Invasive Pretty Printing of R Code

styler allows you to format `.qmd`, `.R`, `.Rmd`, `.Rmarkdown`, `.Rnw`,
and/or `.Rprofile` files, R packages, or entire R source trees according
to a style guide. The following functions can be used for styling:

- [`style_text()`](https://styler.r-lib.org/reference/style_text.md) to
  style a character vector.

- [`style_file()`](https://styler.r-lib.org/reference/style_file.md) to
  style a single file.

- [`style_dir()`](https://styler.r-lib.org/reference/style_dir.md) to
  style all files in a directory.

- [`style_pkg()`](https://styler.r-lib.org/reference/style_pkg.md) to
  style the source files of an R package.

- [RStudio Addins](https://styler.r-lib.org/reference/styler_addins.md)
  to style either selected code or the active file.

## See also

Useful links:

- <https://github.com/r-lib/styler>

- <https://styler.r-lib.org>

- Report bugs at <https://github.com/r-lib/styler/issues>

## Author

**Maintainer**: Lorenz Walthert <lorenz.walthert@icloud.com>

Authors:

- Kirill Müller <kirill@cynkra.com>
  ([ORCID](https://orcid.org/0000-0002-1416-3412))

- Indrajeet Patil <patilindrajeet.science@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-1995-6531))

## Examples

``` r
style_text("call( 1)")
#> call(1)
style_text("1    + 1", strict = FALSE)
#> 1    + 1
style_text("a%>%b", scope = "spaces")
#> a %>% b
style_text("a%>%b; a", scope = "line_breaks")
#> a %>% b; a
style_text("a%>%b; a", scope = "tokens")
#> a %>% b()
#> a
```
