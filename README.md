# roxygen2md

The goal of roxygen2md is to replace Rd syntax with Markdown
in your package's `roxygen2` documentation.
Currently, the following substitutions are carried out:

- `\code{\link{...}}` becomes `[...()]`
- `\code{\link[...]{...}}` becomes `[...::...()]`
- `\code{...}` becomes `` `...` ``

More to come. The substitutions are not completely safe,
please carefully examine the results!

Let me know if this works with your documentation.


## Installation

Install from GitHub using

```r
# install.packages("remotes")
remotes::install_packages("krlmlr/roxygen2md")
```


## Usage

In your package directory, run

```r
roxygen2md::roxygen2md()
```

