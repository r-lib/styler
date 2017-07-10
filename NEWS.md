## styler 0.0-6 (2017-07-10)

* Tidy up README (#43).
* Fix indention of long operator chains (#69).
* Properly format unary operators (#38).
* Fixing indent multiple - one more time (#68).
* Remove spaces before comma (#62).
* Fixing indention with multiple parentheses (#57).
* data.tree gains unique node IDs for proper printing.
* Add `style_file()` function to style a single .R file.
* Add RStudio add-in to style active .R file.
* hotfix: make `style_pkg()` and `style_src()` work by passing flat argument.


## styler 0.0-5 (2017-06-30)

* Correctly deal with comments (spacing before comments, start comment with space)
* T more flexibly (`test_collection()` and friends now support `...`)
* Indention based on curly brackets
* Spacing across different levels of nesting (e.g. a space after `)` in `function(x) {...}`)
* Write tree structure to file via test_collection()` for easy understanding of the nested structure
* Outsource tokenise
* Account for situations where code does not start on line1
* Correctly style comments
* Add style_empty for tailored testing
* Initialize indent in create filler
* Adapt vignette and documentation to visitor concept
* Refine testing to use visiting approach
* Parse multiple expressions: Make nested approach wok on multiple expressions too.
* Internal: introducing the visitor concept instead of "looping" many times through whole nested structure.
* Integrate nested approach in top-level APIs `style_text` and friends via an additional argument. `flat`.
* Add tools for scalable testing. Transform *-in.R with a transformer function and check whether result corresponds to *-out.R


## styler 0.0-4 (2017-06-15)

- Fix `README.Rmd` for compatibility with pkgdown.


## styler 0.0-3 (2017-06-15)

- Technical release for creation of pkgdown documentation.


## styler 0.0-2 (2017-06-15)

- Technical release for creation of pkgdown documentation.


## styler 0.0-1 (2017-06-15)

Initial release, work in progress.

- Create and serialize nested parse data.
- Internal support for indention of expressions with parentheses.
- Adding and removing spaces around operators on flat parse data.
