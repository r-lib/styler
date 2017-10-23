## styler 0.0-9 (2017-10-23)

- Hotfix: utf8 should not be verbose (#245).
- Allow styling of Rmd files(#233).
- Remove duplicate @family (#244).
- Fixing token insertion (#242).
- Capitalize Addin titles (#241).
- Explicit `NULL` creation to make styler compatible with R3.2.0 (#237).
- Improve vignettes (#232).
- Allow exclusion of files with `style_pkg()` and `style_dir()`.
- Correct styling with long strings (#230).
- Add tools for re-indenting tokens (#217).
- Math token spacing (#221).
- Remove outdated line and col information (#218).
- Empty input for styling does not cause an error (#227, #228).
- Tools to insert tokens + application on `if`-`else` clauses (#212).
- Improve example in documentation (#222).
- Fix spacing around in (#214).
- Maintenance: renaming functions / files, extend helper, documentation, if_else etc. (#204).
- Disallow line break after ( for function calls (#210).
- Preserve space between `!` and bang (#209).
- Simplify RStudio Addin (#208, #211).
- Indention based on square brackets (#207).
- Add vignette on introducing styler (#203).
- Indent function declaration without curly braces correctly (#202).
- Fix indention in if-else statement (#200).
- Sorting key (#196).
- Use safe sequences (#197).
- Fix space between two commas (#195).
- Keep single-line pipes on one line (#74).
- Remove tidyr and dplyr dependency (#182, #183, @jimhester).
- Fix parsing inconsistency (#188).
- Substitute create filler (#190).
- Introducing class vertical (#189).
- Adapt line break rules (#172).
- Fix `R CMD check` (#185).
- Force argument evaluation for proper error handling (#179).
- Add nonstrict version of set_space_before_comment (#174).
- Add installation instructions to README (#175).
- Addin to style highlighted region (#143).
- Improve spelling (#168).
- Add coverage badge
- Change badge from WIP to active
- Add the number of files to message (#166).
- Improve documentation (#164).
- Add informative messages while styling files (#165).
- More examples in help file (#161).
- No line breaks after pipe if comment is next token (#160).
- Fixing spacing around `!` (non-bang-bang) (#157).
- Finalize function documentation (#154).
- Review vignette (#158).
- Update bang-bang rule (#156).


## styler 0.0-8 (2017-08-24)

- Vignette on customizing styler (#145).
- No line break after `switch()` and friends (#152).
- Remove flat relicts completely (#151).
- Don't reindent function calls and break line correctly for multi-line calls (#149).
- Set space between "=" and "," (#150).
- Make R CMD Check perfect (#148).
- Adding tests for exception handling with invalid parse data (#139).
- Fix indention by checking for all potential triggers (#142).
- Fix un-indention (#135).
- Support wide characters (#130).
- No spaces around :, :: and :::.
- Redesigning the API (#123).
- Solve eq_sub indention in general (#125).
- Minor refactorings.
- Re-indent token-dependent (#119).
- Supporting more indention patterns.
- Allow raw indention.
- Definitively fixing eol issue with comments.
- Infrastructure.
- Flattening out the parse table.
- New rule: no space after ! -> !!! for tidyeval.
- Fix spacing around '{'.
- Don't drop tokens! Fixes #101.
- EOL spaces in empty comments (and in general) (#98).
- mal-indention in conditional statement due to wrong specification of indent_without_paren) (#95).
- Complicated indentions based on arithmetic and special operators (#96).
- indention interaction on with assignment operator and other operators (#97).


## styler 0.0-7 (2017-07-27)

- curly braces don't move a line up if previous token is comment.
- wrap parse + transform + serialize in new function.
- Speedup of nested styler functions by ~ 4x.
- Use multi line instead of line1 / line2 for indention and unindention.
- Indent multiple.
- add tidyverse rules (indention not yet working properly).
- Refactoring and add token before and afterwards to parse table.
- Implement post visitor instead of complicated move_up child.


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
