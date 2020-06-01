## Intro

This project follows the contributing recommendations outlined by [saamwerk](https://lorenzwalthert.github.io/saamwerk/).
In particular, issues labelled with `Status: Postponed` are closed even if they
are not resolved.

## Contributing code

* Only open a PR when your idea was approved of by a contributor in an issue.
* Add a bullet to NEWS.md referencing the PR, also following the guide lines in 
  [tidyverse style guide](http://style.tidyverse.org), as for your code 
  contributions.
* Make sure your commit pass the pre-commit hooks in this repo. See the 
  `{precommit}` [README.md](https://github.com/lorenzwalthert/precommit) 
  on how to install the pre-commit framework and the R package on your system and 
  then run `precommit::use_precommit()` to make sure the hooks are activated 
  in your local styler clone. If you skip a hook, describe why in the PR. 


## How to dive in and understanding the source code

Read the vignettes. If you are done, come back here.

`devtools::load_all()` 

`debug(style_text)`

`style_text("call(1, 2 + 1)")`

Go broad before you go deep. Before going into the very deep layers of function
calls of `style_text()`, try to understand that `style_text()` consists of a few 
function calls only. 
Go into each of them and try to understand one layer deep. That is, try to 
understand what `make_transformer()` does by reading the names of the functions
that get called, the name of the objects that are created by assigning the output of 
these function calls. Before looking into a functions source code, look at the 
documentation for that function. All internal important functions
are documented and documentation is available also for unexported objects via 
`?` (if you did `devtools::load_all()`. 
Then, go into `parse_transform_serialize()` and so on.

To understand the most fundamental operation in styler, the manipulation of the 
columns related to spacing and line break information, pick a rule from 
`R/rules-*.R`, e.g. `R/rules-spacing`, add a break point to a rule and style a
string where you think this rule will be active. Then, see what happens and how 
this rule is applied on each level of nesting.

## Static code analysis

There are multiple packages that can be used to analyze a code base: 

* [gitsum](https://github.com/lorenzwalthert/gitsum): Parses and summarises git
  repository history. 
* [parsesum](https://github.com/lorenzwalthert/parsesum): Analyses source code 
  through parsing. 

Check out the links above to see how the tools listed could help you
understanding styler.

## Project setup

* The package is developed with the devtools suite, which includes roxgen2 for 
  documentation, testthat for unit testing, pkgdown for html documentation.
* Continuous integration is done with the tic (tasks integrating continuously)
  package. 
* A key development principle of styler is to separate infrastructure from 
  style guide. Hence, whenever possible, transformer functions should be 
  adapted, not the infrastructure should be changed for a specific style guide.
* styler was created in 2017 by Kirill Müller, then turned from a
  proof-of-concept into a ready-for-production tool as part of GSOC 2017 with
  Kirill Müller and Yihui Xie as mentors and Lorenz Walthert as student. 
  

## File Structure

The source code is organized as follows:

| File           | Description                                                |
| -------------: |:-----------------------------------------------------------|
| addins.R       | ui and helpers for the Addins of styler.                   |
| communicate.R  | function to communicate to the user via the console.       |
| compat-dplyr.R | compatibility functions. Since styler does not depend on dplyr, we define the dplyr functions ourself.| 
| compat-tidyr.R | compatibility functions. Since styler does not depend on tidy, we define the tidyr functions ourself.| 
| expr-is.R      | Functions to check whether an expression matches a predicate (e.g. whether it *is* a function call, a curly brace expression etc.). |
| indent.R       | Computation of whether indention is needed (needs_indention()), if so which indices are indented and how indention is it is triggered. |
| initialize.R   | initializer called with the visitor at each nest. | 
| nest.R         | converting from a text representation into a flat and then into a nested parse table representation. |
| nested-to-tree.R | utilities to create a tree representation from text (after text was converted into a nested parse table). |  
| parse.R        | parse text into parse table, minor token manipulation, verification of parsed objects. | 
| reindent.R     | Deals with token-dependent indention and re-indention, opposed to indent.R where all indention is token independent (i.e. a brace just adds one level of indention, whereas in function declaration headers (if mutli-line), indention depends on token position of "function"). |
| relevel.R | Reorganizing the nested parse table, namely relocates expressions on both sides of "%>%" to the same nest. |
| rules-line-break.R, rules-other.R, rules-replacement.R, rules-spacing.R | transformer rules | 
| serialize.R | converts flattened parse table into text representation. Complement operation to the functions in nest.R | 
| set-assert-args.R | Assertion and setting of arguments. | 
| style-guides.R | How to create style guide objects from transformers. | 
|styler.R | General package information. |
| testing.R | function used for testing. | 
| token-create.R | Utilities for creating tokens, mostly to insert braces around mutli-line if statements. | 
| token-define.R | Defines which tokens belong to which group. |
| transform-code.R, transform-files.R | Transformation of code for APIs that manipulate files (e.g. style_file()). |
| ui.R | User interaces. Top-level functions for styling. | 
| unindent.R | Certain tokens cause unindention, e.g. closing braces. | 
| utils.R | low-level general purpose utilities. |
| vertical.R | S3 class for pretty printing of styled code. | 
| visit.R | Functions that apply functions to each level of nesting, either inside out or outside in. | 
| zzz.R | backport imports. |

## Obtaining contextual information

You may have problems understanding some code because documentation is minimal,
some code / functions seem to solve problems you don't understand or handle
cases that seem unreasonable or otherwise incomprehensible. You can resort to 
the following strategies:

* Use full-text search to see where functions are defined or called and how
  different parts of styler depend on it.
* Use `$git blame` to see where changes were introduced. Look at the commit
  message, check changes that were made to the code in the same commit. If you
  are using the GUI of GitHub, you can easily obtain more contextual information
  such as the pull request with which a change was introduced. Often 
  functionality was introduced with testing. So, you can easily see which new 
  tests are related to the new functionality. You can remove the changes in the 
  source code and re-run the tests and see what fails and why.
* Search Issues and Pull Requests on GitHub with the full text search. Make 
  sure you also search for closed Issues and PRs.


## High-level conventions

* The project follows a highly functional approach. This means that 
  functionality should be capsuled into functions, even if they are only called
  once. This makes abstraction from the code easier, reduces the number of lines
  for each function declaration considerably and makes it easier for people not
  familiar with the code base to dive into it.
* All internal functions (except if they are 100% self-explanatory) are to be
  documented.
* New functionality (e.g. in terms of styling rules) needs to be unit tested. If
  the new functionality changes how code is to be styled, the infrastructure 
  with `test_collection()` should be used.
* Cases that are not yet formatted correctly can be labelled with a `FIXME`.
* GitHub is the platform where communication about source code happens. We
  refrain from adding extensive in-line code comments. One can use `git blame` 
  to track when changes were introduced and find the corresponding pull request
  and associated issues to understand the thought process that lead to a change 
  in the source code. This also implies that issues and / or pull request 
  contain verbose explanation of problems and solutions provided. 
  
## Low-level coventions

This project follows the [tidyverse style guide](http://style.tidyverse.org). 
If we refer to specific variables / values etc. in the following sections, you
can use RStudio's full text search to find where 
`remove_line_break_before_round_closing_after_curly()` is declared or called.


### Files

* File names only contain alphanumeric characters and dashes.
* Files are named according to topics / contexts, not according to functions 
  that live in these files.

### Functions

* Function names should be verbs. No abbreviations should be used, we don't 
  care if function names are particularly long. For example, there is a
  function with the name `remove_line_break_before_round_closing_after_curly()`.
* only very low-level functions or functions that don't fit in any other file
  go to `utils.R`.

### Control Flow

* Conditional statements should always evaluate to `TRUE` or `FALSE`, i.e. we 
  don't encourage `if (length(x))` but rather `if (length(x) > 0L)`.
* We avoid loops whenever possible and use functions like `purrr::map()` and 
  friends when possible and prefer them over R base counterparts like 
  `base::lapply()`.

### Boolean Values

Functions that return Boolean values or variables that hold Boolean values are
often prefixed with `is` or `has`. For example, `is_rmd_file(path)` is a 
function that returns `TRUE` if `path` is the path to a `.Rmd` file and `FALSE`
otherwise.  

### Vectors with indices

Vectors that hold indices are often suffixed with `idx`. `else_idx` for example
indicates for every row in a parse table whether it contains an `else` token.

### Closures

The use of closures is discouraged. We prefer to prefill a template function 
with `purrr::partial()`.

## Testing

We have a testing framework powered by `test_collection()`. 
Essentially, there is an \*-in.R file and a \*-out.R file. The \*-in.R file is the
input that is transformed and - if it matches the *-out.R file, the test has 
passed. You can create an \*-in.R file, run `devtools::test(f = "[your file]")`
and an \*-out.R file is generated. If the file matches your expectation, 
you can commit it. Note that files are overwritten and version control should be
used to track failed tests. 
The files are placed in `tests/testthat` under the category they fit. 
Please have a look at the documentation for `test_collection()` and see other 
unit tests. Let me know if there is anything unclear about this.
