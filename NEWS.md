<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# styler 1.10.3

This release was requested by the CRAN team since parser error messages changed, 
which were hard-coded in some unit tests (#1180).

**Minor changes**

- Add a package sticker (#1172, #1173).
- Improve error message for scope (#1176).
- Update lintr config and address newly found lints (#1158).
- Fix new lints about implicit return (#1166).
- Clean new lints (#1149).
- Clean up unnecessary YAML front matter in README (#1165).

**CI**

- Update pre-commit and GitHub Actions (#1177, #1175, #1171, #1171, #1164, #1152, #1148).
- Delete URL check workflow (#1160).

**Testing**

- Suppress warning in io tests (#1169).
- Ensure unit tests check for appropriate error messages in the R parser for 
  R > 4.3 (#1180).
- Remove outdated test about repeated parsing (#1163).
- Update roxygen test comments (#1162).
- Delete unused snapshot (#1159).

We thank everyone who helped making this release possible.

[&#x0040;AshesITR](https://github.com/AshesITR), [&#x0040;averissimo](https://github.com/averissimo),  [&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil), [&#x0040;lorenzwalthert](https://github.com/lorenzwalthert), [&#x0040;mcanouil](https://github.com/mcanouil), [&#x0040;moodymudskipper](https://github.com/moodymudskipper), [&#x0040;olivroy](https://github.com/olivroy), [&#x0040;sbanville-delfi](https://github.com/sbanville-delfi), [&#x0040;sorhawell](https://github.com/sorhawell), [&#x0040;ssh352](https://github.com/ssh352), [&#x0040;swo](https://github.com/swo), and [&#x0040;vertesy](https://github.com/vertesy).

# styler 1.10.2

This release was requested by the CRAN team to fix CRAN warning on invalid 
numeric version inputs (#1143).

**Minor changes**

* Use cli messaging for cache (#1127).
* Use latest (and stable!) pre-commit (#1144).
* Fix CRAN warning on invalid numeric version inputs (#1143).
* Bump JamesIves/github-pages-deploy-action from 4.4.2 to 4.4.3 (#1139).
* fix pre-commit (#1132).
* Don't require dplyr anywhere (#1131).

We thank everyone who helped making this release possible.

[&#x0040;krlmlr](https://github.com/krlmlr), [&#x0040;lorenzwalthert](https://github.com/lorenzwalthert), [&#x0040;MichaelChirico](https://github.com/MichaelChirico), [&#x0040;olivroy](https://github.com/olivroy), [&#x0040;rkrug](https://github.com/rkrug), and [&#x0040;rossdrucker](https://github.com/rossdrucker).

# styler 1.10.1

This release was requested by CRAN due to accidentally populating a user cache while building vignettes for R >= 4.3.0.

* Code quality improvements (#1122).
* Bump JamesIves/github-pages-deploy-action from 4.4.1 to 4.4.2 (#1123).

Thanks to everyone who contributed to this release: [&#x0040;olivroy](https://github.com/olivroy) and [&#x0040;krlmlr](https://github.com/krlmlr).

# styler 1.10.0

This release contains speed-ups between 20% and 40% depending on your use case
thanks to replacing {base} functionality with {vctrs} (#1114). With the speed
boost introduced in version 1.8.0 in Oct. 2022, {styler} is now up to 2x as fast
as before release 1.8.0.

This release was created upon a request by the CRAN team to actively manage not
just cached files but also the potentially empty cache directories they live in
(#1118). Here are the changes in detail:


- Require at least R 3.6 (#1101).
- Prefer {vctrs} functions over slower {base} equivalents (#1114).
- Replace deprecated use of `rlang::with_handlers()` (#1103).
- Remove tail recursion in favor of `repeat` (#1113).
- split `test-public_api.R` for better sharding (#1109).
- 0-pad filenames for sharding (#1110)
- add missing {testthat} snapshots (#1115).
- Bump {touchstone} config (#1104, #1107).
- Bump `actions/checkout` to version 3 in GitHub Actions (#1098).

Thanks for everyone contributing to this release: 

[&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil), [&#x0040;krlmlr](https://github.com/krlmlr), [&#x0040;kyleam](https://github.com/kyleam), [&#x0040;MichaelChirico](https://github.com/MichaelChirico), [&#x0040;mvanaman](https://github.com/mvanaman), [&#x0040;olivroy](https://github.com/olivroy), and [&#x0040;vvarik](https://github.com/vvarik).

# styler 1.9.1

**Bug fixes**

- Fix interaction between cache and stylerignore that could produce invalid code (#1072).
- Don't remove line break around `{{` and comments that can yield invalid code (#1070).
- Styling empty roxygen code examples don't cause errors anymore (#1096).
- Double indent is also kept if there is only one argument (#1094).
- Improved blank lines handling for roxygen examples (#1085).
- style roxygen examples even if nothing comes after (#1089).

**Other**

- Document `"qmd"` as a valid `filetype` (#1091).

Thanks for everyone who contributed to this release:

[&#x0040;dpprdan](https://github.com/dpprdan), [&#x0040;flying-sheep](https://github.com/flying-sheep), [&#x0040;giocomai](https://github.com/giocomai) and [&#x0040;MichaelChirico](https://github.com/MichaelChirico).


# styler 1.9.0

**Features**

- The tidyverse recently introduced double-indention for function declarations
  that don't fit on one line. It indents two levels, i.e. 4 spaces if you `indent_by` two spaces.

```r
# old style: remains compliant and won't be re-styled
my_fun <- function(long_argument = 2,
                   indent_up_to_first = "x") {
  # ...
}

# new style: now also compliant and won't be re-styled
my_fun <- function(
    long_argument = 2,
    indent_double = "x") {
  # ...
}
```

You can also use the R package [{codegrip}](https://github.com/lionel-/codegrip)
to toggle between the two modes (#1083).

**Bug fixes**

- Previously styled code that is now stylerignored should always be formatted 
  correctly. It boils down to the requirement that stylerignore sequences must 
  always be in the same block (#1082).
- styling around `{{` and comments now yields parsable output (#1088).
- trailing blank lines in roxygen code examples are removed (#1085).
- roxygen code examples that don't have any code following after them are now 
  also styled (#1067).

**Other user-facing changes**

- Less noisy communication if R option `styler.cache_root` is not set (#1063).

**Infrastructure**

- use {lintr} config (#1057, #1059) and pre-commit hook (#1064).
- use new {pkgdown} hook, check for parsable yaml and mixed line ending (#1080,
  #1081).
- update GitHub Actions workflow versions one time (#1073) and add dependabot 
  for the future (#1974).
- bdr test for additional examples (#1068).
- check for link rot regularly (#1077, #1086).

**Internals**

- replace retired `purrr::when()` with `if` statements (#1066).
- more integer literals (#1054).
- Consistently use `@examplesIf` for conditionally running examples (#1071).
- document imports in a single file (#1060).
- format YAML files (#1061).

A big shout out to anyone who contributed to this release:

[&#x0040;balthasars](https://github.com/balthasars),
[&#x0040;hadley](https://github.com/hadley), 
[&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil), 
[&#x0040;juliangrimm225](https://github.com/) and 
[&#x0040;krlmlr](https://github.com/krlmlr).

# styler 1.8.1 

**Features**

- Expose internals used with other style guides (\@Robinlovelace + 
  collaborators, #1043, #1052).


**Other**

- Bump minimal version requirement on {withr} as `...` in 
  `withr::local_options()` was introduced only in v.2.3.0 (#1051).
- Rename internal function `set_linebreak_after_ggplot2_plus()` to 
  `set_line_break_after_ggplot2_plus()` for consistency (\@Polkas, #1049).
- Reformat contributing guidelines (#1047).
- Improve YAML formatting for pkgdown (#1042).
- Simplify caching internal's conditionals with `rlang::%||%` (#1041).
- Only run {pkgapi} if available (#1039).
- Typos (\@MichaelChirico, #1038)

This release was requested by CRAN to resolve an R CMD Check note (#1044). 
A big hand to everyone who made this release possible: 

Dave Jarvis, 
[&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil), 
[&#x0040;lorenzwalthert](https://github.com/lorenzwalthert), 
[&#x0040;MichaelChirico](https://github.com/MichaelChirico), 
[&#x0040;Polkas](https://github.com/Polkas), and 
[&#x0040;Robinlovelace](https://github.com/Robinlovelace).


# styler 1.8.0

{styler} 1.8.0 comes with a host of new features, around 40% speed improvement,
bug fixes and the removal of 8 recursive dependencies. We also welcome 
\@IndrajeetPatil as a new contributor to {styler}, who has contributed 
significantly to this and and previous releases.

**Features**

- `style_dir()` and `style_pkg()` now default to styling all supported file
  formats (`.R`, `.Rmd`, `.Rmarkdown`, `.Rnw`, and `.qmd`) in the (package)
  directory (#965, #931, #1033).
- `style_pkg()` now excludes the auto-generated `R/cpp11.R` file (#977).
- minimum needed R version is now bumped to `3.5` (#986).

- alignment is now detected for function declaration in a similar way as for
  function calls (#968).
- new R option `styler.ignore_alignment` controls if alignment should be
  detected (and preserved) or not (#932).


**Bug Fixes**

- alignment is detected in `tibble::tribble()` (and similar) calls with more
  than 3 columns when left aligned (#945).

- fix alignment detection for one column, mixed named/unnamed (#1035).

- if there are only empty lines in a code chunk, they are all removed (#936).

- apply rules for [ to [[ and its closing counterpair (#1030)

- there is now at most one line break after `{` and before `#` (#952, #1022).

- line breaks may be added to function calls to ensure indention symmetry for
  round braces (#975).

- the cache is also invalidated on changing the stylerignore markers (#932).

- `{` is not put on a new line after `=` and in `function() {` for some edge
cases (#939).

- `while ({})` statements are now handled the same way as function statements
  with regards to breaking lines (#967).

- parsing of {roxygen2} example comments now also works for edge cases when
  there is no literal code immediately following after the end of the example
  section (#940).

- files with no tokens in it are now transformed into zero-byte files (#962).

**Documentation**

- old (and outdated) vignettes have been removed (#955). To access them, do 
  `git checkout v1.0.0`.
- minor improvements to the documentation (#958).
- turned off `styler.colored_print.vertical` in vignettes so ANSI output of
 {prettycode} not messing with {pkgdown} (#956, #957).


**Performance and code quality improvements**

- use integer literals and avoid coercions where needed (#994).
- don't preserve names for `unlist()` (#998).
- remove unused variables (#999).
- get rid of lints with performance implications (#1000).
- use more efficient match() alternative (#1001).
- don't use `nrow` arg in `new_tibble()` calls (#1003).
- performance improvements with `if()` + `else()` instead of `ifelse()` (#1006).
- replace tibbles with data frames to improve performance (#1007). 
- simplify `styler_df()` signature (#1009).
- minor cleanup (#1016).
- non-exported and unused functions `odd()` and `even()` were removed
 (#989).
- all (R)md files in this project's source code are now formatted with default 
  pandoc markdown formatter. This conversion is required when using the visual 
  mode in RStudio (#941).
- improved code quality by fixing {lintr} warnings (#960, #1028).


**Dependency related changes**

In total, 8 recursive dependencies are removed: {ellipsis}, {pillar}, 
{rematch2}, {tibble}, {utf8}, {fansi}, {lifecycle}, {pkgconfig}.

- don't import entire tibble package (#1007).
- drop {rematch2} dependency (#1011).


**Infrastructure**

- upgrade testing infra to testthat 3e (#949).
- run tests in parallel (#978).
- run some tests sequentially (#1031)
- better stack tracing for profiling (#979, #980).
- add flags to skip code coverage for zzz.R (#1005).
- error now on R CMD note (#987).
- test on latest Ubuntu instead of Ubuntu 18.04 (#982).
- use latest GitHub Actions for R (#1034).
- update {pkgdown} action to always build, but only deploy on default branch 
  (#946).
- remove pre-commit push hook for news entry (#1023).

A big hand to everyone who made this release possible: 

[&#x0040;behrman](https://github.com/behrman), 
[&#x0040;EngineerDanny](https://github.com/EngineerDanny), [&#x0040;gavinsimpson](https://github.com/gavinsimpson), [&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil), [&#x0040;jabenninghoff](https://github.com/jabenninghoff), 
[&#x0040;krlmlr](https://github.com/krlmlr), 
[&#x0040;lorenzwalthert](https://github.com/lorenzwalthert), [&#x0040;MichaelChirico](https://github.com/MichaelChirico), [&#x0040;moodymudskipper](https://github.com/moodymudskipper), [&#x0040;RaymondBalise](https://github.com/RaymondBalise), [&#x0040;Robinlovelace](https://github.com/Robinlovelace), [&#x0040;sebffischer](https://github.com/sebffischer), 
[&#x0040;sgorm123](https://github.com/sgorm123), [&#x0040;stefanoborini](https://github.com/stefanoborini), and [&#x0040;wdkrnls](https://github.com/wdkrnls).

# styler 1.7.0

-   if `else` follows directly after `if`, line breaks are removed (#935).

**API changes**

-   new R option `styler.cache_root` (defaulting to `"styler"`) that determines
    the sub-directory under the {R.cache} cache directory that {styler} uses.
    Non- default caches won't be cleaned up by {styler}. We suggest
    `"styler-perm"` (also used by {precommit}).

-   stylerignore markers are now interpreted as regular expressions instead of
    comments that must match exactly. This allows to specify multiple markers
    in one regular expression for `styler.ignore_start` and
    `styler.ignore_stop`, e.g. to use markers for lintr and styler on the same
    line, you can use
    `options(styler.ignore_start = "nolint start|styler: off"`:

    ``` r
    # nolint start, styler: off
    1 +1
    # nolint end
    # styler: on
    ```

    As a consequence of this approach, the defaults for `styler.ignore_start`
    and `styler.ignore_stop` omit the `#` (#849).

**Features**

-   {styler} can be ran via GitHub Actions using
    `usethis::use_github_action("style")` (#914).
-   added guarantee that styled code is parsable (#892).
-   Developers can now create style guides with indention characters other than
    spaces (#916).

**Documentation**

-   Add vignette on distributing style guide (#846, #861).
-   Fix argument name `filetype` in Example for `style_dir()` (#855).

**Bug fixes**

-   Piped function without brackets `substitute(x %>% y)` don't get `()` added
    anymore for one level deep (not more yet, see #889), as this can change
    outcome of the code (#876).
-   `.onLoad()` method no longer broken with {cli} \>= 3.1 (#893).
-   Function calls containing `+` should no longer give any error on styling
    when there are comments and line breaks under certain circumstances (#905).
-   rules that add tokens don't break stylerignore sequences anymore (#891).
-   Alignment detection respects stylerignore (#850).
-   Unaligned expressions with quoted key (e.g. `c("x" = 2)`) are now correctly
    detected (#881).
-   `~` causes now indention, like `+`, `-`, `|` etc. (#902).
-   `Warning: Unknown or uninitialised column:` was fixed (#885).
-   function calls with unequal number of token on different lines are no
    longer deemed aligned if there are arbitrary spaces around the tokens on
    the lines with most tokens (#902).
-   if a line starts with `EQ_SUB` (`=`), the corresponding key is moved to
    that line too (#923).
-   ensure a trailing blank line also if the input is cached (#867).
-   Preserve trailing blank line in roxygen examples to simplify concatenation
    of examples (#880).
-   `indenty_by` is now also respected when curly braces are added to an if
    statement by {styler} (#915).
-   An error is now thrown on styling if input unicode characters can't be
    correctly parsed for Windows and R \< 4.2 (#883).
-   styling of text does not error anymore when the R option `OutDec` is set to
    a non-default value (#912).

**Infrastructure**

-   Remove dependency on {xfun} (#866).
-   Remove {glue} dependency that was only used by {touchstone} script and is
    declared as a dependency already in the respective action (#910).
-   Bump minimal R requirement to 3.4 in line with the
    [tidyverse](https://www.tidyverse.org/blog/2019/04/r-version-support/),
    which allowed to remove the dependency at {backports} and some exception
    handling.
-   rename default branch to main (#859).
-   the built package size has been reduced by \~50% by listing `*-in_tree`
    files in `.Rbuildignore` (#879).
-   Enable pre-commit.ci (#843).
-   use pre-commit via GitHub Actions (#872).
-   terminate running jobs on new push to save resources (#888).
-   Use the {touchstone} GitHub Action instead of the literal script (#889).
-   upgrade R CMD check Github Actions to use `v2`.
-   {styler} test are relaxed to not assume a specific error message on the
    wrong usage of `_` (#929).

Thanks to all contributors that made this release possible:

[\@bersbersbers](https://github.com/bersbersbers),
[\@daniel-wrench](https://github.com/daniel-wrench),
[\@dbykova](https://github.com/dbykova),
[\@EngrStudent](https://github.com/EngrStudent),
[\@hadley](https://github.com/hadley),
[\@IndrajeetPatil](https://github.com/IndrajeetPatil),
[\@jam1015](https://github.com/jam1015),
[\@jooyoungseo](https://github.com/jooyoungseo),
[\@kalaschnik](https://github.com/kalaschnik),
[\@kaytif](https://github.com/kaytif), [\@kpagacz](https://github.com/kpagacz),
[\@krlmlr](https://github.com/krlmlr), [\@lionel-](https://github.com/lionel-),
[\@lorenzwalthert](https://github.com/lorenzwalthert),
[\@maelle](https://github.com/maelle),
[\@MichaelChirico](https://github.com/MichaelChirico),
[\@mine-cetinkaya-rundel](https://github.com/mine-cetinkaya-rundel),
[\@neuwirthe](https://github.com/neuwirthe),
[\@Polkas](https://github.com/Polkas), [\@pwang2](https://github.com/pwang2),
[\@sebffischer](https://github.com/sebffischer),
[\@ShixiangWang](https://github.com/ShixiangWang),
[\@ssh352](https://github.com/ssh352), and
[\@xjtusjtu](https://github.com/xjtusjtu).

# styler 1.6.2

-   clean up cache files older than one week (#842).

# styler 1.6.1

-   Files with `.Rmarkdown` extension are now recognized as an R markdown files
    in `style_file()` and friends (#824).

-   Don't break line before comments in pipes (#822).

-   Ordinary comments (starting with `#`) that break a roxygen code example
    block (starting with `#'`) are now recognized and preserved (#830).

-   `@examplesIf` conditions longer than one line after styling throw an error
    for compatibility with {roxygen2} (#833).

-   R Markdown chunk headers are no longer required to be parsable R code
    (#832).

-   Break the line between `%>%` and `{` inside and outside function calls
    (#825).

-   Add language server to third-party integration vignette (#835).

-   improved test setup with fixtures and similar (#798).

We'd like to thank all people who helped making this release possible:

[\@bersbersbers](https://github.com/bersbersbers),
[\@eutwt](https://github.com/eutwt),
[\@IndrajeetPatil](https://github.com/IndrajeetPatil),
[\@j-mammen](https://github.com/j-mammen),
[\@jennybc](https://github.com/jennybc),
[\@JohannesNE](https://github.com/JohannesNE),
[\@jonkeane](https://github.com/jonkeane),
[\@lorenzwalthert](https://github.com/lorenzwalthert), and
[\@MichaelChirico](https://github.com/MichaelChirico).

# styler 1.5.1

## Alignment detection

-   Code with left alignment after `=` in function calls is now recognized as
    aligned and won't be reformatted (#774, #777).

        # already detected previously
        call(
          x  = 12345,
          y2 =    17
        )

        # newly detected
        call(
          x  = 12345,
          y2 = 17
        )

-   Similarly, left aligned after comma is now detected (#785, #786).

        # previously detected
        call(
          x  = 12345, "It's old",
          y2 = 17,      "before"
        )

        tribble(
          ~x,             ~y,
          "another",     1:3,
          "b",       1211234
        )

        # newly detected
        call(
          x = 2,           p = "another",
          y = "hhjkjkbew", x = 3
        )


        tribble(
          ~x,        ~y,
          "another", 1:3,
          "b",       1211234
        )

    Also see `vignette("detect-alignment")`.

## Other new features

-   The base R pipe as introduced in R 4.1.0 is now styled the same way the
    magrittr pipe is (#803).
-   code chunks with explicit `tidy = FALSE` in an Rmd or Rnw code header are
    not styled anymore. This can be handy when the code can't be parsed, e.g.
    within a learnr tutorial (#790).
-   `#>` is recognized as an output marker and no space is added after `#`
    (#771).

## Minor changes and fixes

-   No curly braces are added to else statements if they are within a pipe, as
    this can change evaluation logic of code involving the magrittr dot in rare
    cases (#816).
-   Line breaks between `}` and `else` are removed (#793).
-   In function calls, code after `= #\n` is indented correctly (#814).
-   Multi-expressions containing multiple assignments no longer remove line
    breaks if they are not causing blank lines (#809).
-   `exclude_dirs` in `style_pkg()` is now properly respected if it is a
    sub-directory of a directory that is scheduled for styling (e.g.
    `test/testthat/some/dir`) (#811).
-   The user is not prompted anymore to confirm the creation of a permanent
    cache as R.cache \>= 0.15.0 uses a standard location in line with CRAN
    policies (#819).
-   R code chunks in nested non-R chunks in R markdown don't yield an error
    anymore when document is styled, chunks are still not styled (#788, #794).
-   `cache_activate()` and `cache_deactivate()` now respect the R option
    `styler.quiet` (#797).
-   `multi_line` attribute in parse table is now integer, not boolean (#782).
-   The style guide used in Addin is verified when set via R option (#789).
-   Improve pkgdown author URLs (#775).
-   Upgrade touchstone infra (#799, #805).
-   Don't test on R 3.3 anymore as tidyverse [supports only four previous
    releases](https://www.tidyverse.org/blog/2019/04/r-version-support/)
    (#804).
-   Update Github Actions workflow (#810).

We'd like to thank everyone who has furthered the development of the latest
release of styler through their contributions in issues and pull requests:

[\@ardydavari](https://github.com/ardydavari),
[\@gadenbuie](https://github.com/gadenbuie),
[\@IndrajeetPatil](https://github.com/IndrajeetPatil), \@jasonhan-vassar,
[\@laresbernardo](https://github.com/laresbernardo),
[\@lorenzwalthert](https://github.com/lorenzwalthert),
[\@MichaelChirico](https://github.com/MichaelChirico),
[\@Moohan](https://github.com/Moohan),
[\@njtierney](https://github.com/njtierney),
[\@pat-s](https://github.com/pat-s),
[\@psychelzh](https://github.com/psychelzh),
[\@pvalders](https://github.com/pvalders),
[\@RoyalTS](https://github.com/RoyalTS), and
[\@russHyde](https://github.com/russHyde).

# styler 1.4.1

-   fix interaction between cache and `base_indention`. This also fixes the
    Addin for styling a selection with base indention repeatedly (#764).
-   add more examples to `styler_*` helpfiles (#762).
-   hexadecimal integers now preserve the trailing `L` when styled (#761).
-   add a pre-push hook to make sure news bullets are added to each PR (#765).

Thanks to everyone who contributed to this release:

[\@krlmlr](https://github.com/krlmlr),
[\@lorenzwalthert](https://github.com/lorenzwalthert), and
[\@renkun-ken](https://github.com/renkun-ken).

# styler 1.4.0

## API Changes

**new**

-   `style_file()` and friends gain argument `dry` to control if changes should
    be applied to files or not (#634).

-   `style_file()` and friends gain argument `base_indention` (defaulting to 0)
    to control by how much the output code is indented (#649, #692). The Addin
    for styling a selection picks that up, e.g. you can style a function body
    and indention is preserved (#725).

-   added an option for disabling all communication when using the package
    (`styler.quiet`) (#640).

-   `scope` in `tidyverse_style()` can now be specified with higher granularity
    through `I()`, e.g. `I(c('spaces', 'tokens'))` allows us to style spaces
    and tokens without styling line breaks and indention. Previously, only a
    string was allowed and all less invasive scopes were included, e.g. if you
    wanted to style tokens, you had to always also style spaces, indention,
    line breaks as well (#705, #707).

-   added an option (`styler.test_dir_writeable`) that changes test behavior to
    not directly modify test files in the current directory (#548).

-   New argument `transformers_drop` in `create_style_guide()` to be populated
    with new helper function `specify_transformers_drop()` for specifying
    conditions under which transformers are not going to be used and can
    therefore be omitted without effecting the result of styling (#711).

**deprecated**

-   The environment variable `save_after_styling` is deprecated in favor of the
    R option `styler.save_after_styling` to control if a file is saved after
    styling with the RStudio Addin. Note than in RStudio \>= 1.3.0, you can
    auto-save edits in general (Code -\> Saving -\> Auto-Save), e.g. on idle
    editor or focus loss, so this feature becomes less relevant (#631, #726).

## Major changes

-   styler is now distributed under the MIT license (#751).

-   Documentation overhaul: New README, new "Get started" pkgdown page, new
    vignettes on `strict = FALSE`, `Adoption` renamed to
    `Third-party   integrations` (#741), adding search to pkgdown (#623), group
    functions in pkgdown reference page (#625), minor other doc improvements
    (#643, #618, #614, #677, #651, #667, #672, #687, #752, #754).

-   `@exampleIsf` roxygen tag for conditional examples is now supported (#743).

-   blank lines in function calls and headers are now removed, for the former
    only when there are no comments before or after the blank line (#629, #630,
    #635, #723).

-   speed improvements: 15% faster on new code, 70% on repeated styling of
    compliant code (The latter is not so relevant because it was almost
    instantaneous already). Most relevant contributions were #679, #691, #681,
    #711, #739.

-   `#<<` is now recognized as the xaringan marker and no space is added
    after`#` (#700).

## Minor changes and fixes

-   `style_dir()` and `style_pkg()` now apply directory exclusion recursively
    with `exclude_dirs` (#676).

-   `switch()` now has line breaks after every argument to match the tidyverse
    style guide (#722, #727).

-   unary `+` before a function call does not give an error anymore, as before
    version 1.3.0 (#697).

-   certain combinations of `stylerignore` markers and cached expressions now
    don't give an error anymore (#738).

-   cache is now correctly invalidated when style guide arguments change
    (#647).

-   empty lines are now removed between pipes and assignments (#645, #710).

-   multiple `@examples` roxygen tags in a code block of `#'` are no longer
    squashed (#748).

-   roxygen code examples starting on the same line as the `@examples` tag are
    no longer moved to the next line (#748).

-   always strip trailing spaces and make cache insensitive to it (#626).

-   `style_text()` can now style all input that `is.character()`, not just if
    it inherits from classes `character`, `utf8` or `vertical` (#693).

-   logical operators within square braces are now moved from the start of a
    line to the end of the previous line (#709).

-   spaces are now removed before `[` and `[[` (#713).

-   The internal `create_tree()` only used in testing of styler now works when
    the cache is activated (#688).

-   simplification of internals (#692).

## Infrastructure changes

-   switched from travis and AppVeyor to GitHub Actions (#653, #660).

-   Added basic continuous benchmarking with
    [lorenzwalthert/touchstone](https://github.com/lorenzwalthert/touchstone)
    (#674, #684, #698).

-   include `test-*` files in styling pre-commit hook (#724).

Thanks to all the people who made this release possible:

[\@assignUser](https://github.com/assignUser),
[\@ColmanHumphrey](https://github.com/ColmanHumphrey),
[\@davidchall](https://github.com/davidchall),
[\@espinielli](https://github.com/espinielli),
[\@giko45](https://github.com/giko45), [\@hadley](https://github.com/hadley),
[\@IndrajeetPatil](https://github.com/IndrajeetPatil),
[\@intiben](https://github.com/intiben),
[\@jamespeapen](https://github.com/jamespeapen),
[\@jthomasmock](https://github.com/jthomasmock),
[\@Kalaschnik](https://github.com/Kalaschnik),
[\@kevinushey](https://github.com/kevinushey),
[\@krlmlr](https://github.com/krlmlr),
[\@lcolladotor](https://github.com/lcolladotor),
[\@MichaelChirico](https://github.com/MichaelChirico),
[\@michaelquinn32](https://github.com/michaelquinn32),
[\@mine-cetinkaya-rundel](https://github.com/mine-cetinkaya-rundel),
[\@pat-s](https://github.com/pat-s),
[\@PMassicotte](https://github.com/PMassicotte),
[\@QuLogic](https://github.com/QuLogic),
[\@renkun-ken](https://github.com/renkun-ken),
[\@RichardJActon](https://github.com/RichardJActon),
[\@seed-of-apricot](https://github.com/seed-of-apricot),
[\@select-id-from-users](https://github.com/select-id-from-users),
[\@SimonDedman](https://github.com/SimonDedman),
[\@stefanoborini](https://github.com/stefanoborini),
[\@swsoyee](https://github.com/swsoyee), and
[\@Winterstorm-j](https://github.com/Winterstorm-j).

# styler 1.3.2

Release upon request by the CRAN team.

## Minor changes and fixes

-   Add search and reference sections to pkgdown webpage (#623, #625).
-   various fixes to handle special cases for caching and stylerignore and
    their interaction (#611, #610, #609, #607, #602, #600).
-   also test on macOS (#604).
-   skip timing tests on CRAN as requested by CRAN team because they did not
    pass on all machines (#603).

# styler 1.3.1

Emergency release. In case multiple expressions are on one line and only some
of them are cached, styler can remove code. To reach this state, some of the
expressions must have been styled previously alone and the cache must be
active. Example:

    library(styler)
    cache_activate()
    #> Using cache 1.3.0 at ~/.Rcache/styler/1.3.0.
    style_text("1")
    #> 1
    style_text("1 # comment")
    #> # comment

This is obviously detrimental. We have added additional tests and fixed the
problem (#593, #595), but we want repeat the warning from `?style_file` that
all style APIs apart from `style_text()` overwrite code and that styler can
only check the AST remains valid with `scope < "tokens"`. So use this if you
are conservative. Or deactivate the cache with `deactivate_cache()` until it
has fully matured.

We thank the people who have contributed to this release:

[\@ellessenne](https://github.com/ellessenne) and
[\@renkun-ken](https://github.com/renkun-ken).

# styler 1.3.0

## Breaking changes

-   `style_pkg()` and `style_dir()` gain a new argument `exclude_dirs` to
    exclude directories from styling, by default `renv` and `packrat`. Note
    that the defaults won't change the behavior of `style_pkg()` because it
    does anyways does not style these directories and they were set for
    consistency.

-   `style_file()` and friends now strip `./` in file paths returned invisibly,
    i.e. `./script.R` becomes `script.R` (#568).

## New features

-   ignore certain lines using `# styler: off` and `#styler: on` or custom
    markers, see `?stylerignore` (#560).

-   styler caches results of styling, so applying styler to code it has styled
    before will be instantaneous. This brings large speed boosts in many
    situations, e.g. when `style_pkg()` is run but only a few files have
    changed since the last styling or when using the [styler pre-commit
    hook](https://github.com/lorenzwalthert/precommit). Because styler caches
    by expression, you will also get speed boosts in large files with many
    expressions when you only change a few of them. See `?caching` for details
    (#538, #578).

-   `create_style_guide()` gains two arguments `style_guide_name` and
    `style_guide_version` that are carried as meta data, in particular to
    version third-party style guides and ensure the proper functioning of
    caching. This change is completely invisible to users who don't create and
    distribute their own style guide like `tidyverse_style()` (#572).

## Minor changes and fixes

-   lines are now broken after `+` in `ggplot2` calls for `strict = TRUE`
    (#569).

-   function documentation now contains many more line breaks due to roxygen2
    update to version 7.0.1 (#566).

-   spaces next to the braces in subsetting expressions `[` and `[[` are now
    removed (#580).

-   Adapt to changes in the R parser to make styler pass R CMD check again.
    (#583).

Thanks to all contributors involved, in particular
[\@colearendt](https://github.com/colearendt),
[\@davidski](https://github.com/davidski),
[\@IndrajeetPatil](https://github.com/IndrajeetPatil),
[\@pat-s](https://github.com/pat-s), and
[\@programming-wizard](https://github.com).

# styler 1.2.0

## Breaking changes

-   `style_file()` now correctly styles multiple files from different
    directories. We no longer display the file name of the styled file, but the
    absolute path. This is also reflected in the invisible return value of the
    function (#522).

-   `style_file()` and friends do not write content back to a file when styling
    does not cause any changes in the file. This means the modification date of
    styled files is only changed when the content is changed (#532).

## New features

-   Aligned function calls are detected and remain unchanged if they match the
    styler [definition for aligned function
    calls](https://styler.r-lib.org/articles/detect-alignment.html) (#537).

-   curly-curly (`{{`) syntactic sugar introduced with rlang 0.4.0 is now
    explicitly handled, where previously it was just treated as two consecutive
    curly braces (#528).

-   `style_pkg()`, `style_dir()` and the Addins can now style `.Rprofile`, and
    hidden files are now also styled (#530).

## Minor improvements and fixes

-   Roxygen code examples: leverage `roxygen2` for correct escaping of
    expressions that contain `\`, in particular in `dontrun{}` and friends,
    allow quoted braces that are not matched (#729).

-   Brace expressions in function calls are formatted in a less compact way to
    improve readability. Typical use case: `tryCatch()` (#543).

-   Arguments in function declarations in a context which is indented multiple
    times should now be correct. This typically affects `R6::R6Class()` (#546).

-   Escape characters in roxygen code examples are now correctly escaped
    (#512).

-   Special characters such as `\n` in strings are now preserved in text and
    not turned into literal values like a line break (#554).

-   Style selection Addin now preserves line break when the last line selected
    is an entire line (#520).

-   Style file Addin can now properly handle cancelling (#511).

-   The body of a multi-line function declaration is now indented correctly for
    `strict = FALSE` and also wrapped in curly braces for `strict = TRUE`
    (#536).

-   Advice for contributors in `CONTRIBUTING.md` was updated (#508).

## Adaption

-   styler is now available through the pre-commit hook `style-files` in
    <https://github.com/lorenzwalthert/precommit>.

Thanks to all contributors involved, in particular

[\@Banana1530](https://github.com/Banana1530),
[\@batpigandme](https://github.com/batpigandme),
[\@cpsievert](https://github.com/cpsievert),
[\@ellessenne](https://github.com/ellessenne),
[\@Emiller88](https://github.com/Emiller88),
[\@hadley](https://github.com/hadley),
[\@IndrajeetPatil](https://github.com/IndrajeetPatil),
[\@krlmlr](https://github.com/krlmlr),
[\@lorenzwalthert](https://github.com/lorenzwalthert),
[\@lwjohnst86](https://github.com/lwjohnst86),
[\@michaelquinn32](https://github.com/michaelquinn32),
[\@mine-cetinkaya-rundel](https://github.com/mine-cetinkaya-rundel),
[\@Moohan](https://github.com/Moohan), [\@nxskok](https://github.com/nxskok),
[\@oliverbeagley](https://github.com/oliverbeagley),
[\@pat-s](https://github.com/pat-s), \@reddy-ia, and
[\@russHyde](https://github.com/russHyde)

# styler 1.1.1

This is primarily a maintenance release upon the request of the CRAN team
(#490).

## Major changes

-   Users can now control style configurations for styler Addins (#463, #500),
    using the `Set style` Addin. See `?styler::styler_addins` for details.

-   `return()` is now always put in braces and put on a new line when used in a
    conditional statement (#492).

-   `%>%` almost always causes a line break now for `strict = TRUE` (#503).

## Minor changes

-   `style_pkg()` now also styles the "demo" directory by default (#453).

-   multi-line strings are now styled more consistently (#459).

-   indention in roxygen code example styling (#455) and EOF spacing (#469) was
    fixed.

-   indention for for loop edge case (#457) and comments in pipe chain (#482)
    were fixed.

-   line-break styling around comma is improved (#479).

-   bug that can cause an error when the variable `text` in any name space
    before styler on the search path was defined and did not have length 1 is
    fixed (#484).

-   slightly confusing warning about empty strings caused with roxygen code
    examples and Rmd was removed.

-   right apostrophe to let package pass R CMD Check in strict Latin-1 locale
    was removed (#490, reason for release).

## Adaption of styler

Since it's never been mentioned in the release notes, we also mention here
where else you can use styler functionality:

-   `usethis::use_tidy_style()` styles your project according to the tidyverse
    style guide.

-   `reprex::reprex(style = TRUE)` to prettify reprex code before printing. To
    permanently use `style = TRUE` without specifying it every time, you can
    add the following line to your `.Rprofile` (via
    `usethis::edit_r_profile()`): `options(reprex.styler = TRUE)`.

-   you can pretty-print your R code in RMarkdown reports without having styler
    modifying the source. This feature is implemented as a code chunk option in
    knitr. use `tidy = "styler"` in the header of a code chunks (e.g.
    ```` ```{r   name-of-the-chunk, tidy = "styler"} ````), or
    `knitr::opts_chunk$set(tidy =   "styler")` at the top of your RMarkdown
    script.

-   pretty-printing of [drake](https://github.com/ropensci/drake) workflow data
    frames with `drake::drake_plan_source()`.

-   Adding styler as a fixer to the [ale
    Plug-in](https://github.com/dense-analysis/ale/pull/2401) for VIM.

Thanks to all contributors involved, in particular
[\@ArthurPERE](https://github.com/ArthurPERE),
[\@hadley](https://github.com/hadley), [\@igordot](https://github.com/igordot),
[\@IndrajeetPatil](https://github.com/IndrajeetPatil),
[\@jackwasey](https://github.com/jackwasey),
[\@jcrodriguez1989](https://github.com/jcrodriguez1989),
[\@jennybc](https://github.com/jennybc),
[\@jonmcalder](https://github.com/jonmcalder),
[\@katrinleinweber](https://github.com/katrinleinweber),
[\@krlmlr](https://github.com/krlmlr),
[\@lorenzwalthert](https://github.com/lorenzwalthert),
[\@michaelquinn32](https://github.com/michaelquinn32),
[\@msberends](https://github.com/msberends),
[\@raynamharris](https://github.com/raynamharris),
[\@riccardoporreca](https://github.com/riccardoporreca),
[\@rjake](https://github.com/rjake),
[\@Robinlovelace](https://github.com/Robinlovelace),
[\@skirmer](https://github.com/skirmer),
[\@thalesmello](https://github.com/thalesmello),
[\@tobiasgerstenberg](https://github.com/tobiasgerstenberg),
[\@tvatter](https://github.com/tvatter),
[\@wdearden](https://github.com/wdearden),
[\@wmayner](https://github.com/wmayner), and \@yech1990.

# styler 1.1.0

This release introduces new features and is fully backward-compatible. It also
adapts to changes in the R parser committed into R devel (#419).

## Major Changes

-   styler can now style roxygen code examples in the source code of package
    (#332) as well as Rnw files (#431).

-   the print method for the output of `style_text()` (`print.vertical()`) now
    returns syntax-highlighted code by default, controllable via the option
    `styler.colored_print.vertical` (#417).

-   the README was redesigned (#413).

-   semi-colon expression that contained multiple assignments was fixed (#404).

## Minor Changes

-   cursor position is remembered for styling via Addin (#416).

-   adapt spacing around tilde for multi-token expressions(#424) and brace edge
    case (#425).

-   only add brackets to piped function call if RHS is a symbol (#422).

-   increase coverage again to over 90% (#412).

-   move rule that turns single quotes into double quotes to token modifier in
    \`tidyverse_style_guide() (#406).

-   remove line-breaks before commas (#405).

-   removed package dependency enc in favor of xfun (#442).

Thanks to all contributors for patches, issues and the like: @jonmcalder,
@krlmlr, @IndrajeetPatil, @kalibera, @Hasnep, @kiranmaiganji, @dirkschumacher,
@ClaytonJY, @wlandau, @maurolepore

# styler 1.0.2

This is a maintenance release without any breaking API changes.

## Major Changes

-   Fixed indention for named multi-line function calls (#372).

-   Non-R code chunks in `.Rmd` files are now respected and won't get styled
    (#386).

## Minor Changes

-   Fixing an edge case in which, if very long strings were present in the
    code, tokens could be replaced with wrong text (#384).

-   Spacing around tilde in formulas depends now on whether there is a LHS in
    the formula (#379).

-   Spaces are now also added around `EQ_SUB` (`=`) (#380).

-   Added `CONTRIBUTING.md` to outline guidelines for contributing to styler.

-   More informative error messages for parsing problems (#401, #400).

-   Improved documentation (#387).

Thanks to all contributors for patches, issues and the like: @katrinleinweber,
@krlmlr, @dchiu911, @ramnathv, @aedobbyn, @Bio7, @tonytonov, @samhinshaw, @fny,
@vnijs, @martin-mfg, @NGaffney, @dchiu911.

# styler 1.0.1

This is a maintenance release without any breaking API changes.

## Major & dependency related changes

-   Removed implicit `dplyr` dependency via `purrr:::map_dfr()` (thanks
    @jimhester, #324).

-   Added required minimal version dependency for purr (`>= 0.2.3`) (#338).

-   We rely on the tibble package which was optimized for speed in `v1.4.2` so
    styler should run \~2x as fast
    [(#348)](https://github.com/tidyverse/tibble/pull/348). For that reason,
    styler now depends on `tibble >= 1.4.2`.

-   In the dependency `enc`, a bug was fixed that removed/changed non-ASCII
    characters. Hence, styler now depends on `enc >= 0.2` (#348).

## Minor changes

-   We're now recognizing and respecting more DSLs used in R comments: rplumber
    (`#*`, #306), shebang `#/!` (#345), knitr chunk headers for spinning (`#+`
    / `#-`, #362).

-   Named arguments can stay on the first line if call is multi-line (#318).

-   No space anymore with `tidyverse_style()` after `!!` since with
    `rlang 0.2`, `!!` now binds tighter (#322), spacing around `~` (#316), no
    space anymore around `^` (#308).

-   Code chunks in Rmd documents that don't use the R engine are no longer
    formatted (#313).

-   Various bug fixes and edge case improvements.

Thanks to all contributors for patches, issues and the like: @devSJR, @klrmlr,
@yutannihilation, @samhinshaw, @martin-mfg, @jjramsey, @RMHogervorst, @wlandau,
@llrs, @aaronrudkin, @crew102, @jkgrain, @jennybc, @joranE.

# styler 1.0.0

Initial release.

## stylers

These are functions used to style code. They style a directory, a whole
package, a file or a string.

    style_dir(path = ".", 
      ..., style = tidyverse_style, transformers = style(...), 
      filetype = "R", recursive = TRUE, exclude_files = NULL
    )

    style_pkg(pkg = ".", 
      ..., style = tidyverse_style, transformers = style(...), filetype = "R", 
      exclude_files = "R/RcppExports.R"
    )


    style_file(path, 
      ..., style = tidyverse_style, transformers = style(...)
    )

    style_text(text, ..., style = tidyverse_style, transformers = style(...))

## style guides

These functions are the style guides implemented.

    tidyverse_style(
      scope = "tokens", 
      strict = TRUE, 
      indent_by = 2, 
      start_comments_with_one_space = FALSE, 
      reindention = tidyverse_reindention(), 
      math_token_spacing = tidyverse_math_token_spacing()
    )
    tidyverse_reindention()
    tidyverse_math_token_spacing())

## style guide creators

This function is used to create a style guide.

    create_style_guide(
      initialize = default_style_guide_attributes, 
      line_break = NULL, 
      space = NULL, 
      token = NULL, 
      indention = NULL, 
      use_raw_indention = FALSE, 
      reindention = tidyverse_reindention()
    )

## Helpers

These are helper functions used to specify the style guides in use.

    specify_math_token_spacing(
      zero = NULL, 
      one = c("'+'", "'-'", "'*'", "'/'", "'^'")
    )

    specify_reindention(
      regex_pattern = NULL, 
      indention = 0, 
      comments_only = TRUE
    )
    initialize_default_attributes(pd_flat)
