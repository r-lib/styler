# precommit v0.2.2

- Fixing more CRAN issues (#348).
- Use RSMP for all packages (#348).

# precommit v0.2.1

This is a maintenance release on the request of CRAN (#337) and to improve 
experience with the `language: r` switch conducted in `v0.2.0`. In particular, 
note that projects using {renv} and RStudio are not very compatible with hook 
versions > 0.1.3.9014 and `autoupdate()` results in downgrading the hook 
versions (#346).

**API Changes**


* The argument `--no-warn-cache` in the `style-files` and the `roxygenize` hook 
  is deprecated and will be removed in a future release. Please remove it from 
  your `.pre-commit-config.yaml` (#340. #341).

**Other changes**

* `style-files` hook fails more informatively if required package is not listed
  in `additional_dependencies:` (#333).
* configure all git operations to use LF (#337).
* `deps-in-desc` always excludes `README.md` (#336).
* Document timeout and other problems with pre-commit.ci (#335).
* simplify `roxygenize` problem handling (#338).
* More executables on macOS are detected (#344).
* Only hard dependencies are generated with 
  `snippet_generate("additional-deps-roxygenize")` (#344)

Thanks to all people who contributed to this release: 

[&#x0040;lorenzwalthert](https://github.com/lorenzwalthert), [&#x0040;pat-s](https://github.com/pat-s), and [&#x0040;smingerson](https://github.com/smingerson).

# precommit v0.2.0


This version marks the switch to [`language: r`](https://pre-commit.com/#r) of 
all existing hooks. This means two things:

* creation of isolated pre-commit environments: No 
  more manual dependency management for hooks nor conflicts with your global R 
  library and more consistent output of hooks from different collaborators in a
  project. Thanks to {renv}'s excellent 
  [caching](https://rstudio.github.io/renv/articles/renv.html#cache-1), this 
  hardly consumes any space and is fast.
  This requires the Python package `pre-commit >= 2.11.1` (ideally even
  `>= 2.13.0`). See *Installation/Update** below (#233, #250, #260, #264, #273,
  #315, #313, #308, #301, #300, #295, #285, #328).
* support for continuous integration via [pre-commit.ci](https://pre-commit.ci)
  and [GitHub Actions](https://github.com/pre-commit/action)), 
  that is, running the pre-commit hooks as part of a CI pipeline. This
  means that hook passing can be enforced for pull requests, even if the creator
  did not run the hooks locally. Further, the diff from running the hooks is 
  committed and pushed back to the remote repository. This may fix the failing 
  hook problems in some cases (e.g. `style-files`). See `vignette("ci")` for a
  comparison of the two services (#318). 
  
**API changes**

*  `use_precommit()` gains a new argument `ci` defaulting to `"native"` (for 
  [pre-commit](https://pre-commit.ci)) to set up continuous
  integration. Other allowed values are `"gha"` (for  
  [GitHub Actions](https://github.com/pre-commit/action)) or 
  `NULL` (for no CI). 
* The new exported function `use_ci(ci = "native")` can be used to set up 
  continuous integration for existing repos. The default behavior for `ci` for 
  both functions is governed by the R option `precommit.ci`.
- `version_precommit()` and `update_precommit()` are new functions to check the
  version of the installed pre-commit executable and to update it (#197).
* `style-files` hook gains an argument `--cache-root` that is passed to 
  `options(styler.cache_root = ...)` (#305).
- `style-files` hook now supports the full
  [`style_file()`](https://styler.r-lib.org/dev/reference/style_file.html) API,
  e.g. you can supply `--scope=spaces` and similar via `args:` in your
  `.pre-commit-config.yaml`. See the
  [docs](https://lorenzwalthert.github.io/precommit/articles/available-hooks.html#style-files-1)
  for details.
- `style-files` and `roxygenize` hooks now warn if there is no permanent 
  `{R.cache}` cache set up. You can silence the warning with the hook argument 
  `--no-warn-cache` (#225).


**Installation/Update**

Please follow the 
[update instructions](https://lorenzwalthert.github.io/precommit/dev/#update) or
[installation instructions](https://lorenzwalthert.github.io/precommit/dev/#installation)
depending on whether or not you previously used pre-commit.


**Major changes**

- Because hooks run in a virtual environment and the `roxygenize` hook runs
  `pkgload::load_all()`, you need to list all dependencies of your package in
  `additional_dependencies` field in `.pre-commit-config.yaml`. You will be 
  prompted to add them if they are missing, 
  `precommit::snippet_generate("additional-deps-roxygenize")` generates
  the code you can copy/paste (#247, #248, #249).
- the `parsable-R` hook can now also parse `.Rmd` files (#325).
- In order to avoid multiple installations of the pre-commit framework, a 
  warning is issued if multiple are found so the user can remove them (#266, 
  #273, #277, #278).
* `use_precommit(..., install_hooks = TRUE)` is no longer blocking by default.
  New option `precommit.block_install_hooks` (defaults to `FALSE`) governs the
  behavior (#312).
* Always sort `inst/WORDLIST` (#303).
* `.lintr` and `.gitlab-ci.yml` are not ignored in the spell check hook (#317).
- Warnings are no longer promoted to errors in the styler hook, which is 
  particularly relevant for the apparently random error 
  `Unknown or uninitialised column: text` (#268).
- `deps-in-desc` now checks `.Rprofile`, `.Rmd` and `.Rnw` files in addition to 
  `.R` files (#216).  
- the lintr and styler hook now also check `.Rmd`, `.Rnw` and `.Rprofile` files 
  (#287).
- `{renv}` infra files are not checked anymore by default in the template config
  files (#237).
- `.png`, `.jpeg`, `.pdf` and files in `.github/workflows` are no longer 
  spell-checked in the template config file (#276).


**Minor changes**

* rename default branch to *main* (#307).
* Use dev version of {lintr} to reduce total dependencies from 71 to 59 that
  brings down install time.
- The location of the pre-commit executable is now also recognized on Apple 
  Silicon when installed with Homebrew (#240).
- The location of the pre-commit executable is now also recognized on macOS when
  installed with pip3 and `fs >= 1.5.1` (#330).
- pinning python version to 3.9 for conda until problems related to 3.10 are
  fixed (#310).
- The cache for the roxygen2 hook is now also invalidated for changes in formals 
  if there are no changes in roxygen comments (#214).
- All sub-patterns in the `exclude:` pattern of the spell check hook are now 
  ordered alphabetically (#276).
- The `deps-in-desc` hook now points to the hook argument 
  `--allow_private_imports` when the hook fails due to private imports (#254).
- roxygenize hook is now fully tested (#267).
- Hook scripts were relocated and R hooks now have a file extension (#280).
- Hook dependency updates are proposed by an automatic monthly pull request 
  to `lorenzwalthert/precommit`. This does not affect users directly (#430).
- Updated GitHub Action workflows (#288).
* Use LF line endings in git config to ensure passing tests on Windows for R 
  devel (#321).
- fixing typos (#289).
- fix R CMD Check (#284).


A big hand to all the contributors of this release:

[&#x0040;adamblake](https://github.com/adamblake), 
[&#x0040;arbues6](https://github.com/arbues6), 
[&#x0040;b4D8](https://github.com/b4D8), 
[&#x0040;bart1](https://github.com/bart1), 
[&#x0040;dhersz](https://github.com/dhersz), [&#x0040;joelnitta](https://github.com/joelnitta), 
[&#x0040;jucor](https://github.com/jucor),
[&#x0040;lorenzwalthert](https://github.com/lorenzwalthert), [&#x0040;lukasfeick-sw](https://github.com/lukasfeick-sw), [&#x0040;MarkMc1089](https://github.com/MarkMc1089), 
[&#x0040;njtierney](https://github.com/njtierney), 
[&#x0040;pat-s](https://github.com/pat-s), [&#x0040;pwildenhain](https://github.com/pwildenhain), and [&#x0040;rossdrucker](https://github.com/rossdrucker)

For previous versions of `NEWS.md` with news bullet per patch release, see the 
[latest `NEWS.md` before gathering](https://github.com/lorenzwalthert/precommit/blob/7a8740714ab868d20e981b8b80898d7be050e34e/NEWS.md).

# precommit v0.1.3

This is mainly a CRAN maintenance release because of #201 and includes some 
bug fixes and no API changes.

A big hand to all the contributors of this release:
[&#x0040;fschoner](https://github.com/fschoner), [&#x0040;krzyslom](https://github.com/krzyslom),
[&#x0040;lorenzwalthert](https://github.com/lorenzwalthert), [&#x0040;maurolepore](https://github.com/maurolepore),
[&#x0040;pat-s](https://github.com/pat-s), and [&#x0040;zkamvar](https://github.com/zkamvar).


**Minor Changes**

- install_hooks in `use_precommit()` should now work as expected (#185).
- Switching from Travis to GitHub Actions (#191).
- `use_precommit()` now writes to the right `.Rbuildignore` when `root` is not 
  the current working directory (#188, @krzyslom).
- Documentation improvements (#179, @maurolepore)
- `precommit::uninstall_precommit()` now correctly removes 
  `^\\.pre-commit-config.yaml$`from `.Rbuildignore` and does not show a 
  prompt (#199).

# precommit v0.1.2

Initial CRAN release. See https://lorenzwalthert.github.io/precommit/.

# precommit v0.0.0.9049 

- Roxygen hook cache only includes files that are part of the index and 
  will hence be able to use the cache in cases it was previously invalidated 
  without need (#171).
- styler hook should fail if styling any file fails (#174).


# precommit v0.0.0.9048

- Don't allow legacy hooks by default, delete README hook from usethis 
  informatively (#167).

# precommit v0.0.0.9046

**Major Changes**

- `use_precommit()` gains new `install_hooks = TRUE` parameter. Now all hook
  environments defined in yaml config are by default installed in advance.
  (#163)

# precommit v0.0.0.9044

**Breaking Changes**

- `uninstall_precommit()`'s `scope` argument value `"global"` was renamed to 
  `"user"`.

**Major Changes**

- By default, the spell check hook now exposes a regex pattern in the default
  `.pre-commit-config-config.yaml` for the spell check hook for easy 
  manipulation. Also, additional patterns were excluded such as `.sh` (#157).

**Minor Changes**

- `DESCRIPTION` now links the pkgdown website in addition to the GitHub repo (#155).
- `.Renviron`, `.Rrofile`, `.RData`, `.feather` and `.Rds` is now also excluded
  from the spell checking hook.


# precommit v0.0.0.9042

**Breaking Changes**

A major API review was conducted to make the package ready for a CRAN 
submission in #152:

- all functions have the argument `path_root` renamed to `root`.
- argument `root` now always defaults to `here::here()` for consistency.
- argument `config_source` was renamed to `config_source` in 
  `use_precommit_config()` and `use_precommit()`.
- `use_precommit_config`'s argument `force` gains a default value for 
  consistency with `use_config()`.
- `use_precommit_config`'s argument `verbose` now is in the same position as
  in `use_config()`.

Additional breaking changes are:

- use env `r-precommit` instead of `r-reticulate` to avoid conflicts with other 
  packages commonly installed in `r-reticulate` (#147).
- the hook `spell-check` does no longer take the hook argument `ignore-files` 
  since this was inconsistent with the pre-commit framework. To exclude 
  additional files, specify the `exclude:` key in your `.pre-commit-config.yaml`
  file. If you like to keep excluding the default files, make sure you include 
  the [default regex](https://lorenzwalthert.github.io/precommit/articles/available-hooks.html#spell-check)
  (#153).

# precommit v0.0.0.9040

- roxygenize hook now only ran when `git diff --cached` contains roxygen 
  comments (#151).
- conda is a suggested dependency now, so those who choose a different 
  installation method have a more lightweight dependency graph (#136).
- Use more thoughtful order for hooks, between pkg and project (#142).

# precommit v0.0.0.9031 up to v0.0.0.9038

Test release process.

# precommit v0.0.0.9030

- auto-detect pre-commit executable when installed via pip for Linux (#120), 
  macOS (#131).
- auto-detect pre-commit executable when installed via brew for macOS (#132).
- `use_precommit()` gains new argument `config_source` to copy an existing
  config file into the repo at initialization. The argument defaults to 
  `options('precommit.config_source')` to make it easy for users to use 
  their preferred hooks in every repo they initialize (#111).
- Create `r-precommit` env if not existent before installing into it (#114).
- Unify vignettes on available hooks and arguments (#109).
- Fail fast when repo is not a git repo (#111).
- default config file has spell-check activated (#118).
- also test on macOS (#120).
- template `.pre-commit-config.yaml` includes hook to check files have a EOF
  blank line (#126).
- spell-check hook now removes blank lines hand has no blank line at EOF (#127).
- depreciate `path_pre_commit_exec()` in favor of `path_precommit_exec()` 
  and adapt internals to replace `pre_commit` with `precommit` (#130).
- more coherent error catching for system calls (#135).
- internal refactoring (#136).

# precommit 0.0.0.9017

- **BREAKING:** If you used precommit < 0.0.0.9017 (and if you choose a 
  different installation strategy than `precommit::install_precommit()), you can link your existing 
  executable by setting the R option `precommit.executable` to the path where 
  you stored the pre-commit executable.
  
- Adding tools to work with hooks: ` install_precommit()`, `use_precommit()`,
  `uninstall_precommit()`, `open_config()` and `open_wordlist()` using conda 
  environments on all platforms.

- Make repo a fully R CMD CHECK compliant R package (includes moving hooks to
  `inst/bin` from `bin`.)

- renamed repo from pre-commit-hooks to precommit.

- added pkgdown website, restructured README.
