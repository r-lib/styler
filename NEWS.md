# precommit v0.2.0-rc (Development)

**API changes**

- `version_precommit()` and `update_precommit()` are new functions to check the
  version of the installed pre-commit executable and to update it (#197).
- `style-files` hook now supports the full [`style_file()`](https://styler.r-lib.org/dev/reference/style_file.html) API,
  e.g. you can supply `--scope=spaces` and similar via `args:` in your
  `.pre-commit-config.yaml`. See the [docs](https://lorenzwalthert.github.io/precommit/articles/available-hooks.html#style-files-1)
  for details.
- `style-files` and `roxygenize` hooks now warn if there is no permanent 
  `{R.cache}` cache set up. You can silence the warning with the hook argument 
  `--no-warn-cache` (#225).

**Minor changes**

- The cache for the roxygen2 hook is now also invalidated for changes in formals 
  if there are no changes in roxygen comments (#214).
- `style-files` an `spell-check` now don't check `{renv}` infra files anymore 
  (#237).
- `deps-in-desc` now checks `.Rprofile`, `.Rmd` and `.Rnw` files in addition to 
  `.R` files (#216).  

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
