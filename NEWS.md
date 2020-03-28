# precommit post v0.0.0.9028

- `use_precommit()` gains new argument `path_cp_config_from` to copy an existing
  config file into the repo at initialization. The argument defaults to 
  `options('precommit.path_cp_config_from')` to make it easy for users to use 
  their preferred hooks in every repo they initialize (#111).
- Create `r-reticulate` env if not existent before installing into it (#114).
- Unify vignettes on available hooks and arguments (#109).
- Fail fast when repo is not a git repo (#111).
- default config file has spell-check activated (#118).
- also test on macOS (#120).
- test if pre-commit was installed via pip (#120).
- template `.pre-commit-config.yaml` includes hook to check files have a EOF
  blank line (#125).

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
