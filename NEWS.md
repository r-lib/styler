# precommit post v0.0.0.9028

- Unify vignettes on available hooks and arguments (#109).

# precommit 0.0.0.9017

- **BREAKING:** If you used precommit < 0.0.0.9017 (and if you choose a 
  different installation strategy than `precommit::install_precommit()), you can link your existing 
  executable by setting the R option `precommit.executable` to the path where 
  you stored the pre-commit executable.
  
- Adding tools to work with hooks: ` install_precommit()`, `use_precommit()`,
  `uninstall_precommit()`, `open_config()` and `open_wordlist()` using conda 
  environments on all plattforms.

- Make repo a fully R CMD CHECK compliant R package (includes moving hooks to
  `inst/bin` from `bin`.)

- renamed repo from pre-commit-hooks to precommit.

- added pkgdown website, restructured README.
