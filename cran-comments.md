## Test environments

* local OS X install (10.15.13): R 4.0
* ubuntu 14.04 (on travis-ci): R devel, R 4.0
* win-builder: R devel, R 4.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Downstream Dependencies

None.

## Resubmission 

This is the second re-submission. I changed the following.

- put the word 'styler' in the Description field in DESCRIPTION in quotes since
  it's a package name to fix *Possibly mis-spelled words in DESCRIPTION*.

Some help files have examples which are wrapped into \dontrun and I was asked to
unwrap them if they are executable. They are not because they depend on the 
pre-commit executable which is not available on the CRAN machines.

***


This is the first re-submission. I have: 

- removed the redundant 'for R' from the `Title:` in DESCRIPTION.
- extended the `Description:` field in `DESCRIPTION` to be a longer description 
  of what the package does.
- added return value documentation for exported function in the case there was 
  none before.
- added examples to the exported functions.

I was also asked to add references for the methods I implement but there are 
none as this is a tooling package that does not implement any statistical 
method.
