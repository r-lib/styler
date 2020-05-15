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

This is the first re-submission. I have: 

- removed the redundant 'for R' from the `Title:` in DESCRIPTION.
- extended `Description:` to be a longer description of what the package does in 
  `DESCRIPTION`.
- added return value documentation for exported function in the case there was 
  none before.
- added examples to the exported functions.

I was also asked to add references for the methods I implement but there are 
none as this is a tooling package that does not implement any statistical 
method.
