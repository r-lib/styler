# Capture and post-process the output of `style_file` without causing side effects

Capture and post-process the output of `style_file` without causing side
effects

## Usage

``` r
catch_style_file_output(file_in)
```

## Arguments

- file_in:

  A vector with paths relative to `tests/testthat` the path to the
  reference file.

## Value

A list. Each element is a character vector with the captured output of
[`style_file()`](https://styler.r-lib.org/reference/style_file.md)
called on `file_in` ran in a temp dir to avoid side effects on the input
file (because the next time the test would ran, the file would not need
styling). The styling is carried out with a temporary working directory
change to keep filenames relative and avoid portability issues in the
exact output comparison which is needed when the system that runs the
unit testing (CI) is a different system than the one that created the
reference value. This also implies that the ruler width, which depend on
the path length, will again have the same width on all systems and is
independent of how many characters the path of the temporary directory
has.
