# Run a collection of tests

Run transformations on all \*-in.R files in a test directory and compare
them with their \*-out.R counterpart.

## Usage

``` r
test_collection(
  test,
  sub_test = NULL,
  dry = "off",
  write_tree = FALSE,
  transformer,
  ...
)
```

## Arguments

- test:

  The test to run. It corresponds to a folder name in tests/testthat.

- sub_test:

  A regex pattern to further reduce the amount of test files to be
  tested in the test. `sub_test` must match the beginning of file names
  in tests/testthat. `NULL` matches all files.

- dry:

  To indicate whether styler should run in *dry* mode, i.e. refrain from
  writing back to files .`"on"` and `"fail"` both don't write back, the
  latter returns an error if the input code is not identical to the
  result of styling. "off", the default, writes back if the input and
  output of styling are not identical.

- write_tree:

  Whether or not the tree structure of the test should be computed and
  written to a files.

- transformer:

  A function to apply to the content of `in_item`.

- ...:

  Parameters passed to transformer function.

## Details

Each file name that matches `test` and `sub_test` and ends with "-in.R"
is considered as an input to test. Its counterpart, the reference to
compare it against is the \*-out.R file. It is constructed by taking the
substring of the \*-in.R file before the last dash and adding -out.R. In
contrast to older versions of this function, every \*-out.R file has
just one in file.
