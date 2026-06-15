# Copy a file to a temporary directory

Takes the path to a file as input and returns the path where the
temporary file is stored. Don't forget to unlink once you are done.

## Usage

``` r
copy_to_tempdir(path_perm = testthat_file())
```

## Arguments

- path_perm:

  The path of the file to copy.
