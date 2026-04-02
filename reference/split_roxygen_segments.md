# Split text into roxygen and non-roxygen example segments

Split text into roxygen and non-roxygen example segments

## Usage

``` r
split_roxygen_segments(text, roxygen_examples)
```

## Arguments

- text:

  Roxygen comments

- roxygen_examples:

  Integer sequence that indicates which lines in `text` are roxygen
  examples. Most conveniently obtained with
  [identify_start_to_stop_of_roxygen_examples_from_text](https://styler.r-lib.org/reference/identify_start_to_stop_of_roxygen_examples_from_text.md).

## Value

A list with two elements:

- A list that contains elements grouped into roxygen and non-roxygen
  sections. This list is named `separated`.

- An integer vector with the indices that correspond to roxygen code
  examples in `separated`.
