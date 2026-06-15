# Obtain token table from text

[`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html) is
used to obtain a flat parse table from `text`.

## Usage

``` r
tokenize(text)
```

## Arguments

- text:

  The text to parse.

## Value

A flat parse table

## Details

Apart from the columns provided by
[`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html),
the following columns are added:

- A column "short" with the first five characters of "text".

- A column "pos_id" for (positional id) which can be used for sorting
  (because "id" cannot be used in general). Note that the nth value of
  this column corresponds to n as long as no tokens are inserted.

- A column "child" that contains *nest*s.
