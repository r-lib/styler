# Identify strings that were not fully parsed

Identifies strings that were not fully parsed due to their vast length.

## Usage

``` r
is_insufficiently_parsed_string(pd)
```

## Arguments

- pd:

  A parse table.

## Details

The meaning of the variable `is_problematic_string` in the source code
changes from "all strings" to "all problematic strings", is partly
misleading and this approach was chosen for performance reasons only.
