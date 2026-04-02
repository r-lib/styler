# Add the roxygen mask to code

This function compares `text` with `initial_text` to make sure a mask is
only added to roxygen comments, not ordinary comments

## Usage

``` r
add_roxygen_mask(text, initial_text, example_type)
```

## Arguments

- text:

  Character vector with code.

- initial_text:

  The roxygen code example to style with mask and potentially ordinary
  comments.

- example_type:

  Either 'examples' or 'examplesIf'.
