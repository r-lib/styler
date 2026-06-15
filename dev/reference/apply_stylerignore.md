# Ensure correct positional information for stylerignore expressions

Ensure correct positional information for stylerignore expressions

## Usage

``` r
apply_stylerignore(flattened_pd)
```

## Arguments

- flattened_pd:

  A flattened parse table.

## Details

- Get the positional information for tokens with a stylerignore tag from
  `env_current`, which recorded that information from the input text.

- Replace the computed lag_newlines and lag_spaces information in the
  parse table with this information.

- Because we may remove or add tokens when applying the transformers, it
  is not save to merge via the pos_id of each token in a stylerignore
  sequence. We assume that the start and stop markers are the same after
  styling, so we join all tokens that were initially in a stylerignore
  sequence via the first pos_id in that stylerignore sequence.
