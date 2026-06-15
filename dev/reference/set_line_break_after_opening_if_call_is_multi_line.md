# Sets line break after opening parenthesis

Sets line break after opening parenthesis

## Usage

``` r
set_line_break_after_opening_if_call_is_multi_line(
  pd,
  except_token_after = NULL,
  except_text_before = NULL,
  force_text_before = NULL
)
```

## Arguments

- pd:

  The parse table.

- except_token_after:

  The tokens after the token that cause an exception.

- except_text_before:

  A character vector with text before a token that does not cause a line
  break.

- force_text_before:

  A character vector with text before "'('" that forces a line break
  after every argument in the call.

## Details

In general, every call that is multi-line has a line break after the
opening parenthesis. Exceptions:

- The token right after the parenthesis is a comment, then, the line
  should be broken after the comment only. Governed by
  `except_token_after`.

- The name of the function called is
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html) or similar, where we
  can allow the condition on the same line as the function name, and we
  don't impose rules on the line breaks for the subsequent arguments.
  Governed by `except_text_before`.

- Some calls like [`switch()`](https://rdrr.io/r/base/switch.html)
  statements are always forced to become multi- line. Governed by
  `force_text_before`.
