# Create node from nested parse data

Create node from nested parse data

## Usage

``` r
create_node_from_nested(pd_nested, parent, structure_only)
```

## Arguments

- pd_nested:

  A nested data frame.

- parent:

  The parent of the node to be created.

- structure_only:

  Whether or not create a tree that represents the structure of the
  expression without any information on the tokens. Useful to check
  whether two structures are identical.
