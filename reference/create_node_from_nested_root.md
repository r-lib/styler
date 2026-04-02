# Convert a nested data frame into a node tree

This function is convenient to display all nesting levels of a nested
data frame at once.

## Usage

``` r
create_node_from_nested_root(pd_nested, structure_only)
```

## Arguments

- pd_nested:

  A nested data frame.

- structure_only:

  Whether or not create a tree that represents the structure of the
  expression without any information on the tokens. Useful to check
  whether two structures are identical.

## Value

An object of class "Node" and "R6".

## Examples

``` r
if (rlang::is_installed("data.tree")) {
  withr::with_options(
    list(styler.cache_name = NULL), # temporarily deactivate cache
    {
      code <- "a <- function(x) { if(x > 1) { 1+1 } else {x} }"
      nested_pd <- compute_parse_data_nested(code)
      initialized <- styler:::pre_visit_one(
        nested_pd, default_style_guide_attributes
      )
      styler:::create_node_from_nested_root(initialized,
        structure_only = FALSE
      )
    }
  )
}
#>                                                  levelName
#> 1  ROOT (token: short_text [lag_newlines/spaces] {pos_id})
#> 2   °--expr: a <-  [0/0] {1}                              
#> 3       ¦--expr: a [0/1] {3}                              
#> 4       ¦   °--SYMBOL: a [0/0] {2}                        
#> 5       ¦--LEFT_ASSIGN: <- [0/1] {4}                      
#> 6       °--expr: funct [0/0] {5}                          
#> 7           ¦--FUNCTION: funct [0/0] {6}                  
#> 8           ¦--'(': ( [0/0] {7}                           
#> 9           ¦--SYMBOL_FORMALS: x [0/0] {8}                
#> 10          ¦--')': ) [0/1] {9}                           
#> 11          °--expr: { if( [0/0] {10}                     
#> 12              ¦--'{': { [0/1] {11}                      
#> 13              ¦--expr: if(x  [0/1] {12}                 
#> 14              ¦   ¦--IF: if [0/0] {13}                  
#> 15              ¦   ¦--'(': ( [0/0] {14}                  
#> 16              ¦   ¦--expr: x > 1 [0/0] {15}             
#> 17              ¦   ¦   ¦--expr: x [0/1] {17}             
#> 18              ¦   ¦   ¦   °--SYMBOL: x [0/0] {16}       
#> 19              ¦   ¦   ¦--GT: > [0/1] {18}               
#> 20              ¦   ¦   °--expr: 1 [0/0] {20}             
#> 21              ¦   ¦       °--NUM_CONST: 1 [0/0] {19}    
#> 22              ¦   ¦--')': ) [0/1] {21}                  
#> 23              ¦   ¦--expr: { 1+1 [0/1] {22}             
#> 24              ¦   ¦   ¦--'{': { [0/1] {23}              
#> 25              ¦   ¦   ¦--expr: 1+1 [0/1] {24}           
#> 26              ¦   ¦   ¦   ¦--expr: 1 [0/0] {26}         
#> 27              ¦   ¦   ¦   ¦   °--NUM_CONST: 1 [0/0] {25}
#> 28              ¦   ¦   ¦   ¦--'+': + [0/0] {27}          
#> 29              ¦   ¦   ¦   °--expr: 1 [0/0] {29}         
#> 30              ¦   ¦   ¦       °--NUM_CONST: 1 [0/0] {28}
#> 31              ¦   ¦   °--'}': } [0/0] {30}              
#> 32              ¦   ¦--ELSE: else [0/1] {31}              
#> 33              ¦   °--expr: {x} [0/0] {32}               
#> 34              ¦       ¦--'{': { [0/0] {33}              
#> 35              ¦       ¦--expr: x [0/0] {35}             
#> 36              ¦       ¦   °--SYMBOL: x [0/0] {34}       
#> 37              ¦       °--'}': } [0/0] {36}              
#> 38              °--'}': } [0/0] {37}                      
```
