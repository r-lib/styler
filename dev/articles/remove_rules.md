# Remove rules

If you want to change the behavior of styler to match your desired
style, there are multiple ways:

- Use the tidyverse style guide, but not with the default options.
  Starting point for this approach is the
  [`help("tidyverse_style")`](https://styler.r-lib.org/dev/reference/tidyverse_style.md)
  for the function
  [`tidyverse_style()`](https://styler.r-lib.org/dev/reference/tidyverse_style.md),
  which returns the transformer functions that prettify your code. Most
  of these options are explained in
  [`vignette("styler")`](https://styler.r-lib.org/dev/articles/styler.md).

- If you can’t get styler behaving the way you want using the arguments
  of
  [`tidyverse_style()`](https://styler.r-lib.org/dev/reference/tidyverse_style.md),
  you have another option, which is described in a
  [`vignette("customizing_styler")`](https://styler.r-lib.org/dev/articles/customizing_styler.md):
  Creating your own style guide from scratch. Yes, I admit, it’s pretty
  long and if you don’t want to become a *styler expert*, it may be a
  little bit overwhelming.

- If you don’t care about how to create new rules but you simply want to
  *remove* a rule, I have good news for you: There is a quick way to do
  it. And that’s what the remainder of this vignette focuses on.

Once you are happy with your style guide, you might want to have a look
at how to distribute it, which is described in
[`vignette("distribute_custom_style_guides")`](https://styler.r-lib.org/dev/articles/distribute_custom_style_guides.md).

## Theory

Here are the steps required to deactivate a rule you don’t like

- Figure out which transformer function in the transformers returned by
  `tidyerse_style()` corresponds to the rule you want to remove.

- Set that element in the list to `NULL`, which is equivalent to
  removing it.

- Pass the list to `style_text` as a transformer.

## Practice

Lets assume you want to remove the rule that turns `=` into `<-` for
assignment. That means you want

    string = "hi there" 

to remain unchanged after applying styler. This is not the case if you
use the default style guide of styler:

``` r

library(styler)
style_text("string = 'hi there'")
string <- "hi there"
```

So you need to figure out which rule is responsible for this. Let’s
check the transformer categories used with the tidyverse style guide.

``` r

transformers <- tidyverse_style()
names(transformers)
#>  [1] "initialize"             "line_break"             "space"                 
#>  [4] "token"                  "indention"              "use_raw_indention"     
#>  [7] "reindention"            "style_guide_name"       "style_guide_version"   
#> [10] "more_specs_style_guide" "transformers_drop"      "indent_character"
```

From the aforementioned
[vignette](https://styler.r-lib.org/articles/customizing_styler.html):

> We note that there are different types of transformer functions.
> initialize initializes some variables in the nested parse table (so it
> is not actually a transformer), and the other elements modify either
> spacing, line breaks or tokens. use_raw_indention is not a function,
> it is just an option.

Now, we can look at the names of the rules that are sub-elements of the
transformer categories.

``` r

library(magrittr)
levels <- c("space", "line_break", "indention", "token")
purrr::map(
  levels,
  ~ names(transformers[[.x]])
) %>%
  purrr::set_names(levels)
#> $space
#>  [1] "remove_space_before_closing_paren"         
#>  [2] "remove_space_before_opening_paren"         
#>  [3] "add_space_after_for_if_while"              
#>  [4] "remove_space_before_comma"                 
#>  [5] "style_space_around_math_token"             
#>  [6] "style_space_around_tilde"                  
#>  [7] "spacing_around_op"                         
#>  [8] "remove_space_after_opening_paren"          
#>  [9] "remove_space_after_excl"                   
#> [10] "set_space_after_bang_bang"                 
#> [11] "remove_space_around_dollar"                
#> [12] "remove_space_after_function_declaration"   
#> [13] "remove_space_around_colons"                
#> [14] "start_comments_with_space"                 
#> [15] "remove_space_after_unary_plus_minus_nested"
#> [16] "spacing_before_comments"                   
#> [17] "set_space_between_levels"                  
#> [18] "set_space_between_eq_sub_and_comma"        
#> [19] "set_space_in_curly"                        
#> 
#> $line_break
#>  [1] "remove_empty_lines_after_opening_and_before_closing_braces"
#>  [2] "set_line_break_around_comma_and_or"                        
#>  [3] "set_line_break_after_assignment"                           
#>  [4] "set_line_break_before_curly_opening"                       
#>  [5] "remove_line_break_before_round_closing_after_curly"        
#>  [6] "remove_line_breaks_in_function_declaration"                
#>  [7] "set_line_breaks_between_top_level_exprs"                   
#>  [8] "style_line_break_around_curly"                             
#>  [9] "set_line_break_around_curly_curly"                         
#> [10] "set_line_break_before_closing_call"                        
#> [11] "set_line_break_after_opening_if_call_is_multi_line"        
#> [12] "remove_line_break_in_fun_call"                             
#> [13] "add_line_break_after_pipe"                                 
#> [14] "set_line_break_after_ggplot2_plus"                         
#> 
#> $indention
#> [1] "indent_braces"                                  
#> [2] "unindent_function_declaration"                  
#> [3] "indent_op"                                      
#> [4] "indent_eq_sub"                                  
#> [5] "indent_without_paren"                           
#> [6] "update_indention_reference_function_declaration"
#> 
#> $token
#> [1] "fix_quotes"                                         
#> [2] "force_assignment_op"                                
#> [3] "resolve_semicolon"                                  
#> [4] "add_brackets_in_pipe"                               
#> [5] "wrap_if_else_while_for_function_multi_line_in_curly"
```

Spotted the rule we want to get rid of? It’s under `token` and it’s
called `force_assignment_op`. I agree, we could have chosen a better
name. If you are not sure if you can guess from the name of the rule
what it does you can also have a look at the function declaration of
this (unexported) function.

``` r

styler:::force_assignment_op
#> function (pd) 
#> {
#>     to_replace <- pd$token == "EQ_ASSIGN"
#>     pd$token[to_replace] <- "LEFT_ASSIGN"
#>     pd$text[to_replace] <- "<-"
#>     pd
#> }
#> <bytecode: 0x557679446410>
#> <environment: namespace:styler>
```

Next, you simply set that element to `NULL`.

``` r

transformers$token$force_assignment_op <- NULL
```

And you can use the modified transformer list as input to
[`style_text()`](https://styler.r-lib.org/dev/reference/style_text.md)

``` r

style_text("string = 'hi there'", transformers = transformers)
#> string = "hi there"
```

If you want to use it the same way as
[`tidyverse_style()`](https://styler.r-lib.org/dev/reference/tidyverse_style.md),
here’s the last step:

``` r

eq_assign_style <- function(...) {
  transformers <- tidyverse_style(...)
  transformers$token$force_assignment_op <- NULL
  transformers
}

style_text("string = 'hi there'", style = eq_assign_style)
#> string = "hi there"
```

That’s it. Note that the transformer functions and how they are returned
by
[`tidyverse_style()`](https://styler.r-lib.org/dev/reference/tidyverse_style.md)
is not part of the exposed API. This means that the order, the naming
etc. may change. Also, remember we did not add a rule to replace `<-`
with `=`, but we only removed a rule to replace `=` with `<-`, so `<-`
won’t be touched:

``` r

style_text("string <- 'hi there'", style = eq_assign_style)
#> string <- "hi there"
```

If you want to turn `<-` into `=`, you need to add a rule as described
in
[`vignette("customizing_styler")`](https://styler.r-lib.org/dev/articles/customizing_styler.md).

If you have trouble identifying a rule based on rule names,

- First write an example whose results is not the one you wanted, e.g.

``` r

code <- "
f <- function () {

return (1)
}"
```

is code that will have the first empty line in the function body removed
by styler.

- Then pinpoint the probable rule type (e.g. line breaks if you want
  less new lines).
- In a local styler clone, add e.g. a `return(pd)` at the top of the
  body to deactivate the rule quickly, or add a `print(pd)` or
  [`browser()`](https://rdrr.io/r/base/browser.html) call in the
  functions of that type (e.g. the different functions of
  `R/rules-line-breaks.R`), `load_all()`, run your example, see if that
  function made the change. move the `print(pd)` or
  [`browser()`](https://rdrr.io/r/base/browser.html) call to another
  function if not.
- Once you’ve identified the culprit (in this case
  `style_line_break_around_curly`), set it to `NULL` as shown earlier.

## Some other rules and their transformers

- You don’t like multi-line ifelse statements getting wrapped around
  curly braces: `transformers$token$wrap_if_else_multi_line_in_curly`.

- You don’t like multi-line calls to be broken before the first named
  argument:
  `transformers$line_break$set_line_break_after_opening_if_call_is_multi_line`
  (interacting with
  `transformers$line_break$set_line_break_before_closing_call`).

- You don’t like the line being broken after the pipe:
  `transformers$line_break$add_line_break_after_pipe`

- You don’t like single quotes to be replaced by double quotes:
  `transformers$space$fix_quotes`.

- You don’t like comments to start with one space:
  `transformers$space$start_comments_with_space`

I think you get the idea. I nevertheless recommend using the [tidyverse
style guide](https://style.tidyverse.org/) as is since

- it is a well-established, thought-through style.

- using a consistent style (no matter which) reduces friction in the
  community.

If you have questions, don’t hesitate to create an issue in the GitHub
repo.
