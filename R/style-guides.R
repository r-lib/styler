#' Style guides
#'
#' Format code according to a style guide. Style guides are the input to the
#' argument `style` in [style_file()] and friends.
#' The available style guides are:
#' * the tidyverse style guide (see [tidyverse_style()]).
#' @name style_guides
#' @keywords internal
NULL


#' The tidyverse style
#'
#' Style code according to the tidyverse style guide.
#' @param scope The extent of manipulation. Can range from "none" (least
#'   invasive) to "tokens" (most invasive). See 'Details'. This argument is a
#'   string or a vector of class `AsIs`.
#' @param indent_by How many spaces of indention should be inserted after
#'   operators such as '('.
#' @param strict A logical value indicating whether a set of strict
#'   or not so strict transformer functions should be returned. Compare the
#'   functions returned with or without `strict = TRUE`. For example,
#'   `strict = TRUE` means force *one* space e.g. after "," and *one* line break
#'   e.g. after a closing curly brace. `strict = FALSE` means to set spaces and
#'   line breaks to one if there is none and leave the code untouched otherwise.
#'   See 'Examples'.
#' @param start_comments_with_one_space Whether or not comments should start
#'   with only one space (see [start_comments_with_space()]).
#' @inheritParams create_style_guide
#' @param math_token_spacing A list of parameters that define spacing around
#'   math token, conveniently constructed using [specify_math_token_spacing()].
#' @details
#'
#' The following levels for `scope` are available:
#'
#' * "none": Performs no transformation at all.
#' * "spaces": Manipulates spacing between token on the same line.
#' * "indention": Manipulates the indention, i.e. number of spaces at the
#'   beginning of each line.
#' * "line_breaks": Manipulates line breaks between tokens.
#' * "tokens": manipulates tokens.
#'
#' `scope` can be specified in two ways:
#'
#' - As a string: In this case all less invasive scope levels are implied, e.g.
#'   "line_breaks" includes "indention", "spaces". This is brief and what most
#'   users need.
#' - As vector of class `AsIs`: Each level has to be listed explicitly by
#'   wrapping one ore more levels of the scope in [I()]. This offers more
#'   granular control at the expense of more verbosity.
#'
#' See 'Examples' for details.
#'
#' @family obtain transformers
#' @family style_guides
#' @examples
#' style_text("call( 1)", style = tidyverse_style, scope = "spaces")
#' style_text("call( 1)", transformers = tidyverse_style(strict = TRUE))
#' style_text(c("ab <- 3", "a  <-3"), strict = FALSE) # keeps alignment of "<-"
#' style_text(c("ab <- 3", "a  <-3"), strict = TRUE) # drops alignment of "<-"
#'
#' # styling line breaks only without spaces
#' style_text(c("ab <- 3", "a =3"), strict = TRUE, scope = I(c("line_breaks", "tokens")))
#' @importFrom purrr partial
#' @export
tidyverse_style <- function(scope = "tokens",
                            strict = TRUE,
                            indent_by = 2,
                            start_comments_with_one_space = FALSE,
                            reindention = tidyverse_reindention(),
                            math_token_spacing = tidyverse_math_token_spacing()) {
  args <- as.list(environment())
  scope <- scope_normalize(scope)


  indention_manipulators <- if ("indention" %in% scope) {
    lst(
      indent_braces = partial(indent_braces, indent_by = indent_by),
      unindent_fun_dec,
      indent_op = partial(indent_op, indent_by = indent_by),
      indent_eq_sub = partial(indent_eq_sub, indent_by = indent_by),
      indent_without_paren = partial(indent_without_paren,
        indent_by = indent_by
      ),
      update_indention_ref_fun_dec =
        if ("indention" %in% scope) update_indention_ref_fun_dec
    )
  }
  space_manipulators <- if ("spaces" %in% scope) {
    lst(
      remove_space_before_closing_paren,
      remove_space_before_opening_paren = if (strict) remove_space_before_opening_paren,
      add_space_after_for_if_while,
      add_space_before_brace,
      remove_space_before_comma,
      style_space_around_math_token = partial(
        style_space_around_math_token, strict,
        math_token_spacing$zero,
        math_token_spacing$one
      ),
      style_space_around_tilde = partial(
        style_space_around_tilde,
        strict = strict
      ),
      spacing_around_op = purrr::partial(set_space_around_op,
        strict = strict
      ),
      remove_space_after_opening_paren,
      remove_space_after_excl,
      set_space_after_bang_bang,
      remove_space_before_dollar,
      remove_space_after_fun_dec,
      remove_space_around_colons,
      start_comments_with_space = partial(start_comments_with_space,
        force_one = start_comments_with_one_space
      ),
      remove_space_after_unary_pm_nested,
      spacing_before_comments = if (strict) {
        set_space_before_comments
      } else {
        add_space_before_comments
      },
      set_space_between_levels,
      set_space_between_eq_sub_and_comma,
      set_space_in_curly_curly
    )
  }

  use_raw_indention <- !("indention" %in% scope)

  line_break_manipulators <- if ("line_breaks" %in% scope) {
    lst(
      set_line_break_around_comma,
      set_line_break_before_curly_opening,
      remove_line_break_before_round_closing_after_curly =
        if (strict) remove_line_break_before_round_closing_after_curly,
      remove_line_breaks_in_fun_dec =
        if (strict) remove_line_breaks_in_fun_dec,
      style_line_break_around_curly = partial(
        style_line_break_around_curly,
        strict
      ),
      # must be after style_line_break_around_curly as it remove line
      # breaks again for {{.
      set_line_break_around_curly_curly,
      set_line_break_after_opening_if_call_is_multi_line = if (strict) {
        partial(
          set_line_break_after_opening_if_call_is_multi_line,
          except_token_after = "COMMENT",
          except_text_before = c("switch", "ifelse", "if_else")
        )
      },
      set_line_break_before_closing_call = if (strict) {
        partial(
          set_line_break_before_closing_call,
          except_token_before = "COMMENT"
        )
      },
      purrr::partial(remove_line_break_in_fun_call, strict = strict),
      add_line_break_after_pipe = if (strict) add_line_break_after_pipe,
      set_linebreak_after_ggplot2_plus = if (strict) set_linebreak_after_ggplot2_plus
    )
  }

  token_manipulators <- if ("tokens" %in% scope) {
    lst(
      fix_quotes,
      force_assignment_op,
      resolve_semicolon,
      add_brackets_in_pipe,
      remove_terminal_token_before_and_after,
      wrap_if_else_while_for_fun_multi_line_in_curly =
        if (strict) wrap_if_else_while_for_fun_multi_line_in_curly
    )
  }

  style_guide_name <- "styler::tidyverse_style@https://github.com/r-lib"
  create_style_guide(
    # transformer functions
    initialize             = default_style_guide_attributes,
    line_break             =        line_break_manipulators,
    space                  =             space_manipulators,
    token                  =             token_manipulators,
    indention              =         indention_manipulators,
    # transformer options
    use_raw_indention      =              use_raw_indention,
    reindention            =                    reindention,
    style_guide_name       =               style_guide_name,
    style_guide_version    =                 styler_version,
    more_specs_style_guide =                           args
  )
}

#' Create a style guide
#'
#' This is a helper function to create a style guide, which is technically
#' speaking a named list of groups of transformer functions where each
#' transformer function corresponds to one styling rule. The output of this
#' function can be used as an argument for `style` in top level functions
#' like [style_text()] and friends. Note that for caching to work properly,
#' unquote all inputs to the transformer function if possible with rlang's `!!`,
#' otherwise, they will be passed as references (generic variable names) instead
#' of literals and `styler:::is_cached()` won't pick up changes. See how it's
#' done in [tidyverse_style()] with `indent_by` and other arguments.
#' @param initialize The bare name of a function that initializes various
#'   variables on each level of nesting.
#' @param line_break A list of transformer functions that manipulate line_break
#'   information.
#' @param space A list of transformer functions that manipulate spacing
#'   information.
#' @param token A list of transformer functions that manipulate token text.
#' @param indention A list of transformer functions that manipulate indention.
#' @param use_raw_indention Boolean indicating whether or not the raw indention
#'   should be used.
#' @param reindention A list of parameters for regex re-indention, most
#'   conveniently constructed using [specify_reindention()].
#' @param style_guide_name The name of the style guide. Used as a meta attribute
#'   inside the created style guide, for example for caching. By convention,
#'   this is the style guide qualified by the package namespace plus the
#'   location of the style guide, separated by `@`. For example,
#'   `"styler::tidyverse_style@https://github.com/r-lib"`.
#' @param style_guide_version The version of the style guide. Used as a meta
#'   attribute inside the created style guide, for example for caching. This
#'   should correspond to the version of the R package that exports the
#'   style guide.
#' @param more_specs_style_guide Named vector (coercible to character)
#'   specifying arguments `args` in
#'   `transformer <- list(t1 = purrr::partial(f, arg)` because when
#'   such functions are converted to characters in [styler::cache_make_key()],
#'   they will yield generic code and we loose the specific value of `arg` (see
#'   [styler::cache_make_key()]), even when unquoting these inputs with `!!`
#'   beforehand in `purrr::partial()`.
#' @examples
#' set_line_break_before_curly_opening <- function(pd_flat) {
#'   op <- pd_flat$token %in% "'{'"
#'   pd_flat$lag_newlines[op] <- 1L
#'   pd_flat
#' }
#' set_line_break_before_curly_opening_style <- function() {
#'   create_style_guide(
#'     line_break = tibble::lst(set_line_break_before_curly_opening)
#'   )
#' }
#' style_text(
#'   "a <- function(x) { x }",
#'   style = set_line_break_before_curly_opening_style
#' )
#' @importFrom purrr compact
#' @export
create_style_guide <- function(initialize = default_style_guide_attributes,
                               line_break = NULL,
                               space = NULL,
                               token = NULL,
                               indention = NULL,
                               use_raw_indention = FALSE,
                               reindention = tidyverse_reindention(),
                               style_guide_name = NULL,
                               style_guide_version = NULL,
                               more_specs_style_guide = NULL) {
  lst(
    # transformer functions
    initialize = lst(initialize),
    line_break,
    space,
    token,
    indention,
    # transformer options
    use_raw_indention,
    reindention,
    style_guide_name,
    style_guide_version,
    more_specs_style_guide
  ) %>%
    map(compact)
}


#' Specify what is re-indented how
#'
#' This function returns a list that can be used as an input for the argument
#' `reindention` of the function [tidyverse_style()]. It features sensible
#' defaults, so the user can specify deviations from them conveniently without
#' the need of setting all arguments explicitly.
#' @param regex_pattern Character vector with regular expression patterns that
#'   are to be re-indented with spaces, `NULL` if no reindention needed.
#' @param indention The indention tokens should have if they match
#'   `regex_pattern`.
#' @param comments_only Whether the `regex_reindention_pattern` should only be
#'   matched against comments or against all tokens. Mainly added for
#'   performance.
#' @name reindention
NULL

#' @describeIn reindention Allows to specify which tokens are reindented and
#'   how.
#' @examples
#' style_text("a <- xyz", reindention = specify_reindention(
#'   regex_pattern = "xyz", indention = 4, comments_only = FALSE
#' ))
#' @export
specify_reindention <- function(regex_pattern = NULL,
                                indention = 0,
                                comments_only = TRUE) {
  lst(
    regex_pattern,
    indention,
    comments_only
  )
}

#' @describeIn reindention Simple forwarder to
#' `specify_reindention` with reindention according to the tidyverse style
#' guide.
#' @examples
#' style_text("a <- xyz", reindention = tidyverse_reindention())
#' @export
tidyverse_reindention <- function() {
  specify_reindention(
    regex_pattern = NULL, indention = 0, comments_only = TRUE
  )
}

#' Convert the styling scope to its lower-level representation
#'
#' If `scope` is of class `character` and of length one, the value of the
#' argument and all less-invasive levels are included too (e.g.
#' styling tokens includes styling spaces). If
#' `scope` is of class `AsIs`, every level to be included has to be declared
#' individually. See compare [tidyverse_style()] for the possible levels and
#' their order.
#' @param scope A character vector of length one or a vector of class `AsIs`.
#' @param name The name of the character vector to be displayed if the
#'   construction of the factor fails.
#' @keywords internal
#' @importFrom rlang abort
scope_normalize <- function(scope, name = substitute(scope)) {
  levels <- c("none", "spaces", "indention", "line_breaks", "tokens")
  if (!all((scope %in% levels))) {
    abort(paste(
      "all values in", name, "must be one of the following:",
      paste(levels, collapse = ", ")
    ))
  }

  if (inherits(scope, "AsIs")) {
    factor(as.character(scope), levels = levels, ordered = TRUE)
  } else if (length(scope) == 1) {
    scope <- levels[as.logical(rev(cumsum(scope == rev(levels))))]
    factor(scope, levels = levels, ordered = TRUE)
  } else {
    abort(
      "argument `scope` has to be either of class `AsIs` or length one."
    )
  }
}

#' Specify spacing around math tokens
#'
#' Helper function to create the input for the argument `math_token_spacing`  in
#' [tidyverse_style()].
#' @inheritParams style_space_around_math_token
#' @examples
#' style_text(
#'   "1+1   -3",
#'   math_token_spacing = specify_math_token_spacing(zero = "'+'"),
#'   strict = FALSE
#' )
#' style_text(
#'   "1+1   -3",
#'   math_token_spacing = specify_math_token_spacing(zero = "'+'"),
#'   strict = TRUE
#' )
#' @name math_token_spacing
NULL

#' @describeIn math_token_spacing Allows to fully specify the math token
#'   spacing.
#' @export
specify_math_token_spacing <-
  function(zero = "'^'",
           one = c("'+'", "'-'", "'*'", "'/'")) {
    assert_tokens(c(one, zero))
    lst(
      one = setdiff(c(math_token, one), zero),
      zero
    )
  }

#' @describeIn math_token_spacing Simple forwarder to
#' `specify_math_token_spacing` with spacing around math tokens according to the
#' tidyverse style guide.
#' @examples
#' style_text(
#'   "1+1   -3",
#'   math_token_spacing = tidyverse_math_token_spacing(),
#'   strict = FALSE
#' )
#' style_text(
#'   "1+1   -3",
#'   math_token_spacing = tidyverse_math_token_spacing(),
#'   strict = TRUE
#' )
#' @export
tidyverse_math_token_spacing <- function() {
  specify_math_token_spacing(
    zero = "'^'",
    one = c("'+'", "'-'", "'*'", "'/'")
  )
}
