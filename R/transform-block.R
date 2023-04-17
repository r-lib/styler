#' Parse, transform and serialize a nested parse table
#'
#' We process blocks of nested parse tables for speed. See [cache_find_block()]
#' for details on how a top-level nest is split into blocks.
#' @param pd_nested A block of the nested parse table.
#' @param start_line The line number on which the code starts.
#' @param base_indention Integer scalar indicating by how many spaces the whole
#'   output text should be indented. Note that this is not the same as splitting
#'   by line and add a `base_indention` spaces before the code in the case
#'   multi-line strings are present. See 'Examples'.
#' @inheritParams apply_transformers
#' @examples
#' text_in <- 'x<- function()
#' "here
#' is"
#' NULL
#' 1+ 1
#' '
#' style_text(text_in, base_indention = 3)
#' # not equal to the naive approach
#' styler:::construct_vertical(
#'   paste0(styler:::add_spaces(3), style_text(text_in), sep = "")
#' )
#' @keywords internal
parse_transform_serialize_r_block <- function(pd_nested,
                                              start_line,
                                              transformers,
                                              base_indention) {
  if (!all(pd_nested$is_cached, na.rm = TRUE) || !cache_is_activated()) {
    transformed_pd <- apply_transformers(pd_nested, transformers)
    flattened_pd <- post_visit_one(transformed_pd, extract_terminals) %>%
      enrich_terminals(transformers$use_raw_indention) %>%
      apply_ref_indention() %>%
      set_regex_indention(
        pattern = transformers$reindention$regex_pattern,
        target_indention = transformers$reindention$indention,
        comments_only = transformers$reindention$comments_only
      )
    is_on_newline <- flattened_pd$lag_newlines > 0L
    is_on_newline[1L] <- TRUE
    flattened_pd$lag_spaces[is_on_newline] <- flattened_pd$lag_spaces[is_on_newline] + base_indention
    serialized_transformed_text <- serialize_parse_data_flattened(
      flattened_pd,
      indent_character = transformers$indent_character
    )
  } else {
    serialized_transformed_text <- map2(
      c(0L, find_blank_lines_to_next_expr(pd_nested)[-1L] - 1L),
      paste0(rep_char(" ", base_indention), pd_nested$text),
      ~ c(rep("", .x), .y)
    ) %>%
      unlist(use.names = FALSE)
  }
  c(
    rep("", start_line - as.integer(start_line > 0L)),
    serialized_transformed_text
  )
}

#' Find the groups of expressions that should be processed together
#'
#' @param pd A top-level parse table.
#'
#' @details
#'
#' We want blocks to be formed according to these rules:
#'
#'  - Blocks should contain either cached or uncached expressions only. If a
#'    block contains cached expressions only, it does not have to be processed
#'    and can be returned immediately. If a block contains uncached expressions,
#'    it makes sense to put as many uncached expression in it, since processing
#'    one bigger block has less overhead than processing many smaller blocks.
#'
#'  - Multiple expressions can sit on one row, e.g. in-line comment and commands
#'    separated with ";". This creates a problem when processing each expression
#'    separately because when putting them together, we need complicated handling
#'    of line breaks between them, as it is not *a priori* clear that there is a
#'    line break separating them. To avoid this, we put top-level expressions
#'    that sit on the same line into one block, so the assumption that there is a
#'    line break between each block of expressions holds.
#'
#'  - All expressions in a stylerignore sequence must be in the same block. If
#'    that's not the case, the first expression in a block might not be a
#'    top-level terminal, but another top-level expression.
#'    [apply_stylerignore()]  joins `env_current$stylerignore`, which contains
#'    only terminals, with the first expression in a stylerignore sequence, based
#'    on the first `pos_id` in that stylerignore sequence
#'    (`first_pos_id_in_segment`).
#'
#' @param pd A top-level nest.
#' @keywords internal
cache_find_block <- function(pd) {
  first_after_cache_state_switch <- pd$is_cached != lag(pd$is_cached, default = !pd$is_cached[1L])

  not_first_on_line <- find_blank_lines_to_next_expr(pd) == 0L
  invalid_turning_point_idx <- which(
    not_first_on_line & first_after_cache_state_switch
  )

  first_on_line_idx <- which(!not_first_on_line)
  valid_replacements <- map_int(invalid_turning_point_idx, function(x) {
    first_on_line_idx[last(which(x > first_on_line_idx))]
  })

  turning_points <- setdiff(
    which(first_after_cache_state_switch),
    c(which(pd$stylerignore), invalid_turning_point_idx)
  ) %>%
    c(1L, valid_replacements) %>%
    unique() %>%
    sort()

  turning_points %>%
    unwhich(nrow(pd)) %>%
    cumsum()
}


#' Find blank lines
#'
#' What number of line breaks lay between the expressions?
#' @param pd_nested A nested parse table.
#' @return The line number on which the first token occurs.
#' @keywords internal
find_blank_lines_to_next_expr <- function(pd_nested) {
  pd_nested$line1 - lag(pd_nested$line2, default = 0L)
}

#' Number of lines between cache blocks
#'
#' This is relevant when putting expressions together into a block and preserve
#' blank lines between them. Note that because code does not need to start on
#' line 1, the first element of the output is the number of lines until the
#' first block.
#' @param pd A top-level nest.
#' @keywords internal
find_blank_lines_to_next_block <- function(pd) {
  block_boundary <- pd$block != lag(pd$block, default = 0L)
  find_blank_lines_to_next_expr(pd)[block_boundary]
}
