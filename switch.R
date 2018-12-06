code <- '
call(
  fun(1),
  2,
  3
)
'

y <-
  code %>% compute_parse_data_nested %>% create_filler_nested

y %>% create_node_from_nested_root

