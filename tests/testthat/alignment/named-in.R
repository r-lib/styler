# algorithm: aligned. human: aligned.
call(
  x   = 1, kdd  =  2,
  xy  = 2, n    = 33,
)

# algorithm: aligned. human: aligned.
call(
  x  = 1, kdd  =  2,
  xy = 2, n    = 33,
)

# algorithm: aligned. human: aligned.
call(
  x  = 1, kdd  =  2,
  xy = 2, n    = 33,
)

# algorithm: not aligned (spacing around =). human: aligned (fix: spacing around =).
call(
  x  =1,   kdd  =  2,
  xy =2,   n    = 33,
)

# algorithm: aligned (spacing around coma cannot be detected). human: aligned
# (fix: spacing around comma).
# TODO should detect redundant space before comma (not possible currently
# because this space is hidden in child nest. Would be possible when we would
# attempt to get position of all tokens on the line right away)
call(
  x  = 1,   kdd  =  2,
  xy = 2 ,  n    = 33,
)

# algorithm: aligned. human: not aligned.
call(
  x  = 1,   kdd  =  2,
  xy = 2, n = 33,
)

# algorithm: aligned. human: aligned.
call(
  x  =  1,   kdd  =  2,
  xy = 22, n = 33,
)

# algorithm: aligned. human: not aligned.
call(
  x  = 1, d = 2,
  xy = 22, n = 33,
)


# algorithm: aligned. human: aligned.
call(
  x  = 1,   kdd  =  2, k = "abc",
  xy = 2,   n    = 33, z = "333"
)

# algorithm: aligned. human: aligned (fix spacing around =).
# FIXME need to check also text of argument value.
call(
  x  = 122,   kdd  =  2, k = "abc",
  xy = 2,   n    = 33, z = "333"
)


# algorithm: aligned. human: aligned.
call(
  x  = 1,
  xy = 2,   n    = 33, z = "333"
)

# algorithm: aligned. human: aligned.
call(
  x  = 1, n = 33, z = "333",

  xy = 2,
)

